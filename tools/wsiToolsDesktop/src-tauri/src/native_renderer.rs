//! Native WGPU renderer primitives shared by the desktop launcher and the
//! future fully native wsiTools viewer surface.
//!
//! This module deliberately consumes the R live service's compact
//! `/native-renderer` manifest. It never opens a WSI itself: R keeps ownership
//! of image backends, project state and analysis objects, while Rust requests
//! only the required Deep Zoom-compatible tile URLs.

#![allow(dead_code)] // Migration primitives activate as native controls land.

use base64::{engine::general_purpose::STANDARD as BASE64_STANDARD, Engine as _};
use geo::{BooleanOps, Coord, LineString, MultiPolygon, Polygon};
use serde::{Deserialize, Serialize};
use std::{
    cmp::{max, min},
    collections::{HashMap, HashSet, VecDeque},
    fs,
    io::Read,
    path::PathBuf,
    sync::mpsc,
    thread,
    time::{Duration, Instant, SystemTime, UNIX_EPOCH},
};
use tauri::{Runtime, WebviewWindow};
use winit::{
    application::ApplicationHandler,
    event::{ElementState, KeyEvent, MouseButton, MouseScrollDelta, WindowEvent},
    event_loop::{ActiveEventLoop, EventLoop},
    keyboard::{KeyCode, ModifiersState, PhysicalKey},
    window::{Window, WindowAttributes, WindowId},
};

pub const NATIVE_RENDERER_PROTOCOL: &str = "wsiTools-native-renderer/v1";
// Keep the live R tile backend responsive while a user is panning. A small
// queue lets newly visible tiles reach the decoder before obsolete requests.
const NATIVE_BASE_TILE_QUEUE: usize = 8;
const NATIVE_CHANNEL_TILE_QUEUE: usize = 4;

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct NativeTileSource {
    pub id: String,
    pub width: f64,
    pub height: f64,
    pub tile_size: u32,
    pub tile_overlap: u32,
    pub tile_format: String,
    pub min_level: u32,
    pub max_level: u32,
    pub label: String,
    #[serde(default)]
    pub mpp: serde_json::Value,
    #[serde(default)]
    pub objective_power: Option<f64>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct NativeDenseSource {
    pub id: String,
    #[serde(default)]
    pub name: String,
    #[serde(default)]
    pub source_type: String,
    #[serde(default = "native_visible_default")]
    pub visible: bool,
    #[serde(default)]
    pub min_zoom: f64,
    #[serde(default)]
    pub colour: String,
}

/// A tile-backed channel registered by the live R session. The tile geometry
/// remains server-side; native only keeps an independent bounded GPU cache and
/// the display controls that are already exposed in the browser viewer.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct NativeChannelSource {
    pub id: String,
    pub width: f64,
    pub height: f64,
    #[serde(default = "native_tile_size_default")]
    pub tile_size: u32,
    #[serde(default)]
    pub tile_overlap: u32,
    #[serde(default = "native_channel_format_default")]
    pub tile_format: String,
    #[serde(default)]
    pub min_level: u32,
    pub max_level: u32,
    #[serde(default)]
    pub label: String,
    #[serde(default)]
    pub source_type: String,
    #[serde(default = "native_visible_default")]
    pub visible: bool,
    #[serde(default = "native_opacity_default")]
    pub opacity: f32,
    #[serde(default = "native_channel_colour_default")]
    pub colour: String,
    #[serde(default = "native_gain_default")]
    pub gain: f32,
    #[serde(default)]
    pub contrast_min: f32,
    #[serde(default = "native_contrast_max_default")]
    pub contrast_max: f32,
    #[serde(default)]
    pub target_ids: Vec<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct NativePointSource {
    pub id: String,
    #[serde(default)]
    pub name: String,
    #[serde(default)]
    pub source_type: String,
    #[serde(default = "native_visible_default")]
    pub visible: bool,
    #[serde(default = "native_opacity_default")]
    pub opacity: f32,
    #[serde(default = "native_channel_colour_default")]
    pub colour: String,
    #[serde(default)]
    pub radius: f32,
    #[serde(default)]
    pub count: usize,
    #[serde(default)]
    pub target_ids: Vec<String>,
}

impl NativePointSource {
    fn matches_source(&self, source: &NativeTileSource) -> bool {
        self.target_ids.is_empty() || self.target_ids.iter().any(|target| target == &source.id)
    }
}

impl NativeChannelSource {
    fn tile_source(&self) -> NativeTileSource {
        NativeTileSource {
            id: self.id.clone(),
            width: self.width,
            height: self.height,
            tile_size: self.tile_size,
            tile_overlap: self.tile_overlap,
            tile_format: self.tile_format.clone(),
            min_level: self.min_level,
            max_level: self.max_level,
            label: self.label.clone(),
            mpp: serde_json::Value::Null,
            objective_power: None,
        }
    }

    fn matches_source(&self, source: &NativeTileSource) -> bool {
        self.target_ids.is_empty() || self.target_ids.iter().any(|target| target == &source.id)
    }
}

fn native_tile_size_default() -> u32 {
    512
}
fn native_channel_format_default() -> String {
    "png".to_string()
}
fn native_opacity_default() -> f32 {
    1.0
}
fn native_channel_colour_default() -> String {
    "#ffffff".to_string()
}
fn native_gain_default() -> f32 {
    1.0
}
fn native_contrast_max_default() -> f32 {
    1.0
}

fn native_visible_default() -> bool {
    true
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct NativeRendererManifest {
    pub protocol: String,
    pub tile_route: String,
    pub state_route: String,
    #[serde(default)]
    pub state_snapshot_route: String,
    #[serde(default)]
    pub dense_geojson_route: String,
    #[serde(default)]
    pub native_points_route: String,
    #[serde(default)]
    pub spatial_gene_route: String,
    #[serde(default)]
    pub prediction_route: String,
    #[serde(default)]
    pub prediction_enabled: bool,
    #[serde(default)]
    pub prediction: serde_json::Value,
    #[serde(default)]
    pub proximity_route: String,
    #[serde(default)]
    pub image_export_route: String,
    #[serde(default)]
    pub proximity_enabled: bool,
    #[serde(default)]
    pub segmentation_run_url: String,
    #[serde(default)]
    pub spatial_clusters: serde_json::Value,
    /// Sampled 2D spatial reductions. R keeps all expression values and sends
    /// only browser-equivalent plot points plus stable observation IDs.
    #[serde(default)]
    pub spatial: serde_json::Value,
    #[serde(default)]
    pub cellphenotyper: serde_json::Value,
    pub source_count: usize,
    #[serde(deserialize_with = "deserialize_native_sources")]
    pub sources: Vec<NativeTileSource>,
    #[serde(default, deserialize_with = "deserialize_native_dense_sources")]
    pub dense_sources: Vec<NativeDenseSource>,
    #[serde(default, deserialize_with = "deserialize_native_channel_sources")]
    pub channel_sources: Vec<NativeChannelSource>,
    #[serde(default, deserialize_with = "deserialize_native_point_sources")]
    pub point_sources: Vec<NativePointSource>,
}

fn deserialize_native_point_sources<'de, D>(
    deserializer: D,
) -> Result<Vec<NativePointSource>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    #[derive(Deserialize)]
    #[serde(untagged)]
    enum Sources {
        List(Vec<NativePointSource>),
        Named(HashMap<String, NativePointSource>),
        Null(Option<()>),
    }
    match Sources::deserialize(deserializer)? {
        Sources::List(sources) => Ok(sources),
        Sources::Named(sources) => Ok(sources.into_values().collect()),
        Sources::Null(_) => Ok(Vec::new()),
    }
}

fn deserialize_native_channel_sources<'de, D>(
    deserializer: D,
) -> Result<Vec<NativeChannelSource>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    #[derive(Deserialize)]
    #[serde(untagged)]
    enum Sources {
        List(Vec<NativeChannelSource>),
        Named(HashMap<String, NativeChannelSource>),
        Null(Option<()>),
    }
    match Sources::deserialize(deserializer)? {
        Sources::List(sources) => Ok(sources),
        Sources::Named(sources) => Ok(sources.into_values().collect()),
        Sources::Null(_) => Ok(Vec::new()),
    }
}

fn deserialize_native_dense_sources<'de, D>(
    deserializer: D,
) -> Result<Vec<NativeDenseSource>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    #[derive(Deserialize)]
    #[serde(untagged)]
    enum Sources {
        List(Vec<NativeDenseSource>),
        Named(HashMap<String, NativeDenseSource>),
        Null(Option<()>),
    }
    match Sources::deserialize(deserializer)? {
        Sources::List(sources) => Ok(sources),
        Sources::Named(sources) => Ok(sources.into_values().collect()),
        Sources::Null(_) => Ok(Vec::new()),
    }
}

fn deserialize_native_sources<'de, D>(deserializer: D) -> Result<Vec<NativeTileSource>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    #[derive(Deserialize)]
    #[serde(untagged)]
    enum Sources {
        List(Vec<NativeTileSource>),
        Named(HashMap<String, NativeTileSource>),
    }
    match Sources::deserialize(deserializer)? {
        Sources::List(sources) => Ok(sources),
        // R's `jsonlite` preserves a named list as an object. The source id is
        // already part of the value, so the name is solely transport detail.
        Sources::Named(sources) => Ok(sources.into_values().collect()),
    }
}

/// The only network surface used by the native viewer. R owns image backends
/// and project state; Rust follows the tile route advertised by the session.
#[derive(Clone, Debug)]
pub struct NativeTileEndpoint {
    base_url: String,
    tile_route: String,
    state_route: String,
    state_snapshot_route: String,
    dense_geojson_route: String,
    native_points_route: String,
    spatial_gene_route: String,
    prediction_route: String,
    proximity_route: String,
    image_export_route: String,
    segmentation_run_url: String,
}

impl NativeTileEndpoint {
    pub fn from_live_viewer(
        viewer_url: &str,
        manifest: &NativeRendererManifest,
    ) -> Result<Self, String> {
        let trimmed = viewer_url.trim();
        let scheme_end = trimmed.find("://").ok_or_else(|| {
            "Native viewer requires a live http://localhost viewer URL.".to_string()
        })?;
        let after_scheme = &trimmed[(scheme_end + 3)..];
        let authority_end = after_scheme.find('/').unwrap_or(after_scheme.len());
        let authority = &after_scheme[..authority_end];
        if authority.is_empty() {
            return Err("Native viewer requires a localhost viewer URL.".to_string());
        }
        let route = manifest.tile_route.trim_matches('/');
        if route.is_empty() {
            return Err("Native renderer manifest did not provide a tile route.".to_string());
        }
        Ok(Self {
            base_url: format!("{}://{authority}", &trimmed[..scheme_end]),
            tile_route: route.to_string(),
            state_route: manifest.state_route.trim_matches('/').to_string(),
            state_snapshot_route: manifest.state_snapshot_route.trim_matches('/').to_string(),
            dense_geojson_route: manifest.dense_geojson_route.trim_matches('/').to_string(),
            native_points_route: manifest.native_points_route.trim_matches('/').to_string(),
            spatial_gene_route: manifest.spatial_gene_route.trim_matches('/').to_string(),
            prediction_route: manifest.prediction_route.trim_matches('/').to_string(),
            proximity_route: manifest.proximity_route.trim_matches('/').to_string(),
            image_export_route: manifest.image_export_route.trim_matches('/').to_string(),
            segmentation_run_url: manifest.segmentation_run_url.clone(),
        })
    }

    pub fn url(&self, source: &NativeTileSource, key: TileKey) -> String {
        format!(
            "{}/{}/{}/{}/{}/{}.{}",
            self.base_url,
            self.tile_route,
            source.id,
            key.level,
            key.column,
            key.row,
            source.tile_format
        )
    }

    pub fn state_url(&self) -> Result<String, String> {
        if self.state_route.is_empty() {
            return Err("Native renderer manifest did not provide a state route.".to_string());
        }
        Ok(format!("{}/{}", self.base_url, self.state_route))
    }

    pub fn state_snapshot_url(&self) -> Result<String, String> {
        if self.state_snapshot_route.is_empty() {
            return Err(
                "Native renderer manifest did not provide a state snapshot route.".to_string(),
            );
        }
        Ok(format!("{}/{}", self.base_url, self.state_snapshot_route))
    }

    pub fn dense_geojson_url(&self) -> Result<String, String> {
        if self.dense_geojson_route.is_empty() {
            return Err(
                "Native renderer manifest did not provide a dense GeoJSON route.".to_string(),
            );
        }
        Ok(format!("{}/{}", self.base_url, self.dense_geojson_route))
    }

    pub fn native_points_url(&self) -> Result<String, String> {
        if self.native_points_route.is_empty() {
            return Err(
                "Native renderer manifest did not provide a viewport-point route.".to_string(),
            );
        }
        Ok(format!("{}/{}", self.base_url, self.native_points_route))
    }

    pub fn spatial_gene_url(&self) -> Result<String, String> {
        if self.spatial_gene_route.is_empty() {
            return Err("This live R session did not expose a spatial gene route.".to_string());
        }
        Ok(format!("{}/{}", self.base_url, self.spatial_gene_route))
    }

    pub fn prediction_url(&self) -> Result<String, String> {
        if self.prediction_route.is_empty() {
            return Err("This live R session did not expose a prediction route.".to_string());
        }
        Ok(format!("{}/{}", self.base_url, self.prediction_route))
    }

    pub fn proximity_url(&self) -> Result<String, String> {
        if self.proximity_route.is_empty() {
            return Err("This live R session did not expose a proximity route.".to_string());
        }
        Ok(format!("{}/{}", self.base_url, self.proximity_route))
    }

    pub fn image_export_url(&self) -> Result<String, String> {
        if self.image_export_route.is_empty() {
            return Err("This live R session did not expose an image-export route.".to_string());
        }
        Ok(format!("{}/{}", self.base_url, self.image_export_route))
    }
}

#[derive(Clone, Debug, Deserialize)]
pub struct NativeRendererState {
    pub protocol: String,
    #[serde(default)]
    pub event: String,
    #[serde(default)]
    pub source_id: String,
    #[serde(default)]
    pub revision: u64,
    #[serde(default)]
    pub annotations: serde_json::Value,
    #[serde(default)]
    pub annotations_total: usize,
    #[serde(default)]
    pub annotations_truncated: bool,
    #[serde(default)]
    pub segmentation: serde_json::Value,
    #[serde(default)]
    pub segmentation_total: usize,
    #[serde(default)]
    pub segmentation_truncated: bool,
    #[serde(default)]
    pub selected_roi: Option<serde_json::Value>,
    #[serde(default)]
    pub trajectories: serde_json::Value,
    #[serde(default)]
    pub measurements: serde_json::Value,
    #[serde(default)]
    pub layers: serde_json::Value,
    #[serde(default)]
    pub channel_settings: serde_json::Value,
    #[serde(default)]
    pub stain: serde_json::Value,
    #[serde(default, deserialize_with = "deserialize_native_dense_sources")]
    pub dense_sources: Vec<NativeDenseSource>,
    #[serde(default)]
    pub project: serde_json::Value,
    #[serde(default)]
    pub view: serde_json::Value,
}

pub fn fetch_renderer_state(
    endpoint: &NativeTileEndpoint,
    source_id: Option<&str>,
) -> Result<NativeRendererState, String> {
    let mut url = endpoint.state_snapshot_url()?;
    if let Some(source_id) = source_id.filter(|id| !id.trim().is_empty()) {
        let separator = if url.contains('?') { '&' } else { '?' };
        url.push(separator);
        url.push_str("source_id=");
        url.push_str(&native_url_encode_component(source_id));
    }
    let state: NativeRendererState = ureq::get(&url)
        .timeout(std::time::Duration::from_secs(10))
        .call()
        .map_err(|error| format!("Could not request native renderer state at {url}: {error}"))?
        .into_json()
        .map_err(|error| format!("Native renderer state was not valid JSON: {error}"))?;
    if state.protocol != "wsiTools-native-renderer-state/v1" {
        return Err(format!(
            "Unsupported native renderer state protocol `{}`.",
            state.protocol
        ));
    }
    Ok(state)
}

fn native_url_encode_component(value: &str) -> String {
    value
        .bytes()
        .flat_map(|byte| match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' => {
                vec![byte as char]
            }
            _ => format!("%{byte:02X}").chars().collect(),
        })
        .collect()
}

impl NativeRendererManifest {
    pub fn validate(&self) -> Result<(), String> {
        if self.protocol != NATIVE_RENDERER_PROTOCOL {
            return Err(format!(
                "Unsupported native renderer protocol `{}`. Expected `{NATIVE_RENDERER_PROTOCOL}`.",
                self.protocol
            ));
        }
        if self.sources.is_empty() {
            return Err("The live R session did not expose a native tile source.".to_string());
        }
        for source in &self.sources {
            if source.id.trim().is_empty()
                || !source.width.is_finite()
                || !source.height.is_finite()
                || source.width <= 0.0
                || source.height <= 0.0
                || source.tile_size == 0
                || source.max_level < source.min_level
            {
                return Err(format!(
                    "Invalid tile source in native manifest: `{}`.",
                    source.id
                ));
            }
        }
        for source in &self.channel_sources {
            if source.id.trim().is_empty()
                || !source.width.is_finite()
                || !source.height.is_finite()
                || source.width <= 0.0
                || source.height <= 0.0
                || source.tile_size == 0
                || source.max_level < source.min_level
            {
                return Err(format!(
                    "Invalid native channel tile source in manifest: `{}`.",
                    source.id
                ));
            }
        }
        Ok(())
    }

    pub fn source(&self, id: Option<&str>) -> Result<&NativeTileSource, String> {
        self.validate()?;
        match id {
            Some(id) if !id.trim().is_empty() => self
                .sources
                .iter()
                .find(|source| source.id == id)
                .ok_or_else(|| format!("Native tile source `{id}` was not found.")),
            _ => Ok(&self.sources[0]),
        }
    }
}

pub fn native_manifest_url(viewer_url: &str) -> Result<String, String> {
    let trimmed = viewer_url.trim();
    let scheme_end = trimmed.find("://").ok_or_else(|| {
        "Native rendering requires a live http://localhost viewer URL.".to_string()
    })?;
    let after_scheme = &trimmed[(scheme_end + 3)..];
    let authority_end = after_scheme.find('/').unwrap_or(after_scheme.len());
    let authority = &after_scheme[..authority_end];
    if authority.is_empty() {
        return Err("Native rendering requires a localhost viewer URL.".to_string());
    }
    Ok(format!(
        "{}://{authority}/native-renderer",
        &trimmed[..scheme_end]
    ))
}

pub fn fetch_manifest(viewer_url: &str) -> Result<NativeRendererManifest, String> {
    let url = native_manifest_url(viewer_url)?;
    let response = ureq::get(&url)
        .timeout(std::time::Duration::from_secs(10))
        .call()
        .map_err(|error| format!("Could not request native renderer metadata at {url}: {error}"))?;
    let manifest: NativeRendererManifest = response
        .into_json()
        .map_err(|error| format!("Native renderer metadata was not valid JSON: {error}"))?;
    manifest.validate()?;
    Ok(manifest)
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize)]
pub struct NativeCamera {
    /// Slide pixels represented by one window pixel.
    pub pixels_per_screen_pixel: f64,
    pub center_x: f64,
    pub center_y: f64,
}

impl NativeCamera {
    pub fn fit(source: &NativeTileSource, viewport_width: u32, viewport_height: u32) -> Self {
        Self::fit_dimensions(source.width, source.height, viewport_width, viewport_height)
    }

    pub fn fit_dimensions(
        width: f64,
        height: f64,
        viewport_width: u32,
        viewport_height: u32,
    ) -> Self {
        let width_scale = width / max(1, viewport_width) as f64;
        let height_scale = height / max(1, viewport_height) as f64;
        Self {
            pixels_per_screen_pixel: width_scale.max(height_scale).max(1e-9),
            center_x: width / 2.0,
            center_y: height / 2.0,
        }
    }

    pub fn zoom_about(
        &mut self,
        factor: f64,
        cursor_x: f64,
        cursor_y: f64,
        viewport_width: u32,
        viewport_height: u32,
    ) {
        if !factor.is_finite() || factor <= 0.0 {
            return;
        }
        let before = self.slide_at(cursor_x, cursor_y, viewport_width, viewport_height);
        self.pixels_per_screen_pixel = (self.pixels_per_screen_pixel / factor).max(1e-6);
        let after = self.slide_at(cursor_x, cursor_y, viewport_width, viewport_height);
        self.center_x += before.0 - after.0;
        self.center_y += before.1 - after.1;
    }

    pub fn pan_screen_pixels(&mut self, dx: f64, dy: f64) {
        self.center_x -= dx * self.pixels_per_screen_pixel;
        self.center_y -= dy * self.pixels_per_screen_pixel;
    }

    pub fn slide_at(
        &self,
        screen_x: f64,
        screen_y: f64,
        viewport_width: u32,
        viewport_height: u32,
    ) -> (f64, f64) {
        (
            self.center_x + (screen_x - viewport_width as f64 / 2.0) * self.pixels_per_screen_pixel,
            self.center_y
                + (screen_y - viewport_height as f64 / 2.0) * self.pixels_per_screen_pixel,
        )
    }

    pub fn screen_at(
        &self,
        slide_x: f64,
        slide_y: f64,
        viewport_width: u32,
        viewport_height: u32,
    ) -> (f64, f64) {
        (
            viewport_width as f64 / 2.0 + (slide_x - self.center_x) / self.pixels_per_screen_pixel,
            viewport_height as f64 / 2.0 + (slide_y - self.center_y) / self.pixels_per_screen_pixel,
        )
    }

    pub fn viewport_bounds(
        &self,
        viewport_width: u32,
        viewport_height: u32,
    ) -> (f64, f64, f64, f64) {
        let top_left = self.slide_at(0.0, 0.0, viewport_width, viewport_height);
        let bottom_right = self.slide_at(
            viewport_width as f64,
            viewport_height as f64,
            viewport_width,
            viewport_height,
        );
        (top_left.0, top_left.1, bottom_right.0, bottom_right.1)
    }
}

fn native_source_mpp(source: &NativeTileSource) -> Option<f64> {
    let value = &source.mpp;
    let direct = value
        .as_f64()
        .or_else(|| value.as_str().and_then(|text| text.parse().ok()));
    let object = value.as_object();
    let from_object = object.and_then(|item| {
        item.get("x")
            .or_else(|| item.get("mpp_x"))
            .or_else(|| item.get("microns_per_pixel"))
            .or_else(|| item.get("value"))
            .and_then(serde_json::Value::as_f64)
    });
    direct
        .or(from_object)
        .filter(|mpp| mpp.is_finite() && *mpp > 0.0)
}

fn native_base_magnification(source: &NativeTileSource) -> f64 {
    if let Some(power) = source
        .objective_power
        .filter(|power| power.is_finite() && *power > 0.0)
    {
        power
    } else {
        native_source_mpp(source)
            .map(|mpp| 10.0 / mpp)
            .unwrap_or(40.0)
    }
}

fn native_nice_scale_length(target: f64) -> f64 {
    if !target.is_finite() || target <= 0.0 {
        return 0.0;
    }
    let exponent = target.log10().floor();
    let unit = 10f64.powf(exponent);
    let fraction = target / unit;
    let nice = if fraction >= 5.0 {
        5.0
    } else if fraction >= 2.0 {
        2.0
    } else {
        1.0
    };
    nice * unit
}

fn native_scale_label(microns: f64) -> String {
    if microns >= 1000.0 {
        format!("{:.2} mm", microns / 1000.0).replace(".00", "")
    } else {
        format!("{:.0} um", microns)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct TileKey {
    pub level: u32,
    pub column: u32,
    pub row: u32,
}

/// The first native tile request is a single tile that retains useful overview
/// detail. The absolute Deep Zoom minimum can be a 1x1 average-colour tile,
/// which is not a useful preview. Select the lowest level whose whole slide
/// fits in approximately one normal tile instead.
pub fn native_overview_tile(source: &NativeTileSource) -> TileKey {
    let dimension_ratio = (source.width.max(source.height) / source.tile_size.max(1) as f64)
        .max(1.0);
    let downsample_levels = dimension_ratio.log2().ceil().max(0.0) as u32;
    let level = source
        .max_level
        .saturating_sub(downsample_levels)
        .max(source.min_level);
    TileKey {
        level,
        column: 0,
        row: 0,
    }
}

pub fn level_for_camera(source: &NativeTileSource, camera: NativeCamera) -> u32 {
    let desired_downsample = camera.pixels_per_screen_pixel.max(1.0);
    let desired_from_max = source.max_level as f64 - desired_downsample.log2();
    desired_from_max
        .round()
        .clamp(source.min_level as f64, source.max_level as f64) as u32
}

pub fn visible_tiles(
    source: &NativeTileSource,
    camera: NativeCamera,
    viewport_width: u32,
    viewport_height: u32,
) -> Vec<TileKey> {
    let level = level_for_camera(source, camera);
    let downsample = 2f64.powi((source.max_level - level) as i32);
    let (xmin, ymin) = camera.slide_at(0.0, 0.0, viewport_width, viewport_height);
    let (xmax, ymax) = camera.slide_at(
        viewport_width as f64,
        viewport_height as f64,
        viewport_width,
        viewport_height,
    );
    let tile_span = source.tile_size as f64 * downsample;
    let cols = (source.width / tile_span).ceil().max(1.0) as i32;
    let rows = (source.height / tile_span).ceil().max(1.0) as i32;
    let first_col = max(0, (xmin / tile_span).floor() as i32 - 1);
    let last_col = min(cols - 1, (xmax / tile_span).floor() as i32 + 1);
    let first_row = max(0, (ymin / tile_span).floor() as i32 - 1);
    let last_row = min(rows - 1, (ymax / tile_span).floor() as i32 + 1);
    let mut output = Vec::new();
    for row in first_row..=last_row {
        for column in first_col..=last_col {
            output.push(TileKey {
                level,
                column: column as u32,
                row: row as u32,
            });
        }
    }
    output
}

fn tile_intersects_viewport(
    source: &NativeTileSource,
    key: TileKey,
    camera: NativeCamera,
    viewport_width: u32,
    viewport_height: u32,
) -> bool {
    let downsample = 2f64.powi((source.max_level - key.level) as i32);
    let span = source.tile_size as f64 * downsample;
    let left = key.column as f64 * span;
    let top = key.row as f64 * span;
    let right = (left + span).min(source.width);
    let bottom = (top + span).min(source.height);
    let (view_left, view_top) = camera.slide_at(0.0, 0.0, viewport_width, viewport_height);
    let (view_right, view_bottom) = camera.slide_at(
        viewport_width as f64,
        viewport_height as f64,
        viewport_width,
        viewport_height,
    );
    right > view_left && left < view_right && bottom > view_top && top < view_bottom
}

/// Bounded request plan for one native render frame. Keeping this independent
/// of GPU resources lets the scheduler reject obsolete pan/zoom work before a
/// JPEG/PNG decoder or texture allocation is touched.
pub fn native_tile_request_plan(
    source: &NativeTileSource,
    camera: NativeCamera,
    viewport_width: u32,
    viewport_height: u32,
    maximum_tiles: usize,
) -> Vec<TileKey> {
    let mut keys = visible_tiles(source, camera, viewport_width, viewport_height);
    let level = level_for_camera(source, camera);
    let center_col = (camera.center_x
        / (source.tile_size as f64 * 2f64.powi((source.max_level - level) as i32)))
    .floor();
    let center_row = (camera.center_y
        / (source.tile_size as f64 * 2f64.powi((source.max_level - level) as i32)))
    .floor();
    keys.sort_by(|left, right| {
        let left_distance =
            (left.column as f64 - center_col).powi(2) + (left.row as f64 - center_row).powi(2);
        let right_distance =
            (right.column as f64 - center_col).powi(2) + (right.row as f64 - center_row).powi(2);
        left_distance.total_cmp(&right_distance)
    });
    keys.truncate(maximum_tiles.max(1));
    keys
}

fn native_tile_request_plan_transformed(
    source: &NativeTileSource,
    transform: NativeImageTransform,
    camera: NativeCamera,
    viewport_width: u32,
    viewport_height: u32,
    maximum_tiles: usize,
) -> Vec<TileKey> {
    let level = level_for_camera(source, camera);
    let downsample = 2f64.powi((source.max_level - level) as i32);
    let tile_span = source.tile_size as f64 * downsample;
    let (display_xmin, display_ymin, display_xmax, display_ymax) =
        camera.viewport_bounds(viewport_width, viewport_height);
    let corners = [
        (display_xmin, display_ymin),
        (display_xmax, display_ymin),
        (display_xmax, display_ymax),
        (display_xmin, display_ymax),
    ]
    .into_iter()
    .map(|point| transform.inverse_map_point(source, point))
    .collect::<Vec<_>>();
    let xmin = corners
        .iter()
        .map(|point| point.0)
        .fold(f64::INFINITY, f64::min)
        .clamp(0.0, source.width);
    let ymin = corners
        .iter()
        .map(|point| point.1)
        .fold(f64::INFINITY, f64::min)
        .clamp(0.0, source.height);
    let xmax = corners
        .iter()
        .map(|point| point.0)
        .fold(f64::NEG_INFINITY, f64::max)
        .clamp(0.0, source.width);
    let ymax = corners
        .iter()
        .map(|point| point.1)
        .fold(f64::NEG_INFINITY, f64::max)
        .clamp(0.0, source.height);
    let cols = (source.width / tile_span).ceil().max(1.0) as i32;
    let rows = (source.height / tile_span).ceil().max(1.0) as i32;
    let first_col = max(0, (xmin / tile_span).floor() as i32 - 1);
    let last_col = min(cols - 1, (xmax / tile_span).floor() as i32 + 1);
    let first_row = max(0, (ymin / tile_span).floor() as i32 - 1);
    let last_row = min(rows - 1, (ymax / tile_span).floor() as i32 + 1);
    let source_center = transform.inverse_map_point(source, (camera.center_x, camera.center_y));
    let center_col = (source_center.0 / tile_span).floor();
    let center_row = (source_center.1 / tile_span).floor();
    let mut keys = Vec::new();
    for row in first_row..=last_row {
        for column in first_col..=last_col {
            keys.push(TileKey {
                level,
                column: column as u32,
                row: row as u32,
            });
        }
    }
    keys.sort_by(|left, right| {
        let left_distance =
            (left.column as f64 - center_col).powi(2) + (left.row as f64 - center_row).powi(2);
        let right_distance =
            (right.column as f64 - center_col).powi(2) + (right.row as f64 - center_row).powi(2);
        left_distance.total_cmp(&right_distance)
    });
    keys.truncate(maximum_tiles.max(1));
    keys
}

fn tile_intersects_viewport_transformed(
    source: &NativeTileSource,
    transform: NativeImageTransform,
    key: TileKey,
    camera: NativeCamera,
    viewport_width: u32,
    viewport_height: u32,
) -> bool {
    let downsample = 2f64.powi((source.max_level - key.level) as i32);
    let span = source.tile_size as f64 * downsample;
    let left = key.column as f64 * span;
    let top = key.row as f64 * span;
    let right = (left + span).min(source.width);
    let bottom = (top + span).min(source.height);
    let transformed = [(left, top), (right, top), (right, bottom), (left, bottom)]
        .into_iter()
        .map(|point| transform.map_point(source, point))
        .collect::<Vec<_>>();
    let tile_left = transformed
        .iter()
        .map(|point| point.0)
        .fold(f64::INFINITY, f64::min);
    let tile_top = transformed
        .iter()
        .map(|point| point.1)
        .fold(f64::INFINITY, f64::min);
    let tile_right = transformed
        .iter()
        .map(|point| point.0)
        .fold(f64::NEG_INFINITY, f64::max);
    let tile_bottom = transformed
        .iter()
        .map(|point| point.1)
        .fold(f64::NEG_INFINITY, f64::max);
    let (view_left, view_top, view_right, view_bottom) =
        camera.viewport_bounds(viewport_width, viewport_height);
    tile_right > view_left
        && tile_left < view_right
        && tile_bottom > view_top
        && tile_top < view_bottom
}

#[derive(Debug)]
pub struct NativeDecodedTile {
    pub source_id: String,
    pub key: TileKey,
    pub width: u32,
    pub height: u32,
    pub rgba: Vec<u8>,
}

/// One-worker bounded fetcher. A single worker avoids stampeding local image
/// backends during rapid pan/zoom; obsolete requests remain cheap to discard
/// when their result returns to the GPU event loop.
pub struct NativeTileFetcher {
    requests: mpsc::SyncSender<TileKey>,
    results: mpsc::Receiver<Result<NativeDecodedTile, String>>,
}

/// Strict, one-way state writer for the live R bridge.  The native renderer
/// sends a small allowlisted event payload; it never evaluates R expressions
/// or accepts executable content from the viewer surface.
pub struct NativeStateSync {
    requests: mpsc::SyncSender<String>,
}

/// Bounded polling reader for the compact native snapshot.  It runs off the
/// redraw thread and coalesces refresh requests in exactly the same way as
/// tile loading, so R-side overlay updates cannot make panning stutter.
pub struct NativeStateFetcher {
    requests: mpsc::SyncSender<Option<String>>,
    results: mpsc::Receiver<Result<NativeRendererState, String>>,
}

#[derive(Clone, Debug)]
struct NativeDenseRequest {
    source_id: String,
    xmin: f64,
    ymin: f64,
    xmax: f64,
    ymax: f64,
    zoom: f64,
}

#[derive(Clone, Debug, Deserialize)]
struct NativeDenseResponse {
    #[serde(default)]
    ok: bool,
    #[serde(default)]
    loaded: bool,
    #[serde(default)]
    source_id: String,
    #[serde(default)]
    layer: Option<NativeDenseLayer>,
}

#[derive(Clone, Debug, Deserialize)]
struct NativeDenseLayer {
    #[serde(default)]
    items: Vec<serde_json::Value>,
}

#[derive(Clone, Debug)]
struct NativePointRequest {
    source_id: String,
    xmin: f64,
    ymin: f64,
    xmax: f64,
    ymax: f64,
    zoom: f64,
    max_items: usize,
    spatial_transform: NativeSpatialTransform,
}

/// A compact global spatial-coordinate transform.  The actual point table
/// stays in R; the transform travels with each bounded viewport request.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
struct NativeSpatialTransform {
    #[serde(default = "native_transform_scale_default")]
    scale_x: f64,
    #[serde(default = "native_transform_scale_default")]
    scale_y: f64,
    #[serde(default)]
    offset_x: f64,
    #[serde(default)]
    offset_y: f64,
    #[serde(default)]
    rotation_degrees: f64,
    #[serde(default)]
    flip_horizontal: bool,
    #[serde(default)]
    flip_vertical: bool,
    #[serde(default)]
    center_x: f64,
    #[serde(default)]
    center_y: f64,
}

fn native_transform_scale_default() -> f64 { 1.0 }

#[derive(Clone, Debug, Deserialize)]
struct NativePointResponse {
    #[serde(default)]
    ok: bool,
    #[serde(default)]
    source_id: String,
    #[serde(default)]
    total: usize,
    #[serde(default)]
    represented: usize,
    #[serde(default)]
    stride: usize,
    #[serde(default)]
    points: Vec<NativePointItem>,
}

#[derive(Clone, Debug, Deserialize)]
struct NativePointItem {
    x: f32,
    y: f32,
    #[serde(default)]
    radius: f32,
    #[serde(default = "native_channel_colour_default")]
    colour: String,
    #[serde(default)]
    id: String,
    #[serde(default)]
    cluster_values: HashMap<String, String>,
}

/// Viewport-bounded dense geometry reader. It reuses the browser endpoint so
/// huge cell GeoJSON files remain R-side and are never transferred wholesale.
struct NativeDenseFetcher {
    requests: mpsc::SyncSender<NativeDenseRequest>,
    results: mpsc::Receiver<Result<NativeDenseResponse, String>>,
}

/// Point data shares the dense overlay discipline: R filters the active
/// viewport and the native process only receives a bounded, zoom-aware slice.
/// This keeps a million-cell object out of both the UI thread and GPU memory.
struct NativePointFetcher {
    requests: mpsc::SyncSender<NativePointRequest>,
    results: mpsc::Receiver<Result<NativePointResponse, String>>,
}

/// Runs trajectory profiling next to the complete R-side point layer. The
/// response is intentionally limited to bin summaries and point colours; the
/// native renderer never downloads the full spatial observation table.
struct NativeTrajectoryProfileFetcher {
    requests: mpsc::SyncSender<serde_json::Value>,
    results: mpsc::Receiver<Result<serde_json::Value, String>>,
}

/// Fetches only a requested gene's value-to-colour mapping from the existing
/// R endpoint. Geometry remains viewport-bounded in `NativePointFetcher`.
struct NativeGeneFetcher {
    requests: mpsc::SyncSender<String>,
    results: mpsc::Receiver<Result<NativeGeneResponse, String>>,
}

/// Sends a selected ROI to the optional local R cell-segmentation endpoint.
/// The endpoint accepts only configured StarDist/IHC/Mesmer engine names.
struct NativeSegmentationFetcher {
    requests: mpsc::SyncSender<(String, serde_json::Value)>,
    results: mpsc::Receiver<Result<serde_json::Value, String>>,
}

/// Runs a category/ROI proximity request through the allowlisted live R
/// endpoint. The native process only supplies selection identifiers; R owns
/// spatial data, annotation association, units, and statistics.
struct NativeProximityFetcher {
    requests: mpsc::SyncSender<(String, serde_json::Value)>,
    results: mpsc::Receiver<Result<(String, serde_json::Value), String>>,
}

/// Calls the existing allowlisted PLS-LDA endpoint. Feature matrices never
/// leave R; Rust sends only source/options and ROI selection identifiers.
struct NativePredictionFetcher {
    requests: mpsc::SyncSender<serde_json::Value>,
    results: mpsc::Receiver<Result<serde_json::Value, String>>,
}

/// Exports a full-resolution viewport or selected ROI through the existing R
/// endpoint. Rust supplies only an explicit save path and slide coordinates;
/// pixel reads stay in the configured R/OpenSlide/libvips backend.
struct NativeImageExportFetcher {
    requests: mpsc::SyncSender<serde_json::Value>,
    results: mpsc::Receiver<Result<serde_json::Value, String>>,
}

struct NativeGeneResponse {
    gene: String,
    colours: HashMap<String, String>,
}

impl NativeStateFetcher {
    pub fn start(endpoint: NativeTileEndpoint) -> Self {
        let (request_tx, request_rx) = mpsc::sync_channel::<Option<String>>(1);
        let (result_tx, result_rx) = mpsc::channel();
        thread::spawn(move || {
            while let Ok(source_id) = request_rx.recv() {
                if result_tx
                    .send(fetch_renderer_state(&endpoint, source_id.as_deref()))
                    .is_err()
                {
                    break;
                }
            }
        });
        Self {
            requests: request_tx,
            results: result_rx,
        }
    }

    pub fn refresh(&self, source_id: Option<String>) {
        let _ = self.requests.try_send(source_id);
    }

    pub fn try_next(&self) -> Option<Result<NativeRendererState, String>> {
        self.results.try_recv().ok()
    }
}

impl NativeDenseFetcher {
    fn start(endpoint: NativeTileEndpoint) -> Option<Self> {
        let url = endpoint.dense_geojson_url().ok()?;
        // A project can expose several independent dense layers (for example,
        // cells and tissue labels). Keep a small bounded queue so each visible
        // layer receives the same viewport without unbounded R work.
        let (request_tx, request_rx) = mpsc::sync_channel::<NativeDenseRequest>(8);
        let (result_tx, result_rx) = mpsc::channel();
        thread::spawn(move || {
            while let Ok(request) = request_rx.recv() {
                let body = serde_json::json!({
                    "source_id": request.source_id,
                    "xmin": request.xmin,
                    "ymin": request.ymin,
                    "xmax": request.xmax,
                    "ymax": request.ymax,
                    "zoom": request.zoom,
                })
                .to_string();
                let result = ureq::post(&url)
                    .set("Content-Type", "application/json")
                    .timeout(Duration::from_secs(10))
                    .send_string(&body)
                    .map_err(|error| format!("Could not fetch dense native overlay: {error}"))
                    .and_then(|response| {
                        response
                            .into_json::<NativeDenseResponse>()
                            .map_err(|error| {
                                format!("Dense native overlay was not valid JSON: {error}")
                            })
                    });
                if result_tx.send(result).is_err() {
                    break;
                }
            }
        });
        Some(Self {
            requests: request_tx,
            results: result_rx,
        })
    }

    fn request(&self, request: NativeDenseRequest) {
        let _ = self.requests.try_send(request);
    }

    fn try_next(&self) -> Option<Result<NativeDenseResponse, String>> {
        self.results.try_recv().ok()
    }
}

impl NativePointFetcher {
    fn start(endpoint: NativeTileEndpoint) -> Option<Self> {
        let url = endpoint.native_points_url().ok()?;
        let (request_tx, request_rx) = mpsc::sync_channel::<NativePointRequest>(8);
        let (result_tx, result_rx) = mpsc::channel();
        thread::spawn(move || {
            while let Ok(request) = request_rx.recv() {
                let body = serde_json::json!({
                    "source_id": request.source_id,
                    "xmin": request.xmin,
                    "ymin": request.ymin,
                    "xmax": request.xmax,
                    "ymax": request.ymax,
                    "zoom": request.zoom,
                    "max_items": request.max_items,
                    "spatial_transform": request.spatial_transform,
                })
                .to_string();
                let result = ureq::post(&url)
                    .set("Content-Type", "application/json")
                    .timeout(Duration::from_secs(10))
                    .send_string(&body)
                    .map_err(|error| format!("Could not fetch native viewport points: {error}"))
                    .and_then(|response| {
                        response
                            .into_json::<NativePointResponse>()
                            .map_err(|error| {
                                format!("Native viewport points were not valid JSON: {error}")
                            })
                    });
                if result_tx.send(result).is_err() {
                    break;
                }
            }
        });
        Some(Self {
            requests: request_tx,
            results: result_rx,
        })
    }

    fn request(&self, request: NativePointRequest) {
        let _ = self.requests.try_send(request);
    }

    fn try_next(&self) -> Option<Result<NativePointResponse, String>> {
        self.results.try_recv().ok()
    }
}

impl NativeTrajectoryProfileFetcher {
    fn start(endpoint: NativeTileEndpoint) -> Option<Self> {
        let url = endpoint.native_points_url().ok()?;
        let (request_tx, request_rx) = mpsc::sync_channel::<serde_json::Value>(1);
        let (result_tx, result_rx) = mpsc::channel();
        thread::spawn(move || {
            while let Ok(payload) = request_rx.recv() {
                let result = ureq::post(&url)
                    .set("Content-Type", "application/json")
                    .timeout(Duration::from_secs(60))
                    .send_string(&payload.to_string())
                    .map_err(|error| format!("Trajectory profile request failed: {error}"))
                    .and_then(|response| response.into_json::<serde_json::Value>()
                        .map_err(|error| format!("Trajectory profile response was not valid JSON: {error}")));
                if result_tx.send(result).is_err() { break; }
            }
        });
        Some(Self { requests: request_tx, results: result_rx })
    }

    fn request(&self, request: serde_json::Value) { let _ = self.requests.try_send(request); }
    fn try_next(&self) -> Option<Result<serde_json::Value, String>> { self.results.try_recv().ok() }
}

impl NativeGeneFetcher {
    fn start(endpoint: NativeTileEndpoint) -> Option<Self> {
        let url = endpoint.spatial_gene_url().ok()?;
        let (request_tx, request_rx) = mpsc::sync_channel::<String>(1);
        let (result_tx, result_rx) = mpsc::channel();
        thread::spawn(move || {
            while let Ok(gene) = request_rx.recv() {
                let body = serde_json::json!({ "gene": gene }).to_string();
                let result = ureq::post(&url)
                    .set("Content-Type", "application/json")
                    .timeout(Duration::from_secs(30))
                    .send_string(&body)
                    .map_err(|error| format!("Could not fetch gene expression from R: {error}"))
                    .and_then(|response| response.into_json::<serde_json::Value>()
                        .map_err(|error| format!("Spatial gene response was not valid JSON: {error}")))
                    .and_then(native_gene_response_from_json);
                if result_tx.send(result).is_err() { break; }
            }
        });
        Some(Self { requests: request_tx, results: result_rx })
    }

    fn request(&self, gene: String) { let _ = self.requests.try_send(gene); }
    fn try_next(&self) -> Option<Result<NativeGeneResponse, String>> { self.results.try_recv().ok() }
}

impl NativeSegmentationFetcher {
    fn start(run_url: String) -> Option<Self> {
        let run_url = run_url.trim().to_string();
        if run_url.is_empty() { return None; }
        let (request_tx, request_rx) = mpsc::sync_channel::<(String, serde_json::Value)>(1);
        let (result_tx, result_rx) = mpsc::channel();
        thread::spawn(move || {
            while let Ok((engine, roi)) = request_rx.recv() {
                let body = serde_json::json!({ "engine": engine, "roi": roi }).to_string();
                let result = ureq::post(&run_url)
                    .set("Content-Type", "application/json")
                    .timeout(Duration::from_secs(300))
                    .send_string(&body)
                    .map_err(|error| format!("Cell segmentation request failed: {error}"))
                    .and_then(|response| response.into_json::<serde_json::Value>()
                        .map_err(|error| format!("Cell segmentation response was not valid JSON: {error}")));
                if result_tx.send(result).is_err() { break; }
            }
        });
        Some(Self { requests: request_tx, results: result_rx })
    }

    fn request(&self, engine: String, roi: serde_json::Value) {
        let _ = self.requests.try_send((engine, roi));
    }

    fn try_next(&self) -> Option<Result<serde_json::Value, String>> {
        self.results.try_recv().ok()
    }
}

impl NativeProximityFetcher {
    fn start(endpoint: NativeTileEndpoint) -> Option<Self> {
        let url = endpoint.proximity_url().ok()?;
        let (request_tx, request_rx) = mpsc::sync_channel::<(String, serde_json::Value)>(1);
        let (result_tx, result_rx) = mpsc::channel();
        thread::spawn(move || {
            while let Ok((action, payload)) = request_rx.recv() {
                let result = serde_json::to_string(&payload)
                    .map_err(|error| format!("Could not encode proximity request: {error}"))
                    .and_then(|body| {
                        ureq::post(&url)
                            .set("Content-Type", "application/json")
                            .timeout(Duration::from_secs(120))
                            .send_string(&body)
                            .map_err(|error| format!("Proximity request failed: {error}"))
                            .and_then(|response| response.into_json::<serde_json::Value>()
                                .map_err(|error| format!("Proximity response was not valid JSON: {error}")))
                    });
                if result_tx.send(result.map(|body| (action, body))).is_err() { break; }
            }
        });
        Some(Self { requests: request_tx, results: result_rx })
    }

    fn request(&self, action: &str, payload: serde_json::Value) { let _ = self.requests.try_send((action.to_string(), payload)); }
    fn try_next(&self) -> Option<Result<(String, serde_json::Value), String>> { self.results.try_recv().ok() }
}

impl NativePredictionFetcher {
    fn start(endpoint: NativeTileEndpoint) -> Option<Self> {
        let url = endpoint.prediction_url().ok()?;
        let (request_tx, request_rx) = mpsc::sync_channel::<serde_json::Value>(1);
        let (result_tx, result_rx) = mpsc::channel();
        thread::spawn(move || {
            while let Ok(payload) = request_rx.recv() {
                let result = serde_json::to_string(&payload)
                    .map_err(|error| format!("Could not encode prediction request: {error}"))
                    .and_then(|body| {
                        ureq::post(&url)
                            .set("Content-Type", "application/json")
                            .timeout(Duration::from_secs(180))
                            .send_string(&body)
                            .map_err(|error| format!("PLS-LDA prediction request failed: {error}"))
                            .and_then(|response| response.into_json::<serde_json::Value>()
                                .map_err(|error| format!("Prediction response was not valid JSON: {error}")))
                    });
                if result_tx.send(result).is_err() { break; }
            }
        });
        Some(Self { requests: request_tx, results: result_rx })
    }
    fn request(&self, payload: serde_json::Value) { let _ = self.requests.try_send(payload); }
    fn try_next(&self) -> Option<Result<serde_json::Value, String>> { self.results.try_recv().ok() }
}

impl NativeImageExportFetcher {
    fn start(endpoint: NativeTileEndpoint) -> Option<Self> {
        let url = endpoint.image_export_url().ok()?;
        let (request_tx, request_rx) = mpsc::sync_channel::<serde_json::Value>(1);
        let (result_tx, result_rx) = mpsc::channel();
        thread::spawn(move || {
            while let Ok(payload) = request_rx.recv() {
                let result = serde_json::to_string(&payload)
                    .map_err(|error| format!("Could not encode image-export request: {error}"))
                    .and_then(|body| {
                        ureq::post(&url)
                            .set("Content-Type", "application/json")
                            .timeout(Duration::from_secs(300))
                            .send_string(&body)
                            .map_err(|error| format!("Full-resolution image export failed: {error}"))
                            .and_then(|response| response.into_json::<serde_json::Value>()
                                .map_err(|error| format!("Image-export response was not valid JSON: {error}")))
                    });
                if result_tx.send(result).is_err() { break; }
            }
        });
        Some(Self { requests: request_tx, results: result_rx })
    }
    fn request(&self, payload: serde_json::Value) { let _ = self.requests.try_send(payload); }
    fn try_next(&self) -> Option<Result<serde_json::Value, String>> { self.results.try_recv().ok() }
}

fn native_gene_response_from_json(value: serde_json::Value) -> Result<NativeGeneResponse, String> {
    let gene = value.get("gene").and_then(|x| x.as_str()).unwrap_or("").trim().to_string();
    if gene.is_empty() { return Err("R did not return a gene name.".to_string()); }
    let mut colours = HashMap::new();
    if let Some(points) = value.get("points").and_then(|x| x.as_array()) {
        for point in points {
            let colour = point.get("colour").and_then(|x| x.as_str()).unwrap_or("#d1d5db");
            for key in ["id", "barcode", "label"] {
                if let Some(id) = point.get(key).and_then(|x| x.as_str()).filter(|x| !x.is_empty()) {
                    colours.insert(id.to_string(), colour.to_string());
                }
            }
        }
    }
    if let (Some(keys), Some(values)) = (
        value.pointer("/packed_points/keys").and_then(|x| x.as_array()),
        value.pointer("/packed_points/values").and_then(|x| x.as_array()),
    ) {
        let minimum = value.pointer("/range/min").and_then(|x| x.as_f64()).unwrap_or(0.0);
        let maximum = value.pointer("/range/max").and_then(|x| x.as_f64()).unwrap_or(1.0);
        for (key, raw) in keys.iter().zip(values.iter()) {
            if let (Some(id), Some(raw)) = (key.as_str(), raw.as_f64()) {
                colours.insert(id.to_string(), native_gene_colour(raw, minimum, maximum));
            }
        }
    }
    Ok(NativeGeneResponse { gene, colours })
}

fn native_cluster_fields(value: &serde_json::Value) -> Vec<(String, String, HashMap<String, String>)> {
    value.get("fields").and_then(serde_json::Value::as_array).into_iter().flatten().filter_map(|field| {
        let id = field.get("field")?.as_str()?.trim().to_string();
        if id.is_empty() { return None; }
        let label = field.get("label").and_then(serde_json::Value::as_str).unwrap_or(&id).to_string();
        let palette = field.get("levels").and_then(serde_json::Value::as_array).into_iter().flatten().filter_map(|level| {
            Some((level.get("value")?.as_str()?.to_string(), level.get("colour")?.as_str()?.to_string()))
        }).collect();
        Some((id, label, palette))
    }).collect()
}

#[derive(Clone, Debug)]
struct NativeReductionPoint {
    label: String,
    x: f64,
    y: f64,
    colour: String,
}

#[derive(Clone, Debug)]
struct NativeReductionPlot {
    id: String,
    label: String,
    points: Vec<NativeReductionPoint>,
}

/// Read the browser-equivalent, already sampled two-dimensional reduction
/// payload. Invalid rows are discarded rather than making a native plot block
/// WSI interaction.
fn native_spatial_reduction_plots(value: &serde_json::Value) -> Vec<NativeReductionPlot> {
    value.get("plots").and_then(serde_json::Value::as_array).into_iter().flatten().filter_map(|plot| {
        let id = plot.get("id")?.as_str()?.trim().to_string();
        if id.is_empty() { return None; }
        let label = plot.get("label").and_then(serde_json::Value::as_str).unwrap_or(&id).to_string();
        let points = plot.get("points").and_then(serde_json::Value::as_array).into_iter().flatten().filter_map(|point| {
            let label = point.get("label").and_then(serde_json::Value::as_str)
                .or_else(|| point.get("spot_id").and_then(serde_json::Value::as_str))?.trim().to_string();
            let x = point.get("x")?.as_f64()?;
            let y = point.get("y")?.as_f64()?;
            if label.is_empty() || !x.is_finite() || !y.is_finite() { return None; }
            let colour = point.get("colour").and_then(serde_json::Value::as_str)
                .or_else(|| point.get("color").and_then(serde_json::Value::as_str))
                .unwrap_or("#38bdf8").to_string();
            Some(NativeReductionPoint { label, x, y, colour })
        }).collect::<Vec<_>>();
        (!points.is_empty()).then_some(NativeReductionPlot { id, label, points })
    }).collect()
}

/// Convert R's data.frame JSON representation (columns of equal-length
/// vectors) or a normal JSON row array into a compact native table. This is
/// shared by live statistics responses without requiring an expression matrix
/// in Rust.
fn native_json_table_rows(value: &serde_json::Value) -> Vec<serde_json::Value> {
    if let Some(rows) = value.as_array() {
        return rows.iter().filter(|row| row.is_object()).cloned().collect();
    }
    let Some(columns) = value.as_object() else { return Vec::new(); };
    let count = columns.values().filter_map(serde_json::Value::as_array).map(Vec::len).max().unwrap_or(0);
    (0..count).map(|index| {
        let mut row = serde_json::Map::new();
        for (name, values) in columns {
            let value = values.as_array().and_then(|items| items.get(index)).cloned().unwrap_or_else(|| values.clone());
            row.insert(name.clone(), value);
        }
        serde_json::Value::Object(row)
    }).collect()
}

fn native_prediction_sources(value: &serde_json::Value) -> Vec<(String, String, bool, usize)> {
    value.get("sources").and_then(serde_json::Value::as_array).into_iter().flatten().filter_map(|source| {
        let id = source.get("id")?.as_str()?.trim().to_string();
        if id.is_empty() { return None; }
        let label = source.get("label").and_then(serde_json::Value::as_str).unwrap_or(&id).to_string();
        let reduction = source.get("type").and_then(serde_json::Value::as_str) == Some("reduction");
        let dimensions = source.get("dimension_count").and_then(serde_json::Value::as_u64).unwrap_or(0) as usize;
        Some((id, label, reduction, dimensions))
    }).collect()
}

#[derive(Clone, Debug)]
struct NativeGrandqcItem {
    id: String,
    label: String,
    path: String,
}

impl NativeGrandqcItem {
    fn payload(&self) -> serde_json::Value {
        serde_json::json!({
            "id": self.id,
            "label": self.label,
            "path": self.path
        })
    }
}

fn native_grandqc_items(value: &serde_json::Value) -> Vec<NativeGrandqcItem> {
    value
        .get("grandqc")
        .and_then(|grandqc| grandqc.get("enabled").and_then(serde_json::Value::as_bool).filter(|enabled| *enabled).map(|_| grandqc))
        .and_then(|grandqc| grandqc.get("geojsons"))
        .and_then(serde_json::Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(|item| {
            let path = item.get("path")?.as_str()?.trim().to_string();
            if path.is_empty() {
                return None;
            }
            let id = item.get("id").and_then(serde_json::Value::as_str).unwrap_or("grandqc").to_string();
            let label = item.get("label").and_then(serde_json::Value::as_str).unwrap_or(&id).to_string();
            Some(NativeGrandqcItem { id, label, path })
        })
        .collect()
}

/// Reuse the browser viewer's restrained dark pathology palette in the native
/// shell. The WGPU canvas remains native, but surrounding controls should not
/// look like a debug application.
fn native_browser_visuals() -> egui::Visuals {
    let panel = egui::Color32::from_rgb(32, 35, 40);
    let surface = egui::Color32::from_rgb(41, 45, 51);
    let border = egui::Color32::from_rgb(69, 75, 84);
    let text = egui::Color32::from_rgb(241, 245, 249);
    let muted = egui::Color32::from_rgb(166, 179, 202);
    let accent = egui::Color32::from_rgb(94, 234, 212);
    let accent_soft = egui::Color32::from_rgba_unmultiplied(94, 234, 212, 46);
    let mut visuals = egui::Visuals::dark();
    visuals.override_text_color = Some(text);
    visuals.panel_fill = panel;
    visuals.window_fill = panel;
    visuals.extreme_bg_color = egui::Color32::from_rgb(18, 22, 29);
    visuals.faint_bg_color = surface;
    visuals.code_bg_color = surface;
    visuals.hyperlink_color = accent;
    visuals.selection.bg_fill = accent_soft;
    visuals.selection.stroke = egui::Stroke::new(1.0, accent);
    visuals.window_corner_radius = egui::CornerRadius::same(6);
    visuals.menu_corner_radius = egui::CornerRadius::same(6);
    visuals.window_stroke = egui::Stroke::new(1.0, border);
    visuals.button_frame = true;
    visuals.interact_cursor = Some(egui::CursorIcon::PointingHand);
    visuals.widgets.noninteractive.bg_fill = panel;
    visuals.widgets.noninteractive.weak_bg_fill = panel;
    visuals.widgets.noninteractive.bg_stroke = egui::Stroke::new(1.0, border);
    visuals.widgets.noninteractive.fg_stroke = egui::Stroke::new(1.0, muted);
    visuals.widgets.inactive.bg_fill = surface;
    visuals.widgets.inactive.weak_bg_fill = surface;
    visuals.widgets.inactive.bg_stroke = egui::Stroke::new(1.0, border);
    visuals.widgets.inactive.fg_stroke = egui::Stroke::new(1.0, text);
    visuals.widgets.hovered.bg_fill = egui::Color32::from_rgb(45, 69, 74);
    visuals.widgets.hovered.weak_bg_fill = accent_soft;
    visuals.widgets.hovered.bg_stroke = egui::Stroke::new(1.0, accent);
    visuals.widgets.hovered.fg_stroke = egui::Stroke::new(1.0, text);
    visuals.widgets.active.bg_fill = egui::Color32::from_rgb(24, 128, 118);
    visuals.widgets.active.weak_bg_fill = egui::Color32::from_rgb(24, 128, 118);
    visuals.widgets.active.bg_stroke = egui::Stroke::new(1.0, accent);
    visuals.widgets.active.fg_stroke = egui::Stroke::new(1.0, text);
    visuals
}

#[derive(Clone, Debug)]
struct NativeKodamaItem {
    id: String,
    label: String,
    path: String,
    profile: String,
    shift_dx: f64,
    shift_dy: f64,
}

impl NativeKodamaItem {
    fn payload(&self) -> serde_json::Value {
        serde_json::json!({
            "id": self.id,
            "label": self.label,
            "path": self.path,
            "profile": self.profile,
            "shift_dx": self.shift_dx,
            "shift_dy": self.shift_dy
        })
    }
}

/// KODAMA refinement geometry stays file-backed until the user requests it.
/// The browser config includes the full GeoJSON for its own renderer; the
/// native menu deliberately keeps only the addressable path and slide shift.
fn native_kodama_items(value: &serde_json::Value) -> Vec<NativeKodamaItem> {
    value
        .get("kodama")
        .and_then(|kodama| kodama.get("enabled").and_then(serde_json::Value::as_bool).filter(|enabled| *enabled).map(|_| kodama))
        .and_then(|kodama| kodama.get("geojsons"))
        .and_then(serde_json::Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(|item| {
            let path = item.get("path")?.as_str()?.trim().to_string();
            if path.is_empty() {
                return None;
            }
            let id = item.get("id").and_then(serde_json::Value::as_str).unwrap_or("kodama").to_string();
            let label = item.get("label").and_then(serde_json::Value::as_str).unwrap_or(&id).to_string();
            let profile = item.get("profile").and_then(serde_json::Value::as_str).unwrap_or("").to_string();
            let shift_dx = item.get("shift_dx").and_then(serde_json::Value::as_f64).unwrap_or(0.0);
            let shift_dy = item.get("shift_dy").and_then(serde_json::Value::as_f64).unwrap_or(0.0);
            Some(NativeKodamaItem { id, label, path, profile, shift_dx, shift_dy })
        })
        .collect()
}

fn native_proximity_annotations(
    annotations: &[NativeAnnotationShape],
) -> Vec<(String, String)> {
    let mut values = HashMap::<String, String>::new();
    for shape in annotations {
        let feature = &shape.feature;
        let id = feature.get("id").and_then(json_id).or_else(|| {
            feature.get("properties")
                .and_then(|properties| properties.get("roi_id"))
                .and_then(json_id)
        });
        if let Some(id) = id {
            values.insert(format!("roi:{id}"), annotation_feature_label(feature));
        }
        let category = annotation_feature_label(feature);
        if !category.trim().is_empty() {
            values.insert(format!("class:{category}"), format!("All {category} annotations"));
        }
    }
    let mut values = values.into_iter().collect::<Vec<_>>();
    values.sort_by(|a, b| a.1.cmp(&b.1));
    values
}

fn native_gene_colour(value: f64, minimum: f64, maximum: f64) -> String {
    if !value.is_finite() { return "#d1d5db".to_string(); }
    let span = (maximum - minimum).abs().max(1e-12);
    let t = ((value - minimum) / span).clamp(0.0, 1.0);
    // A compact viridis-like ramp, matching the browser's intent without
    // sending a colour string for every point in a packed R response.
    let stops = [(68.0, 1.0, 84.0), (59.0, 82.0, 139.0), (33.0, 145.0, 140.0), (94.0, 201.0, 98.0), (253.0, 231.0, 37.0)];
    let scaled = t * (stops.len() - 1) as f64;
    let index = scaled.floor() as usize;
    let fraction = scaled - index as f64;
    let a = stops[index.min(stops.len() - 1)];
    let b = stops[(index + 1).min(stops.len() - 1)];
    format!("#{:02x}{:02x}{:02x}",
        (a.0 + (b.0 - a.0) * fraction).round() as u8,
        (a.1 + (b.1 - a.1) * fraction).round() as u8,
        (a.2 + (b.2 - a.2) * fraction).round() as u8)
}

impl NativeStateSync {
    pub fn start(endpoint: NativeTileEndpoint) -> Result<Self, String> {
        let state_url = endpoint.state_url()?;
        let (request_tx, request_rx) = mpsc::sync_channel::<String>(1);
        thread::spawn(move || {
            while let Ok(body) = request_rx.recv() {
                let _ = ureq::post(&state_url)
                    .set("Content-Type", "application/json")
                    .timeout(std::time::Duration::from_secs(5))
                    .send_string(&body);
            }
        });
        Ok(Self {
            requests: request_tx,
        })
    }

    pub fn viewport_changed(&self, camera: NativeCamera, width: u32, height: u32) {
        if !camera.center_x.is_finite()
            || !camera.center_y.is_finite()
            || !camera.pixels_per_screen_pixel.is_finite()
        {
            return;
        }
        let body = format!(
            concat!(
                "{{\"event\":\"viewport_changed\",",
                "\"view\":{{\"center_x\":{:.8},\"center_y\":{:.8},",
                "\"pixels_per_screen_pixel\":{:.8},\"width\":{},\"height\":{}}},",
                "\"detail\":{{\"renderer\":\"native_wgpu\"}}}}"
            ),
            camera.center_x, camera.center_y, camera.pixels_per_screen_pixel, width, height
        );
        let _ = self.requests.try_send(body);
    }

    fn project_source_selected(&self, source: &NativeTileSource) {
        let body = serde_json::json!({
            "event": "project_image_selected",
            "detail": {
                "renderer": "native_wgpu",
                "native_wgpu_source_id": source.id,
                "native_wgpu_source_label": source.label
            }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn image_transform_changed(&self, transform: NativeImageTransform) {
        let body = serde_json::json!({
            "event": "image_transform_updated",
            "view": {
                "image_transform": {
                    "rotation": transform.normalized_rotation(),
                    "flip_x": transform.flip_x,
                    "flip_y": transform.flip_y
                }
            },
            "detail": { "renderer": "native_wgpu" }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn spatial_registration_changed(&self, transform: &NativeSpatialTransform) {
        let body = serde_json::json!({
            "event": "spatial_registration_updated",
            "detail": {
                "renderer": "native_wgpu",
                "native_wgpu_spatial_transform": transform
            }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn spatial_object_save_requested(
        &self,
        path: &std::path::Path,
        point_source_id: &str,
        transform: &NativeSpatialTransform,
    ) {
        let body = serde_json::json!({
            "event": "spatial_object_save_requested",
            "detail": {
                "renderer": "native_wgpu",
                "native_wgpu_spatial_object_path": path.to_string_lossy(),
                "native_wgpu_point_source_id": point_source_id,
                "native_wgpu_spatial_transform": transform
            }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    pub fn roi_selection_changed(&self, feature: Option<&serde_json::Value>) {
        let event = if feature.is_some() {
            "roi_selected"
        } else {
            "roi_deselected"
        };
        let selected_roi = feature.cloned().unwrap_or(serde_json::Value::Null);
        let body = serde_json::json!({
            "event": event,
            "selected_roi": selected_roi,
            "detail": { "renderer": "native_wgpu" }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    pub fn roi_created(&self, feature: serde_json::Value) {
        let body = serde_json::json!({
            "event": "roi_created",
            "detail": {
                "renderer": "native_wgpu",
                "native_wgpu_roi": feature
            }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    pub fn roi_updated(&self, feature: serde_json::Value) {
        let body = serde_json::json!({
            "event": "roi_updated",
            "detail": { "renderer": "native_wgpu", "native_wgpu_roi": feature }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn geojson_imported(&self, features: Vec<serde_json::Value>) {
        if features.is_empty() {
            return;
        }
        let body = serde_json::json!({
            "event": "geojson_imported",
            "detail": {
                "renderer": "native_wgpu",
                "native_wgpu_geojson_features": features
            }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn geojson_import_path(&self, path: &std::path::Path) {
        let body = serde_json::json!({
            "event": "geojson_imported",
            "detail": {
                "renderer": "native_wgpu",
                "native_wgpu_geojson_path": path.to_string_lossy()
            }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn grandqc_load(&self, items: Vec<serde_json::Value>) {
        if items.is_empty() {
            return;
        }
        let body = serde_json::json!({
            "event": "grandqc_loaded",
            "detail": {
                "renderer": "native_wgpu",
                "native_wgpu_grandqc_items": items
            }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn grandqc_cleared(&self) {
        let body = serde_json::json!({
            "event": "grandqc_cleared",
            "detail": { "renderer": "native_wgpu" }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn kodama_load(&self, items: Vec<serde_json::Value>) {
        if items.is_empty() {
            return;
        }
        let body = serde_json::json!({
            "event": "kodama_loaded",
            "detail": {
                "renderer": "native_wgpu",
                "native_wgpu_kodama_items": items
            }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn kodama_cleared(&self) {
        let body = serde_json::json!({
            "event": "kodama_cleared",
            "detail": { "renderer": "native_wgpu" }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn trajectory_profile_finished(&self, rows: &[serde_json::Value]) {
        let body = serde_json::json!({
            "event": "trajectory_profile_finished",
            "detail": {
                "renderer": "native_wgpu",
                "trajectory_profile": rows
            }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn trajectory_profile_cleared(&self) {
        let body = serde_json::json!({
            "event": "trajectory_profile_cleared",
            "detail": { "renderer": "native_wgpu" }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn segmentation_import_path(&self, path: &std::path::Path) {
        let body = serde_json::json!({
            "event": "segmentation_added",
            "detail": {
                "renderer": "native_wgpu",
                "native_wgpu_segmentation_path": path.to_string_lossy()
            }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn roi_exported(&self, count: usize) {
        let body = serde_json::json!({
            "event": "roi_exported",
            "detail": { "renderer": "native_wgpu", "count": count }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn annotation_association_exported(&self, path: &std::path::Path) {
        let body = serde_json::json!({
            "event": "annotation_spots_exported",
            "detail": {
                "renderer": "native_wgpu",
                "native_wgpu_annotation_csv_path": path.to_string_lossy()
            }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn project_save_requested(&self, path: &std::path::Path) {
        let body = serde_json::json!({
            "event": "project_save_requested",
            "detail": {
                "renderer": "native_wgpu",
                "native_wgpu_project_path": path.to_string_lossy()
            }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn project_open_requested(&self, path: &std::path::Path) {
        let body = serde_json::json!({
            "event": "project_opened",
            "detail": {
                "renderer": "native_wgpu",
                "native_wgpu_project_open_path": path.to_string_lossy()
            }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn measurement_added(&self, measurement: serde_json::Value) {
        let body = serde_json::json!({
            "event": "measurement_added",
            "detail": { "renderer": "native_wgpu", "native_wgpu_measurement": measurement }
        });
        if let Ok(body) = serde_json::to_string(&body) { let _ = self.requests.try_send(body); }
    }

    fn trajectories_cleared(&self) {
        let body = serde_json::json!({
            "event": "trajectories_cleared",
            "detail": { "renderer": "native_wgpu" }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn measurements_cleared(&self) {
        let body = serde_json::json!({
            "event": "measurements_cleared",
            "detail": { "renderer": "native_wgpu" }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn image_exported(&self, result: &serde_json::Value) {
        let body = serde_json::json!({
            "event": "image_exported",
            "detail": {
                "renderer": "native_wgpu",
                "file": result.get("file").cloned().unwrap_or(serde_json::Value::Null),
                "format": result.get("format").cloned().unwrap_or(serde_json::Value::Null),
                "scope": result.get("scope").cloned().unwrap_or(serde_json::Value::Null)
            }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn measurement_deleted(&self, id: &str) {
        let body = serde_json::json!({
            "event": "measurement_deleted",
            "detail": {
                "renderer": "native_wgpu",
                "native_wgpu_delete_measurement_id": id
            }
        });
        if let Ok(body) = serde_json::to_string(&body) { let _ = self.requests.try_send(body); }
    }

    fn stain_updated(&self, mode: &str, base_visible: bool, base_opacity: f32) {
        let body = serde_json::json!({
            "event": "stain_updated",
            "detail": {
                "renderer": "native_wgpu",
                "native_wgpu_stain_mode": mode,
                "native_wgpu_base_visible": base_visible,
                "native_wgpu_base_opacity": base_opacity.clamp(0.0, 1.0)
            }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn trajectory_created(&self, trajectory: serde_json::Value) {
        let body = serde_json::json!({
            "event": "trajectory_added",
            "detail": {
                "renderer": "native_wgpu",
                "native_wgpu_trajectory": trajectory
            }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn trajectory_deleted(&self, trajectory_id: &str) {
        if trajectory_id.trim().is_empty() {
            return;
        }
        let body = serde_json::json!({
            "event": "trajectory_deleted",
            "detail": {
                "renderer": "native_wgpu",
                "native_wgpu_delete_trajectory_id": trajectory_id
            }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn roi_deleted(&self, roi_id: &str) {
        if roi_id.trim().is_empty() {
            return;
        }
        let body = serde_json::json!({
            "event": "roi_deleted",
            "detail": {
                "renderer": "native_wgpu",
                "native_wgpu_delete_roi_id": roi_id
            }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn annotation_snapshot_restored(
        &self,
        event: &str,
        annotations: Vec<serde_json::Value>,
        trajectories: Vec<serde_json::Value>,
    ) {
        let body = serde_json::json!({
            "event": event,
            "detail": {
                "renderer": "native_wgpu",
                "native_wgpu_annotations": annotations,
                "native_wgpu_trajectories": trajectories
            }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn layer_updated(&self, id: &str, visible: bool, opacity: f32) {
        let body = serde_json::json!({
            "event": "layer_updated",
            "layers": [{
                "id": id,
                "visible": visible,
                "opacity": opacity.clamp(0.0, 1.0)
            }],
            "detail": { "renderer": "native_wgpu" }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn spatial_points_selected(&self, plot_id: &str, labels: &HashSet<String>) {
        let mut labels = labels.iter().cloned().collect::<Vec<_>>();
        labels.sort();
        let body = serde_json::json!({
            "event": "seurat_spots_selected",
            "seurat_selection": {
                "labels": labels,
                "count": labels.len(),
                "matched_count": labels.len()
            },
            "detail": {
                "renderer": "native_wgpu",
                "plot_id": plot_id,
                "count": labels.len()
            }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn kodama_cells_selected(&self, plot_id: &str, labels: &HashSet<String>) {
        let mut labels = labels.iter().cloned().collect::<Vec<_>>();
        labels.sort();
        let body = serde_json::json!({
            "event": "kodama_cells_selected",
            "kodama_selection": {
                "labels": labels,
                "count": labels.len(),
                "matched_count": labels.len()
            },
            "detail": {
                "renderer": "native_wgpu",
                "plot_id": plot_id,
                "count": labels.len()
            }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }

    fn channel_updated(
        &self,
        id: &str,
        visible: bool,
        opacity: f32,
        colour: &str,
        gain: f32,
        contrast_min: f32,
        contrast_max: f32,
    ) {
        let body = serde_json::json!({
            "event": "channel_updated",
            "id": id,
            "settings": {
                "visible": visible,
                "opacity": opacity.clamp(0.0, 1.0),
                "colour": colour,
                "gain": gain.max(0.0),
                "contrast_min": contrast_min.clamp(0.0, 1.0),
                "contrast_max": contrast_max.clamp(0.0, 1.0)
            },
            "detail": { "renderer": "native_wgpu" }
        });
        if let Ok(body) = serde_json::to_string(&body) {
            let _ = self.requests.try_send(body);
        }
    }
}

#[derive(Debug)]
pub struct NativeTileCache<T> {
    capacity: usize,
    values: HashMap<TileKey, T>,
    order: VecDeque<TileKey>,
}

impl<T> NativeTileCache<T> {
    pub fn new(capacity: usize) -> Self {
        Self {
            capacity: capacity.max(1),
            values: HashMap::new(),
            order: VecDeque::new(),
        }
    }
    pub fn contains(&self, key: &TileKey) -> bool {
        self.values.contains_key(key)
    }
    pub fn insert(&mut self, key: TileKey, value: T) {
        self.order.retain(|entry| entry != &key);
        self.values.insert(key, value);
        self.order.push_back(key);
        while self.values.len() > self.capacity {
            if let Some(oldest) = self.order.pop_front() {
                self.values.remove(&oldest);
            }
        }
    }
    pub fn len(&self) -> usize {
        self.values.len()
    }
    pub fn clear(&mut self) {
        self.values.clear();
        self.order.clear();
    }
}

impl NativeTileFetcher {
    pub fn start(endpoint: NativeTileEndpoint, source: NativeTileSource, capacity: usize) -> Self {
        let (request_tx, request_rx) = mpsc::sync_channel::<TileKey>(capacity.max(1));
        let (result_tx, result_rx) = mpsc::channel();
        thread::spawn(move || {
            while let Ok(key) = request_rx.recv() {
                let result =
                    fetch_native_tile(&endpoint, &source, key).map(|(width, height, rgba)| {
                        NativeDecodedTile {
                            source_id: source.id.clone(),
                            key,
                            width,
                            height,
                            rgba,
                        }
                    });
                if result_tx.send(result).is_err() {
                    break;
                }
            }
        });
        Self {
            requests: request_tx,
            results: result_rx,
        }
    }

    pub fn request(&self, key: TileKey) -> bool {
        self.requests.try_send(key).is_ok()
    }
    pub fn try_next(&self) -> Option<Result<NativeDecodedTile, String>> {
        self.results.try_recv().ok()
    }
}

pub fn tile_url(
    base_url: &str,
    manifest: &NativeRendererManifest,
    source: &NativeTileSource,
    key: TileKey,
) -> String {
    NativeTileEndpoint {
        base_url: base_url.trim_end_matches('/').to_string(),
        tile_route: manifest.tile_route.trim_matches('/').to_string(),
        state_route: manifest.state_route.trim_matches('/').to_string(),
        state_snapshot_route: manifest.state_snapshot_route.trim_matches('/').to_string(),
        dense_geojson_route: manifest.dense_geojson_route.trim_matches('/').to_string(),
        native_points_route: manifest.native_points_route.trim_matches('/').to_string(),
        spatial_gene_route: manifest.spatial_gene_route.trim_matches('/').to_string(),
        prediction_route: manifest.prediction_route.trim_matches('/').to_string(),
        proximity_route: manifest.proximity_route.trim_matches('/').to_string(),
        image_export_route: manifest.image_export_route.trim_matches('/').to_string(),
        segmentation_run_url: manifest.segmentation_run_url.clone(),
    }
    .url(source, key)
}

#[derive(Clone, Debug, Serialize)]
pub struct NativeRendererDiagnostics {
    pub available: bool,
    pub backend: String,
    pub adapter: String,
    pub device_type: String,
    pub message: String,
}

pub fn probe_wgpu() -> Result<NativeRendererDiagnostics, String> {
    pollster::block_on(async {
        let instance = wgpu::Instance::default();
        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::HighPerformance,
                force_fallback_adapter: false,
                compatible_surface: None,
            })
            .await
            .ok_or_else(|| "No WGPU adapter was available.".to_string())?;
        let info = adapter.get_info();
        Ok(NativeRendererDiagnostics {
            available: true,
            backend: format!("{:?}", info.backend),
            adapter: info.name,
            device_type: format!("{:?}", info.device_type),
            message: "WGPU adapter available. Native viewer tile composition can use this device."
                .to_string(),
        })
    })
}

/// Launches the first true native viewer window. It intentionally starts from
/// the same versioned R manifest as the browser viewer. Tile drawing and the
/// native overlay tool surface are layered on this event-loop host rather than
/// reimplementing image opening in Rust.
pub fn run_native_viewer(viewer_url: String, startup_status: Option<PathBuf>) -> Result<(), String> {
    let manifest = match fetch_manifest(&viewer_url) {
        Ok(manifest) => manifest,
        Err(error) => {
            write_native_startup_status(&startup_status, "error", &error);
            return Err(error);
        }
    };
    let source = match manifest.source(None) {
        Ok(source) => source.clone(),
        Err(error) => {
            write_native_startup_status(&startup_status, "error", &error);
            return Err(error);
        }
    };
    let sources = manifest.sources.clone();
    let dense_sources = manifest.dense_sources.clone();
    let channel_sources = manifest.channel_sources.clone();
    let point_sources = manifest.point_sources.clone();
    let spatial_clusters = manifest.spatial_clusters.clone();
    let spatial_config = manifest.spatial.clone();
    let cellphenotyper_config = manifest.cellphenotyper.clone();
    let prediction_config = manifest.prediction.clone();
    let endpoint = match NativeTileEndpoint::from_live_viewer(&viewer_url, &manifest) {
        Ok(endpoint) => endpoint,
        Err(error) => {
            write_native_startup_status(&startup_status, "error", &error);
            return Err(error);
        }
    };
    let event_loop = match EventLoop::new() {
        Ok(event_loop) => event_loop,
        Err(error) => {
            let message = format!("Could not create native viewer event loop: {error}");
            write_native_startup_status(&startup_status, "error", &message);
            return Err(message);
        }
    };
    let mut app = NativeViewerApp {
        source,
        sources,
        source_index: 0,
        endpoint: endpoint.clone(),
        window: None,
        renderer: None,
        tile_fetchers: HashMap::new(),
        state_sync: None,
        state_fetcher: None,
        dense_fetcher: None,
        point_fetcher: None,
        trajectory_profile_fetcher: None,
        gene_fetcher: None,
        prediction_fetcher: if manifest.prediction_enabled {
            NativePredictionFetcher::start(endpoint.clone())
        } else { None },
        image_export_fetcher: None,
        proximity_fetcher: if manifest.proximity_enabled {
            NativeProximityFetcher::start(endpoint.clone())
        } else {
            None
        },
        segmentation_fetcher: NativeSegmentationFetcher::start(manifest.segmentation_run_url.clone()),
        segmentation_run_url: manifest.segmentation_run_url.clone(),
        dense_sources,
        point_sources,
        channel_sources,
        channel_fetchers: HashMap::new(),
        last_dense_camera: None,
        last_dense_refresh: Instant::now(),
        last_point_camera: None,
        last_point_refresh: Instant::now(),
        last_state_refresh: Instant::now(),
        inflight: HashSet::new(),
        error: None,
        drag_anchor: None,
        dragging: false,
        cursor_position: None,
        last_left_press: None,
        tool: NativeInteractionTool::Pan,
        draft_points: Vec::new(),
        selected_feature: None,
        selected_annotation_ids: HashSet::new(),
        selected_trajectory: None,
        selected_measurement_id: None,
        editing_roi: None,
        curve_edit: None,
        brush_operation: NativeBrushOperation::New,
        brush_target: None,
        annotation_undo: VecDeque::new(),
        annotation_redo: VecDeque::new(),
        modifiers: ModifiersState::default(),
        brush_size_slide: 32.0,
        trajectory_area_width_slide: 512.0,
        annotation_label: "Annotation".to_string(),
        annotation_colour: "#22C55E".to_string(),
        show_project_panel: true,
        show_annotation_panel: true,
        show_layer_panel: true,
        show_history_panel: false,
        show_help_panel: false,
        show_annotation_labels: true,
        history: VecDeque::new(),
        last_history_revision: None,
        active_gene: String::new(),
        gene_colours: HashMap::new(),
        trajectory_profile_colours: HashMap::new(),
        trajectory_profile_rows: Vec::new(),
        trajectory_profile_source: String::new(),
        trajectory_profile_feature: "count".to_string(),
        trajectory_profile_bins: 20,
        trajectory_profile_width: 250.0,
        show_trajectory_profile: false,
        spatial_point_scale: 1.0,
        spatial_clusters,
        spatial_config,

        cellphenotyper_config,
        show_spatial_plot: false,
        spatial_plot_index: 0,
        spatial_selected_labels: HashSet::new(),
        spatial_plot_drag_start: None,
        prediction_config,
        prediction_source: String::new(),
        prediction_train: Vec::new(),
        active_cluster: String::new(),
        spatial_transform: NativeSpatialTransform::default(),
        proximity_query: String::new(),
        proximity_target: String::new(),
        proximity_stats_feature: "auto".to_string(),
        proximity_stats_method: "spearman".to_string(),
        proximity_stats_rows: Vec::new(),
        show_proximity_stats: false,
        segmentation_engine: "stardist_he".to_string(),
        multi_view_layout: 1,
        multi_view_sync: false,
        active_pane: 0,
        panes: Vec::new(),
        overview_textures: HashMap::new(),
        egui_context: None,
        egui_state: None,
        startup_status,
    };
    event_loop.run_app(&mut app).map_err(|error| {
        let message = format!("Native viewer event loop failed: {error}");
        write_native_startup_status(&app.startup_status, "error", &message);
        message
    })
}

fn write_native_startup_status(path: &Option<PathBuf>, state: &str, message: &str) {
    let Some(path) = path else { return; };
    let payload = serde_json::json!({
        "state": state,
        "message": message,
        "timestamp_ms": SystemTime::now().duration_since(UNIX_EPOCH).map(|value| value.as_millis()).unwrap_or_default()
    });
    let _ = fs::write(path, serde_json::to_vec(&payload).unwrap_or_default());
}

struct NativeViewerApp {
    source: NativeTileSource,
    sources: Vec<NativeTileSource>,
    source_index: usize,
    endpoint: NativeTileEndpoint,
    window: Option<std::sync::Arc<Window>>,
    renderer: Option<NativeWindowRenderer>,
    tile_fetchers: HashMap<String, NativeTileFetcher>,
    state_sync: Option<NativeStateSync>,
    state_fetcher: Option<NativeStateFetcher>,
    dense_fetcher: Option<NativeDenseFetcher>,
    point_fetcher: Option<NativePointFetcher>,
    trajectory_profile_fetcher: Option<NativeTrajectoryProfileFetcher>,
    gene_fetcher: Option<NativeGeneFetcher>,
    prediction_fetcher: Option<NativePredictionFetcher>,
    image_export_fetcher: Option<NativeImageExportFetcher>,
    proximity_fetcher: Option<NativeProximityFetcher>,
    segmentation_fetcher: Option<NativeSegmentationFetcher>,
    segmentation_run_url: String,
    dense_sources: Vec<NativeDenseSource>,
    point_sources: Vec<NativePointSource>,
    channel_sources: Vec<NativeChannelSource>,
    channel_fetchers: HashMap<String, NativeTileFetcher>,
    last_state_refresh: Instant,
    last_dense_camera: Option<NativeCamera>,
    last_dense_refresh: Instant,
    last_point_camera: Option<NativeCamera>,
    last_point_refresh: Instant,
    inflight: HashSet<(String, TileKey)>,
    error: Option<String>,
    drag_anchor: Option<(f64, f64)>,
    dragging: bool,
    cursor_position: Option<(f64, f64)>,
    last_left_press: Option<Instant>,
    tool: NativeInteractionTool,
    draft_points: Vec<(f32, f32)>,
    selected_feature: Option<serde_json::Value>,
    selected_annotation_ids: HashSet<String>,
    selected_trajectory: Option<serde_json::Value>,
    selected_measurement_id: Option<String>,
    editing_roi: Option<NativeRoiVertexEdit>,
    curve_edit: Option<NativeCurveEdit>,
    brush_operation: NativeBrushOperation,
    brush_target: Option<serde_json::Value>,
    annotation_undo: VecDeque<NativeAnnotationSnapshot>,
    annotation_redo: VecDeque<NativeAnnotationSnapshot>,
    modifiers: ModifiersState,
    brush_size_slide: f32,
    trajectory_area_width_slide: f32,
    annotation_label: String,
    annotation_colour: String,
    show_project_panel: bool,
    show_annotation_panel: bool,
    show_layer_panel: bool,
    show_history_panel: bool,
    show_help_panel: bool,
    show_annotation_labels: bool,
    history: VecDeque<String>,
    last_history_revision: Option<u64>,
    active_gene: String,
    gene_colours: HashMap<String, String>,
    trajectory_profile_colours: HashMap<String, String>,
    trajectory_profile_rows: Vec<serde_json::Value>,
    trajectory_profile_source: String,
    trajectory_profile_feature: String,
    trajectory_profile_bins: i32,
    trajectory_profile_width: f64,
    show_trajectory_profile: bool,
    spatial_point_scale: f32,
    spatial_clusters: serde_json::Value,
    spatial_config: serde_json::Value,
    cellphenotyper_config: serde_json::Value,
    show_spatial_plot: bool,
    spatial_plot_index: usize,
    spatial_selected_labels: HashSet<String>,
    spatial_plot_drag_start: Option<egui::Pos2>,
    prediction_config: serde_json::Value,
    prediction_source: String,
    prediction_train: Vec<String>,
    active_cluster: String,
    spatial_transform: NativeSpatialTransform,
    proximity_query: String,
    proximity_target: String,
    proximity_stats_feature: String,
    proximity_stats_method: String,
    proximity_stats_rows: Vec<serde_json::Value>,
    show_proximity_stats: bool,
    segmentation_engine: String,
    multi_view_layout: usize,
    multi_view_sync: bool,
    active_pane: usize,
    panes: Vec<NativeViewPane>,
    // One small texture per source, created from the coarsest tile already
    // requested at startup. This avoids a separate preview pipeline.
    overview_textures: HashMap<String, egui::TextureHandle>,
    egui_context: Option<egui::Context>,
    egui_state: Option<egui_winit::State>,
    startup_status: Option<PathBuf>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum NativeInteractionTool {
    Pan,
    Polygon,
    Brush,
    Edit,
    Trajectory,
    Measurement,
}

#[derive(Clone, Debug)]
struct NativeRoiVertexEdit {
    feature: serde_json::Value,
    vertex_index: usize,
    original_ring: Vec<(f32, f32)>,
    original_point: (f32, f32),
    soft_span: usize,
}

#[derive(Clone, Debug)]
struct NativeCurveEdit {
    feature: serde_json::Value,
    start_after: usize,
    original_ring: Vec<(f32, f32)>,
    points: Vec<(f32, f32)>,
}

#[derive(Clone, Debug)]
struct NativeAnnotationSnapshot {
    annotations: Vec<serde_json::Value>,
    trajectories: Vec<serde_json::Value>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum NativeBrushOperation {
    New,
    Extend,
    Subtract,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum NativeStainDisplay {
    Original,
    Hematoxylin,
    Eosin,
    Residual,
}

impl NativeStainDisplay {
    fn label(self) -> &'static str {
        match self {
            Self::Original => "Original H&E",
            Self::Hematoxylin => "Hematoxylin",
            Self::Eosin => "Eosin",
            Self::Residual => "Residual",
        }
    }

    fn shader_mode(self) -> f32 {
        match self {
            Self::Original => 0.0,
            Self::Hematoxylin => 1.0,
            Self::Eosin => 2.0,
            Self::Residual => 3.0,
        }
    }

    fn from_shader_mode(mode: f32) -> Self {
        if mode < 0.5 { Self::Original }
        else if mode < 1.5 { Self::Hematoxylin }
        else if mode < 2.5 { Self::Eosin }
        else { Self::Residual }
    }

    fn from_state(value: &serde_json::Value) -> Self {
        match value
            .get("native_wgpu_stain_mode")
            .or_else(|| value.get("display"))
            .and_then(serde_json::Value::as_str)
            .unwrap_or("original")
            .trim()
            .to_ascii_lowercase()
            .as_str()
        {
            "hematoxylin" | "haematoxylin" => Self::Hematoxylin,
            "eosin" => Self::Eosin,
            "residual" => Self::Residual,
            _ => Self::Original,
        }
    }

    fn state_name(self) -> &'static str {
        match self {
            Self::Original => "original",
            Self::Hematoxylin => "hematoxylin",
            Self::Eosin => "eosin",
            Self::Residual => "residual",
        }
    }
}

/// Independent state for one native multi-view pane. Tile textures are shared
/// through the renderer's bounded source cache, while navigation is local to
/// the pane unless explicit linking is enabled.
#[derive(Clone, Copy, Debug)]
struct NativeViewPane {
    source_index: usize,
    camera: NativeCamera,
    transform: NativeImageTransform,
}

fn native_source_is_assigned_elsewhere(
    panes: &[NativeViewPane],
    active_pane: usize,
    source_index: usize,
) -> bool {
    panes
        .iter()
        .enumerate()
        .any(|(pane_index, pane)| pane_index != active_pane && pane.source_index == source_index)
}

#[derive(Clone, Copy, Debug)]
struct NativePaneRect {
    x: u32,
    y: u32,
    width: u32,
    height: u32,
}

#[derive(Clone)]
struct NativeRenderPane {
    source: NativeTileSource,
    camera: NativeCamera,
    transform: NativeImageTransform,
    rect: NativePaneRect,
}

fn native_multiview_count(value: usize) -> usize {
    // Match the browser viewer's project layouts. The renderer intentionally
    // leaves newly created panes empty instead of cloning the active slide.
    value.clamp(1, 12)
}

fn native_pane_rects(layout: usize, width: u32, height: u32) -> Vec<NativePaneRect> {
    let layout = native_multiview_count(layout);
    let (columns, rows): (usize, usize) = match layout {
        1 => (1, 1),
        2 => (2, 1),
        3 => (2, 2),
        4 => (2, 2),
        5 | 6 => (3, 2),
        7..=9 => (3, 3),
        10..=12 => (4, 3),
        _ => unreachable!("native_multiview_count constrains layout"),
    };
    let top = 42_u32.min(height);
    let bottom = 52_u32.min(height.saturating_sub(top));
    let available_height = height.saturating_sub(top + bottom).max(1);
    let cell_width = (width / columns as u32).max(1);
    let cell_height = (available_height / rows as u32).max(1);
    (0..layout)
        .map(|index| {
            let column = (index % columns) as u32;
            let row = (index / columns) as u32;
            let x = column * cell_width;
            let y = top + row * cell_height;
            let right = if column + 1 == columns as u32 {
                width
            } else {
                x + cell_width
            };
            let bottom_edge = if row + 1 == rows as u32 {
                top + available_height
            } else {
                y + cell_height
            };
            NativePaneRect {
                x,
                y,
                width: right.saturating_sub(x).max(1),
                height: bottom_edge.saturating_sub(y).max(1),
            }
        })
        .collect()
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct NativeImageTransform {
    rotation: u16,
    flip_x: bool,
    flip_y: bool,
}

impl NativeImageTransform {
    fn normalized_rotation(self) -> u16 {
        match self.rotation % 360 {
            90 | 180 | 270 => self.rotation % 360,
            _ => 0,
        }
    }

    fn display_dimensions(self, source: &NativeTileSource) -> (f64, f64) {
        match self.normalized_rotation() {
            90 | 270 => (source.height, source.width),
            _ => (source.width, source.height),
        }
    }

    fn map_point(self, source: &NativeTileSource, point: (f64, f64)) -> (f64, f64) {
        let (mut x, mut y) = match self.normalized_rotation() {
            90 => (source.height - point.1, point.0),
            180 => (source.width - point.0, source.height - point.1),
            270 => (point.1, source.width - point.0),
            _ => point,
        };
        let (width, height) = self.display_dimensions(source);
        if self.flip_x {
            x = width - x;
        }
        if self.flip_y {
            y = height - y;
        }
        (x, y)
    }

    fn inverse_map_point(self, source: &NativeTileSource, point: (f64, f64)) -> (f64, f64) {
        let (width, height) = self.display_dimensions(source);
        let mut x = point.0;
        let mut y = point.1;
        if self.flip_x {
            x = width - x;
        }
        if self.flip_y {
            y = height - y;
        }
        match self.normalized_rotation() {
            90 => (y, source.height - x),
            180 => (source.width - x, source.height - y),
            270 => (source.width - y, x),
            _ => (x, y),
        }
    }

    fn rotate_clockwise(&mut self, degrees: u16) {
        self.rotation = (self.rotation + degrees) % 360;
    }
}

struct NativeEguiFrame {
    primitives: Vec<egui::ClippedPrimitive>,
    textures_delta: egui::TexturesDelta,
    pixels_per_point: f32,
}

impl ApplicationHandler for NativeViewerApp {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.window.is_some() {
            return;
        }
        let title = format!("wsiTools - {}", self.source.label);
        let window = match event_loop.create_window(
            WindowAttributes::default()
                .with_title(title)
                .with_inner_size(winit::dpi::LogicalSize::new(1500.0, 950.0))
                .with_min_inner_size(winit::dpi::LogicalSize::new(960.0, 700.0)),
        ) {
            Ok(window) => std::sync::Arc::new(window),
            Err(error) => {
                self.error = Some(format!(
                    "Could not create native WGPU viewer window: {error}"
                ));
                event_loop.exit();
                return;
            }
        };
        match NativeWindowRenderer::new(
            window.clone(),
            &self.source,
            &self.sources,
            &self.channel_sources,
        ) {
            Ok(renderer) => {
                let initial_camera = renderer.camera_state;
                self.renderer = Some(renderer);
                self.tile_fetchers = self
                    .sources
                    .iter()
                    .cloned()
                    .map(|source| {
                        let id = source.id.clone();
                        (
                            id,
                            NativeTileFetcher::start(
                                self.endpoint.clone(),
                                source,
                                NATIVE_BASE_TILE_QUEUE,
                            ),
                        )
                    })
                    .collect();
                // Ask for one complete low-resolution image before the normal
                // viewport plan. This is deliberately one bounded tile per
                // source, not a full-slide decode or pyramid prebuild.
                for source in &self.sources {
                    if let Some(fetcher) = self.tile_fetchers.get(&source.id) {
                        let key = native_overview_tile(source);
                        if fetcher.request(key) {
                            self.inflight.insert((source.id.clone(), key));
                        }
                    }
                }
                self.panes = vec![NativeViewPane {
                    source_index: self.source_index,
                    camera: initial_camera,
                    transform: NativeImageTransform::default(),
                }];
                self.channel_fetchers = self
                    .channel_sources
                    .iter()
                    .cloned()
                    .map(|channel| {
                        let source = channel.tile_source();
                        let id = source.id.clone();
                        (
                            id,
                            NativeTileFetcher::start(
                                self.endpoint.clone(),
                                source,
                                NATIVE_CHANNEL_TILE_QUEUE,
                            ),
                        )
                    })
                    .collect();
                self.state_sync = NativeStateSync::start(self.endpoint.clone()).ok();
                self.state_fetcher = Some(NativeStateFetcher::start(self.endpoint.clone()));
                self.image_export_fetcher = NativeImageExportFetcher::start(self.endpoint.clone());
                let egui_context = egui::Context::default();
                egui_context.set_visuals(native_browser_visuals());
                egui_context.style_mut(|style| {
                    style.spacing.item_spacing = egui::vec2(7.0, 7.0);
                    style.spacing.button_padding = egui::vec2(9.0, 5.0);
                    style.spacing.window_margin = egui::Margin::same(10);
                });
                self.egui_state = Some(egui_winit::State::new(
                    egui_context.clone(),
                    egui::ViewportId::ROOT,
                    &window,
                    None,
                    None,
                    None,
                ));
                self.egui_context = Some(egui_context);
                if !self.dense_sources.is_empty() {
                    self.dense_fetcher = NativeDenseFetcher::start(self.endpoint.clone());
                }
                if !self.point_sources.is_empty() {
                    self.point_fetcher = NativePointFetcher::start(self.endpoint.clone());
                    self.trajectory_profile_fetcher = NativeTrajectoryProfileFetcher::start(self.endpoint.clone());
                    self.gene_fetcher = NativeGeneFetcher::start(self.endpoint.clone());
                }
                self.request_state_refresh();
                self.window = Some(window);
                // The renderer is launched as a child of the Tauri starter.
                // Explicitly foreground its own Winit window once the GPU
                // surface is ready so it cannot remain hidden behind the
                // launcher or the browser viewer on macOS.
                if let Some(window) = &self.window {
                    window.set_visible(true);
                    window.focus_window();
                }
                self.schedule_visible_tiles();
                self.sync_viewport();
                write_native_startup_status(
                    &self.startup_status,
                    "ready",
                    "Native Rust/WGPU window and GPU renderer are ready.",
                );
            }
            Err(error) => {
                write_native_startup_status(&self.startup_status, "error", &error);
                self.error = Some(error);
                event_loop.exit();
            }
        }
    }

    fn window_event(&mut self, event_loop: &ActiveEventLoop, _: WindowId, event: WindowEvent) {
        let egui_consumed = match (self.window.as_ref(), self.egui_state.as_mut()) {
            (Some(window), Some(egui_state)) => egui_state.on_window_event(window, &event).consumed,
            _ => false,
        };
        if egui_consumed {
            return;
        }
        match event {
            WindowEvent::CloseRequested => event_loop.exit(),
            WindowEvent::Resized(size) => {
                if let Some(renderer) = self.renderer.as_mut() {
                    renderer.resize(size.width, size.height);
                }
                self.schedule_visible_tiles();
                self.sync_viewport();
            }
            WindowEvent::RedrawRequested => {
                let egui_frame = self.prepare_egui_frame();
                let panes = self.native_render_panes();
                if let Some(renderer) = self.renderer.as_mut() {
                    if renderer.render_panes(&panes, egui_frame).is_err() {
                        renderer.reconfigure();
                    }
                }
            }
            WindowEvent::MouseWheel { delta, .. } => {
                let amount = match delta {
                    MouseScrollDelta::LineDelta(_, y) => y,
                    MouseScrollDelta::PixelDelta(p) => p.y as f32 / 50.0,
                };
                if let Some(cursor) = self.cursor_position {
                    if let Some(index) = self.pane_at_screen(cursor) {
                        self.activate_pane(index);
                    }
                }
                let active_rect = self.active_pane_rect();
                let active_cursor = self.active_local_cursor();
                if let (Some(renderer), Some(rect)) = (self.renderer.as_mut(), active_rect) {
                    let center = active_cursor
                        .unwrap_or((rect.width as f64 / 2.0, rect.height as f64 / 2.0));
                    renderer.camera_state.zoom_about(
                        1.2_f32.powf(amount) as f64,
                        center.0,
                        center.1,
                        rect.width,
                        rect.height,
                    );
                    renderer.sync_camera();
                }
                self.sync_active_pane_from_renderer();
                self.schedule_visible_tiles();
                self.sync_viewport();
            }
            WindowEvent::ModifiersChanged(modifiers) => {
                self.modifiers = modifiers.state();
            }
            WindowEvent::KeyboardInput {
                event:
                    KeyEvent {
                        state: ElementState::Pressed,
                        physical_key: PhysicalKey::Code(key),
                        repeat: false,
                        ..
                    },
                ..
            } => {
                if (self.modifiers.control_key() || self.modifiers.super_key()) && key == KeyCode::KeyZ {
                    self.restore_annotation_history(self.modifiers.shift_key());
                    return;
                }
                if (self.modifiers.control_key() || self.modifiers.super_key()) && key == KeyCode::KeyY {
                    self.restore_annotation_history(true);
                    return;
                }
                match key {
                KeyCode::BracketLeft => self.switch_source(-1),
                KeyCode::BracketRight => self.switch_source(1),
                KeyCode::ArrowLeft => self.pan_by_keyboard(0.12, 0.0),
                KeyCode::ArrowRight => self.pan_by_keyboard(-0.12, 0.0),
                KeyCode::ArrowUp => self.pan_by_keyboard(0.0, 0.12),
                KeyCode::ArrowDown => self.pan_by_keyboard(0.0, -0.12),
                KeyCode::Equal | KeyCode::NumpadAdd => self.zoom_by_keyboard(1.35),
                KeyCode::Minus | KeyCode::NumpadSubtract => self.zoom_by_keyboard(1.0 / 1.35),
                KeyCode::Home | KeyCode::KeyR => self.reset_view(),
                KeyCode::KeyP => self.toggle_polygon_tool(),
                KeyCode::KeyB => self.toggle_brush_tool(),
                KeyCode::KeyE => self.toggle_edit_tool(),
                KeyCode::KeyT => self.toggle_trajectory_tool(),
                KeyCode::KeyM => self.toggle_measurement_tool(),
                KeyCode::Enter => self.commit_polygon(),
                KeyCode::Escape => self.cancel_polygon(),
                KeyCode::Delete | KeyCode::Backspace => self.delete_selected_object(),
                    _ => {}
                }
            }
            WindowEvent::MouseInput {
                state,
                button: MouseButton::Left,
                ..
            } => {
                if state == ElementState::Pressed {
                    if let Some(cursor) = self.cursor_position {
                        if let Some(index) = self.pane_at_screen(cursor) {
                            self.activate_pane(index);
                        }
                    }
                }
                if self.tool == NativeInteractionTool::Polygon {
                    if state == ElementState::Pressed {
                        let local_cursor = self.active_local_cursor();
                        let active_rect = self.active_pane_rect();
                        if let (Some(renderer), Some(cursor), Some(rect)) =
                            (self.renderer.as_mut(), local_cursor, active_rect)
                        {
                            let display_point = renderer.camera_state.slide_at(
                                cursor.0,
                                cursor.1,
                                rect.width,
                                rect.height,
                            );
                            let point = renderer
                                .image_transform
                                .inverse_map_point(&renderer.source, display_point);
                            self.draft_points.push((point.0 as f32, point.1 as f32));
                            if let Err(error) = renderer.set_draft_polygon(&self.draft_points) {
                                self.error = Some(error);
                            }
                        }
                    }
                    self.dragging = false;
                    return;
                }
                if self.tool == NativeInteractionTool::Edit {
                    if state == ElementState::Pressed {
                        if !self.start_roi_vertex_edit() {
                            self.start_roi_curve_edit();
                        }
                    } else {
                        if self.editing_roi.is_some() {
                            self.finish_roi_vertex_edit();
                        } else {
                            self.finish_roi_curve_edit();
                        }
                    }
                    self.dragging = self.editing_roi.is_some() || self.curve_edit.is_some();
                    self.drag_anchor = None;
                    return;
                }
                if matches!(
                    self.tool,
                    NativeInteractionTool::Brush | NativeInteractionTool::Trajectory | NativeInteractionTool::Measurement
                ) {
                    if state == ElementState::Pressed {
                        self.draft_points.clear();
                        self.prepare_brush_operation();
                        self.append_draft_screen_point();
                        self.dragging = true;
                    } else if self.dragging {
                        self.dragging = false;
                        if self.tool == NativeInteractionTool::Brush {
                            self.commit_brush();
                        } else if self.tool == NativeInteractionTool::Trajectory {
                            self.commit_trajectory();
                        } else {
                            self.commit_measurement();
                        }
                    }
                    self.drag_anchor = None;
                    return;
                }
                if state == ElementState::Pressed {
                    let is_double_click = self
                        .last_left_press
                        .map(|previous| previous.elapsed() <= Duration::from_millis(350))
                        .unwrap_or(false);
                    self.last_left_press = Some(Instant::now());
                    if is_double_click {
                        let local_cursor = self.active_local_cursor();
                        let active_rect = self.active_pane_rect();
                        if let (Some(renderer), Some(sync), Some(cursor), Some(rect)) = (
                            self.renderer.as_ref(),
                            self.state_sync.as_ref(),
                            local_cursor,
                            active_rect,
                        ) {
                            let selected =
                                renderer.roi_at_screen_in_view(cursor, rect.width, rect.height);
                            self.selected_feature = selected.clone();
                            sync.roi_selection_changed(selected.as_ref());
                        }
                    }
                }
                self.dragging = state == ElementState::Pressed;
                if !self.dragging {
                    self.drag_anchor = None;
                }
            }
            WindowEvent::CursorMoved { position, .. } => {
                let next = (position.x, position.y);
                self.cursor_position = Some(next);
                let previous = if self.dragging {
                    self.drag_anchor.replace(next)
                } else {
                    self.drag_anchor = None;
                    None
                };
                if self.dragging
                    && matches!(
                        self.tool,
                        NativeInteractionTool::Brush | NativeInteractionTool::Trajectory | NativeInteractionTool::Measurement
                    )
                {
                    self.append_draft_screen_point();
                } else if self.dragging && self.tool == NativeInteractionTool::Edit {
                    if self.editing_roi.is_some() {
                        self.move_roi_vertex_edit();
                    } else {
                        self.append_roi_curve_edit_point();
                    }
                } else if let Some(previous) = previous {
                    if let Some(renderer) = self.renderer.as_mut() {
                        renderer.pan((next.0 - previous.0) as f32, (next.1 - previous.1) as f32);
                    }
                    self.sync_active_pane_from_renderer();
                    self.schedule_visible_tiles();
                    self.sync_viewport();
                }
            }
            _ => {}
        }
    }

    fn about_to_wait(&mut self, _: &ActiveEventLoop) {
        self.drain_tile_results();
        self.drain_state_results();
        self.drain_gene_results();
        self.drain_prediction_results();
        self.drain_proximity_results();
        self.drain_trajectory_profile_results();
        self.drain_image_export_results();
        self.drain_segmentation_results();
        self.drain_dense_results();
        self.drain_point_results();
        if self.last_state_refresh.elapsed() >= Duration::from_millis(500) {
            self.request_state_refresh();
        }
        self.schedule_visible_tiles();
        self.request_dense_geometry();
        self.request_viewport_points();
        if let Some(window) = &self.window {
            window.request_redraw();
        }
    }
}

impl NativeViewerApp {
    fn pane_rects(&self) -> Vec<NativePaneRect> {
        let (width, height) = self
            .renderer
            .as_ref()
            .map(|renderer| (renderer.config.width, renderer.config.height))
            .unwrap_or((1500, 950));
        native_pane_rects(self.multi_view_layout, width, height)
    }

    fn active_pane_rect(&self) -> Option<NativePaneRect> {
        self.pane_rects().get(self.active_pane).copied()
    }

    fn pane_at_screen(&self, screen: (f64, f64)) -> Option<usize> {
        self.pane_rects().iter().position(|rect| {
            screen.0 >= rect.x as f64
                && screen.0 < (rect.x + rect.width) as f64
                && screen.1 >= rect.y as f64
                && screen.1 < (rect.y + rect.height) as f64
        })
    }

    fn active_local_cursor(&self) -> Option<(f64, f64)> {
        let cursor = self.cursor_position?;
        let rect = self.active_pane_rect()?;
        Some((cursor.0 - rect.x as f64, cursor.1 - rect.y as f64))
    }

    fn native_render_panes(&self) -> Vec<NativeRenderPane> {
        let rects = self.pane_rects();
        self.panes
            .iter()
            .zip(rects)
            .filter_map(|(pane, rect)| {
                self.sources
                    .get(pane.source_index)
                    .cloned()
                    .map(|source| NativeRenderPane {
                        source,
                        camera: pane.camera,
                        transform: pane.transform,
                        rect,
                    })
            })
            .collect()
    }

    fn sync_active_pane_from_renderer(&mut self) {
        let (Some(renderer), Some(pane)) =
            (self.renderer.as_ref(), self.panes.get_mut(self.active_pane))
        else {
            return;
        };
        pane.source_index = self.source_index;
        pane.camera = renderer.camera_state;
        pane.transform = renderer.image_transform;
        self.link_active_pane_view();
    }

    /// Mirror the active view into the other panes only when the user has
    /// explicitly enabled linked navigation. Coordinates are normalized to
    /// each source's displayed dimensions and zoom is normalized to fit, so
    /// different-sized slides retain equivalent context without sharing a
    /// camera in raw slide pixels.
    fn link_active_pane_view(&mut self) {
        if !self.multi_view_sync || self.panes.len() < 2 {
            return;
        }
        let rects = self.pane_rects();
        let Some(active) = self.panes.get(self.active_pane).copied() else {
            return;
        };
        let Some(active_source) = self.sources.get(active.source_index) else {
            return;
        };
        let Some(active_rect) = rects.get(self.active_pane) else {
            return;
        };
        let (active_width, active_height) = active.transform.display_dimensions(active_source);
        let active_fit = NativeCamera::fit_dimensions(
            active_width,
            active_height,
            active_rect.width,
            active_rect.height,
        );
        let zoom_ratio =
            (active_fit.pixels_per_screen_pixel / active.camera.pixels_per_screen_pixel).max(1e-6);
        let x_fraction = (active.camera.center_x / active_width.max(1.0)).clamp(0.0, 1.0);
        let y_fraction = (active.camera.center_y / active_height.max(1.0)).clamp(0.0, 1.0);
        for (index, pane) in self.panes.iter_mut().enumerate() {
            if index == self.active_pane || pane.source_index >= self.sources.len() {
                continue;
            }
            let Some(rect) = rects.get(index) else {
                continue;
            };
            let source = &self.sources[pane.source_index];
            let (width, height) = pane.transform.display_dimensions(source);
            let fit = NativeCamera::fit_dimensions(width, height, rect.width, rect.height);
            pane.camera.center_x = x_fraction * width;
            pane.camera.center_y = y_fraction * height;
            pane.camera.pixels_per_screen_pixel =
                (fit.pixels_per_screen_pixel / zoom_ratio).max(1e-6);
        }
    }

    fn activate_pane(&mut self, index: usize) {
        if index >= self.panes.len() {
            return;
        }
        self.sync_active_pane_from_renderer();
        self.active_pane = index;
        let pane = self.panes[index];
        if pane.source_index >= self.sources.len() {
            return;
        }
        let previous_source_id = self.source.id.clone();
        self.source_index = pane.source_index;
        self.source = self.sources[self.source_index].clone();
        if let Some(renderer) = self.renderer.as_mut() {
            renderer.set_source(self.source.clone());
            renderer.camera_state = pane.camera;
            renderer.image_transform = pane.transform;
            renderer.sync_camera();
        }
        // Focusing a populated pane must also focus its R-side project state.
        // Without this, multi-view drawing and navigation work but subsequent
        // annotation events can be applied to whichever slide was active last.
        if previous_source_id != self.source.id {
            if let Some(sync) = self.state_sync.as_ref() {
                sync.project_source_selected(&self.source);
            }
            self.request_state_refresh();
            self.last_dense_camera = None;
            self.last_point_camera = None;
        }
    }

    fn set_multi_view_layout(&mut self, layout: usize) {
        let layout = native_multiview_count(layout);
        self.sync_active_pane_from_renderer();
        if self.panes.is_empty() {
            return;
        }
        let template = self.panes[self.active_pane.min(self.panes.len() - 1)];
        let mut panes = self.panes.clone();
        panes.truncate(layout);
        while panes.len() < layout {
            let mut pane = template;
            // Browser multi-view deliberately leaves new slots blank. Native
            // keeps the pane state but assigns no source until the user picks
            // one from Project; this avoids silently duplicating a slide.
            pane.source_index = usize::MAX;
            panes.push(pane);
        }
        self.multi_view_layout = layout;
        self.panes = panes;
        self.active_pane = self.active_pane.min(layout - 1);
        self.activate_pane(self.active_pane);
        self.record_history(format!("Multi-view layout: {layout} pane(s)"));
    }

    fn record_history(&mut self, message: impl Into<String>) {
        self.history.push_back(message.into());
        while self.history.len() > 160 {
            self.history.pop_front();
        }
    }

    fn annotation_snapshot(&self) -> NativeAnnotationSnapshot {
        self.renderer.as_ref().map(|renderer| NativeAnnotationSnapshot {
            annotations: renderer
                .annotation_shapes
                .iter()
                .map(|shape| shape.feature.clone())
                .collect(),
            trajectories: renderer.trajectory_items.clone(),
        }).unwrap_or_else(|| NativeAnnotationSnapshot {
            annotations: Vec::new(),
            trajectories: Vec::new(),
        })
    }

    fn push_annotation_undo(&mut self) {
        self.annotation_undo.push_back(self.annotation_snapshot());
        while self.annotation_undo.len() > 20 {
            self.annotation_undo.pop_front();
        }
        self.annotation_redo.clear();
    }

    fn restore_annotation_history(&mut self, redo: bool) {
        let snapshot = if redo {
            self.annotation_redo.pop_back()
        } else {
            self.annotation_undo.pop_back()
        };
        let Some(snapshot) = snapshot else {
            self.error = Some(if redo { "Nothing to redo." } else { "Nothing to undo." }.to_string());
            return;
        };
        let current = self.annotation_snapshot();
        if redo {
            self.annotation_undo.push_back(current);
        } else {
            self.annotation_redo.push_back(current);
        }
        if let Some(sync) = self.state_sync.as_ref() {
            sync.annotation_snapshot_restored(
                if redo { "annotation_redo" } else { "annotation_undo" },
                snapshot.annotations,
                snapshot.trajectories,
            );
        }
        self.selected_feature = None;
        self.selected_trajectory = None;
        self.record_history(if redo { "Annotation redo applied" } else { "Annotation undo applied" });
        self.request_state_refresh();
    }

    fn merge_checked_annotations(&mut self) {
        let Some(renderer) = self.renderer.as_ref() else {
            return;
        };
        let selected = renderer
            .annotation_shapes
            .iter()
            .filter_map(|shape| {
                let id = shape.feature.get("id").and_then(json_id).or_else(|| {
                    shape.feature.get("properties")
                        .and_then(|properties| properties.get("roi_id"))
                        .and_then(json_id)
                })?;
                self.selected_annotation_ids.contains(&id).then(|| shape.feature.clone())
            })
            .collect::<Vec<_>>();
        if selected.len() < 2 {
            self.error = Some("Select at least two editable ROIs to merge.".to_string());
            return;
        }
        let merged = match native_merge_annotation_features(&selected) {
            Ok(feature) => feature,
            Err(error) => {
                self.error = Some(error);
                return;
            }
        };
        let retained = renderer
            .annotation_shapes
            .iter()
            .filter_map(|shape| {
                let id = shape.feature.get("id").and_then(json_id).or_else(|| {
                    shape.feature.get("properties")
                        .and_then(|properties| properties.get("roi_id"))
                        .and_then(json_id)
                })?;
                (!self.selected_annotation_ids.contains(&id)).then(|| shape.feature.clone())
            })
            .collect::<Vec<_>>();
        let mut annotations = retained;
        annotations.push(merged.clone());
        let trajectories = renderer.trajectory_items.clone();
        let merged_id = merged.get("id").and_then(json_id).unwrap_or_default();
        self.push_annotation_undo();
        if let Some(sync) = self.state_sync.as_ref() {
            sync.annotation_snapshot_restored(
                "rois_merged",
                annotations,
                trajectories,
            );
        }
        self.selected_annotation_ids.clear();
        if !merged_id.is_empty() {
            self.selected_annotation_ids.insert(merged_id);
        }
        self.selected_feature = Some(merged);
        self.record_history(format!("Merged {} same-class ROI(s)", selected.len()));
        self.request_state_refresh();
    }

    fn fill_selected_annotation_holes(&mut self) {
        let Some(mut feature) = self.selected_feature.clone() else {
            self.error = Some("Select an ROI before filling holes.".to_string());
            return;
        };
        let Some(geometry) = feature.get("geometry").and_then(native_geojson_multi_polygon) else {
            self.error = Some("Selected ROI has no polygon geometry.".to_string());
            return;
        };
        let filled = MultiPolygon(
            geometry.0.into_iter().map(|polygon| Polygon::new(polygon.exterior().clone(), Vec::new())).collect(),
        );
        feature["geometry"] = native_multi_polygon_geojson(&filled);
        if let Some(properties) = feature.get_mut("properties").and_then(serde_json::Value::as_object_mut) {
            properties.insert("filled_holes".to_string(), serde_json::Value::Bool(true));
        }
        self.push_annotation_undo();
        self.selected_feature = Some(feature.clone());
        if let Some(sync) = self.state_sync.as_ref() { sync.roi_updated(feature); }
        self.record_history("Filled selected ROI holes".to_string());
        self.request_state_refresh();
    }

    fn split_selected_annotation(&mut self) {
        let Some(feature) = self.selected_feature.clone() else {
            self.error = Some("Select an ROI before splitting it.".to_string());
            return;
        };
        let parts = match native_split_annotation_feature(&feature) {
            Ok(parts) => parts,
            Err(error) => {
                self.error = Some(error);
                return;
            }
        };
        let original_id = feature.get("id").and_then(json_id).or_else(|| {
            feature.get("properties")
                .and_then(|properties| properties.get("roi_id"))
                .and_then(json_id)
        });
        let Some(original_id) = original_id else {
            self.error = Some("Selected ROI has no stable identifier.".to_string());
            return;
        };
        let Some(renderer) = self.renderer.as_ref() else {
            self.error = Some("The native renderer is not ready.".to_string());
            return;
        };
        let mut annotations = renderer
            .annotation_shapes
            .iter()
            .filter_map(|shape| {
                let id = shape.feature.get("id").and_then(json_id).or_else(|| {
                    shape.feature.get("properties")
                        .and_then(|properties| properties.get("roi_id"))
                        .and_then(json_id)
                })?;
                (id != original_id).then(|| shape.feature.clone())
            })
            .collect::<Vec<_>>();
        let trajectories = renderer.trajectory_items.clone();
        annotations.extend(parts.iter().cloned());
        self.push_annotation_undo();
        if let Some(sync) = self.state_sync.as_ref() {
            sync.annotation_snapshot_restored("roi_split", annotations, trajectories);
        }
        self.selected_annotation_ids.clear();
        if let Some(first) = parts.first().cloned() {
            if let Some(id) = first.get("id").and_then(json_id) {
                self.selected_annotation_ids.insert(id);
            }
            self.selected_feature = Some(first);
        }
        self.record_history(format!("Split selected multipart ROI into {} ROI(s)", parts.len()));
        self.request_state_refresh();
    }

    fn delete_selected_object(&mut self) {
        if let Some(feature) = self.selected_feature.take() {
            self.push_annotation_undo();
            let id = feature.get("id").and_then(json_id).or_else(|| {
                feature
                    .get("properties")
                    .and_then(|properties| properties.get("roi_id"))
                    .and_then(json_id)
            });
            if let (Some(id), Some(sync)) = (id, self.state_sync.as_ref()) {
                self.selected_annotation_ids.remove(&id);
                sync.roi_deleted(&id);
                self.request_state_refresh();
            }
            return;
        }
        if let Some(trajectory) = self.selected_trajectory.take() {
            self.push_annotation_undo();
            if let (Some(id), Some(sync)) =
                (trajectory_item_id(&trajectory), self.state_sync.as_ref())
            {
                sync.trajectory_deleted(&id);
                self.request_state_refresh();
            }
            return;
        }
        if let Some(id) = self.selected_measurement_id.take() {
            if let Some(sync) = self.state_sync.as_ref() {
                sync.measurement_deleted(&id);
                self.request_state_refresh();
            }
        }
    }

    fn prepare_egui_frame(&mut self) -> Option<NativeEguiFrame> {
        let (Some(context), Some(window)) = (self.egui_context.clone(), self.window.clone()) else {
            return None;
        };
        let pane_rects = self.pane_rects();
        let label_panes = self.native_render_panes();
        let state = self.egui_state.as_mut()?;
        let raw_input = state.take_egui_input(&window);
        let source_labels = self
            .sources
            .iter()
            .map(|source| source.label.clone())
            .collect::<Vec<_>>();
        let source_index = self.source_index;
        let source_count = source_labels.len();
        let active_pane = self.active_pane;
        let pane_count = self.panes.len().max(1);
        let pane_labels = self
            .panes
            .iter()
            .zip(pane_rects.iter().copied())
            .enumerate()
            .map(|(index, (pane, rect))| {
                let label = self
                    .sources
                    .get(pane.source_index)
                    .map(|source| source.label.clone())
                    .unwrap_or_else(|| "Empty".to_string());
                (index, rect, label, index == active_pane)
            })
            .collect::<Vec<_>>();
        let empty_panes = self
            .panes
            .iter()
            .zip(pane_rects.iter().copied())
            .enumerate()
            .filter_map(|(index, (pane, rect))| {
                (pane.source_index >= self.sources.len()).then_some((index, rect))
            })
            .collect::<Vec<_>>();
        let pane_scales = self
            .panes
            .iter()
            .zip(pane_rects)
            .filter_map(|(pane, rect)| {
                let source = self.sources.get(pane.source_index)?;
                let mpp = native_source_mpp(source)?;
                let target_px = 112.0;
                let scale_um =
                    native_nice_scale_length(target_px * mpp * pane.camera.pixels_per_screen_pixel);
                let width_px = (scale_um / (mpp * pane.camera.pixels_per_screen_pixel))
                    .clamp(40.0, 160.0) as f32;
                Some((rect, width_px, native_scale_label(scale_um)))
            })
            .collect::<Vec<_>>();
        let scale_info = self.renderer.as_ref().map(|renderer| {
            let mpp = native_source_mpp(&renderer.source);
            let base_magnification = native_base_magnification(&renderer.source);
            let magnification = base_magnification / renderer.camera_state.pixels_per_screen_pixel;
            (
                mpp,
                base_magnification,
                magnification,
                renderer.camera_state,
            )
        });
        let annotation_count = self
            .renderer
            .as_ref()
            .map(|renderer| renderer.annotation_shapes.len())
            .unwrap_or(0);
        let trajectory_entries = self
            .renderer
            .as_ref()
            .map(|renderer| {
                renderer
                    .trajectory_items
                    .iter()
                    .filter_map(|item| {
                        Some((
                            trajectory_item_id(item)?,
                            trajectory_item_label(item),
                            item.clone(),
                        ))
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        let trajectory_count = trajectory_entries.len();
        let measurement_entries = self
            .renderer
            .as_ref()
            .map(|renderer| {
                renderer
                    .measurement_items
                    .iter()
                    .filter_map(|item| {
                        let id = item.get("id").and_then(json_id)?;
                        let distance_um = item
                            .get("distance_um")
                            .and_then(serde_json::Value::as_f64);
                        let distance_px = item
                            .get("distance_px")
                            .and_then(serde_json::Value::as_f64);
                        let label = match distance_um {
                            Some(value) if value.is_finite() => format!("{value:.1} um"),
                            _ => format!("{:.1} px", distance_px.unwrap_or(0.0)),
                        };
                        Some((id, label))
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        let annotation_entries = self
            .renderer
            .as_ref()
            .map(|renderer| {
                renderer
                    .annotation_shapes
                    .iter()
                    .filter_map(|shape| {
                        let id = shape.feature.get("id").and_then(json_id).or_else(|| {
                            shape
                                .feature
                                .get("properties")
                                .and_then(|properties| properties.get("roi_id"))
                                .and_then(json_id)
                        })?;
                        Some((
                            id,
                            annotation_feature_label(&shape.feature),
                            shape.feature.clone(),
                        ))
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        // Labels must be positioned against each pane's independent camera.
        // Keeping this in the native renderer makes split views as informative
        // as the single-view mode without drawing labels over the wrong slide.
        let annotation_labels = self
            .renderer
            .as_ref()
            .map(|renderer| {
                label_panes
                    .iter()
                    .flat_map(|pane| renderer.annotation_label_positions_for_pane(pane, 80))
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        let dense_sources = self.dense_sources.clone();
        let native_points = self.point_sources.clone();
        let native_channels = self
            .renderer
            .as_ref()
            .map(|renderer| {
                renderer
                    .channel_sources
                    .iter()
                    .filter_map(|(id, source)| {
                        let style = renderer.channel_styles.get(id)?;
                        Some((
                            id.clone(),
                            source.label.clone(),
                            style.visible,
                            style.opacity,
                            source.colour.clone(),
                            style.gain,
                            style.contrast_min,
                            style.contrast_max,
                        ))
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        let active_stain_display = self
            .renderer
            .as_ref()
            .map(|renderer| NativeStainDisplay::from_shader_mode(renderer.base_style.stain_mode))
            .unwrap_or(NativeStainDisplay::Original);
        let mut base_image_visible = self.renderer.as_ref().map(|renderer| renderer.base_style.visible).unwrap_or(true);
        let mut base_image_opacity = self.renderer.as_ref().map(|renderer| renderer.base_style.opacity).unwrap_or(1.0);
        let mut base_image_changed = false;
        let tool = self.tool;
        let mut annotation_label = self.annotation_label.clone();
        let mut annotation_colour = self.annotation_colour.clone();
        let mut brush_size_slide = self.brush_size_slide;
        let mut trajectory_area_width_slide = self.trajectory_area_width_slide;
        let selected_annotation = self
            .selected_feature
            .as_ref()
            .and_then(|feature| feature.get("id"))
            .and_then(json_id);
        let selected_trajectory = self
            .selected_trajectory
            .as_ref()
            .and_then(trajectory_item_id);
        let selected_measurement_id = self.selected_measurement_id.clone();
        let mut select_source = None;
        let navigator_snapshot = self.renderer.as_ref().map(|renderer| {
            let (width, height) = renderer.image_transform.display_dimensions(&renderer.source);
            (
                width,
                height,
                renderer.camera_state,
                renderer.config.width,
                renderer.config.height,
            )
        });
        let navigator_texture = self.overview_textures.get(&self.source.id).cloned();
        let mut navigator_pan = None;
        let mut zoom_factor = None;
        let mut reset_view = false;
        let mut rotate_by = None;
        let mut flip_horizontal = false;
        let mut flip_vertical = false;
        let mut reset_orientation = false;
        let mut magnification_target = None;
        let mut toggle_polygon = false;
        let mut toggle_brush = false;
        let mut toggle_edit = false;
        let mut toggle_trajectory = false;
        let mut toggle_measurement = false;
        let mut commit_polygon = false;
        let mut cancel_polygon = false;
        let mut undo_annotations = false;
        let mut redo_annotations = false;
        let mut show_project = self.show_project_panel;
        let mut show_annotation = self.show_annotation_panel;
        let mut show_layers = self.show_layer_panel;
        let mut show_history = self.show_history_panel;
        let mut show_help = self.show_help_panel;
        let mut show_annotation_labels = self.show_annotation_labels;
        let history_entries = self.history.iter().cloned().collect::<Vec<_>>();
        let mut clear_history = false;
        let mut dense_visibility = Vec::new();
        let mut point_display = Vec::new();
        let mut channel_display = Vec::new();
        let mut select_stain_display = None;
        let mut select_annotation = None;
        let mut delete_selected_annotation = false;
        let mut delete_checked_annotations = false;
        let mut clear_checked_annotations = false;
        let mut merge_checked_annotations = false;
        let mut checked_annotation_ids = self.selected_annotation_ids.clone();
        let mut update_selected_annotation = None;
        let mut fill_selected_annotation_holes = false;
        let mut split_selected_annotation = false;
        let mut selected_annotation_category = self.selected_feature.as_ref()
            .map(annotation_feature_label).unwrap_or_default();
        let mut selected_annotation_colour = self.selected_feature.as_ref()
            .map(annotation_feature_colour).unwrap_or_else(|| "#22C55E".to_string());
        let mut select_trajectory = None;
        let mut delete_selected_trajectory = false;
        let mut clear_trajectories = false;
        let mut create_trajectory_area = false;
        let mut update_trajectory_area = false;
        let mut edit_trajectory_area = false;
        let mut select_measurement = None;
        let mut delete_selected_measurement = false;
        let mut clear_measurements = false;
        let mut import_geojson = false;
        let mut export_geojson = false;
        let grandqc_items = native_grandqc_items(&self.cellphenotyper_config);
        let kodama_items = native_kodama_items(&self.cellphenotyper_config);
        let mut load_grandqc_item: Option<usize> = None;
        let mut load_all_grandqc = false;
        let mut clear_grandqc = false;
        let mut load_kodama_item: Option<usize> = None;
        let mut load_all_kodama = false;
        let mut clear_kodama = false;
        let mut save_project = false;
        let mut open_project = false;
        let mut save_screenshot = false;
        let mut export_image_scope: Option<&str> = None;
        let mut gene_name = self.active_gene.clone();
        let mut fetch_gene = false;
        let mut reset_spatial_colours = false;
        let mut spatial_point_scale = self.spatial_point_scale;
        let mut spatial_transform = self.spatial_transform.clone();
        let mut reset_spatial_registration = false;
        let mut save_registered_spatial_object = false;
        let cluster_fields = native_cluster_fields(&self.spatial_clusters);
        let mut spatial_plots = native_spatial_reduction_plots(&self.spatial_config);
        let spatial_plot_count = spatial_plots.len();
        let kodama_plots = native_spatial_reduction_plots(
            self.cellphenotyper_config.get("kodama").unwrap_or(&serde_json::Value::Null),
        );
        let kodama_plot_ids = kodama_plots.iter().map(|plot| plot.id.clone()).collect::<HashSet<_>>();
        spatial_plots.extend(kodama_plots);
        let mut show_spatial_plot = self.show_spatial_plot;
        let mut spatial_plot_index = self.spatial_plot_index.min(spatial_plots.len().saturating_sub(1));
        let mut spatial_selected_labels = self.spatial_selected_labels.clone();
        let mut spatial_plot_drag_start = self.spatial_plot_drag_start;
        let mut spatial_selection_changed = false;
        let mut cluster_field = self.active_cluster.clone();
        let mut apply_cluster = false;
        let prediction_sources = native_prediction_sources(&self.prediction_config);
        let mut prediction_source = if prediction_sources.iter().any(|(id, _, _, _)| id == &self.prediction_source) {
            self.prediction_source.clone()
        } else { prediction_sources.first().map(|(id, _, _, _)| id.clone()).unwrap_or_default() };
        let mut prediction_train = self.prediction_train.clone();
        prediction_train.retain(|id| annotation_entries.iter().any(|(roi_id, _, _)| roi_id == id));
        let mut prediction_ncomp = 2_i32;
        let mut prediction_dims = prediction_sources.iter().find(|(id, _, _, _)| id == &prediction_source)
            .filter(|(_, _, reduction, _)| *reduction).map(|(_, _, _, dimensions)| (*dimensions).min(10).max(1) as i32).unwrap_or(0);
        let prediction_available = self.prediction_fetcher.is_some() && !prediction_sources.is_empty() && !annotation_entries.is_empty();
        let mut run_prediction = false;
        let segmentation_available = !self.segmentation_run_url.trim().is_empty();
        let mut segmentation_engine = self.segmentation_engine.clone();
        let mut run_segmentation = false;
        let mut import_segmentation = false;
        let proximity_annotations = self.renderer.as_ref()
            .map(|renderer| native_proximity_annotations(&renderer.annotation_shapes))
            .unwrap_or_default();
        let proximity_available = self.proximity_fetcher.is_some()
            && !self.point_sources.is_empty()
            && proximity_annotations.len() >= 2;
        let association_available = self.proximity_fetcher.is_some()
            && !self.point_sources.is_empty()
            && !annotation_entries.is_empty();
        let mut proximity_query = if proximity_annotations.iter().any(|(id, _)| id == &self.proximity_query) {
            self.proximity_query.clone()
        } else { proximity_annotations.first().map(|(id, _)| id.clone()).unwrap_or_default() };
        let mut proximity_target = if proximity_annotations.iter().any(|(id, _)| id == &self.proximity_target) {
            self.proximity_target.clone()
        } else { proximity_annotations.get(1).map(|(id, _)| id.clone()).unwrap_or_default() };
        let mut run_proximity = false;
        let mut proximity_stats_feature = self.proximity_stats_feature.clone();
        let mut proximity_stats_method = self.proximity_stats_method.clone();
        let mut run_proximity_stats = false;
        let mut show_proximity_stats = self.show_proximity_stats;
        let proximity_stats_rows = self.proximity_stats_rows.clone();
        let mut trajectory_profile_source = if self.point_sources.iter().any(|source| source.id == self.trajectory_profile_source) {
            self.trajectory_profile_source.clone()
        } else { self.point_sources.first().map(|source| source.id.clone()).unwrap_or_default() };
        let mut trajectory_profile_feature = self.trajectory_profile_feature.clone();
        let mut trajectory_profile_bins = self.trajectory_profile_bins;
        let mut trajectory_profile_width = self.trajectory_profile_width;
        let mut run_trajectory_profile = false;
        let mut clear_trajectory_profile = false;
        let mut show_trajectory_profile = self.show_trajectory_profile;
        let trajectory_profile_rows = self.trajectory_profile_rows.clone();
        let mut run_annotation_association = false;
        let mut export_annotation_association = false;
        let mut multi_view_layout = self.multi_view_layout;
        let mut multi_view_custom = self.multi_view_layout as i32;
        let mut multi_view_sync = self.multi_view_sync;
        let output = context.run(raw_input, |ctx| {
            egui::TopBottomPanel::top("native_top_toolbar").show(ctx, |ui| {
                ui.horizontal_wrapped(|ui| {
                    ui.strong(egui::RichText::new("wsiTools").size(18.0));
                    ui.weak("Live R-connected pathology viewer");
                    let sync_label = if self.state_sync.is_some() {
                        egui::RichText::new("R sync: live").color(egui::Color32::from_rgb(94, 234, 212))
                    } else {
                        egui::RichText::new("R sync: unavailable").color(egui::Color32::from_rgb(251, 191, 36))
                    };
                    ui.label(sync_label);
                    ui.separator();
                    ui.menu_button("Project", |ui| {
                        if ui.button("Fit image").clicked() {
                            reset_view = true;
                            ui.close_menu();
                        }
                        if ui.button("Save project...").clicked() {
                            save_project = true;
                            ui.close_menu();
                        }
                        if ui.button("Open project...").clicked() {
                            open_project = true;
                            ui.close_menu();
                        }
                        ui.checkbox(&mut show_project, "Project panel");
                        ui.checkbox(&mut show_layers, "Layers panel");
                        ui.checkbox(&mut show_history, "History panel");
                    });
                    ui.menu_button("View", |ui| {
                        ui.label("Magnification");
                        ui.horizontal(|ui| {
                            for value in [5.0, 10.0, 20.0, 40.0] {
                                if ui.button(format!("{}x", value as i32)).clicked() {
                                    magnification_target = Some(value);
                                    ui.close_menu();
                                }
                            }
                        });
                        if let Some((_, _, magnification, _)) = scale_info {
                            ui.weak(format!("Current ~{magnification:.1}x"));
                        }
                        if ui.button("Save screenshot...").clicked() {
                            save_screenshot = true;
                            ui.close_menu();
                        }
                        if ui.button("Export full-resolution viewport...").clicked() {
                            export_image_scope = Some("viewport");
                            ui.close_menu();
                        }
                        if ui
                            .add_enabled(self.selected_feature.is_some(), egui::Button::new("Export selected ROI image..."))
                            .clicked()
                        {
                            export_image_scope = Some("selected_roi");
                            ui.close_menu();
                        }
                        ui.separator();
                        if ui.button("Rotate left 90 deg").clicked() {
                            rotate_by = Some(270);
                            ui.close_menu();
                        }
                        if ui.button("Rotate right 90 deg").clicked() {
                            rotate_by = Some(90);
                            ui.close_menu();
                        }
                        if ui.button("Flip horizontal").clicked() {
                            flip_horizontal = true;
                            ui.close_menu();
                        }
                        if ui.button("Flip vertical").clicked() {
                            flip_vertical = true;
                            ui.close_menu();
                        }
                        if ui.button("Reset orientation").clicked() {
                            reset_orientation = true;
                            ui.close_menu();
                        }
                        ui.separator();
                        ui.label("Multi-view");
                        ui.horizontal(|ui| {
                            for layout in [1_usize, 2, 4, 6] {
                                if ui
                                    .selectable_label(
                                        multi_view_layout == layout,
                                        format!("{layout} view{}", if layout == 1 { "" } else { "s" }),
                                    )
                                    .clicked()
                                {
                                    multi_view_layout = layout;
                                    ui.close_menu();
                                }
                            }
                        });
                        ui.horizontal(|ui| {
                            ui.label("Custom panes");
                            let changed = ui
                                .add(egui::DragValue::new(&mut multi_view_custom).range(1..=12))
                                .changed();
                            if changed {
                                multi_view_layout = multi_view_custom.clamp(1, 12) as usize;
                            }
                        });
                        ui.checkbox(&mut multi_view_sync, "Link zoom/pan");
                    });
                    ui.menu_button("Stains", |ui| {
                        ui.label("Base image");
                        if ui.checkbox(&mut base_image_visible, "Show H&E / base image").changed() {
                            base_image_changed = true;
                        }
                        if ui.add(egui::Slider::new(&mut base_image_opacity, 0.0..=1.0).text("Base opacity")).changed() {
                            base_image_changed = true;
                        }
                        ui.separator();
                        ui.label("Brightfield display");
                        for display in [
                            NativeStainDisplay::Original,
                            NativeStainDisplay::Hematoxylin,
                            NativeStainDisplay::Eosin,
                            NativeStainDisplay::Residual,
                        ] {
                            if ui
                                .selectable_label(active_stain_display == display, display.label())
                                .clicked()
                            {
                                select_stain_display = Some(display);
                                ui.close_menu();
                            }
                        }
                        ui.separator();
                        ui.label("Tiled channel layers");
                        if native_channels.is_empty() {
                            ui.weak("No tiled channel layers are available for this project.");
                        }
                        for (
                            id,
                            name,
                            initial_visible,
                            initial_opacity,
                            initial_colour,
                            initial_gain,
                            initial_minimum,
                            initial_maximum,
                        ) in &native_channels
                        {
                            let mut visible = *initial_visible;
                            let mut opacity = *initial_opacity;
                            if ui.checkbox(&mut visible, name).changed()
                                || ui
                                    .add(
                                        egui::Slider::new(&mut opacity, 0.0..=1.0)
                                            .text("Opacity"),
                                    )
                                    .changed()
                            {
                                channel_display.push((
                                    id.clone(),
                                    visible,
                                    opacity,
                                    initial_colour.clone(),
                                    *initial_gain,
                                    *initial_minimum,
                                    *initial_maximum,
                                ));
                            }
                        }
                    });
                    if !self.point_sources.is_empty() {
                        ui.menu_button("Spatial", |ui| {
                            ui.label("Colour visible coordinates by gene");
                            ui.horizontal(|ui| {
                                ui.text_edit_singleline(&mut gene_name);
                                if ui.button("Apply").clicked() {
                                    fetch_gene = true;
                                    ui.close_menu();
                                }
                            });
                            if !self.active_gene.is_empty() {
                                ui.weak(format!("Active gene: {}", self.active_gene));
                            }
                            if !self.active_gene.is_empty() || !self.active_cluster.is_empty() {
                                if ui.button("Restore original coordinate colours").clicked() {
                                    reset_spatial_colours = true;
                                    ui.close_menu();
                                }
                            }
                            ui.add(
                                egui::Slider::new(&mut spatial_point_scale, 0.1..=4.0)
                                    .logarithmic(true)
                                    .text("Coordinate size"),
                            );
                            if !spatial_plots.is_empty() {
                                ui.separator();
                                ui.label("Dimensionality reduction");
                                for (index, plot) in spatial_plots.iter().enumerate() {
                                    if ui.button(&plot.label).clicked() {
                                        spatial_plot_index = index;
                                        show_spatial_plot = true;
                                        ui.close_menu();
                                    }
                                }
                            }
                            if !cluster_fields.is_empty() {
                                ui.separator();
                                ui.label("Colour coordinates by cluster");
                                for (field, label, _) in &cluster_fields {
                                    ui.radio_value(&mut cluster_field, field.clone(), label);
                                }
                                if ui.button("Apply cluster colours").clicked() {
                                    apply_cluster = true;
                                    ui.close_menu();
                                }
                            }
                            ui.separator();
                            ui.label("Coordinate registration");
                            ui.weak("Transforms visible spatial coordinates in R before GPU rendering.");
                            ui.horizontal(|ui| {
                                ui.label("Move");
                                ui.add(egui::DragValue::new(&mut spatial_transform.offset_x).speed(5.0).prefix("X "));
                                ui.add(egui::DragValue::new(&mut spatial_transform.offset_y).speed(5.0).prefix("Y "));
                            });
                            ui.horizontal(|ui| {
                                ui.label("Scale");
                                ui.add(egui::DragValue::new(&mut spatial_transform.scale_x).speed(0.01).range(0.05..=20.0).prefix("X "));
                                ui.add(egui::DragValue::new(&mut spatial_transform.scale_y).speed(0.01).range(0.05..=20.0).prefix("Y "));
                            });
                            ui.horizontal(|ui| {
                                ui.label("Rotate");
                                ui.add(egui::DragValue::new(&mut spatial_transform.rotation_degrees).speed(1.0).suffix(" deg"));
                            });
                            ui.horizontal(|ui| {
                                ui.checkbox(&mut spatial_transform.flip_horizontal, "Flip H");
                                ui.checkbox(&mut spatial_transform.flip_vertical, "Flip V");
                                if ui.button("Reset").clicked() { reset_spatial_registration = true; }
                            });
                            if ui.button("Save registered spatial object...").clicked() {
                                save_registered_spatial_object = true;
                                ui.close_menu();
                            }
                        });
                    }
                    if spatial_plots.len() > spatial_plot_count || !kodama_items.is_empty() {
                        ui.menu_button("CellPhenotyper", |ui| {
                            if !kodama_items.is_empty() {
                                ui.label("KODAMA MedSAM refinements");
                                if ui.button("Load all KODAMA GeoJSON files").clicked() {
                                    load_all_kodama = true;
                                    ui.close_menu();
                                }
                                if ui.button("Clear KODAMA refinements").clicked() {
                                    clear_kodama = true;
                                    ui.close_menu();
                                }
                                ui.separator();
                                for (index, item) in kodama_items.iter().enumerate() {
                                    if ui.button(format!("Load {}", item.label)).clicked() {
                                        load_kodama_item = Some(index);
                                        ui.close_menu();
                                    }
                                }
                                if spatial_plots.len() > spatial_plot_count {
                                    ui.separator();
                                }
                            }
                            if spatial_plots.len() > spatial_plot_count {
                                ui.label("KODAMA embedding");
                                for (index, plot) in spatial_plots.iter().enumerate().skip(spatial_plot_count) {
                                    if ui.button(&plot.label).clicked() {
                                        spatial_plot_index = index;
                                        show_spatial_plot = true;
                                        ui.close_menu();
                                    }
                                }
                            }
                        });
                    }
                    if !grandqc_items.is_empty() {
                        ui.menu_button("Artifacts", |ui| {
                            ui.label("GrandQC artifact annotations");
                            if ui.button("Load all GrandQC files").clicked() {
                                load_all_grandqc = true;
                                ui.close_menu();
                            }
                            if ui.button("Clear GrandQC annotations").clicked() {
                                clear_grandqc = true;
                                ui.close_menu();
                            }
                            ui.separator();
                            for (index, item) in grandqc_items.iter().enumerate() {
                                if ui.button(format!("Load {}", item.label)).clicked() {
                                    load_grandqc_item = Some(index);
                                    ui.close_menu();
                                }
                            }
                        });
                    }
                    if self.prediction_fetcher.is_some() {
                        ui.menu_button("Prediction", |ui| {
                            ui.label("PLS-LDA annotation prediction in R");
                            if prediction_sources.is_empty() {
                                ui.weak("No spatial or cell feature source is available.");
                            } else {
                                egui::ComboBox::from_id_salt("native_prediction_source")
                                    .selected_text(prediction_sources.iter().find(|(id, _, _, _)| id == &prediction_source).map(|(_, label, _, _)| label.as_str()).unwrap_or("Feature source"))
                                    .show_ui(ui, |ui| {
                                        for (id, label, _, _) in &prediction_sources {
                                            ui.selectable_value(&mut prediction_source, id.clone(), label);
                                        }
                                    });
                                ui.add(egui::Slider::new(&mut prediction_ncomp, 1..=10).text("PLS components"));
                                if prediction_dims > 0 {
                                    let max_dims = prediction_sources.iter().find(|(id, _, _, _)| id == &prediction_source).map(|(_, _, _, dimensions)| (*dimensions).max(1) as i32).unwrap_or(1);
                                    ui.add(egui::Slider::new(&mut prediction_dims, 1..=max_dims).text("Reduction dimensions"));
                                }
                                ui.separator();
                                ui.label("Training annotation(s)");
                                for (id, label, _) in &annotation_entries {
                                    let mut selected = prediction_train.iter().any(|selected_id| selected_id == id);
                                    if ui.checkbox(&mut selected, label).changed() {
                                        if selected { prediction_train.push(id.clone()); }
                                        else { prediction_train.retain(|selected_id| selected_id != id); }
                                    }
                                }
                                if ui.add_enabled(prediction_available && !prediction_train.is_empty(), egui::Button::new("Predict all non-training points")).clicked() {
                                    run_prediction = true;
                                    ui.close_menu();
                                }
                            }
                        });
                    }
                    if segmentation_available || self.state_sync.is_some() {
                        ui.menu_button("Cells", |ui| {
                            if segmentation_available {
                                ui.label("Segment the selected ROI in R");
                                for (engine, label) in [
                                    ("stardist_he", "StarDist H&E"),
                                    ("stardist_ihc", "StarDist IHC"),
                                    ("mesmer_dapi", "Mesmer DAPI"),
                                ] {
                                    ui.radio_value(&mut segmentation_engine, engine.to_string(), label);
                                }
                                if self.selected_feature.is_none() {
                                    ui.weak("Double-click an ROI, then run selected ROI.");
                                }
                                if ui
                                    .add_enabled(self.selected_feature.is_some(), egui::Button::new("Run selected ROI"))
                                    .clicked()
                                {
                                    run_segmentation = true;
                                    ui.close_menu();
                                }
                                ui.separator();
                            }
                            if ui.button("Import cell segmentation...").clicked() {
                                import_segmentation = true;
                                ui.close_menu();
                            }
                        });
                    }
                    ui.menu_button("Annotations", |ui| {
                        ui.horizontal(|ui| {
                            if ui
                                .add_enabled(!self.annotation_undo.is_empty(), egui::Button::new("Undo"))
                                .clicked()
                            {
                                undo_annotations = true;
                                ui.close_menu();
                            }
                            if ui
                                .add_enabled(!self.annotation_redo.is_empty(), egui::Button::new("Redo"))
                                .clicked()
                            {
                                redo_annotations = true;
                                ui.close_menu();
                            }
                        });
                        ui.separator();
                        if ui
                            .selectable_label(tool == NativeInteractionTool::Pan, "Pan")
                            .clicked()
                        {
                            if tool == NativeInteractionTool::Polygon {
                                toggle_polygon = true;
                            }
                            ui.close_menu();
                        }
                        if ui
                            .selectable_label(
                                tool == NativeInteractionTool::Polygon,
                                "Draw polygon",
                            )
                            .clicked()
                        {
                            if tool != NativeInteractionTool::Polygon {
                                toggle_polygon = true;
                            }
                            ui.close_menu();
                        }
                        if ui
                            .selectable_label(tool == NativeInteractionTool::Brush, "Paint brush")
                            .clicked()
                        {
                            toggle_brush = true;
                            ui.close_menu();
                        }
                        if ui
                            .selectable_label(tool == NativeInteractionTool::Edit, "Edit selected ROI")
                            .clicked()
                        {
                            toggle_edit = true;
                            ui.close_menu();
                        }
                        ui.checkbox(&mut show_annotation, "Annotations panel");
                        ui.checkbox(&mut show_annotation_labels, "Show ROI labels");
                        ui.separator();
                        if ui.button("Import GeoJSON").clicked() {
                            import_geojson = true;
                            ui.close_menu();
                        }
                        if ui.button("Export GeoJSON").clicked() {
                            export_geojson = true;
                            ui.close_menu();
                        }
                        if association_available {
                            ui.separator();
                            if ui.button("Associate spatial points/cells").clicked() {
                                run_annotation_association = true;
                                ui.close_menu();
                            }
                            if ui.button("Export association CSV...").clicked() {
                                export_annotation_association = true;
                                ui.close_menu();
                            }
                        }
                    });
                    ui.menu_button("Trajectories", |ui| {
                        if ui
                            .selectable_label(
                                tool == NativeInteractionTool::Trajectory,
                                "Draw trajectory",
                            )
                            .clicked()
                        {
                            toggle_trajectory = true;
                            ui.close_menu();
                        }
                        if ui.selectable_label(tool == NativeInteractionTool::Measurement, "Measure distance").clicked() {
                            toggle_measurement = true;
                            ui.close_menu();
                        }
                        if ui
                            .add_enabled(!measurement_entries.is_empty(), egui::Button::new("Clear measurements"))
                            .clicked()
                        {
                            clear_measurements = true;
                            ui.close_menu();
                        }
                        ui.separator();
                        ui.label("Trajectory area");
                        ui.add(
                            egui::Slider::new(&mut trajectory_area_width_slide, 16.0..=5000.0)
                                .logarithmic(true)
                                .text("width (px)"),
                        );
                        if ui
                            .add_enabled(selected_trajectory.is_some(), egui::Button::new("Create area"))
                            .clicked()
                        {
                            create_trajectory_area = true;
                            ui.close_menu();
                        }
                        if ui
                            .add_enabled(selected_trajectory.is_some(), egui::Button::new("Update area"))
                            .clicked()
                        {
                            update_trajectory_area = true;
                            ui.close_menu();
                        }
                        if ui
                            .add_enabled(selected_trajectory.is_some(), egui::Button::new("Edit area border"))
                            .clicked()
                        {
                            edit_trajectory_area = true;
                            ui.close_menu();
                        }
                        ui.separator();
                        ui.label("Proximity analysis in R");
                        if proximity_annotations.is_empty() {
                            ui.weak("Draw or import two tissue annotations first.");
                        } else {
                            egui::ComboBox::from_id_salt("native_proximity_query")
                                .selected_text(proximity_annotations.iter().find(|(id, _)| id == &proximity_query).map(|(_, label)| label.as_str()).unwrap_or("Measure inside"))
                                .show_ui(ui, |ui| {
                                    for (id, label) in &proximity_annotations {
                                        ui.selectable_value(&mut proximity_query, id.clone(), label);
                                    }
                                });
                            egui::ComboBox::from_id_salt("native_proximity_target")
                                .selected_text(proximity_annotations.iter().find(|(id, _)| id == &proximity_target).map(|(_, label)| label.as_str()).unwrap_or("Distance from"))
                                .show_ui(ui, |ui| {
                                    for (id, label) in &proximity_annotations {
                                        ui.selectable_value(&mut proximity_target, id.clone(), label);
                                    }
                                });
                            if ui.add_enabled(proximity_available && proximity_query != proximity_target, egui::Button::new("Run proximity")).clicked() {
                                run_proximity = true;
                                ui.close_menu();
                            }
                            ui.separator();
                            ui.label("Distance statistics in R");
                            egui::ComboBox::from_id_salt("native_proximity_stats_feature")
                                .selected_text(if proximity_stats_feature == "auto" { "Auto: raw expression / cell table" } else { proximity_stats_feature.as_str() })
                                .show_ui(ui, |ui| {
                                    ui.selectable_value(&mut proximity_stats_feature, "auto".to_string(), "Auto: raw expression / cell table");
                                    for (id, label, _, _) in &prediction_sources {
                                        ui.selectable_value(&mut proximity_stats_feature, id.clone(), label);
                                    }
                                });
                            ui.horizontal(|ui| {
                                ui.radio_value(&mut proximity_stats_method, "spearman".to_string(), "Spearman");
                                ui.radio_value(&mut proximity_stats_method, "pearson".to_string(), "Pearson");
                                ui.radio_value(&mut proximity_stats_method, "MINE".to_string(), "MINE");
                            });
                            if ui.add_enabled(proximity_available && proximity_query != proximity_target, egui::Button::new("Run statistics")).clicked() {
                                run_proximity_stats = true;
                                ui.close_menu();
                            }
                            if ui.add_enabled(!proximity_stats_rows.is_empty(), egui::Button::new("Show statistics table")).clicked() {
                                show_proximity_stats = true;
                                ui.close_menu();
                            }
                        }
                        ui.separator();
                        ui.label("Gradient profile along trajectory in R");
                        if self.point_sources.is_empty() {
                            ui.weak("No spatial point or cell layer is available.");
                        } else {
                            egui::ComboBox::from_id_salt("native_trajectory_profile_source")
                                .selected_text(self.point_sources.iter().find(|source| source.id == trajectory_profile_source).map(|source| source.name.as_str()).unwrap_or("Point source"))
                                .show_ui(ui, |ui| {
                                    for source in &self.point_sources {
                                        ui.selectable_value(&mut trajectory_profile_source, source.id.clone(), &source.name);
                                    }
                                });
                            ui.horizontal(|ui| {
                                ui.label("Feature");
                                ui.text_edit_singleline(&mut trajectory_profile_feature).on_hover_text("count, a numeric point field, or a categorical point field");
                            });
                            ui.add(egui::Slider::new(&mut trajectory_profile_bins, 2..=100).text("Bins"));
                            ui.add(egui::Slider::new(&mut trajectory_profile_width, 10.0..=5000.0).logarithmic(true).text("Width (px)"));
                            let profile_ready = self.trajectory_profile_fetcher.is_some()
                                && selected_trajectory.is_some()
                                && !trajectory_profile_source.is_empty();
                            if ui.add_enabled(profile_ready, egui::Button::new("Run gradient profile")).clicked() {
                                run_trajectory_profile = true;
                                ui.close_menu();
                            }
                            if ui.add_enabled(!trajectory_profile_rows.is_empty(), egui::Button::new("Show gradient profile table")).clicked() {
                                show_trajectory_profile = true;
                                ui.close_menu();
                            }
                            if ui.add_enabled(!trajectory_profile_rows.is_empty(), egui::Button::new("Clear gradient profile")).clicked() {
                                clear_trajectory_profile = true;
                                ui.close_menu();
                            }
                            if selected_trajectory.is_none() { ui.weak("Select a saved trajectory first."); }
                        }
                        ui.separator();
                        if trajectory_entries.is_empty() {
                            ui.weak("No saved trajectories");
                        } else {
                            for (id, label, item) in &trajectory_entries {
                                if ui
                                    .selectable_label(
                                        selected_trajectory.as_deref() == Some(id.as_str()),
                                        label,
                                    )
                                    .clicked()
                                {
                                    select_trajectory = Some(item.clone());
                                    ui.close_menu();
                                }
                            }
                        }
                        if ui
                            .add_enabled(!trajectory_entries.is_empty(), egui::Button::new("Clear all trajectories"))
                            .clicked()
                        {
                            clear_trajectories = true;
                            ui.close_menu();
                        }
                    });
                    ui.menu_button("Help", |ui| {
                        if ui.button("Keyboard shortcuts").clicked() {
                            show_help = true;
                            ui.close_menu();
                        }
                        if ui.button("Native viewer guide").clicked() {
                            show_help = true;
                            ui.close_menu();
                        }
                    });
                    ui.separator();
                    egui::ComboBox::from_id_salt("native_source_select")
                        .selected_text(format!(
                            "{} ({}/{})",
                            source_labels
                                .get(source_index)
                                .map(String::as_str)
                                .unwrap_or("Slide"),
                            source_index + 1,
                            source_count
                        ))
                        .show_ui(ui, |ui| {
                            for (index, label) in source_labels.iter().enumerate() {
                                if ui.selectable_label(index == source_index, label).clicked() {
                                    select_source = Some(index);
                                }
                            }
                        });
                    ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                        ui.label(if tool == NativeInteractionTool::Polygon {
                            "Polygon: click vertices, Enter saves, Esc cancels"
                        } else if tool == NativeInteractionTool::Edit {
                            "Edit: drag a selected ROI vertex, Esc returns to pan"
                        } else {
                            "Pan"
                        });
                    });
                });
            });

            // Keep image navigation where the browser viewer puts it: a compact
            // floating dock that never consumes a permanent strip of canvas.
            egui::Area::new(egui::Id::new("native_navigation_dock"))
                .anchor(egui::Align2::RIGHT_CENTER, egui::vec2(-14.0, -8.0))
                .order(egui::Order::Foreground)
                .show(ctx, |ui| {
                    egui::Frame::window(ui.style())
                        .inner_margin(egui::Margin::same(6))
                        .show(ui, |ui| {
                            ui.vertical(|ui| {
                                if ui.button("RL").on_hover_text("Rotate left 90 degrees").clicked() {
                                    rotate_by = Some(270);
                                }
                                if ui.button("RR").on_hover_text("Rotate right 90 degrees").clicked() {
                                    rotate_by = Some(90);
                                }
                                ui.separator();
                                if ui.button("FH").on_hover_text("Flip horizontally").clicked() {
                                    flip_horizontal = true;
                                }
                                if ui.button("FV").on_hover_text("Flip vertically").clicked() {
                                    flip_vertical = true;
                                }
                                ui.separator();
                                if ui.button("-").on_hover_text("Zoom out").clicked() {
                                    zoom_factor = Some(1.0 / 1.35);
                                }
                                if ui.button("+").on_hover_text("Zoom in").clicked() {
                                    zoom_factor = Some(1.35);
                                }
                                if ui.button("Fit").on_hover_text("Fit current slide").clicked() {
                                    reset_view = true;
                                }
                            });
                        });
                });

            // A compact overview mirrors the browser minimap without retaining a
            // second full-resolution image. It is driven by the active native
            // camera and therefore remains correct after pan, zoom, rotation,
            // source switches, and independent multi-view navigation.
            if let Some((slide_width, slide_height, camera, viewport_width, viewport_height)) = navigator_snapshot {
                egui::Area::new(egui::Id::new("native_overview_navigator"))
                    .anchor(egui::Align2::RIGHT_BOTTOM, egui::vec2(-74.0, -66.0))
                    .order(egui::Order::Foreground)
                    .show(ctx, |ui| {
                        egui::Frame::window(ui.style())
                            .inner_margin(egui::Margin::same(6))
                            .show(ui, |ui| {
                                ui.label(egui::RichText::new("Overview").small().strong());
                                let desired = egui::vec2(176.0, 116.0);
                                let (response, painter) = ui.allocate_painter(
                                    desired,
                                    egui::Sense::click_and_drag(),
                                );
                                let bounds = response.rect.shrink(2.0);
                                let aspect = (slide_width / slide_height).max(1e-9) as f32;
                                let (map_width, map_height) = if bounds.width() / bounds.height() > aspect {
                                    (bounds.height() * aspect, bounds.height())
                                } else {
                                    (bounds.width(), bounds.width() / aspect)
                                };
                                let map = egui::Rect::from_center_size(
                                    bounds.center(),
                                    egui::vec2(map_width, map_height),
                                );
                                painter.rect_filled(map, 3.0, egui::Color32::from_rgb(26, 32, 44));
                                painter.rect_stroke(
                                    map,
                                    3.0,
                                    egui::Stroke::new(1.0, egui::Color32::from_rgb(100, 116, 139)),
                                    egui::StrokeKind::Inside,
                                );
                                if let Some(texture) = &navigator_texture {
                                    painter.image(
                                        texture.id(),
                                        map,
                                        egui::Rect::from_min_max(
                                            egui::pos2(0.0, 0.0),
                                            egui::pos2(1.0, 1.0),
                                        ),
                                        egui::Color32::WHITE,
                                    );
                                } else {
                                    // Keep a stable placeholder until the bounded
                                    // coarsest-tile request completes.
                                    painter.circle_filled(
                                        map.center(),
                                        (map.width().min(map.height()) * 0.22).max(6.0),
                                        egui::Color32::from_rgba_unmultiplied(217, 70, 239, 72),
                                    );
                                }
                                let visible_width = (camera.pixels_per_screen_pixel * viewport_width as f64)
                                    .clamp(1.0, slide_width);
                                let visible_height = (camera.pixels_per_screen_pixel * viewport_height as f64)
                                    .clamp(1.0, slide_height);
                                let left = (camera.center_x - visible_width / 2.0)
                                    .clamp(0.0, (slide_width - visible_width).max(0.0));
                                let top = (camera.center_y - visible_height / 2.0)
                                    .clamp(0.0, (slide_height - visible_height).max(0.0));
                                let viewport = egui::Rect::from_min_size(
                                    egui::pos2(
                                        map.left() + (left / slide_width) as f32 * map.width(),
                                        map.top() + (top / slide_height) as f32 * map.height(),
                                    ),
                                    egui::vec2(
                                        (visible_width / slide_width) as f32 * map.width(),
                                        (visible_height / slide_height) as f32 * map.height(),
                                    ),
                                );
                                painter.rect_stroke(
                                    viewport,
                                    1.5,
                                    egui::Stroke::new(1.5, egui::Color32::from_rgb(94, 234, 212)),
                                    egui::StrokeKind::Inside,
                                );
                                if (response.clicked() || response.dragged())
                                    && response.interact_pointer_pos().is_some()
                                {
                                    let pointer = response.interact_pointer_pos().unwrap();
                                    let x = ((pointer.x - map.left()) / map.width()).clamp(0.0, 1.0);
                                    let y = ((pointer.y - map.top()) / map.height()).clamp(0.0, 1.0);
                                    navigator_pan = Some((x as f64 * slide_width, y as f64 * slide_height));
                                }
                            });
                    });
            }

            egui::TopBottomPanel::bottom("native_scale_bar")
                .exact_height(52.0)
                .show(ctx, |ui| {
                    ui.vertical_centered(|ui| {
                        if let Some((Some(mpp), base, magnification, camera)) = scale_info {
                            let target_px = 140.0;
                            let scale_um = native_nice_scale_length(target_px * mpp * camera.pixels_per_screen_pixel);
                            let bar_px = (scale_um / (mpp * camera.pixels_per_screen_pixel)) as f32;
                            let (rect, _) = ui.allocate_exact_size(
                                egui::vec2(bar_px.clamp(48.0, 220.0), 8.0),
                                egui::Sense::hover(),
                            );
                            let stroke = egui::Stroke::new(2.0, egui::Color32::WHITE);
                            ui.painter().line_segment(
                                [rect.left_bottom(), rect.right_bottom()],
                                stroke,
                            );
                            ui.label(format!(
                                "{} | ~{magnification:.1}x (full resolution ~{base:.0}x)",
                                native_scale_label(scale_um)
                            ));
                        } else if let Some((_, base, magnification, _)) = scale_info {
                            ui.weak(format!(
                                "Scale unavailable | estimated ~{magnification:.1}x (full resolution ~{base:.0}x)"
                            ));
                        }
                    });
                });

            if show_project {
                egui::Window::new(egui::RichText::new("Project").strong())
                    .open(&mut show_project)
                    .default_pos(egui::pos2(16.0, 76.0))
                    .default_width(310.0)
                    .default_height(260.0)
                    .resizable(true)
                    .collapsible(false)
                    .show(ctx, |ui| {
                        ui.weak(format!(
                            "{} slide{}",
                            source_count,
                            if source_count == 1 { "" } else { "s" }
                        ));
                        if pane_count > 1 {
                            ui.weak(format!(
                                "Active pane {} of {} - choose a slide for this pane",
                                active_pane + 1,
                                pane_count
                            ));
                        }
                        ui.separator();
                        for (index, label) in source_labels.iter().enumerate() {
                            if ui.selectable_label(index == source_index, label).clicked() {
                                select_source = Some(index);
                            }
                        }
                    });
            }

            if show_annotation {
                egui::Window::new(egui::RichText::new("Annotations").strong())
                    .open(&mut show_annotation)
                    .default_pos(egui::pos2(16.0, 352.0))
                    .default_width(340.0)
                    .default_height(580.0)
                    .resizable(true)
                    .collapsible(false)
                    .show(ctx, |ui| {
                        ui.weak(format!(
                            "{annotation_count} ROI{}",
                            if annotation_count == 1 { "" } else { "s" }
                        ));
                        ui.weak(format!(
                            "{trajectory_count} trajectory{}",
                            if trajectory_count == 1 { "" } else { "ies" }
                        ));
                        ui.separator();
                        if ui.button("Pan").clicked() && tool == NativeInteractionTool::Polygon {
                            toggle_polygon = true;
                        }
                        if ui.button("Draw polygon").clicked()
                            && tool != NativeInteractionTool::Polygon
                        {
                            toggle_polygon = true;
                        }
                        if ui.button("Paint brush").clicked()
                            && tool != NativeInteractionTool::Brush
                        {
                            toggle_brush = true;
                        }
                        if ui.button("Edit selected ROI").clicked() {
                            toggle_edit = true;
                        }
                        if ui.button("Draw trajectory").clicked()
                            && tool != NativeInteractionTool::Trajectory
                        {
                            toggle_trajectory = true;
                        }
                        if ui.button("Measure distance").clicked()
                            && tool != NativeInteractionTool::Measurement
                        {
                            toggle_measurement = true;
                        }
                        ui.separator();
                        ui.label("New annotation");
                        ui.horizontal(|ui| {
                            ui.label("Category");
                            ui.text_edit_singleline(&mut annotation_label);
                        });
                        ui.horizontal(|ui| {
                            ui.label("Colour");
                            ui.text_edit_singleline(&mut annotation_colour);
                        });
                        ui.add(
                            egui::Slider::new(&mut brush_size_slide, 2.0..=512.0)
                                .logarithmic(true)
                                .text("Brush px"),
                        );
                        ui.horizontal(|ui| {
                            if ui.button("Save polygon").clicked() {
                                commit_polygon = true;
                            }
                            if ui.button("Cancel").clicked() {
                                cancel_polygon = true;
                            }
                        });
                        ui.separator();
                        ui.label("ROIs");
                        ui.checkbox(&mut show_annotation_labels, "Show ROI labels");
                        if !checked_annotation_ids.is_empty() {
                            ui.horizontal(|ui| {
                                ui.weak(format!("{} selected", checked_annotation_ids.len()));
                                if ui.button("Clear selection").clicked() {
                                    clear_checked_annotations = true;
                                }
                                if ui.button("Delete selected").clicked() {
                                    delete_checked_annotations = true;
                                }
                                if checked_annotation_ids.len() >= 2
                                    && ui.button("Merge same class").clicked()
                                {
                                    merge_checked_annotations = true;
                                }
                            });
                        }
                        if selected_annotation.is_some()
                            && ui.button("Delete selected ROI").clicked()
                        {
                            delete_selected_annotation = true;
                        }
                        if let Some(selected) = self.selected_feature.as_ref() {
                            ui.separator();
                            ui.label("Selected annotation");
                            ui.horizontal(|ui| {
                                ui.label("Category");
                                ui.text_edit_singleline(&mut selected_annotation_category);
                            });
                            ui.horizontal(|ui| {
                                ui.label("Colour");
                                ui.text_edit_singleline(&mut selected_annotation_colour);
                            });
                            if ui.button("Apply category").clicked() {
                                let mut updated = selected.clone();
                                if let Some(properties) = updated.get_mut("properties").and_then(serde_json::Value::as_object_mut) {
                                    properties.insert("name".to_string(), serde_json::Value::String(selected_annotation_category.trim().to_string()));
                                    properties.insert("colour".to_string(), serde_json::Value::String(selected_annotation_colour.trim().to_string()));
                                    let classification = properties.entry("classification").or_insert_with(|| serde_json::json!({}));
                                    if let Some(classification) = classification.as_object_mut() {
                                        classification.insert("name".to_string(), serde_json::Value::String(selected_annotation_category.trim().to_string()));
                                        classification.insert("color".to_string(), serde_json::Value::String(selected_annotation_colour.trim().to_string()));
                                    }
                                }
                                update_selected_annotation = Some(updated);
                            }
                            if ui.button("Fill holes").clicked() {
                                fill_selected_annotation_holes = true;
                            }
                            if ui.button("Split multipart").clicked() {
                                split_selected_annotation = true;
                            }
                        }
                        egui::ScrollArea::vertical()
                            .id_salt("native_roi_list")
                            .max_height(420.0)
                            .show(ui, |ui| {
                                if annotation_entries.is_empty() {
                                    ui.weak("No R-side annotations on this slide.");
                                }
                                for (id, label, feature) in &annotation_entries {
                                    ui.horizontal(|ui| {
                                        let mut checked = checked_annotation_ids.contains(id);
                                        if ui.checkbox(&mut checked, "").changed() {
                                            if checked {
                                                checked_annotation_ids.insert(id.clone());
                                            } else {
                                                checked_annotation_ids.remove(id);
                                            }
                                        }
                                        if ui
                                            .selectable_label(
                                                selected_annotation.as_deref() == Some(id.as_str()),
                                                format!("{label}  ({id})"),
                                            )
                                            .clicked()
                                        {
                                            select_annotation = Some(feature.clone());
                                        }
                                    });
                                }
                            });
                        ui.separator();
                        ui.label("Trajectories");
                        ui.add(
                            egui::Slider::new(&mut trajectory_area_width_slide, 16.0..=5000.0)
                                .logarithmic(true)
                                .text("area width (px)"),
                        );
                        ui.horizontal(|ui| {
                            if ui
                                .add_enabled(selected_trajectory.is_some(), egui::Button::new("Create area"))
                                .clicked()
                            {
                                create_trajectory_area = true;
                            }
                            if ui
                                .add_enabled(selected_trajectory.is_some(), egui::Button::new("Edit border"))
                                .clicked()
                            {
                                edit_trajectory_area = true;
                            }
                        });
                        if selected_trajectory.is_some()
                            && ui.button("Delete selected trajectory").clicked()
                        {
                            delete_selected_trajectory = true;
                        }
                        egui::ScrollArea::vertical()
                            .id_salt("native_trajectory_list")
                            .max_height(180.0)
                            .show(ui, |ui| {
                                if trajectory_entries.is_empty() {
                                    ui.weak("No saved trajectories.");
                                }
                                for (id, label, item) in &trajectory_entries {
                                    if ui
                                        .selectable_label(
                                            selected_trajectory.as_deref() == Some(id.as_str()),
                                            format!("{label}  ({id})"),
                                        )
                                        .clicked()
                                    {
                                        select_trajectory = Some(item.clone());
                                    }
                                }
                            });
                        ui.separator();
                        ui.label("Measurements");
                        if selected_measurement_id.is_some()
                            && ui.button("Delete selected measurement").clicked()
                        {
                            delete_selected_measurement = true;
                        }
                        egui::ScrollArea::vertical()
                            .id_salt("native_measurement_list")
                            .max_height(120.0)
                            .show(ui, |ui| {
                                if measurement_entries.is_empty() {
                                    ui.weak("No saved measurements.");
                                }
                                for (id, label) in &measurement_entries {
                                    if ui
                                        .selectable_label(
                                            selected_measurement_id.as_deref() == Some(id.as_str()),
                                            format!("{label}  ({id})"),
                                        )
                                        .clicked()
                                    {
                                        select_measurement = Some(id.clone());
                                    }
                                }
                            });
                    });
            }

            if show_layers {
                egui::Window::new(egui::RichText::new("Layers").strong())
                    .open(&mut show_layers)
                    .default_pos(egui::pos2(342.0, 76.0))
                    .default_width(300.0)
                    .default_height(250.0)
                    .resizable(true)
                    .collapsible(false)
                    .show(ctx, |ui| {
                        ui.label("Dense vector layers");
                        if dense_sources.is_empty() {
                            ui.weak("No dense vector layers for this project.");
                        }
                        for source in &dense_sources {
                            let mut visible = source.visible;
                            if ui.checkbox(&mut visible, &source.name).changed() {
                                dense_visibility.push((source.id.clone(), visible));
                            }
                        }
                        if !native_points.is_empty() {
                            ui.separator();
                            ui.label("Spatial points");
                            for source in &native_points {
                                let mut visible = source.visible;
                                let mut opacity = source.opacity;
                                let visible_changed =
                                    ui.checkbox(&mut visible, &source.name).changed();
                                let opacity_changed = ui
                                    .add(egui::Slider::new(&mut opacity, 0.0..=1.0).text("Opacity"))
                                    .changed();
                                ui.weak(format!("{} visible points", source.count));
                                if visible_changed || opacity_changed {
                                    point_display.push((source.id.clone(), visible, opacity));
                                }
                            }
                        }
                        if !native_channels.is_empty() {
                            ui.separator();
                            ui.label("Tiled channels");
                            for (
                                id,
                                name,
                                initial_visible,
                                initial_opacity,
                                initial_colour,
                                initial_gain,
                                initial_minimum,
                                initial_maximum,
                            ) in &native_channels
                            {
                                let mut visible = *initial_visible;
                                let mut opacity = *initial_opacity;
                                let mut colour = initial_colour.clone();
                                let mut gain = *initial_gain;
                                let mut contrast_min = *initial_minimum;
                                let mut contrast_max = *initial_maximum;
                                let visible_changed = ui.checkbox(&mut visible, name).changed();
                                let opacity_changed = ui
                                    .add(egui::Slider::new(&mut opacity, 0.0..=1.0).text("Opacity"))
                                    .changed();
                                ui.horizontal(|ui| {
                                    ui.label("Colour");
                                    ui.text_edit_singleline(&mut colour);
                                });
                                let gain_changed = ui
                                    .add(egui::Slider::new(&mut gain, 0.0..=4.0).text("Gain"))
                                    .changed();
                                let min_changed = ui
                                    .add(
                                        egui::Slider::new(&mut contrast_min, 0.0..=1.0).text("Min"),
                                    )
                                    .changed();
                                let max_changed = ui
                                    .add(
                                        egui::Slider::new(&mut contrast_max, 0.0..=1.0).text("Max"),
                                    )
                                    .changed();
                                let colour_changed = colour != *initial_colour;
                                if visible_changed
                                    || opacity_changed
                                    || colour_changed
                                    || gain_changed
                                    || min_changed
                                    || max_changed
                                {
                                    channel_display.push((
                                        id.clone(),
                                        visible,
                                        opacity,
                                        colour,
                                        gain,
                                        contrast_min,
                                        contrast_max,
                                    ));
                                }
                            }
                        }
                    });
            }
            if show_history {
                egui::Window::new("History")
                    .open(&mut show_history)
                    .default_width(440.0)
                    .default_height(280.0)
                    .resizable(true)
                    .show(ctx, |ui| {
                        ui.horizontal(|ui| {
                            if ui.button("Copy").clicked() {
                                ui.ctx().copy_text(history_entries.join("\n"));
                            }
                            if ui.button("Clear").clicked() {
                                clear_history = true;
                            }
                        });
                        ui.separator();
                        egui::ScrollArea::vertical()
                            .id_salt("native_history_log")
                            .stick_to_bottom(true)
                            .show(ui, |ui| {
                                if history_entries.is_empty() {
                                    ui.weak("No native viewer events yet.");
                                } else {
                                    for entry in &history_entries {
                                        ui.monospace(entry);
                                    }
                                }
                            });
                    });
            }
            if show_proximity_stats {
                egui::Window::new("Proximity statistics")
                    .open(&mut show_proximity_stats)
                    .default_width(760.0)
                    .default_height(440.0)
                    .resizable(true)
                    .show(ctx, |ui| {
                        ui.label(format!("{} ranked feature{}", proximity_stats_rows.len(), if proximity_stats_rows.len() == 1 { "" } else { "s" }));
                        ui.weak("Full results remain available in R through viewer$get_proximity_stats().");
                        ui.separator();
                        egui::ScrollArea::both().id_salt("native_proximity_statistics").auto_shrink([false, false]).show(ui, |ui| {
                            egui::Grid::new("native_proximity_statistics_table")
                                .striped(true)
                                .min_col_width(76.0)
                                .show(ui, |ui| {
                                    for heading in ["Rank", "Feature", "Method", "Correlation", "MIC", "P value", "Bins", "Points"] {
                                        ui.strong(heading);
                                    }
                                    ui.end_row();
                                    for row in proximity_stats_rows.iter().take(250) {
                                        let cell = |name: &str| row.get(name).map(|value| {
                                            if let Some(text) = value.as_str() { text.to_string() }
                                            else if value.is_number() { format!("{value}") }
                                            else { String::new() }
                                        }).unwrap_or_default();
                                        for name in ["rank", "feature", "method", "correlation", "MIC", "p_value", "n_bins", "n_points"] {
                                            ui.label(cell(name));
                                        }
                                        ui.end_row();
                                    }
                                });
                        });
                    });
            }
            if show_trajectory_profile {
                egui::Window::new("Trajectory gradient profile")
                    .open(&mut show_trajectory_profile)
                    .default_width(760.0)
                    .default_height(420.0)
                    .resizable(true)
                    .show(ctx, |ui| {
                        ui.label(format!("{} trajectory bin{}", trajectory_profile_rows.len(), if trajectory_profile_rows.len() == 1 { "" } else { "s" }));
                        ui.weak("Full results remain available in R through viewer$get_trajectory_profile().");
                        ui.separator();
                        egui::ScrollArea::both().id_salt("native_trajectory_profile").auto_shrink([false, false]).show(ui, |ui| {
                            egui::Grid::new("native_trajectory_profile_table").striped(true).min_col_width(78.0).show(ui, |ui| {
                                for heading in ["Bin", "Distance", "Count", "Mean", "Median", "Dominant"] { ui.strong(heading); }
                                ui.end_row();
                                for row in trajectory_profile_rows.iter().take(250) {
                                    let cell = |name: &str| row.get(name).map(|value| if let Some(text) = value.as_str() { text.to_string() } else if value.is_number() { format!("{value}") } else { String::new() }).unwrap_or_default();
                                    for name in ["bin", "distance_px", "count", "mean", "median", "dominant"] { ui.label(cell(name)); }
                                    ui.end_row();
                                }
                            });
                        });
                    });
            }
            if show_help {
                egui::Window::new("wsiTools Native Help")
                    .open(&mut show_help)
                    .default_width(520.0)
                    .default_height(430.0)
                    .resizable(true)
                    .show(ctx, |ui| {
                        ui.heading("Quick guide");
                        ui.label("R stays in charge of tiles, annotations, spatial data, and analyses. The native window draws only the visible tiles and overlays.");
                        ui.separator();
                        ui.strong("Navigation");
                        egui::Grid::new("native_help_navigation").striped(true).show(ui, |ui| {
                            ui.monospace("Drag / Arrow keys"); ui.label("Pan the active view"); ui.end_row();
                            ui.monospace("Mouse wheel, +, -"); ui.label("Zoom the active view"); ui.end_row();
                            ui.monospace("Home or R"); ui.label("Fit the active slide"); ui.end_row();
                            ui.monospace("[ / ]"); ui.label("Previous / next project image"); ui.end_row();
                        });
                        ui.add_space(8.0);
                        ui.strong("Drawing and selection");
                        egui::Grid::new("native_help_annotation").striped(true).show(ui, |ui| {
                            ui.monospace("P"); ui.label("Draw polygon; Enter saves, Escape cancels"); ui.end_row();
                            ui.monospace("B"); ui.label("Paint brush"); ui.end_row();
                            ui.monospace("T"); ui.label("Draw trajectory"); ui.end_row();
                            ui.monospace("M"); ui.label("Measure distance"); ui.end_row();
                            ui.monospace("Double-click"); ui.label("Select a visible annotation"); ui.end_row();
                            ui.monospace("Delete / Backspace"); ui.label("Remove the selected annotation, trajectory, or measurement"); ui.end_row();
                        });
                        ui.add_space(8.0);
                        ui.strong("Live workflows");
                        ui.label("Use Spatial to colour visible coordinates by a gene or open a dimensionality-reduction plot. Drag in a reduction plot to select observations; only the active image viewport is refreshed. Cells appears only when the R session was started with stardist = TRUE; it runs StarDist or Mesmer on a selected ROI. Save project writes R-side annotations, measurements, trajectories, layers, and settings.");
                    });
            }
            if show_spatial_plot {
                if let Some(plot) = spatial_plots.get(spatial_plot_index) {
                    egui::Window::new(&plot.label)
                        .open(&mut show_spatial_plot)
                        .default_width(620.0)
                        .default_height(520.0)
                        .resizable(true)
                        .show(ctx, |ui| {
                            ui.horizontal(|ui| {
                                ui.weak(format!("{} sampled points", plot.points.len()));
                                if ui.button("Clear selection").clicked() && !spatial_selected_labels.is_empty() {
                                    spatial_selected_labels.clear();
                                    spatial_selection_changed = true;
                                }
                                if !spatial_selected_labels.is_empty() {
                                    ui.strong(format!("{} selected", spatial_selected_labels.len()));
                                }
                            });
                            ui.weak("Drag to lasso-select; Shift adds and Alt subtracts. Click selects the nearest point.");
                            let desired = egui::vec2(ui.available_width().max(360.0), (ui.available_height() - 8.0).max(260.0));
                            let (response, painter) = ui.allocate_painter(desired, egui::Sense::click_and_drag());
                            let rect = response.rect.shrink(10.0);
                            painter.rect_filled(rect, 4.0, egui::Color32::from_rgb(15, 23, 42));
                            painter.rect_stroke(rect, 4.0, egui::Stroke::new(1.0, egui::Color32::from_rgb(71, 85, 105)), egui::StrokeKind::Inside);
                            let xmin = plot.points.iter().map(|point| point.x).fold(f64::INFINITY, f64::min);
                            let xmax = plot.points.iter().map(|point| point.x).fold(f64::NEG_INFINITY, f64::max);
                            let ymin = plot.points.iter().map(|point| point.y).fold(f64::INFINITY, f64::min);
                            let ymax = plot.points.iter().map(|point| point.y).fold(f64::NEG_INFINITY, f64::max);
                            let xspan = (xmax - xmin).abs().max(1e-9);
                            let yspan = (ymax - ymin).abs().max(1e-9);
                            let point_pos = |point: &NativeReductionPoint| {
                                egui::pos2(
                                    rect.left() + (((point.x - xmin) / xspan) as f32 * rect.width()),
                                    rect.bottom() - (((point.y - ymin) / yspan) as f32 * rect.height()),
                                )
                            };
                            for point in &plot.points {
                                let mut colour = parse_hex_colour(&point.colour);
                                if spatial_selected_labels.contains(&point.label) {
                                    colour = [0.98, 0.80, 0.10, 1.0];
                                }
                                let color = egui::Color32::from_rgba_unmultiplied(
                                    (colour[0] * 255.0).round() as u8,
                                    (colour[1] * 255.0).round() as u8,
                                    (colour[2] * 255.0).round() as u8,
                                    if spatial_selected_labels.contains(&point.label) { 255 } else { 205 },
                                );
                                painter.circle_filled(point_pos(point), if spatial_selected_labels.contains(&point.label) { 3.7 } else { 2.2 }, color);
                            }
                            if response.drag_started() {
                                spatial_plot_drag_start = response.interact_pointer_pos();
                            }
                            if let (Some(start), Some(current)) = (spatial_plot_drag_start, response.interact_pointer_pos()) {
                                let selection = egui::Rect::from_two_pos(start, current);
                                painter.rect_stroke(selection, 1.0, egui::Stroke::new(1.5, egui::Color32::from_rgb(250, 204, 21)), egui::StrokeKind::Inside);
                            }
                            if response.drag_stopped() {
                                if let (Some(start), Some(end)) = (spatial_plot_drag_start.take(), response.interact_pointer_pos()) {
                                    let selection = egui::Rect::from_two_pos(start, end);
                                    let modifiers = ui.input(|input| input.modifiers);
                                    if !modifiers.shift && !modifiers.alt { spatial_selected_labels.clear(); }
                                    for point in &plot.points {
                                        if selection.contains(point_pos(point)) {
                                            if modifiers.alt { spatial_selected_labels.remove(&point.label); }
                                            else { spatial_selected_labels.insert(point.label.clone()); }
                                        }
                                    }
                                    spatial_selection_changed = true;
                                }
                            } else if response.clicked() {
                                if let Some(cursor) = response.interact_pointer_pos() {
                                    let nearest = plot.points.iter().min_by(|left, right| {
                                        let a = point_pos(left).distance_sq(cursor);
                                        let b = point_pos(right).distance_sq(cursor);
                                        a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal)
                                    });
                                    if let Some(point) = nearest.filter(|point| point_pos(point).distance(cursor) <= 12.0) {
                                        let modifiers = ui.input(|input| input.modifiers);
                                        if !modifiers.shift && !modifiers.alt { spatial_selected_labels.clear(); }
                                        if modifiers.alt { spatial_selected_labels.remove(&point.label); }
                                        else { spatial_selected_labels.insert(point.label.clone()); }
                                        spatial_selection_changed = true;
                                    }
                                }
                            }
                        });
                }
            }
            if show_annotation_labels {
                let pixels_per_point = ctx.pixels_per_point().max(1e-6);
                let painter = ctx.layer_painter(egui::LayerId::new(
                    egui::Order::Foreground,
                    egui::Id::new("native_annotation_labels"),
                ));
                for (label, (x, y), colour) in &annotation_labels {
                    let colour = egui::Color32::from_rgba_unmultiplied(
                        (colour[0] * 255.0).round() as u8,
                        (colour[1] * 255.0).round() as u8,
                        (colour[2] * 255.0).round() as u8,
                        255,
                    );
                    let galley = painter.layout_no_wrap(
                        label.clone(),
                        egui::FontId::proportional(12.0),
                        egui::Color32::WHITE,
                    );
                    let center = egui::pos2(*x / pixels_per_point, *y / pixels_per_point);
                    let rect = egui::Rect::from_center_size(
                        center,
                        galley.size() + egui::vec2(10.0, 5.0),
                    );
                    painter.rect_filled(
                        rect,
                        3.0,
                        egui::Color32::from_rgba_unmultiplied(3, 7, 18, 204),
                    );
                    painter.rect_stroke(
                        rect,
                        3.0,
                        egui::Stroke::new(1.0, colour),
                        egui::StrokeKind::Outside,
                    );
                    painter.galley(center - galley.size() * 0.5, galley, egui::Color32::WHITE);
                }
            }
            if pane_labels.len() > 1 {
                let pixels_per_point = ctx.pixels_per_point().max(1e-6);
                let painter = ctx.layer_painter(egui::LayerId::new(
                    egui::Order::Foreground,
                    egui::Id::new("native_multiview_pane_labels"),
                ));
                for (index, rect, label, is_active) in &pane_labels {
                    let pane = egui::Rect::from_min_size(
                        egui::pos2(rect.x as f32 / pixels_per_point, rect.y as f32 / pixels_per_point),
                        egui::vec2(rect.width as f32 / pixels_per_point, rect.height as f32 / pixels_per_point),
                    );
                    painter.rect_stroke(
                        pane.shrink(1.0),
                        0.0,
                        egui::Stroke::new(
                            if *is_active { 2.0 } else { 1.0 },
                            if *is_active {
                                egui::Color32::from_rgb(45, 212, 191)
                            } else {
                                egui::Color32::from_rgba_unmultiplied(148, 163, 184, 156)
                            },
                        ),
                        egui::StrokeKind::Inside,
                    );
                    let text = format!("Pane {}  |  {}", index + 1, label);
                    let galley = painter.layout_no_wrap(
                        text,
                        egui::FontId::proportional(11.0),
                        egui::Color32::WHITE,
                    );
                    let origin = pane.left_top() + egui::vec2(10.0, 10.0);
                    let badge = egui::Rect::from_min_size(
                        origin,
                        galley.size() + egui::vec2(10.0, 5.0),
                    );
                    painter.rect_filled(
                        badge,
                        3.0,
                        egui::Color32::from_rgba_unmultiplied(3, 7, 18, 210),
                    );
                    painter.galley(
                        badge.min + egui::vec2(5.0, 2.5),
                        galley,
                        egui::Color32::WHITE,
                    );
                }
            }
            if !empty_panes.is_empty() {
                let pixels_per_point = ctx.pixels_per_point().max(1e-6);
                let painter = ctx.layer_painter(egui::LayerId::new(
                    egui::Order::Foreground,
                    egui::Id::new("native_empty_multiview_panes"),
                ));
                for (index, rect) in &empty_panes {
                    let rect = egui::Rect::from_min_size(
                        egui::pos2(rect.x as f32 / pixels_per_point, rect.y as f32 / pixels_per_point),
                        egui::vec2(rect.width as f32 / pixels_per_point, rect.height as f32 / pixels_per_point),
                    )
                    .shrink(10.0);
                    painter.rect_stroke(
                        rect,
                        6.0,
                        egui::Stroke::new(1.0, egui::Color32::from_rgb(71, 85, 105)),
                        egui::StrokeKind::Inside,
                    );
                    painter.text(
                        rect.center(),
                        egui::Align2::CENTER_CENTER,
                        format!("Pane {}\nClick here, then choose a slide in Project", index + 1),
                        egui::FontId::proportional(13.0),
                        egui::Color32::from_rgb(203, 213, 225),
                    );
                }
            }
            if pane_scales.len() > 1 {
                let pixels_per_point = ctx.pixels_per_point().max(1e-6);
                let painter = ctx.layer_painter(egui::LayerId::new(
                    egui::Order::Foreground,
                    egui::Id::new("native_multiview_scales"),
                ));
                for (rect, width_px, label) in &pane_scales {
                    let x = rect.x as f32 + 18.0;
                    let y = (rect.y + rect.height).saturating_sub(18) as f32;
                    let start = egui::pos2(x / pixels_per_point, y / pixels_per_point);
                    let end = egui::pos2((x + width_px) / pixels_per_point, y / pixels_per_point);
                    painter.line_segment(
                        [start, end],
                        egui::Stroke::new(2.0, egui::Color32::WHITE),
                    );
                    painter.text(
                        egui::pos2(x / pixels_per_point, (y - 6.0) / pixels_per_point),
                        egui::Align2::LEFT_BOTTOM,
                        label,
                        egui::FontId::proportional(11.0),
                        egui::Color32::WHITE,
                    );
                }
            }
        });
        state.handle_platform_output(&window, output.platform_output);
        let pixels_per_point = egui_winit::pixels_per_point(&context, &window);
        let primitives = context.tessellate(output.shapes, pixels_per_point);
        let frame = NativeEguiFrame {
            primitives,
            textures_delta: output.textures_delta,
            pixels_per_point,
        };
        if let Some(index) = select_source {
            self.select_source(index);
        }
        if let Some(factor) = zoom_factor {
            self.zoom_by_keyboard(factor);
        }
        if reset_view {
            self.reset_view();
        }
        if let Some(degrees) = rotate_by {
            self.update_image_transform(|transform| transform.rotate_clockwise(degrees));
        }
        if flip_horizontal {
            self.update_image_transform(|transform| transform.flip_x = !transform.flip_x);
        }
        if flip_vertical {
            self.update_image_transform(|transform| transform.flip_y = !transform.flip_y);
        }
        if reset_orientation {
            self.update_image_transform(|transform| *transform = NativeImageTransform::default());
        }
        if let Some(target) = magnification_target {
            self.set_magnification(target);
        }
        if toggle_polygon {
            self.toggle_polygon_tool();
        }
        if toggle_brush {
            self.toggle_brush_tool();
        }
        if toggle_edit {
            self.toggle_edit_tool();
        }
        if toggle_trajectory {
            self.toggle_trajectory_tool();
        }
        if toggle_measurement {
            self.toggle_measurement_tool();
        }
        if commit_polygon {
            self.commit_polygon();
        }
        if cancel_polygon {
            self.cancel_polygon();
        }
        if undo_annotations {
            self.restore_annotation_history(false);
        }
        if redo_annotations {
            self.restore_annotation_history(true);
        }
        if clear_checked_annotations {
            checked_annotation_ids.clear();
        }
        self.selected_annotation_ids = checked_annotation_ids;
        if delete_checked_annotations && !self.selected_annotation_ids.is_empty() {
            let ids = self.selected_annotation_ids.drain().collect::<Vec<_>>();
            self.push_annotation_undo();
            if let Some(sync) = self.state_sync.as_ref() {
                for id in &ids {
                    sync.roi_deleted(id);
                }
            }
            if self.selected_feature.as_ref().and_then(|feature| {
                feature.get("id").and_then(json_id).or_else(|| {
                    feature.get("properties")
                        .and_then(|properties| properties.get("roi_id"))
                        .and_then(json_id)
                })
            }).is_some_and(|id| ids.iter().any(|candidate| candidate == &id)) {
                self.selected_feature = None;
            }
            self.record_history(format!("Deleted {} selected ROI(s)", ids.len()));
            self.request_state_refresh();
        }
        if merge_checked_annotations {
            self.merge_checked_annotations();
        }
        if let Some(feature) = select_annotation {
            self.selected_feature = Some(feature.clone());
            if let Some(id) = feature.get("id").and_then(json_id).or_else(|| {
                feature.get("properties")
                    .and_then(|properties| properties.get("roi_id"))
                    .and_then(json_id)
            }) {
                self.selected_annotation_ids.insert(id);
            }
            self.selected_trajectory = None;
            self.selected_measurement_id = None;
            if let Some(sync) = self.state_sync.as_ref() {
                sync.roi_selection_changed(Some(&feature));
            }
        }
        if let Some(feature) = update_selected_annotation {
            self.selected_feature = Some(feature.clone());
            if let Some(sync) = self.state_sync.as_ref() { sync.roi_updated(feature); }
            self.request_state_refresh();
        }
        if fill_selected_annotation_holes {
            self.fill_selected_annotation_holes();
        }
        if split_selected_annotation {
            self.split_selected_annotation();
        }
        if delete_selected_annotation {
            if let Some(feature) = self.selected_feature.take() {
                if let Some(id) = feature.get("id").and_then(json_id).or_else(|| {
                    feature
                        .get("properties")
                        .and_then(|properties| properties.get("roi_id"))
                        .and_then(json_id)
                }) {
                    if let Some(sync) = self.state_sync.as_ref() {
                        self.selected_annotation_ids.remove(&id);
                        sync.roi_deleted(&id);
                    }
                }
            }
        }
        if let Some(trajectory) = select_trajectory {
            self.selected_feature = None;
            self.selected_trajectory = Some(trajectory);
            self.selected_measurement_id = None;
            if let Some(sync) = self.state_sync.as_ref() {
                sync.roi_selection_changed(None);
            }
        }
        if clear_trajectories {
            self.selected_trajectory = None;
            if let Some(sync) = self.state_sync.as_ref() {
                sync.trajectories_cleared();
            }
            self.record_history("All trajectories cleared".to_string());
            self.request_state_refresh();
        }
        if let Some(id) = select_measurement {
            self.selected_feature = None;
            self.selected_trajectory = None;
            self.selected_measurement_id = Some(id);
            if let Some(sync) = self.state_sync.as_ref() {
                sync.roi_selection_changed(None);
            }
        }
        if delete_selected_measurement {
            if let Some(id) = self.selected_measurement_id.take() {
                if let Some(sync) = self.state_sync.as_ref() {
                    sync.measurement_deleted(&id);
                }
                self.request_state_refresh();
            }
        }
        if clear_measurements {
            self.selected_measurement_id = None;
            if let Some(sync) = self.state_sync.as_ref() {
                sync.measurements_cleared();
            }
            self.record_history("All measurements cleared".to_string());
            self.request_state_refresh();
        }
        if import_geojson {
            self.import_geojson_annotations();
        }
        if export_geojson {
            self.export_geojson_annotations();
        }
        if load_all_grandqc || load_grandqc_item.is_some() {
            let selected = if load_all_grandqc {
                grandqc_items.clone()
            } else {
                load_grandqc_item
                    .and_then(|index| grandqc_items.get(index).cloned())
                    .into_iter()
                    .collect()
            };
            if let Some(sync) = self.state_sync.as_ref() {
                sync.grandqc_load(selected.iter().map(NativeGrandqcItem::payload).collect());
                self.request_state_refresh();
                self.record_history(format!("GrandQC import requested for {} file(s)", selected.len()));
                self.error = Some("Importing GrandQC artifact annotations in R...".to_string());
            } else {
                self.error = Some("GrandQC import requires the live R bridge.".to_string());
            }
        }
        if clear_grandqc {
            if let Some(sync) = self.state_sync.as_ref() {
                sync.grandqc_cleared();
                self.request_state_refresh();
                self.record_history("GrandQC annotations clear requested".to_string());
            } else {
                self.error = Some("Clearing GrandQC annotations requires the live R bridge.".to_string());
            }
        }
        if load_all_kodama || load_kodama_item.is_some() {
            let selected = if load_all_kodama {
                kodama_items.clone()
            } else {
                load_kodama_item
                    .and_then(|index| kodama_items.get(index).cloned())
                    .into_iter()
                    .collect()
            };
            if let Some(sync) = self.state_sync.as_ref() {
                sync.kodama_load(selected.iter().map(NativeKodamaItem::payload).collect());
                self.request_state_refresh();
                self.record_history(format!("KODAMA refinement import requested for {} file(s)", selected.len()));
                self.error = Some("Importing KODAMA refinement annotations in R...".to_string());
            } else {
                self.error = Some("KODAMA refinements require the live R bridge.".to_string());
            }
        }
        if clear_kodama {
            if let Some(sync) = self.state_sync.as_ref() {
                sync.kodama_cleared();
                self.request_state_refresh();
                self.record_history("KODAMA refinements clear requested".to_string());
            } else {
                self.error = Some("Clearing KODAMA refinements requires the live R bridge.".to_string());
            }
        }
        if let Some(display) = select_stain_display {
            if let Some(renderer) = self.renderer.as_mut() {
                renderer.set_stain_display(display);
                if base_image_changed {
                    renderer.set_base_display(base_image_visible, base_image_opacity);
                }
            }
            if let Some(sync) = self.state_sync.as_ref() {
                sync.stain_updated(display.state_name(), base_image_visible, base_image_opacity);
            }
            self.record_history(format!("Stain display: {}", display.label()));
        } else if base_image_changed {
            if let Some(renderer) = self.renderer.as_mut() {
                renderer.set_base_display(base_image_visible, base_image_opacity);
            }
            if let Some(sync) = self.state_sync.as_ref() {
                sync.stain_updated(active_stain_display.state_name(), base_image_visible, base_image_opacity);
            }
            self.record_history(format!("Base image: {} at {:.0}% opacity", if base_image_visible { "shown" } else { "hidden" }, base_image_opacity * 100.0));
        }
        if save_project {
            self.save_project();
        }
        if open_project {
            self.open_project();
        }
        if save_screenshot {
            self.save_screenshot();
        }
        if let Some(scope) = export_image_scope {
            self.export_full_resolution_image(scope);
        }
        if fetch_gene {
            self.fetch_spatial_gene(gene_name);
        }
        if reset_spatial_colours {
            self.reset_spatial_colours();
        }
        if (spatial_point_scale - self.spatial_point_scale).abs() > f32::EPSILON {
            self.spatial_point_scale = spatial_point_scale.clamp(0.1, 4.0);
            self.last_point_camera = None;
            self.record_history(format!("Coordinate size: {:.2}x", self.spatial_point_scale));
        }
        if save_registered_spatial_object {
            self.save_registered_spatial_object();
        }
        if apply_cluster {
            self.apply_spatial_cluster(cluster_field.clone());
        }
        if reset_spatial_registration {
            spatial_transform = NativeSpatialTransform::default();
        }
        let registration_changed = (spatial_transform.scale_x - self.spatial_transform.scale_x).abs() > f64::EPSILON
            || (spatial_transform.scale_y - self.spatial_transform.scale_y).abs() > f64::EPSILON
            || (spatial_transform.offset_x - self.spatial_transform.offset_x).abs() > f64::EPSILON
            || (spatial_transform.offset_y - self.spatial_transform.offset_y).abs() > f64::EPSILON
            || (spatial_transform.rotation_degrees - self.spatial_transform.rotation_degrees).abs() > f64::EPSILON
            || spatial_transform.flip_horizontal != self.spatial_transform.flip_horizontal
            || spatial_transform.flip_vertical != self.spatial_transform.flip_vertical;
        if spatial_transform.scale_x.is_finite()
            && spatial_transform.scale_y.is_finite()
            && spatial_transform.offset_x.is_finite()
            && spatial_transform.offset_y.is_finite()
            && spatial_transform.rotation_degrees.is_finite()
            && registration_changed {
            self.spatial_transform = spatial_transform;
            self.last_point_camera = None;
            if let Some(sync) = self.state_sync.as_ref() {
                sync.spatial_registration_changed(&self.spatial_transform);
            }
            self.record_history("Spatial coordinate registration updated".to_string());
        }
        self.prediction_source = prediction_source;
        self.prediction_train = prediction_train;
        if run_prediction {
            self.run_prediction(prediction_ncomp as usize, prediction_dims as usize);
        }
        if run_segmentation {
            self.run_selected_roi_segmentation(segmentation_engine.clone());
        }
        if import_segmentation {
            self.import_cell_segmentation();
        }
        if run_proximity {
            self.run_proximity_analysis(proximity_query, proximity_target);
        } else if run_proximity_stats {
            self.run_proximity_statistics(
                proximity_query,
                proximity_target,
                proximity_stats_feature.clone(),
                proximity_stats_method.clone(),
            );
        } else {
            self.proximity_query = proximity_query;
            self.proximity_target = proximity_target;
        }
        self.proximity_stats_feature = proximity_stats_feature;
        self.proximity_stats_method = proximity_stats_method;
        self.show_proximity_stats = show_proximity_stats;
        self.trajectory_profile_source = trajectory_profile_source;
        self.trajectory_profile_feature = trajectory_profile_feature;
        self.trajectory_profile_bins = trajectory_profile_bins.clamp(2, 100);
        self.trajectory_profile_width = trajectory_profile_width.clamp(10.0, 5000.0);
        self.show_trajectory_profile = show_trajectory_profile;
        if run_trajectory_profile {
            self.run_trajectory_profile();
        }
        if clear_trajectory_profile {
            self.clear_trajectory_profile();
        }
        if run_annotation_association {
            self.associate_points_to_annotations();
        }
        if export_annotation_association {
            self.export_annotation_association_csv();
        }
        if delete_selected_trajectory {
            if let Some(trajectory) = self.selected_trajectory.take() {
                if let (Some(id), Some(sync)) =
                    (trajectory_item_id(&trajectory), self.state_sync.as_ref())
                {
                    sync.trajectory_deleted(&id);
                    self.request_state_refresh();
                }
            }
        }
        self.trajectory_area_width_slide = trajectory_area_width_slide.clamp(16.0, 5000.0);
        if create_trajectory_area {
            self.create_or_update_trajectory_area(false);
        }
        if update_trajectory_area {
            self.create_or_update_trajectory_area(true);
        }
        if edit_trajectory_area {
            self.edit_selected_trajectory_area();
        }
        self.show_project_panel = show_project;
        self.show_annotation_panel = show_annotation;
        self.show_layer_panel = show_layers;
        self.show_history_panel = show_history;
        self.show_help_panel = show_help;
        self.show_annotation_labels = show_annotation_labels;
        if let Some((center_x, center_y)) = navigator_pan {
            if let Some(renderer) = self.renderer.as_mut() {
                let (width, height) = renderer.image_transform.display_dimensions(&renderer.source);
                renderer.camera_state.center_x = center_x.clamp(0.0, width);
                renderer.camera_state.center_y = center_y.clamp(0.0, height);
                renderer.sync_camera();
            }
            self.schedule_visible_tiles();
            self.sync_viewport();
        }
        self.show_spatial_plot = show_spatial_plot;
        self.spatial_plot_index = spatial_plot_index;
        self.spatial_plot_drag_start = spatial_plot_drag_start;
        if spatial_selection_changed {
            self.spatial_selected_labels = spatial_selected_labels;
                if let Some(plot) = spatial_plots.get(self.spatial_plot_index) {
                if let Some(sync) = self.state_sync.as_ref() {
                    if kodama_plot_ids.contains(&plot.id) {
                        sync.kodama_cells_selected(&plot.id, &self.spatial_selected_labels);
                    } else {
                        sync.spatial_points_selected(&plot.id, &self.spatial_selected_labels);
                    }
                }
                self.record_history(format!("Selected {} spatial observation(s) from {}", self.spatial_selected_labels.len(), plot.label));
            }
            // Reload only the current viewport so highlighting never creates a
            // whole-slide point buffer.
            self.last_point_camera = None;
        }
        if clear_history {
            self.history.clear();
        }
        if multi_view_layout != self.multi_view_layout {
            self.set_multi_view_layout(multi_view_layout);
        }
        self.multi_view_sync = multi_view_sync;
        self.annotation_label = if annotation_label.trim().is_empty() {
            "Annotation".to_string()
        } else {
            annotation_label.trim().to_string()
        };
        self.annotation_colour = if parse_hex_colour(&annotation_colour) == [0.0, 0.0, 0.0, 1.0]
            && !annotation_colour.trim().eq_ignore_ascii_case("#000000")
        {
            self.annotation_colour.clone()
        } else {
            annotation_colour.trim().to_string()
        };
        self.brush_size_slide = brush_size_slide.clamp(2.0, 512.0);
        self.segmentation_engine = segmentation_engine;
        if !dense_visibility.is_empty() {
            for (id, visible) in dense_visibility {
                if let Some(source) = self.dense_sources.iter_mut().find(|source| source.id == id) {
                    source.visible = visible;
                }
            }
            self.last_dense_camera = None;
        }
        if !point_display.is_empty() {
            for (id, visible, opacity) in point_display {
                if let Some(source) = self.point_sources.iter_mut().find(|source| source.id == id) {
                    source.visible = visible;
                    source.opacity = opacity.clamp(0.0, 1.0);
                }
                if !visible {
                    if let Some(renderer) = self.renderer.as_mut() {
                        renderer.point_overlays.remove(&id);
                    }
                }
                if let Some(sync) = self.state_sync.as_ref() {
                    sync.layer_updated(&id, visible, opacity);
                }
            }
            self.last_point_camera = None;
        }
        if !channel_display.is_empty() {
            for (id, visible, opacity, colour, gain, contrast_min, contrast_max) in channel_display
            {
                if let Some(renderer) = self.renderer.as_mut() {
                    renderer.set_channel_display(
                        &id,
                        visible,
                        opacity,
                        &colour,
                        gain,
                        contrast_min,
                        contrast_max,
                    );
                }
                if let Some(channel) = self
                    .channel_sources
                    .iter_mut()
                    .find(|channel| channel.id == id)
                {
                    channel.visible = visible;
                    channel.opacity = opacity;
                    channel.colour = colour.clone();
                    channel.gain = gain;
                    channel.contrast_min = contrast_min;
                    channel.contrast_max = contrast_max;
                }
                if let Some(sync) = self.state_sync.as_ref() {
                    sync.channel_updated(
                        &id,
                        visible,
                        opacity,
                        &colour,
                        gain,
                        contrast_min,
                        contrast_max,
                    );
                }
            }
            self.schedule_visible_tiles();
        }
        Some(frame)
    }

    fn schedule_visible_tiles(&mut self) {
        let Some(renderer) = self.renderer.as_ref() else {
            return;
        };
        let pane_rects = self.pane_rects();
        for (pane_index, pane) in self.panes.iter().enumerate() {
            let Some(source) = self.sources.get(pane.source_index) else {
                continue;
            };
            let Some(rect) = pane_rects.get(pane_index) else {
                continue;
            };
            let Some(fetcher) = self.tile_fetchers.get(&source.id) else {
                continue;
            };
            for key in renderer.request_plan_for(
                source,
                pane.transform,
                pane.camera,
                rect.width,
                rect.height,
                40,
            ) {
                let request = (source.id.clone(), key);
                if renderer.has_source_tile(&source.id, &key) || self.inflight.contains(&request) {
                    continue;
                }
                if fetcher.request(key) {
                    self.inflight.insert(request);
                } else {
                    break;
                }
            }
        }
        // Channel layers use the same pane-local request plan as the base
        // image. A channel is not tied to whichever pane was last clicked.
        for (pane_index, pane) in self.panes.iter().enumerate() {
            let (Some(source), Some(rect)) = (
                self.sources.get(pane.source_index),
                pane_rects.get(pane_index),
            ) else {
                continue;
            };
            for channel in self
                .channel_sources
                .iter()
                .filter(|channel| channel.visible && channel.matches_source(source))
            {
                let Some(fetcher) = self.channel_fetchers.get(&channel.id) else {
                    continue;
                };
                let channel_source = channel.tile_source();
                let plan = if pane.transform == NativeImageTransform::default() {
                    native_tile_request_plan(
                        &channel_source,
                        pane.camera,
                        rect.width,
                        rect.height,
                        20,
                    )
                } else {
                    native_tile_request_plan_transformed(
                        &channel_source,
                        pane.transform,
                        pane.camera,
                        rect.width,
                        rect.height,
                        20,
                    )
                };
                for key in plan {
                    let request = (channel.id.clone(), key);
                    let cached = renderer
                        .channel_tiles
                        .get(&channel.id)
                        .map(|cache| cache.contains(&key))
                        .unwrap_or(false);
                    if cached || self.inflight.contains(&request) {
                        continue;
                    }
                    if fetcher.request(key) {
                        self.inflight.insert(request);
                    } else {
                        break;
                    }
                }
            }
        }
    }

    fn drain_tile_results(&mut self) {
        let mut completed = Vec::new();
        for fetcher in self.tile_fetchers.values() {
            while let Some(result) = fetcher.try_next() {
                completed.push(result);
            }
        }
        for fetcher in self.channel_fetchers.values() {
            while let Some(result) = fetcher.try_next() {
                completed.push(result);
            }
        }
        for result in completed {
            match result {
                Ok(tile) => {
                    self.inflight.remove(&(tile.source_id.clone(), tile.key));
                    let is_overview = self
                        .sources
                        .iter()
                        .find(|source| source.id == tile.source_id)
                        .map(|source| tile.key == native_overview_tile(source))
                        .unwrap_or(false);
                    if is_overview && !self.overview_textures.contains_key(&tile.source_id) {
                        if let Some(context) = self.egui_context.as_ref() {
                            let image = egui::ColorImage::from_rgba_unmultiplied(
                                [tile.width as usize, tile.height as usize],
                                &tile.rgba,
                            );
                            let texture = context.load_texture(
                                format!("native-overview-{}", tile.source_id),
                                image,
                                egui::TextureOptions::LINEAR,
                            );
                            self.overview_textures
                                .insert(tile.source_id.clone(), texture);
                        }
                    }
                    if let Some(renderer) = self.renderer.as_mut() {
                        if let Err(error) = renderer.upload_tile(tile) {
                            self.error = Some(error);
                        }
                    }
                }
                Err(error) => self.error = Some(error),
            }
        }
    }

    fn sync_viewport(&self) {
        if let (Some(renderer), Some(sync)) = (self.renderer.as_ref(), self.state_sync.as_ref()) {
            let mut source_camera = renderer.camera_state;
            let source_center = renderer.image_transform.inverse_map_point(
                &renderer.source,
                (source_camera.center_x, source_camera.center_y),
            );
            source_camera.center_x = source_center.0;
            source_camera.center_y = source_center.1;
            sync.viewport_changed(source_camera, renderer.config.width, renderer.config.height);
        }
    }

    fn update_image_transform<F>(&mut self, update: F)
    where
        F: FnOnce(&mut NativeImageTransform),
    {
        let Some(renderer) = self.renderer.as_mut() else {
            return;
        };
        update(&mut renderer.image_transform);
        renderer.reset_camera();
        if let Some(sync) = self.state_sync.as_ref() {
            sync.image_transform_changed(renderer.image_transform);
        }
        self.last_dense_camera = None;
        self.last_point_camera = None;
        self.schedule_visible_tiles();
        self.sync_viewport();
    }

    fn request_state_refresh(&mut self) {
        if let Some(fetcher) = self.state_fetcher.as_ref() {
            fetcher.refresh(Some(self.source.id.clone()));
            self.last_state_refresh = Instant::now();
        }
    }

    fn fetch_spatial_gene(&mut self, gene: String) {
        let gene = gene.trim().to_string();
        if gene.is_empty() {
            self.error = Some("Enter a gene name first.".to_string());
            return;
        }
        let Some(fetcher) = self.gene_fetcher.as_ref() else {
            self.error = Some("This live project has no spatial gene-expression endpoint.".to_string());
            return;
        };
        fetcher.request(gene.clone());
        self.record_history(format!("Gene colour requested: {gene}"));
        self.error = Some(format!("Fetching {gene} from R..."));
    }

    fn run_proximity_analysis(&mut self, query: String, target: String) {
        if query.trim().is_empty() || target.trim().is_empty() || query == target {
            self.error = Some("Choose distinct Measure inside and Distance from annotations.".to_string());
            return;
        }
        let Some(fetcher) = self.proximity_fetcher.as_ref() else {
            self.error = Some("This live project has no proximity-analysis endpoint.".to_string());
            return;
        };
        let source = self.point_sources.iter().find(|source| source.visible)
            .or_else(|| self.point_sources.first())
            .map(|source| source.id.clone())
            .unwrap_or_else(|| "spatial:points".to_string());
        fetcher.request("distance", serde_json::json!({
            "point_source": source,
            "query_annotations": [query.clone()],
            "target_annotations": [target.clone()]
        }));
        self.proximity_query = query;
        self.proximity_target = target;
        self.record_history("Proximity analysis requested in R".to_string());
        self.error = Some("Running proximity analysis in R...".to_string());
    }

    fn run_proximity_statistics(
        &mut self,
        query: String,
        target: String,
        feature_source: String,
        method: String,
    ) {
        if query.trim().is_empty() || target.trim().is_empty() || query == target {
            self.error = Some("Choose distinct Measure inside and Distance from annotations.".to_string());
            return;
        }
        let Some(fetcher) = self.proximity_fetcher.as_ref() else {
            self.error = Some("This live project has no proximity-statistics endpoint.".to_string());
            return;
        };
        let source = self.point_sources.iter().find(|source| source.visible)
            .or_else(|| self.point_sources.first())
            .map(|source| source.id.clone())
            .unwrap_or_else(|| "spatial:points".to_string());
        fetcher.request("stats", serde_json::json!({
            "action": "stats",
            "point_source": source,
            "query_annotations": [query.clone()],
            "target_annotations": [target.clone()],
            "feature_source": feature_source,
            "method": method,
            "quantile_step": 0.005,
            "max_features": 5000
        }));
        self.proximity_query = query;
        self.proximity_target = target;
        self.record_history("Proximity statistics requested in R".to_string());
        self.error = Some("Running proximity statistics in R...".to_string());
    }

    fn run_trajectory_profile(&mut self) {
        let Some(fetcher) = self.trajectory_profile_fetcher.as_ref() else {
            self.error = Some("This live project has no trajectory-profile endpoint.".to_string());
            return;
        };
        let Some(trajectory) = self.selected_trajectory.as_ref() else {
            self.error = Some("Select a saved trajectory before profiling.".to_string());
            return;
        };
        let points = trajectory_item_points(trajectory);
        if points.len() < 2 || self.trajectory_profile_source.trim().is_empty() {
            self.error = Some("Choose a point source and a trajectory with at least two points.".to_string());
            return;
        }
        let trajectory_payload = serde_json::json!({
            "id": trajectory_item_id(trajectory).unwrap_or_default(),
            "name": trajectory_item_label(trajectory),
            "points": points.into_iter().map(|(x, y)| serde_json::json!({"x": x, "y": y})).collect::<Vec<_>>()
        });
        fetcher.request(serde_json::json!({
            "action": "trajectory_profile",
            "source_id": self.trajectory_profile_source,
            "trajectory": trajectory_payload,
            "feature": self.trajectory_profile_feature.trim(),
            "bins": self.trajectory_profile_bins,
            "width_px": self.trajectory_profile_width,
            "spatial_transform": self.spatial_transform,
        }));
        self.record_history("Trajectory gradient profile requested in R".to_string());
        self.error = Some("Calculating trajectory gradient profile in R...".to_string());
    }

    fn clear_trajectory_profile(&mut self) {
        self.trajectory_profile_rows.clear();
        self.trajectory_profile_colours.clear();
        self.show_trajectory_profile = false;
        self.last_point_camera = None;
        if let Some(sync) = self.state_sync.as_ref() { sync.trajectory_profile_cleared(); }
        self.record_history("Trajectory gradient profile cleared".to_string());
        self.error = Some("Trajectory gradient profile cleared.".to_string());
    }

    fn associate_points_to_annotations(&mut self) {
        let Some(fetcher) = self.proximity_fetcher.as_ref() else {
            self.error = Some("This live project has no annotation-association endpoint.".to_string());
            return;
        };
        let source = self.point_sources.iter().find(|source| source.visible)
            .or_else(|| self.point_sources.first())
            .map(|source| source.id.clone())
            .unwrap_or_else(|| "spatial:points".to_string());
        fetcher.request("associate", serde_json::json!({
            "action": "associate",
            "point_source": source
        }));
        self.record_history("Annotation association requested in R".to_string());
        self.error = Some("Associating spatial points/cells with tissue annotations in R...".to_string());
    }

    fn export_annotation_association_csv(&mut self) {
        let Some(path) = rfd::FileDialog::new()
            .set_file_name("wsiTools_annotation_association.csv")
            .add_filter("CSV table", &["csv"])
            .save_file()
        else { return; };
        let Some(sync) = self.state_sync.as_ref() else {
            self.error = Some("Association export requires a live R session.".to_string());
            return;
        };
        sync.annotation_association_exported(&path);
        self.record_history(format!("Association CSV export requested: {}", path.display()));
        self.error = Some(format!("Saving annotation association CSV to {}...", path.display()));
    }

    fn run_prediction(&mut self, ncomp: usize, reduction_dims: usize) {
        let Some(fetcher) = self.prediction_fetcher.as_ref() else {
            self.error = Some("This live project has no PLS-LDA prediction endpoint.".to_string());
            return;
        };
        if self.prediction_source.trim().is_empty() || self.prediction_train.is_empty() {
            self.error = Some("Select a feature source and at least one training annotation.".to_string());
            return;
        }
        let mut payload = serde_json::json!({
            "feature_source": self.prediction_source,
            "train_annotations": self.prediction_train,
            "test_annotations": ["__all_unlabelled__"],
            "ncomp": ncomp.clamp(1, 10),
            "method": "simpls",
            "scaling": "autoscaling",
            "max_features": 5000
        });
        if reduction_dims > 0 {
            payload["reduction_dims"] = serde_json::json!(reduction_dims);
        }
        fetcher.request(payload);
        self.record_history("PLS-LDA prediction requested in R".to_string());
        self.error = Some("Running PLS-LDA prediction in R...".to_string());
    }

    fn save_project(&mut self) {
        let stem = native_file_stem(&self.source.label);
        let Some(path) = rfd::FileDialog::new()
            .set_file_name(&format!("{stem}.wsiproject"))
            .add_filter("wsiTools project", &["wsiproject"])
            .save_file()
        else {
            return;
        };
        let Some(sync) = self.state_sync.as_ref() else {
            self.error = Some(
                "Project saving requires a live R session. Use wsi_viewer_native() or save from R with viewer$save_project()."
                    .to_string(),
            );
            return;
        };
        sync.project_save_requested(&path);
        self.record_history(format!("Project save requested: {}", path.display()));
        self.error = Some(format!("Saving complete native project state to {}", path.display()));
    }

    fn open_project(&mut self) {
        let Some(path) = rfd::FileDialog::new()
            .add_filter("wsiTools project", &["wsiproject"])
            .pick_folder()
        else {
            return;
        };
        let Some(sync) = self.state_sync.as_ref() else {
            self.error = Some(
                "Opening a project requires a live R session. Use wsi_viewer_native() so R can restore project state."
                    .to_string(),
            );
            return;
        };
        sync.project_open_requested(&path);
        self.request_state_refresh();
        self.record_history(format!("Project restore requested: {}", path.display()));
        self.error = Some(format!("Restoring project state from {}...", path.display()));
    }

    fn save_registered_spatial_object(&mut self) {
        let Some(source) = self.point_sources.iter().find(|source| source.visible)
            .or_else(|| self.point_sources.first()) else {
            self.error = Some("No live spatial point source is available to save.".to_string());
            return;
        };
        let Some(path) = rfd::FileDialog::new()
            .set_file_name("wsiTools_registered_spatial_object.rds")
            .add_filter("R spatial object", &["rds"])
            .save_file() else { return; };
        let Some(sync) = self.state_sync.as_ref() else {
            self.error = Some("Saving a spatial object requires a live R session.".to_string());
            return;
        };
        let mut transform = self.spatial_transform.clone();
        if let Some(renderer) = self.renderer.as_ref() {
            transform.center_x = renderer.source.width / 2.0;
            transform.center_y = renderer.source.height / 2.0;
        }
        sync.spatial_object_save_requested(&path, &source.id, &transform);
        self.record_history(format!("Registered spatial object save requested: {}", path.display()));
        self.error = Some(format!("Saving registered spatial object to {}...", path.display()));
    }

    fn save_screenshot(&mut self) {
        let Some(renderer) = self.renderer.as_mut() else {
            self.error = Some("The native renderer is not ready to capture a screenshot.".to_string());
            return;
        };
        let stem = native_file_stem(&renderer.source.label);
        let Some(path) = rfd::FileDialog::new()
            .set_file_name(&format!("{stem}_view.png"))
            .add_filter("PNG image", &["png"])
            .add_filter("JPEG image", &["jpg", "jpeg"])
            .add_filter("SVG image", &["svg"])
            .save_file()
        else {
            return;
        };
        renderer.request_screenshot(path.clone());
        self.record_history(format!("Screenshot requested: {}", path.display()));
        self.error = Some("Capturing the current tissue view with visible overlays...".to_string());
        if let Some(window) = self.window.as_ref() {
            window.request_redraw();
        }
    }

    fn export_full_resolution_image(&mut self, scope: &str) {
        let Some(fetcher) = self.image_export_fetcher.as_ref() else {
            self.error = Some("This live R session did not expose full-resolution image export.".to_string());
            return;
        };
        let Some(renderer) = self.renderer.as_ref() else { return; };
        let region = if scope == "selected_roi" {
            let Some(feature) = self.selected_feature.as_ref() else {
                self.error = Some("Select an ROI before exporting its full-resolution image.".to_string());
                return;
            };
            let Some((xmin, ymin, xmax, ymax)) = native_feature_bounds(feature) else {
                self.error = Some("The selected ROI does not have a valid polygon geometry.".to_string());
                return;
            };
            (xmin, ymin, xmax, ymax)
        } else {
            renderer.source_viewport_bounds()
        };
        let width = (region.2 - region.0).ceil().max(1.0);
        let height = (region.3 - region.1).ceil().max(1.0);
        let stem = native_file_stem(&renderer.source.label);
        let suggested = if scope == "selected_roi" {
            format!("{stem}_selected_roi.tiff")
        } else {
            format!("{stem}_viewport.tiff")
        };
        let Some(path) = rfd::FileDialog::new()
            .set_file_name(&suggested)
            .add_filter("TIFF image", &["tif", "tiff"])
            .add_filter("PNG image", &["png"])
            .add_filter("JPEG image", &["jpg", "jpeg"])
            .save_file()
        else { return; };
        let extension = path.extension().and_then(|value| value.to_str()).unwrap_or("tiff").to_ascii_lowercase();
        let format = match extension.as_str() {
            "png" => "png",
            "jpg" | "jpeg" => "jpeg",
            _ => "tiff",
        };
        let output_dir = path.parent().map(|value| value.to_string_lossy().to_string()).unwrap_or_else(|| ".".to_string());
        let filename = path.file_name().map(|value| value.to_string_lossy().to_string()).unwrap_or(suggested);
        fetcher.request(serde_json::json!({
            "scope": scope,
            "format": format,
            "region": {
                "x": region.0.floor().max(0.0),
                "y": region.1.floor().max(0.0),
                "width": width,
                "height": height,
                "level": 0
            },
            "output_dir": output_dir,
            "filename": filename,
            "overwrite": true
        }));
        self.record_history(format!("Full-resolution {scope} export requested: {}", path.display()));
        self.error = Some(format!("Exporting {:.0} x {:.0} px {} through R...", width, height, format.to_uppercase()));
    }

    fn import_geojson_annotations(&mut self) {
        let Some(path) = rfd::FileDialog::new()
            .add_filter("GeoJSON", &["geojson", "json"])
            .pick_file()
        else {
            return;
        };
        if let Some(sync) = self.state_sync.as_ref() {
            // R owns parsing and decides whether the file should stay as
            // editable ROIs or be indexed as a viewport-only vector layer.
            // This keeps a dense file off the native UI thread and GPU.
            sync.geojson_import_path(&path);
            self.request_state_refresh();
            self.record_history(format!("Import requested: {}", path.display()));
            self.error = Some("Importing GeoJSON in R; large files will load progressively by viewport.".to_string());
        } else {
            self.error = Some("GeoJSON import requires the live R bridge.".to_string());
        }
    }

    fn import_cell_segmentation(&mut self) {
        let Some(path) = rfd::FileDialog::new()
            .add_filter("Cell segmentation", &["geojson", "json", "csv", "tsv", "txt", "png", "jpg", "jpeg", "tif", "tiff"])
            .pick_file()
        else {
            return;
        };
        let Some(sync) = self.state_sync.as_ref() else {
            self.error = Some("Cell-segmentation import requires the live R bridge.".to_string());
            return;
        };
        // The file path is deliberately the only data sent from native. R
        // identifies its format, indexes dense geometry, and returns only the
        // active viewport to the GPU renderer.
        sync.segmentation_import_path(&path);
        self.request_state_refresh();
        self.record_history(format!("Cell segmentation import requested: {}", path.display()));
        self.error = Some("Importing cell segmentation in R; large geometry will load progressively by viewport.".to_string());
    }

    fn export_geojson_annotations(&mut self) {
        let Some(renderer) = self.renderer.as_ref() else {
            return;
        };
        let features = renderer
            .annotation_shapes
            .iter()
            .map(|shape| shape.feature.clone())
            .collect::<Vec<_>>();
        if features.is_empty() {
            self.error = Some("There are no loaded editable annotations to export for this source.".to_string());
            return;
        }
        let stem = native_file_stem(&renderer.source.label);
        let Some(path) = rfd::FileDialog::new()
            .set_file_name(&format!("{stem}_annotations.geojson"))
            .add_filter("GeoJSON", &["geojson"])
            .save_file()
        else {
            return;
        };
        let geojson = serde_json::json!({
            "type": "FeatureCollection",
            "features": features
        });
        match serde_json::to_string_pretty(&geojson)
            .map_err(|error| format!("Could not encode GeoJSON: {error}"))
            .and_then(|text| {
                std::fs::write(&path, text)
                    .map_err(|error| format!("Could not save {}: {error}", path.display()))
            }) {
            Ok(()) => {
                if let Some(sync) = self.state_sync.as_ref() {
                    sync.roi_exported(features.len());
                }
                self.record_history(format!("Exported {} editable annotation(s)", features.len()));
                self.error = Some(format!("Exported {} editable annotation(s) to {}", features.len(), path.display()));
            }
            Err(error) => {
                self.record_history(format!("Export failed: {error}"));
                self.error = Some(error);
            }
        }
    }

    fn drain_state_results(&mut self) {
        let Some(fetcher) = self.state_fetcher.as_ref() else {
            return;
        };
        let mut latest = None;
        while let Some(result) = fetcher.try_next() {
            latest = Some(result);
        }
        match latest {
            Some(Ok(snapshot)) => {
                if self.dense_sources != snapshot.dense_sources {
                    self.dense_sources = snapshot.dense_sources.clone();
                    if !self.dense_sources.is_empty() && self.dense_fetcher.is_none() {
                        self.dense_fetcher = NativeDenseFetcher::start(self.endpoint.clone());
                    }
                    self.last_dense_camera = None;
                }
                if self.last_history_revision != Some(snapshot.revision) {
                    self.record_history(format!(
                        "R state {} | source: {} | revision: {}",
                        snapshot.event, snapshot.source_id, snapshot.revision
                    ));
                    self.last_history_revision = Some(snapshot.revision);
                }
                if let Some(renderer) = self.renderer.as_mut() {
                    if let Err(error) = renderer.set_overlay_state(snapshot) {
                        self.record_history(format!("R state error: {error}"));
                        self.error = Some(error);
                    }
                }
            }
            Some(Err(error)) => {
                self.record_history(format!("R state fetch failed: {error}"));
                self.error = Some(error);
            }
            None => {}
        }
    }

    fn request_dense_geometry(&mut self) {
        let (Some(fetcher), Some(renderer)) = (self.dense_fetcher.as_ref(), self.renderer.as_ref())
        else {
            return;
        };
        if self.last_dense_refresh.elapsed() < Duration::from_millis(350) {
            return;
        }
        let camera = renderer.camera_state;
        let needs_refresh = self.last_dense_camera.map_or(true, |previous| {
            let width = renderer.config.width as f64 * camera.pixels_per_screen_pixel;
            let height = renderer.config.height as f64 * camera.pixels_per_screen_pixel;
            (camera.center_x - previous.center_x).abs() > width * 0.15
                || (camera.center_y - previous.center_y).abs() > height * 0.15
                || ((camera.pixels_per_screen_pixel / previous.pixels_per_screen_pixel) - 1.0).abs()
                    > 0.12
        });
        if !needs_refresh {
            return;
        }
        let (xmin, ymin, xmax, ymax) = renderer.source_viewport_bounds();
        let (display_width, display_height) = renderer
            .image_transform
            .display_dimensions(&renderer.source);
        let fit = NativeCamera::fit_dimensions(
            display_width,
            display_height,
            renderer.config.width,
            renderer.config.height,
        );
        let zoom = (fit.pixels_per_screen_pixel / camera.pixels_per_screen_pixel).max(0.01);
        for source in self.dense_sources.iter().filter(|source| source.visible) {
            fetcher.request(NativeDenseRequest {
                source_id: source.id.clone(),
                xmin,
                ymin,
                xmax,
                ymax,
                zoom,
            });
        }
        self.last_dense_camera = Some(camera);
        self.last_dense_refresh = Instant::now();
    }

    fn drain_dense_results(&mut self) {
        let Some(fetcher) = self.dense_fetcher.as_ref() else {
            return;
        };
        while let Some(result) = fetcher.try_next() {
            match result {
                Ok(response) if response.ok && response.loaded => {
                    if let (Some(layer), Some(renderer)) = (response.layer, self.renderer.as_mut())
                    {
                        if let Err(error) =
                            renderer.set_dense_overlay(&response.source_id, layer.items)
                        {
                            self.error = Some(error);
                        }
                    }
                }
                Ok(_) => {}
                Err(error) => self.error = Some(error),
            }
        }
    }

    fn request_viewport_points(&mut self) {
        let (Some(fetcher), Some(renderer)) = (self.point_fetcher.as_ref(), self.renderer.as_ref())
        else {
            return;
        };
        if self.last_point_refresh.elapsed() < Duration::from_millis(300) {
            return;
        }
        let camera = renderer.camera_state;
        let needs_refresh = self.last_point_camera.map_or(true, |previous| {
            let width = renderer.config.width as f64 * camera.pixels_per_screen_pixel;
            let height = renderer.config.height as f64 * camera.pixels_per_screen_pixel;
            (camera.center_x - previous.center_x).abs() > width * 0.12
                || (camera.center_y - previous.center_y).abs() > height * 0.12
                || ((camera.pixels_per_screen_pixel / previous.pixels_per_screen_pixel) - 1.0).abs()
                    > 0.08
        });
        if !needs_refresh {
            return;
        }
        let (xmin, ymin, xmax, ymax) = renderer.source_viewport_bounds();
        let (display_width, display_height) = renderer
            .image_transform
            .display_dimensions(&renderer.source);
        let fit = NativeCamera::fit_dimensions(
            display_width,
            display_height,
            renderer.config.width,
            renderer.config.height,
        );
        let zoom = (fit.pixels_per_screen_pixel / camera.pixels_per_screen_pixel).max(0.01);
        for source in self
            .point_sources
            .iter()
            .filter(|source| source.visible && source.matches_source(&renderer.source))
        {
            fetcher.request(NativePointRequest {
                source_id: source.id.clone(),
                xmin,
                ymin,
                xmax,
                ymax,
                zoom,
                max_items: 50_000,
                spatial_transform: NativeSpatialTransform {
                    center_x: renderer.source.width / 2.0,
                    center_y: renderer.source.height / 2.0,
                    ..self.spatial_transform.clone()
                },
            });
        }
        self.last_point_camera = Some(camera);
        self.last_point_refresh = Instant::now();
    }

    fn drain_point_results(&mut self) {
        let Some(fetcher) = self.point_fetcher.as_ref() else {
            return;
        };
        while let Some(result) = fetcher.try_next() {
            match result {
                Ok(mut response) if response.ok => {
                    for point in &mut response.points {
                        point.radius = (point.radius * self.spatial_point_scale).max(0.1);
                        if !self.active_cluster.is_empty() {
                            if let Some((_, _, palette)) = native_cluster_fields(&self.spatial_clusters)
                                .into_iter().find(|(field, _, _)| field == &self.active_cluster)
                            {
                                if let Some(value) = point.cluster_values.get(&self.active_cluster) {
                                    if let Some(colour) = palette.get(value) { point.colour = colour.clone(); }
                                }
                            }
                        }
                        if !self.active_gene.is_empty() {
                            if let Some(colour) = self.gene_colours.get(&point.id) {
                            point.colour = colour.clone();
                            }
                        }
                        if let Some(colour) = self.trajectory_profile_colours.get(&point.id) {
                            point.colour = colour.clone();
                        }
                        if self.spatial_selected_labels.contains(&point.id) {
                            point.colour = "#facc15".to_string();
                            point.radius *= 1.45;
                        }
                    }
                    if let Some(renderer) = self.renderer.as_mut() {
                        if let Err(error) = renderer.set_point_overlay(
                            &response.source_id,
                            response.points,
                            response.represented,
                            response.total,
                            response.stride,
                            self.point_sources
                                .iter()
                                .find(|source| source.id == response.source_id)
                                .map(|source| source.opacity)
                                .unwrap_or(1.0),
                        ) {
                            self.error = Some(error);
                        }
                    }
                }
                Ok(_) => {}
                Err(error) => self.error = Some(error),
            }
        }
    }

    fn drain_gene_results(&mut self) {
        let Some(fetcher) = self.gene_fetcher.as_ref() else { return; };
        let mut latest = None;
        while let Some(result) = fetcher.try_next() { latest = Some(result); }
        match latest {
            Some(Ok(result)) => {
                self.active_gene = result.gene.clone();
                self.active_cluster.clear();
                self.gene_colours = result.colours;
                self.last_point_camera = None;
                self.record_history(format!("Gene colour applied: {}", self.active_gene));
                self.error = Some(format!("{} values loaded from R; refreshing visible coordinates.", self.active_gene));
            }
            Some(Err(error)) => {
                self.record_history(format!("Gene colour failed: {error}"));
                self.error = Some(error);
            }
            None => {}
        }
    }

    fn apply_spatial_cluster(&mut self, field: String) {
        if !native_cluster_fields(&self.spatial_clusters).iter().any(|(id, _, _)| id == &field) {
            self.error = Some("The selected cluster field is not available for this spatial object.".to_string());
            return;
        }
        self.active_cluster = field.clone();
        self.active_gene.clear();
        self.gene_colours.clear();
        self.last_point_camera = None;
        self.record_history(format!("Cluster colour applied: {field}"));
        self.error = Some(format!("Colouring visible coordinates by {field}."));
    }

    fn reset_spatial_colours(&mut self) {
        self.active_gene.clear();
        self.active_cluster.clear();
        self.gene_colours.clear();
        // R remains the source of the original palette. Requesting the current
        // viewport again avoids retaining a large point payload in Rust.
        self.last_point_camera = None;
        self.record_history("Coordinate colours restored".to_string());
        self.error = Some("Restoring original coordinate colours.".to_string());
    }

    fn run_selected_roi_segmentation(&mut self, engine: String) {
        let Some(feature) = self.selected_feature.clone() else {
            self.error = Some("Select an ROI before running cell segmentation.".to_string());
            return;
        };
        let Some(fetcher) = self.segmentation_fetcher.as_ref() else {
            self.error = Some("This live R session was not started with cell segmentation enabled.".to_string());
            return;
        };
        fetcher.request(engine.clone(), feature);
        self.record_history(format!("Cell segmentation requested: {engine}"));
        self.error = Some(format!("Running {engine} on the selected ROI in R..."));
    }

    fn drain_segmentation_results(&mut self) {
        let Some(fetcher) = self.segmentation_fetcher.as_ref() else { return; };
        let mut latest = None;
        while let Some(result) = fetcher.try_next() { latest = Some(result); }
        match latest {
            Some(Ok(result)) => {
                let added = result.get("added").and_then(serde_json::Value::as_u64).unwrap_or(0);
                self.record_history(format!("Cell segmentation finished: {added} object(s)"));
                self.error = Some(format!("Cell segmentation finished: {added} object(s). Refreshing overlay."));
                self.request_state_refresh();
            }
            Some(Err(error)) => {
                self.record_history(format!("Cell segmentation failed: {error}"));
                self.error = Some(error);
            }
            None => {}
        }
    }

    fn drain_image_export_results(&mut self) {
        let Some(fetcher) = self.image_export_fetcher.as_ref() else { return; };
        let mut latest = None;
        while let Some(result) = fetcher.try_next() { latest = Some(result); }
        match latest {
            Some(Ok(result)) => {
                let export = result.get("image_export").unwrap_or(&result);
                let file = export.get("file").and_then(serde_json::Value::as_str).unwrap_or("image file");
                let format = export.get("format").and_then(serde_json::Value::as_str).unwrap_or("image");
                if let Some(sync) = self.state_sync.as_ref() { sync.image_exported(export); }
                self.record_history(format!("Full-resolution {} export finished: {file}", format.to_uppercase()));
                self.error = Some(format!("Full-resolution {} export saved: {file}", format.to_uppercase()));
            }
            Some(Err(error)) => {
                self.record_history(format!("Full-resolution image export failed: {error}"));
                self.error = Some(error);
            }
            None => {}
        }
    }

    fn drain_proximity_results(&mut self) {
        let Some(fetcher) = self.proximity_fetcher.as_ref() else { return; };
        let mut latest = None;
        while let Some(result) = fetcher.try_next() { latest = Some(result); }
        match latest {
            Some(Ok((action, result))) if action == "stats" || action == "statistics" => {
                let rows = native_json_table_rows(
                    result.get("proximity_stats_rows").unwrap_or(&serde_json::Value::Null),
                );
                let info = result.get("proximity_stats").cloned().unwrap_or_default();
                let count = info.get("count").and_then(serde_json::Value::as_u64).unwrap_or(rows.len() as u64);
                self.proximity_stats_rows = rows;
                self.show_proximity_stats = !self.proximity_stats_rows.is_empty();
                self.record_history(format!("Proximity statistics finished: {count} feature(s)"));
                self.error = Some(format!("Proximity statistics finished: {count} ranked feature(s)."));
                self.request_state_refresh();
            }
            Some(Ok((action, result))) if action == "associate" => {
                let info = result.get("annotation_association").cloned().unwrap_or_default();
                let assigned = info.get("assigned_count").and_then(serde_json::Value::as_u64).unwrap_or(0);
                let count = info.get("count").and_then(serde_json::Value::as_u64).unwrap_or(0);
                self.record_history(format!("Annotation association finished: {assigned} of {count} point(s)"));
                self.error = Some(format!("Associated {assigned} of {count} points/cells with tissue annotations."));
                self.request_state_refresh();
            }
            Some(Ok((_, result))) => {
                let info = result.get("proximity").cloned().unwrap_or_default();
                let count = info.get("count").and_then(serde_json::Value::as_u64).unwrap_or(0);
                let median = info.get("median_distance_um").and_then(serde_json::Value::as_f64);
                let suffix = median.map(|value| format!("; median {value:.1} um")).unwrap_or_default();
                self.record_history(format!("Proximity finished: {count} point(s){suffix}"));
                self.error = Some(format!("Proximity finished for {count} point(s){suffix}."));
                self.request_state_refresh();
            }
            Some(Err(error)) => {
                self.record_history(format!("Proximity failed: {error}"));
                self.error = Some(error);
            }
            None => {}
        }
    }

    fn drain_trajectory_profile_results(&mut self) {
        let Some(fetcher) = self.trajectory_profile_fetcher.as_ref() else { return; };
        let mut latest = None;
        while let Some(result) = fetcher.try_next() { latest = Some(result); }
        match latest {
            Some(Ok(result)) if result.get("ok").and_then(serde_json::Value::as_bool) != Some(false) => {
                let rows = native_json_table_rows(result.get("trajectory_profile").unwrap_or(&serde_json::Value::Null));
                let colours = result.get("colours").and_then(serde_json::Value::as_object)
                    .map(|values| values.iter().filter_map(|(id, colour)| colour.as_str().map(|colour| (id.clone(), colour.to_string()))).collect())
                    .unwrap_or_default();
                let count = result.get("count").and_then(serde_json::Value::as_u64).unwrap_or(0);
                self.trajectory_profile_rows = rows;
                self.trajectory_profile_colours = colours;
                self.show_trajectory_profile = !self.trajectory_profile_rows.is_empty();
                self.last_point_camera = None;
                if let Some(sync) = self.state_sync.as_ref() { sync.trajectory_profile_finished(&self.trajectory_profile_rows); }
                self.record_history(format!("Trajectory gradient profile finished: {count} point(s)"));
                self.error = Some(format!("Trajectory gradient profile finished: {count} point(s)."));
            }
            Some(Ok(result)) => {
                let error = result.get("error").and_then(serde_json::Value::as_str).unwrap_or("Trajectory profile failed.");
                self.record_history(format!("Trajectory gradient profile failed: {error}"));
                self.error = Some(error.to_string());
            }
            Some(Err(error)) => {
                self.record_history(format!("Trajectory gradient profile failed: {error}"));
                self.error = Some(error);
            }
            None => {}
        }
    }

    fn drain_prediction_results(&mut self) {
        let Some(fetcher) = self.prediction_fetcher.as_ref() else { return; };
        let mut latest = None;
        while let Some(result) = fetcher.try_next() { latest = Some(result); }
        match latest {
            Some(Ok(result)) => {
                let info = result.get("prediction").cloned().unwrap_or_default();
                let count = info.get("count").and_then(serde_json::Value::as_u64).unwrap_or(0);
                let classes = info.get("classes").and_then(serde_json::Value::as_array).map_or(0, Vec::len);
                self.record_history(format!("PLS-LDA prediction finished: {count} point(s), {classes} class(es)"));
                self.error = Some(format!("PLS-LDA predicted {count} point(s) across {classes} class(es). Refreshing coordinate colours."));
                self.last_point_camera = None;
                self.request_state_refresh();
            }
            Some(Err(error)) => {
                self.record_history(format!("PLS-LDA prediction failed: {error}"));
                self.error = Some(error);
            }
            None => {}
        }
    }

    fn switch_source(&mut self, direction: isize) {
        if self.sources.len() < 2 {
            return;
        }
        let count = self.sources.len() as isize;
        let next = (self.source_index as isize + direction).rem_euclid(count) as usize;
        self.select_source(next);
    }

    fn select_source(&mut self, index: usize) {
        if index >= self.sources.len() {
            return;
        }
        if native_source_is_assigned_elsewhere(&self.panes, self.active_pane, index) {
            self.record_history(format!(
                "{} is already displayed in another pane",
                self.sources[index].label
            ));
            return;
        }
        if let Some(pane) = self.panes.get_mut(self.active_pane) {
            pane.source_index = index;
        }
        self.source_index = index;
        self.source = self.sources[self.source_index].clone();
        self.record_history(format!("Selected source: {}", self.source.label));
        self.inflight.clear();
        self.last_dense_camera = None;
        self.last_point_camera = None;
        if let Some(renderer) = self.renderer.as_mut() {
            renderer.set_source(self.source.clone());
            if let Some(pane) = self.panes.get_mut(self.active_pane) {
                pane.camera = renderer.camera_state;
                pane.transform = renderer.image_transform;
            }
        }
        if let Some(window) = self.window.as_ref() {
            window.set_title(&format!(
                "wsiTools - {} ({}/{})",
                self.source.label,
                self.source_index + 1,
                self.sources.len()
            ));
        }
        if let Some(sync) = self.state_sync.as_ref() {
            sync.project_source_selected(&self.source);
        }
        self.request_state_refresh();
        self.schedule_visible_tiles();
        self.sync_viewport();
    }

    fn zoom_by_keyboard(&mut self, factor: f32) {
        if let Some(renderer) = self.renderer.as_mut() {
            let center = (
                renderer.config.width as f64 / 2.0,
                renderer.config.height as f64 / 2.0,
            );
            renderer.zoom(factor, center);
        }
        self.schedule_visible_tiles();
        self.sync_viewport();
    }

    fn set_magnification(&mut self, target: f64) {
        if !target.is_finite() || target <= 0.0 {
            return;
        }
        if let Some(renderer) = self.renderer.as_mut() {
            let base = native_base_magnification(&renderer.source);
            renderer.camera_state.pixels_per_screen_pixel = (base / target).max(1e-6);
        }
        self.schedule_visible_tiles();
        self.sync_viewport();
    }

    fn pan_by_keyboard(&mut self, horizontal: f32, vertical: f32) {
        if let Some(renderer) = self.renderer.as_mut() {
            renderer.pan(
                horizontal * renderer.config.width as f32,
                vertical * renderer.config.height as f32,
            );
        }
        self.schedule_visible_tiles();
        self.sync_viewport();
    }

    fn reset_view(&mut self) {
        if let Some(renderer) = self.renderer.as_mut() {
            renderer.reset_camera();
        }
        self.schedule_visible_tiles();
        self.sync_viewport();
    }

    fn toggle_polygon_tool(&mut self) {
        if self.tool == NativeInteractionTool::Polygon {
            self.cancel_polygon();
        } else {
            self.draft_points.clear();
            self.tool = NativeInteractionTool::Polygon;
        }
    }

    fn toggle_brush_tool(&mut self) {
        if self.tool == NativeInteractionTool::Brush {
            self.cancel_polygon();
        } else {
            self.draft_points.clear();
            self.tool = NativeInteractionTool::Brush;
        }
    }

    fn prepare_brush_operation(&mut self) {
        self.brush_operation = NativeBrushOperation::New;
        self.brush_target = None;
        let Some(feature) = self.selected_feature.clone() else { return; };
        if self.modifiers.alt_key() || self.modifiers.super_key() {
            self.brush_operation = NativeBrushOperation::Subtract;
            self.brush_target = Some(feature);
        } else if annotation_feature_label(&feature) == self.annotation_label {
            self.brush_operation = NativeBrushOperation::Extend;
            self.brush_target = Some(feature);
        }
    }

    fn toggle_edit_tool(&mut self) {
        if self.tool == NativeInteractionTool::Edit {
            self.cancel_polygon();
        } else if self.selected_feature.is_none() {
            self.error = Some("Double-click an annotation to select it before editing its boundary.".to_string());
        } else {
            self.draft_points.clear();
            self.tool = NativeInteractionTool::Edit;
            self.error = Some("Edit mode: drag a vertex of the selected polygon. Press Escape to return to pan.".to_string());
        }
    }

    fn start_roi_vertex_edit(&mut self) -> bool {
        let (Some(cursor), Some(rect), Some(renderer), Some(feature)) = (
            self.active_local_cursor(),
            self.active_pane_rect(),
            self.renderer.as_ref(),
            self.selected_feature.clone(),
        ) else { return false; };
        let Some(index) = native_nearest_polygon_vertex(&feature, renderer, cursor, rect, 18.0) else {
            return false;
        };
        let ring = native_polygon_outer_ring(&feature);
        if index >= ring.len() {
            self.error = Some("Could not read the selected ROI boundary.".to_string());
            return false;
        }
        let closed = ring.len() > 3 && ring.first() == ring.last();
        let vertex_count = if closed { ring.len().saturating_sub(1) } else { ring.len() };
        let trajectory_area = feature
            .get("properties")
            .and_then(|properties| properties.get("trajectory_area"))
            .and_then(serde_json::Value::as_bool)
            .unwrap_or(false);
        let soft_span = if trajectory_area {
            ((vertex_count as f32 * 0.035).ceil() as usize).clamp(2, vertex_count.saturating_div(3).max(2))
        } else {
            2.min(vertex_count.saturating_div(3).max(1))
        };
        self.editing_roi = Some(NativeRoiVertexEdit {
            original_point: ring[index],
            original_ring: ring,
            feature,
            vertex_index: index,
            soft_span,
        });
        true
    }

    fn move_roi_vertex_edit(&mut self) {
        let cursor = self.active_local_cursor();
        let rect = self.active_pane_rect();
        let (Some(edit), Some(cursor), Some(rect), Some(renderer)) = (
            self.editing_roi.as_mut(), cursor, rect, self.renderer.as_mut(),
        ) else { return; };
        let display = renderer.camera_state.slide_at(cursor.0, cursor.1, rect.width, rect.height);
        let point = renderer.image_transform.inverse_map_point(&renderer.source, display);
        if !native_soft_move_polygon_vertex(
            &mut edit.feature,
            &edit.original_ring,
            edit.vertex_index,
            edit.original_point,
            (point.0 as f32, point.1 as f32),
            edit.soft_span,
        ) {
            self.error = Some("Only simple Polygon ROI boundaries can be edited natively at present.".to_string());
            self.editing_roi = None;
            return;
        }
        let outline = native_polygon_outer_ring(&edit.feature);
        if let Err(error) = renderer.set_draft_polygon(&outline) {
            self.error = Some(error);
        }
    }

    fn finish_roi_vertex_edit(&mut self) {
        let Some(edit) = self.editing_roi.take() else { return; };
        self.push_annotation_undo();
        self.selected_feature = Some(edit.feature.clone());
        if let Some(renderer) = self.renderer.as_mut() {
            renderer.clear_draft_polygon();
        }
        if let Some(sync) = self.state_sync.as_ref() {
            sync.roi_updated(edit.feature);
        }
        self.record_history("Selected ROI boundary updated with smooth local editing".to_string());
        self.request_state_refresh();
    }

    fn start_roi_curve_edit(&mut self) {
        let (Some(cursor), Some(rect), Some(renderer), Some(feature)) = (
            self.active_local_cursor(),
            self.active_pane_rect(),
            self.renderer.as_ref(),
            self.selected_feature.clone(),
        ) else { return; };
        let Some(start_after) = native_nearest_polygon_segment(&feature, renderer, cursor, rect, 18.0) else {
            self.error = Some("Start a curve edit on the visible boundary of the selected ROI.".to_string());
            return;
        };
        let display = renderer.camera_state.slide_at(cursor.0, cursor.1, rect.width, rect.height);
        let point = renderer.image_transform.inverse_map_point(&renderer.source, display);
        self.curve_edit = Some(NativeCurveEdit {
            original_ring: native_polygon_outer_ring(&feature),
            feature,
            start_after,
            points: vec![(point.0 as f32, point.1 as f32)],
        });
        self.error = Some("Curve edit: drag across the ROI boundary, then release to smooth that section.".to_string());
    }

    fn append_roi_curve_edit_point(&mut self) {
        let (Some(cursor), Some(rect), Some(renderer), Some(curve)) = (
            self.active_local_cursor(),
            self.active_pane_rect(),
            self.renderer.as_ref(),
            self.curve_edit.as_mut(),
        ) else { return; };
        let display = renderer.camera_state.slide_at(cursor.0, cursor.1, rect.width, rect.height);
        let point = renderer.image_transform.inverse_map_point(&renderer.source, display);
        let point = (point.0 as f32, point.1 as f32);
        if curve.points.last().is_none_or(|last| (last.0 - point.0).hypot(last.1 - point.1) >= 1.0) {
            curve.points.push(point);
        }
    }

    fn finish_roi_curve_edit(&mut self) {
        let Some(curve) = self.curve_edit.take() else { return; };
        if curve.points.len() < 2 {
            return;
        }
        let Some(end_after) = native_nearest_polygon_segment_by_slide_point(
            &curve.original_ring,
            *curve.points.last().unwrap(),
        ) else { return; };
        let mut feature = curve.feature;
        if !native_replace_polygon_arc_with_smooth_curve(
            &mut feature,
            &curve.original_ring,
            curve.start_after,
            end_after,
            &curve.points,
        ) {
            self.error = Some("Could not update that ROI boundary arc.".to_string());
            return;
        }
        self.push_annotation_undo();
        self.selected_feature = Some(feature.clone());
        if let Some(sync) = self.state_sync.as_ref() {
            sync.roi_updated(feature);
        }
        self.record_history("Selected ROI boundary updated with a smooth curve".to_string());
        self.request_state_refresh();
    }

    fn toggle_trajectory_tool(&mut self) {
        if self.tool == NativeInteractionTool::Trajectory {
            self.cancel_polygon();
        } else {
            self.draft_points.clear();
            self.tool = NativeInteractionTool::Trajectory;
        }
    }

    fn toggle_measurement_tool(&mut self) {
        if self.tool == NativeInteractionTool::Measurement {
            self.cancel_polygon();
        } else {
            self.draft_points.clear();
            self.tool = NativeInteractionTool::Measurement;
        }
    }

    fn append_draft_screen_point(&mut self) {
        let local_cursor = self.active_local_cursor();
        let active_rect = self.active_pane_rect();
        let (Some(renderer), Some(cursor), Some(rect)) =
            (self.renderer.as_mut(), local_cursor, active_rect)
        else {
            return;
        };
        let display_point =
            renderer
                .camera_state
                .slide_at(cursor.0, cursor.1, rect.width, rect.height);
        let point = renderer
            .image_transform
            .inverse_map_point(&renderer.source, display_point);
        let point = (point.0 as f32, point.1 as f32);
        let minimum_spacing = if self.tool == NativeInteractionTool::Brush {
            (self.brush_size_slide * 0.12).max(1.0)
        } else {
            2.0
        };
        if self.draft_points.last().is_some_and(|previous| {
            let dx = previous.0 - point.0;
            let dy = previous.1 - point.1;
            dx.hypot(dy) < minimum_spacing
        }) {
            return;
        }
        self.draft_points.push(point);
        if self.tool == NativeInteractionTool::Brush {
            let outline = native_brush_outline(&self.draft_points, self.brush_size_slide * 0.5);
            if let Err(error) = renderer.set_draft_polygon(&outline) {
                self.error = Some(error);
            }
        } else if let Err(error) = renderer.set_draft_polygon(&self.draft_points) {
            self.error = Some(error);
        }
    }

    fn cancel_polygon(&mut self) {
        self.tool = NativeInteractionTool::Pan;
        self.draft_points.clear();
        self.editing_roi = None;
        self.curve_edit = None;
        self.brush_operation = NativeBrushOperation::New;
        self.brush_target = None;
        if let Some(renderer) = self.renderer.as_mut() {
            renderer.clear_draft_polygon();
        }
    }

    fn commit_polygon(&mut self) {
        if self.draft_points.len() < 3 {
            self.cancel_polygon();
            return;
        }
        let mut coordinates = self
            .draft_points
            .iter()
            .map(|&(x, y)| vec![x, y])
            .collect::<Vec<_>>();
        coordinates.push(vec![self.draft_points[0].0, self.draft_points[0].1]);
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis();
        let feature = serde_json::json!({
            "type": "Feature",
            "id": format!("native_roi_{timestamp}"),
            "properties": {
                "name": self.annotation_label,
                "classification": { "name": self.annotation_label, "color": self.annotation_colour },
                "colour": self.annotation_colour,
                "visible": true
            },
            "geometry": { "type": "Polygon", "coordinates": [coordinates] }
        });
        self.push_annotation_undo();
        if let Some(sync) = self.state_sync.as_ref() {
            sync.roi_created(feature);
        }
        self.cancel_polygon();
        self.request_state_refresh();
    }

    fn commit_brush(&mut self) {
        let outline = native_brush_outline(&self.draft_points, self.brush_size_slide * 0.5);
        if outline.len() < 3 {
            self.cancel_polygon();
            return;
        }
        if let Some(mut feature) = self.brush_target.clone() {
            let operation = self.brush_operation;
            if native_apply_brush_to_feature(&mut feature, &outline, operation) {
                self.push_annotation_undo();
                self.selected_feature = Some(feature.clone());
                if let Some(sync) = self.state_sync.as_ref() {
                    sync.roi_updated(feature);
                }
                self.record_history(match operation {
                    NativeBrushOperation::Extend => "Brush added to selected same-class ROI",
                    NativeBrushOperation::Subtract => "Brush subtracted from selected ROI",
                    NativeBrushOperation::New => "Brush ROI updated",
                }.to_string());
                self.cancel_polygon();
                self.request_state_refresh();
                return;
            }
            self.error = Some("This brush edit needs a simple Polygon ROI; a new annotation was created instead.".to_string());
        }
        self.draft_points = outline;
        self.commit_polygon();
    }

    fn commit_measurement(&mut self) {
        if self.draft_points.len() < 2 { self.cancel_polygon(); return; }
        let start = self.draft_points[0];
        let end = *self.draft_points.last().unwrap();
        let distance_px = ((end.0 - start.0).powi(2) + (end.1 - start.1).powi(2)).sqrt();
        let distance_um = native_source_mpp(&self.source).map(|mpp| distance_px as f64 * mpp);
        let timestamp = SystemTime::now().duration_since(UNIX_EPOCH).unwrap_or_default().as_millis();
        let measurement = serde_json::json!({
            "id": format!("native_measurement_{timestamp}"),
            "start": {"x": start.0, "y": start.1},
            "end": {"x": end.0, "y": end.1},
            "distance_px": distance_px,
            "distance_um": distance_um
        });
        if let Some(sync) = self.state_sync.as_ref() { sync.measurement_added(measurement); }
        self.record_history(format!("Measurement added: {:.1} px", distance_px));
        self.cancel_polygon();
        self.request_state_refresh();
    }

    fn commit_trajectory(&mut self) {
        if self.draft_points.len() < 2 {
            self.cancel_polygon();
            return;
        }
        let points = self
            .draft_points
            .iter()
            .map(|&(x, y)| serde_json::json!({ "x": x, "y": y }))
            .collect::<Vec<_>>();
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis();
        let length_px = native_path_length(&self.draft_points);
        let trajectory = serde_json::json!({
            "id": format!("native_trajectory_{timestamp}"),
            "name": "Trajectory",
            "n": points.len(),
            "control_points": points,
            "points": self.draft_points.iter().map(|&(x, y)| serde_json::json!({ "x": x, "y": y })).collect::<Vec<_>>(),
            "length_px": length_px,
            "created": timestamp.to_string()
        });
        self.push_annotation_undo();
        if let Some(sync) = self.state_sync.as_ref() {
            sync.trajectory_created(trajectory);
        }
        self.cancel_polygon();
        self.request_state_refresh();
    }

    /// Materialise the selected centreline as an ordinary, flat-capped ROI.
    /// Keeping this as a GeoJSON ROI makes the area editable and available to
    /// the existing R annotation, association, and proximity workflows.
    fn create_or_update_trajectory_area(&mut self, update_only: bool) {
        let Some(trajectory) = self.selected_trajectory.clone() else {
            self.error = Some("Select a trajectory before creating its area.".to_string());
            return;
        };
        let Some(trajectory_id) = trajectory_item_id(&trajectory) else {
            self.error = Some("The selected trajectory has no stable id.".to_string());
            return;
        };
        let points = trajectory_item_points(&trajectory);
        let outline = native_trajectory_area_outline(&points, self.trajectory_area_width_slide);
        if outline.len() < 3 {
            self.error = Some("The selected trajectory needs at least two distinct points.".to_string());
            return;
        }
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis();
        let mut coordinates = outline
            .iter()
            .map(|&(x, y)| vec![x, y])
            .collect::<Vec<_>>();
        coordinates.push(vec![outline[0].0, outline[0].1]);
        let existing = self.renderer.as_ref().and_then(|renderer| {
            renderer.annotation_shapes.iter().find_map(|shape| {
                trajectory_area_trajectory_id(&shape.feature)
                    .filter(|id| id == &trajectory_id)
                    .map(|_| shape.feature.clone())
            })
        });
        if update_only && existing.is_none() {
            self.error = Some("Create a trajectory area before updating it.".to_string());
            return;
        }
        let mut feature = existing.unwrap_or_else(|| serde_json::json!({
            "type": "Feature",
            "id": format!("native_trajectory_area_{timestamp}"),
            "properties": {},
            "geometry": { "type": "Polygon", "coordinates": [] }
        }));
        let properties = feature
            .as_object_mut()
            .and_then(|root| root.entry("properties").or_insert_with(|| serde_json::json!({})).as_object_mut());
        let Some(properties) = properties else {
            self.error = Some("Could not update the trajectory area properties.".to_string());
            return;
        };
        properties.insert("name".to_string(), serde_json::json!("Trajectory border"));
        properties.insert("classification".to_string(), serde_json::json!({
            "name": self.annotation_label,
            "color": self.annotation_colour
        }));
        properties.insert("colour".to_string(), serde_json::json!(self.annotation_colour));
        properties.insert("visible".to_string(), serde_json::json!(true));
        properties.insert("source".to_string(), serde_json::json!("trajectory"));
        properties.insert("trajectory_area".to_string(), serde_json::json!(true));
        properties.insert("trajectory_flat_caps".to_string(), serde_json::json!(true));
        properties.insert("trajectory_id".to_string(), serde_json::json!(trajectory_id));
        properties.insert("trajectory_width_px".to_string(), serde_json::json!(self.trajectory_area_width_slide));
        properties.insert("trajectory_point_count".to_string(), serde_json::json!(points.len()));
        properties.insert("wsiToolsTrajectory".to_string(), serde_json::json!({
            "id": trajectory_item_id(&trajectory),
            "name": trajectory_item_label(&trajectory),
            "width_px": self.trajectory_area_width_slide,
            "point_count": points.len(),
            "flat_caps": true
        }));
        if let Some(root) = feature.as_object_mut() {
            root.insert("geometry".to_string(), serde_json::json!({
                "type": "Polygon",
                "coordinates": [coordinates]
            }));
        }
        self.push_annotation_undo();
        if let Some(sync) = self.state_sync.as_ref() {
            if update_only { sync.roi_updated(feature.clone()); } else { sync.roi_created(feature.clone()); }
        }
        self.selected_feature = Some(feature);
        self.selected_trajectory = None;
        self.record_history(if update_only {
            format!("Trajectory area updated: {:.0} px", self.trajectory_area_width_slide)
        } else {
            format!("Trajectory area created: {:.0} px", self.trajectory_area_width_slide)
        });
        self.request_state_refresh();
    }

    fn edit_selected_trajectory_area(&mut self) {
        let Some(trajectory) = self.selected_trajectory.clone() else {
            self.error = Some("Select a trajectory before editing its area.".to_string());
            return;
        };
        let Some(trajectory_id) = trajectory_item_id(&trajectory) else { return; };
        let feature = self.renderer.as_ref().and_then(|renderer| {
            renderer.annotation_shapes.iter().find_map(|shape| {
                trajectory_area_trajectory_id(&shape.feature)
                    .filter(|id| id == &trajectory_id)
                    .map(|_| shape.feature.clone())
            })
        });
        if feature.is_none() {
            self.create_or_update_trajectory_area(false);
            if self.selected_feature.is_some() {
                self.tool = NativeInteractionTool::Edit;
                self.error = Some("Trajectory border created. Drag a border vertex after the R state refreshes. Press Escape to return to pan.".to_string());
            }
            return;
        }
        let feature = feature.unwrap();
        self.selected_feature = Some(feature.clone());
        self.selected_trajectory = None;
        if let Some(sync) = self.state_sync.as_ref() {
            sync.roi_selection_changed(Some(&feature));
        }
        self.tool = NativeInteractionTool::Edit;
        self.error = Some("Edit mode: drag a trajectory border vertex. Press Escape to return to pan.".to_string());
    }
}

struct NativeWindowRenderer {
    surface: wgpu::Surface<'static>,
    device: wgpu::Device,
    queue: wgpu::Queue,
    config: wgpu::SurfaceConfiguration,
    pipeline: wgpu::RenderPipeline,
    tile_layout: wgpu::BindGroupLayout,
    camera: wgpu::Buffer,
    overlay_pipeline: wgpu::RenderPipeline,
    overlay_fill_pipeline: wgpu::RenderPipeline,
    overlay_bind_group: wgpu::BindGroup,
    egui_renderer: egui_wgpu::Renderer,
    source: NativeTileSource,
    image_transform: NativeImageTransform,
    camera_state: NativeCamera,
    // Keep a small number of recently visited base sources resident. This is
    // deliberately bounded: revisiting a project slide/CZI scene becomes
    // immediate without turning a multi-slide project into a whole-slide GPU
    // cache.
    source_tiles: HashMap<String, NativeTileCache<NativeGpuTile>>,
    source_tile_order: VecDeque<String>,
    source_tile_capacity: usize,
    max_source_tile_caches: usize,
    source_views: HashMap<String, NativeSourceViewState>,
    base_sources: HashMap<String, NativeTileSource>,
    base_style: NativeTileRenderStyle,
    channel_sources: HashMap<String, NativeChannelSource>,
    channel_styles: HashMap<String, NativeTileRenderStyle>,
    channel_tiles: HashMap<String, NativeTileCache<NativeGpuTile>>,
    // GPU overlay snapshots are source-scoped. The R bridge sends the active
    // source's state, but native multi-view must retain the last valid state
    // for other panes instead of clearing it on every source switch.
    saved_source_overlays: HashMap<String, NativeSourceOverlayState>,
    overlays: Vec<NativeOverlayLine>,
    overlay_fills: Vec<NativeOverlayFill>,
    dense_overlays: HashMap<String, Vec<NativeOverlayLine>>,
    point_overlays: HashMap<String, NativeOverlayFill>,
    draft_overlays: Vec<NativeOverlayLine>,
    annotation_shapes: Vec<NativeAnnotationShape>,
    trajectory_items: Vec<serde_json::Value>,
    measurement_items: Vec<serde_json::Value>,
    overlay_revision: Option<u64>,
    pending_screenshot: Option<std::path::PathBuf>,
}

struct NativeScreenshotCapture {
    path: std::path::PathBuf,
    buffer: wgpu::Buffer,
    width: u32,
    height: u32,
    padded_bytes_per_row: u32,
    bgra: bool,
}

struct NativeGpuTile {
    _texture: wgpu::Texture,
    bind_group: wgpu::BindGroup,
    vertices: wgpu::Buffer,
}

#[derive(Default)]
struct NativeSourceOverlayState {
    overlays: Vec<NativeOverlayLine>,
    fills: Vec<NativeOverlayFill>,
    dense: HashMap<String, Vec<NativeOverlayLine>>,
    points: HashMap<String, NativeOverlayFill>,
    annotation_shapes: Vec<NativeAnnotationShape>,
    trajectory_items: Vec<serde_json::Value>,
    measurement_items: Vec<serde_json::Value>,
    revision: Option<u64>,
}

/// Per-source camera/orientation state. This is kept separate from tile
/// residency so revisiting a slide restores its own context even if its least
/// recently used textures have been evicted from the GPU cache.
#[derive(Clone, Copy)]
struct NativeSourceViewState {
    camera: NativeCamera,
    transform: NativeImageTransform,
}

struct NativeTileRenderStyle {
    buffer: wgpu::Buffer,
    visible: bool,
    opacity: f32,
    colour: [f32; 4],
    gain: f32,
    contrast_min: f32,
    contrast_max: f32,
    channel_mode: bool,
    stain_mode: f32,
}

fn native_tile_render_style(
    device: &wgpu::Device,
    visible: bool,
    opacity: f32,
    colour: [f32; 4],
    gain: f32,
    contrast_min: f32,
    contrast_max: f32,
    channel_mode: bool,
    stain_mode: f32,
    label: &str,
) -> NativeTileRenderStyle {
    use wgpu::util::DeviceExt;
    let style = NativeTileRenderStyle {
        buffer: device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some(label),
            contents: bytemuck::cast_slice(&native_tile_style_values(
                opacity,
                colour,
                gain,
                contrast_min,
                contrast_max,
                channel_mode,
                stain_mode,
            )),
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
        }),
        visible,
        opacity,
        colour,
        gain,
        contrast_min,
        contrast_max,
        channel_mode,
        stain_mode,
    };
    style
}

fn native_tile_style_values(
    opacity: f32,
    colour: [f32; 4],
    gain: f32,
    contrast_min: f32,
    contrast_max: f32,
    channel_mode: bool,
    stain_mode: f32,
) -> [f32; 12] {
    [
        colour[0],
        colour[1],
        colour[2],
        opacity.clamp(0.0, 1.0),
        gain.max(0.0),
        contrast_min.clamp(0.0, 1.0),
        contrast_max
            .clamp(0.0, 1.0)
            .max(contrast_min.clamp(0.0, 1.0) + 1e-6),
        if channel_mode { 1.0 } else { 0.0 },
        stain_mode.clamp(0.0, 3.0),
        0.0,
        0.0,
        0.0,
    ]
}

#[derive(Default)]
struct NativeChannelSettingsPatch {
    id: String,
    visible: Option<bool>,
    opacity: Option<f32>,
    colour: Option<String>,
    gain: Option<f32>,
    contrast_min: Option<f32>,
    contrast_max: Option<f32>,
}

fn native_channel_settings_patch(row: &serde_json::Value) -> Option<NativeChannelSettingsPatch> {
    let object = row.as_object()?;
    let id = object.get("id")?.as_str()?.trim().to_string();
    (!id.is_empty()).then(|| NativeChannelSettingsPatch {
        id,
        visible: object.get("visible").and_then(serde_json::Value::as_bool),
        opacity: object
            .get("opacity")
            .and_then(serde_json::Value::as_f64)
            .map(|value| value as f32),
        colour: object
            .get("colour")
            .or_else(|| object.get("color"))
            .and_then(serde_json::Value::as_str)
            .map(str::to_string),
        gain: object
            .get("gain")
            .and_then(serde_json::Value::as_f64)
            .map(|value| value as f32),
        contrast_min: object
            .get("contrast_min")
            .and_then(serde_json::Value::as_f64)
            .map(|value| value as f32),
        contrast_max: object
            .get("contrast_max")
            .and_then(serde_json::Value::as_f64)
            .map(|value| value as f32),
    })
}

fn native_channel_settings_rows(value: &serde_json::Value) -> Vec<NativeChannelSettingsPatch> {
    if let Some(rows) = value.as_array() {
        return rows
            .iter()
            .filter_map(native_channel_settings_patch)
            .collect();
    }
    let Some(columns) = value.as_object() else {
        return Vec::new();
    };
    let ids = columns.get("id").and_then(serde_json::Value::as_array);
    let Some(ids) = ids else {
        return native_channel_settings_patch(value).into_iter().collect();
    };
    ids.iter()
        .enumerate()
        .filter_map(|(index, id)| {
            let id = id.as_str()?.trim();
            (!id.is_empty()).then(|| {
                let field = |name: &str| {
                    columns
                        .get(name)
                        .and_then(serde_json::Value::as_array)
                        .and_then(|values| values.get(index))
                };
                NativeChannelSettingsPatch {
                    id: id.to_string(),
                    visible: field("visible").and_then(serde_json::Value::as_bool),
                    opacity: field("opacity")
                        .and_then(serde_json::Value::as_f64)
                        .map(|number| number as f32),
                    colour: field("colour")
                        .or_else(|| field("color"))
                        .and_then(serde_json::Value::as_str)
                        .map(str::to_string),
                    gain: field("gain")
                        .and_then(serde_json::Value::as_f64)
                        .map(|number| number as f32),
                    contrast_min: field("contrast_min")
                        .and_then(serde_json::Value::as_f64)
                        .map(|number| number as f32),
                    contrast_max: field("contrast_max")
                        .and_then(serde_json::Value::as_f64)
                        .map(|number| number as f32),
                }
            })
        })
        .collect()
}

struct NativeOverlayLine {
    vertices: wgpu::Buffer,
    vertex_count: u32,
}

struct NativeOverlayFill {
    vertices: wgpu::Buffer,
    vertex_count: u32,
}

#[derive(Clone)]
struct NativeAnnotationShape {
    feature: serde_json::Value,
    rings: Vec<Vec<(f32, f32)>>,
}

impl NativeWindowRenderer {
    fn new(
        window: std::sync::Arc<Window>,
        source: &NativeTileSource,
        sources: &[NativeTileSource],
        channels: &[NativeChannelSource],
    ) -> Result<Self, String> {
        pollster::block_on(async move {
            let instance = wgpu::Instance::default();
            let surface = instance
                .create_surface(window.clone())
                .map_err(|error| format!("Could not create native WGPU surface: {error}"))?;
            let adapter = instance
                .request_adapter(&wgpu::RequestAdapterOptions {
                    power_preference: wgpu::PowerPreference::HighPerformance,
                    force_fallback_adapter: false,
                    compatible_surface: Some(&surface),
                })
                .await
                .ok_or_else(|| "No compatible GPU adapter for the native viewer.".to_string())?;
            let (device, queue) = adapter
                .request_device(
                    &wgpu::DeviceDescriptor {
                        label: Some("wsiTools native viewer"),
                        required_features: wgpu::Features::empty(),
                        // The native window can be Retina-sized even when its
                        // logical dimensions are modest. Request the selected
                        // adapter's real limits rather than the conservative
                        // WebGL-style defaults (which cap a surface at 2048).
                        required_limits: adapter.limits(),
                        memory_hints: wgpu::MemoryHints::Performance,
                    },
                    None,
                )
                .await
                .map_err(|error| format!("Could not create native viewer GPU device: {error}"))?;
            let size = window.inner_size();
            let caps = surface.get_capabilities(&adapter);
            let format = caps
                .formats
                .iter()
                .copied()
                .find(|format| format.is_srgb())
                .or_else(|| caps.formats.first().copied())
                .ok_or_else(|| "Native viewer surface has no texture format.".to_string())?;
            let config = wgpu::SurfaceConfiguration {
                usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::COPY_SRC,
                format,
                width: size.width.max(1),
                height: size.height.max(1),
                present_mode: wgpu::PresentMode::AutoVsync,
                alpha_mode: caps
                    .alpha_modes
                    .first()
                    .copied()
                    .unwrap_or(wgpu::CompositeAlphaMode::Auto),
                view_formats: vec![],
                desired_maximum_frame_latency: 2,
            };
            surface.configure(&device, &config);
            let layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: Some("wsiTools native tile layout"),
                entries: &[
                    wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            multisampled: false,
                            view_dimension: wgpu::TextureViewDimension::D2,
                            sample_type: wgpu::TextureSampleType::Float { filterable: true },
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 1,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 2,
                        visibility: wgpu::ShaderStages::VERTEX,
                        ty: wgpu::BindingType::Buffer {
                            ty: wgpu::BufferBindingType::Uniform,
                            has_dynamic_offset: false,
                            min_binding_size: None,
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 3,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Buffer {
                            ty: wgpu::BufferBindingType::Uniform,
                            has_dynamic_offset: false,
                            min_binding_size: None,
                        },
                        count: None,
                    },
                ],
            });
            use wgpu::util::DeviceExt;
            let camera = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some("wsiTools native camera"),
                contents: bytemuck::cast_slice(&[
                    source.width as f32 / 2.0,
                    source.height as f32 / 2.0,
                    NativeCamera::fit(source, config.width, config.height).pixels_per_screen_pixel
                        as f32,
                    0.0,
                    config.width as f32,
                    config.height as f32,
                    0.0,
                    0.0,
                    source.width as f32,
                    source.height as f32,
                    source.width as f32,
                    source.height as f32,
                    0.0,
                    0.0,
                    0.0,
                    0.0,
                ]),
                usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            });
            let base_style = native_tile_render_style(
                &device,
                true,
                1.0,
                [1.0, 1.0, 1.0, 1.0],
                1.0,
                0.0,
                1.0,
                false,
                0.0,
                "wsiTools native H&E style",
            );
            let mut channel_sources = HashMap::new();
            let mut channel_styles = HashMap::new();
            let mut channel_tiles = HashMap::new();
            for channel in channels {
                channel_styles.insert(
                    channel.id.clone(),
                    native_tile_render_style(
                        &device,
                        channel.visible,
                        channel.opacity,
                        parse_hex_colour(&channel.colour),
                        channel.gain,
                        channel.contrast_min,
                        channel.contrast_max,
                        true,
                        0.0,
                        "wsiTools native channel style",
                    ),
                );
                channel_tiles.insert(channel.id.clone(), NativeTileCache::new(180));
                channel_sources.insert(channel.id.clone(), channel.clone());
            }
            let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
                label: Some("wsiTools native tile shader"),
                source: wgpu::ShaderSource::Wgsl(NATIVE_TILE_SHADER.into()),
            });
            let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                label: Some("wsiTools native pipeline layout"),
                bind_group_layouts: &[&layout],
                push_constant_ranges: &[],
            });
            let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                label: Some("wsiTools native tile pipeline"),
                layout: Some(&pipeline_layout),
                vertex: wgpu::VertexState {
                    module: &shader,
                    entry_point: Some("vs_main"),
                    compilation_options: Default::default(),
                    buffers: &[wgpu::VertexBufferLayout {
                        array_stride: 16,
                        step_mode: wgpu::VertexStepMode::Vertex,
                        attributes: &[
                            wgpu::VertexAttribute {
                                offset: 0,
                                shader_location: 0,
                                format: wgpu::VertexFormat::Float32x2,
                            },
                            wgpu::VertexAttribute {
                                offset: 8,
                                shader_location: 1,
                                format: wgpu::VertexFormat::Float32x2,
                            },
                        ],
                    }],
                },
                fragment: Some(wgpu::FragmentState {
                    module: &shader,
                    entry_point: Some("fs_main"),
                    compilation_options: Default::default(),
                    targets: &[Some(wgpu::ColorTargetState {
                        format,
                        blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                        write_mask: wgpu::ColorWrites::ALL,
                    })],
                }),
                primitive: wgpu::PrimitiveState::default(),
                depth_stencil: None,
                multisample: wgpu::MultisampleState::default(),
                multiview: None,
                cache: None,
            });
            let overlay_layout =
                device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                    label: Some("wsiTools native overlay layout"),
                    entries: &[wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::VERTEX,
                        ty: wgpu::BindingType::Buffer {
                            ty: wgpu::BufferBindingType::Uniform,
                            has_dynamic_offset: false,
                            min_binding_size: None,
                        },
                        count: None,
                    }],
                });
            let overlay_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
                label: Some("wsiTools native overlay camera"),
                layout: &overlay_layout,
                entries: &[wgpu::BindGroupEntry {
                    binding: 0,
                    resource: camera.as_entire_binding(),
                }],
            });
            let overlay_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
                label: Some("wsiTools native annotation shader"),
                source: wgpu::ShaderSource::Wgsl(NATIVE_OVERLAY_SHADER.into()),
            });
            let overlay_pipeline_layout =
                device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                    label: Some("wsiTools native annotation pipeline layout"),
                    bind_group_layouts: &[&overlay_layout],
                    push_constant_ranges: &[],
                });
            let overlay_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                label: Some("wsiTools native annotation pipeline"),
                layout: Some(&overlay_pipeline_layout),
                vertex: wgpu::VertexState {
                    module: &overlay_shader,
                    entry_point: Some("vs_main"),
                    compilation_options: Default::default(),
                    buffers: &[wgpu::VertexBufferLayout {
                        array_stride: 24,
                        step_mode: wgpu::VertexStepMode::Vertex,
                        attributes: &[
                            wgpu::VertexAttribute {
                                offset: 0,
                                shader_location: 0,
                                format: wgpu::VertexFormat::Float32x2,
                            },
                            wgpu::VertexAttribute {
                                offset: 8,
                                shader_location: 1,
                                format: wgpu::VertexFormat::Float32x4,
                            },
                        ],
                    }],
                },
                fragment: Some(wgpu::FragmentState {
                    module: &overlay_shader,
                    entry_point: Some("fs_main"),
                    compilation_options: Default::default(),
                    targets: &[Some(wgpu::ColorTargetState {
                        format,
                        blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                        write_mask: wgpu::ColorWrites::ALL,
                    })],
                }),
                primitive: wgpu::PrimitiveState {
                    topology: wgpu::PrimitiveTopology::LineStrip,
                    strip_index_format: None,
                    ..Default::default()
                },
                depth_stencil: None,
                multisample: wgpu::MultisampleState::default(),
                multiview: None,
                cache: None,
            });
            let overlay_fill_pipeline =
                device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                    label: Some("wsiTools native annotation fill pipeline"),
                    layout: Some(&overlay_pipeline_layout),
                    vertex: wgpu::VertexState {
                        module: &overlay_shader,
                        entry_point: Some("vs_main"),
                        compilation_options: Default::default(),
                        buffers: &[wgpu::VertexBufferLayout {
                            array_stride: 24,
                            step_mode: wgpu::VertexStepMode::Vertex,
                            attributes: &[
                                wgpu::VertexAttribute {
                                    offset: 0,
                                    shader_location: 0,
                                    format: wgpu::VertexFormat::Float32x2,
                                },
                                wgpu::VertexAttribute {
                                    offset: 8,
                                    shader_location: 1,
                                    format: wgpu::VertexFormat::Float32x4,
                                },
                            ],
                        }],
                    },
                    fragment: Some(wgpu::FragmentState {
                        module: &overlay_shader,
                        entry_point: Some("fs_main"),
                        compilation_options: Default::default(),
                        targets: &[Some(wgpu::ColorTargetState {
                            format,
                            blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                            write_mask: wgpu::ColorWrites::ALL,
                        })],
                    }),
                    primitive: wgpu::PrimitiveState {
                        topology: wgpu::PrimitiveTopology::TriangleList,
                        strip_index_format: None,
                        ..Default::default()
                    },
                    depth_stencil: None,
                    multisample: wgpu::MultisampleState::default(),
                    multiview: None,
                    cache: None,
                });
            let egui_renderer = egui_wgpu::Renderer::new(&device, format, None, 1, false);
            let image_transform = NativeImageTransform::default();
            let (display_width, display_height) = image_transform.display_dimensions(source);
            let camera_state = NativeCamera::fit_dimensions(
                display_width,
                display_height,
                config.width,
                config.height,
            );
            Ok(Self {
                surface,
                device,
                queue,
                config,
                pipeline,
                tile_layout: layout,
                camera,
                overlay_pipeline,
                overlay_fill_pipeline,
                overlay_bind_group,
                egui_renderer,
                source: source.clone(),
                image_transform,
                camera_state,
                source_tiles: {
                    let mut caches = HashMap::new();
                    caches.insert(source.id.clone(), NativeTileCache::new(192));
                    caches
                },
                source_tile_order: VecDeque::from([source.id.clone()]),
                source_tile_capacity: 192,
                max_source_tile_caches: 4,
                source_views: HashMap::new(),
                base_sources: sources
                    .iter()
                    .cloned()
                    .map(|item| (item.id.clone(), item))
                    .collect(),
                base_style,
                channel_sources,
                channel_styles,
                channel_tiles,
                saved_source_overlays: HashMap::new(),
                overlays: Vec::new(),
                overlay_fills: Vec::new(),
                dense_overlays: HashMap::new(),
                point_overlays: HashMap::new(),
                draft_overlays: Vec::new(),
                annotation_shapes: Vec::new(),
                trajectory_items: Vec::new(),
                measurement_items: Vec::new(),
                overlay_revision: None,
                pending_screenshot: None,
            })
        })
    }

    fn resize(&mut self, width: u32, height: u32) {
        if width > 0 && height > 0 {
            self.config.width = width;
            self.config.height = height;
            self.reconfigure();
            self.sync_camera();
        }
    }
    fn reconfigure(&self) {
        self.surface.configure(&self.device, &self.config);
    }
    fn zoom(&mut self, factor: f32, cursor: (f64, f64)) {
        self.camera_state.zoom_about(
            factor as f64,
            cursor.0,
            cursor.1,
            self.config.width,
            self.config.height,
        );
        self.sync_camera();
    }
    fn pan(&mut self, dx: f32, dy: f32) {
        self.camera_state.pan_screen_pixels(dx as f64, dy as f64);
        self.sync_camera();
    }
    fn sync_camera(&self) {
        self.sync_camera_for(
            &self.source,
            self.image_transform,
            self.camera_state,
            self.config.width,
            self.config.height,
        );
    }
    fn sync_camera_for(
        &self,
        source: &NativeTileSource,
        transform: NativeImageTransform,
        camera: NativeCamera,
        width: u32,
        height: u32,
    ) {
        let (display_width, display_height) = transform.display_dimensions(source);
        self.queue.write_buffer(
            &self.camera,
            0,
            bytemuck::cast_slice(&[
                camera.center_x as f32,
                camera.center_y as f32,
                camera.pixels_per_screen_pixel as f32,
                transform.normalized_rotation() as f32,
                width as f32,
                height as f32,
                if transform.flip_x { 1.0 } else { 0.0 },
                if transform.flip_y { 1.0 } else { 0.0 },
                source.width as f32,
                source.height as f32,
                display_width as f32,
                display_height as f32,
                0.0,
                0.0,
                0.0,
                0.0,
            ]),
        );
    }
    fn request_plan(&self, maximum_tiles: usize) -> Vec<TileKey> {
        self.request_plan_for(
            &self.source,
            self.image_transform,
            self.camera_state,
            self.config.width,
            self.config.height,
            maximum_tiles,
        )
    }

    fn request_plan_for(
        &self,
        source: &NativeTileSource,
        transform: NativeImageTransform,
        camera: NativeCamera,
        width: u32,
        height: u32,
        maximum_tiles: usize,
    ) -> Vec<TileKey> {
        if transform == NativeImageTransform::default() {
            native_tile_request_plan(source, camera, width, height, maximum_tiles)
        } else {
            native_tile_request_plan_transformed(
                source,
                transform,
                camera,
                width,
                height,
                maximum_tiles,
            )
        }
    }
    fn has_tile(&self, key: &TileKey) -> bool {
        self.has_source_tile(&self.source.id, key)
    }

    fn has_source_tile(&self, source_id: &str, key: &TileKey) -> bool {
        self.source_tiles
            .get(source_id)
            .is_some_and(|cache| cache.contains(key))
    }

    fn touch_source_tile_cache(&mut self, source_id: &str) {
        if let Some(position) = self.source_tile_order.iter().position(|id| id == source_id) {
            self.source_tile_order.remove(position);
        }
        self.source_tile_order.push_back(source_id.to_string());
        while self.source_tile_order.len() > self.max_source_tile_caches {
            if let Some(oldest) = self.source_tile_order.pop_front() {
                if oldest != self.source.id {
                    self.source_tiles.remove(&oldest);
                    // Dense GeoJSON and spatial-point buffers can outweigh a
                    // tile cache. Keep their residency bounded by the same
                    // recently visited-source policy.
                    self.saved_source_overlays.remove(&oldest);
                }
            }
        }
    }

    fn ensure_source_tile_cache(&mut self, source_id: &str) {
        self.source_tiles
            .entry(source_id.to_string())
            .or_insert_with(|| NativeTileCache::new(self.source_tile_capacity));
        self.touch_source_tile_cache(source_id);
    }
    fn set_channel_display(
        &mut self,
        id: &str,
        visible: bool,
        opacity: f32,
        colour: &str,
        gain: f32,
        contrast_min: f32,
        contrast_max: f32,
    ) {
        let Some(style) = self.channel_styles.get_mut(id) else {
            return;
        };
        style.visible = visible;
        style.opacity = opacity.clamp(0.0, 1.0);
        style.colour = parse_hex_colour(colour);
        style.gain = gain.max(0.0);
        style.contrast_min = contrast_min.clamp(0.0, 1.0);
        style.contrast_max = contrast_max.clamp(style.contrast_min + 1e-6, 1.0);
        self.queue.write_buffer(
            &style.buffer,
            0,
            bytemuck::cast_slice(&native_tile_style_values(
                style.opacity,
                style.colour,
                style.gain,
                style.contrast_min,
                style.contrast_max,
                style.channel_mode,
                style.stain_mode,
            )),
        );
        if let Some(channel) = self.channel_sources.get_mut(id) {
            channel.visible = visible;
            channel.opacity = style.opacity;
            channel.colour = colour.to_string();
            channel.gain = style.gain;
            channel.contrast_min = style.contrast_min;
            channel.contrast_max = style.contrast_max;
        }
    }
    fn set_source(&mut self, source: NativeTileSource) {
        let previous_id = self.source.id.clone();
        self.source_views.insert(
            previous_id,
            NativeSourceViewState {
                camera: self.camera_state,
                transform: self.image_transform,
            },
        );
        self.save_active_overlay_state();
        self.source = source;
        let source_id = self.source.id.clone();
        self.ensure_source_tile_cache(&source_id);
        if let Some(view) = self.source_views.get(&source_id).copied() {
            self.camera_state = view.camera;
            self.image_transform = view.transform;
        } else {
            self.image_transform = NativeImageTransform::default();
            self.reset_camera();
        }
        self.restore_active_overlay_state();
        self.draft_overlays.clear();
        self.sync_camera();
    }

    fn save_active_overlay_state(&mut self) {
        let state = NativeSourceOverlayState {
            overlays: std::mem::take(&mut self.overlays),
            fills: std::mem::take(&mut self.overlay_fills),
            dense: std::mem::take(&mut self.dense_overlays),
            points: std::mem::take(&mut self.point_overlays),
            annotation_shapes: std::mem::take(&mut self.annotation_shapes),
            trajectory_items: std::mem::take(&mut self.trajectory_items),
            measurement_items: std::mem::take(&mut self.measurement_items),
            revision: self.overlay_revision.take(),
        };
        self.saved_source_overlays
            .insert(self.source.id.clone(), state);
    }

    fn restore_active_overlay_state(&mut self) {
        let state = self
            .saved_source_overlays
            .remove(&self.source.id)
            .unwrap_or_default();
        self.overlays = state.overlays;
        self.overlay_fills = state.fills;
        self.dense_overlays = state.dense;
        self.point_overlays = state.points;
        self.annotation_shapes = state.annotation_shapes;
        self.trajectory_items = state.trajectory_items;
        self.measurement_items = state.measurement_items;
        self.overlay_revision = state.revision;
    }
    fn reset_camera(&mut self) {
        let (width, height) = self.image_transform.display_dimensions(&self.source);
        self.camera_state =
            NativeCamera::fit_dimensions(width, height, self.config.width, self.config.height);
        self.sync_camera();
    }
    fn source_viewport_bounds(&self) -> (f64, f64, f64, f64) {
        let (display_xmin, display_ymin, display_xmax, display_ymax) = self
            .camera_state
            .viewport_bounds(self.config.width, self.config.height);
        let corners = [
            (display_xmin, display_ymin),
            (display_xmax, display_ymin),
            (display_xmax, display_ymax),
            (display_xmin, display_ymax),
        ];
        let mut xmin = f64::INFINITY;
        let mut ymin = f64::INFINITY;
        let mut xmax = f64::NEG_INFINITY;
        let mut ymax = f64::NEG_INFINITY;
        for corner in corners {
            let (x, y) = self.image_transform.inverse_map_point(&self.source, corner);
            xmin = xmin.min(x);
            ymin = ymin.min(y);
            xmax = xmax.max(x);
            ymax = ymax.max(y);
        }
        (
            xmin.clamp(0.0, self.source.width),
            ymin.clamp(0.0, self.source.height),
            xmax.clamp(0.0, self.source.width),
            ymax.clamp(0.0, self.source.height),
        )
    }
    fn set_overlay_state(&mut self, state: NativeRendererState) -> Result<(), String> {
        self.apply_channel_settings(&state.channel_settings);
        self.set_stain_display(NativeStainDisplay::from_state(&state.stain));
        let base_visible = state.stain.get("native_wgpu_base_visible")
            .and_then(serde_json::Value::as_bool).unwrap_or(true);
        let base_opacity = state.stain.get("native_wgpu_base_opacity")
            .and_then(serde_json::Value::as_f64).unwrap_or(1.0) as f32;
        self.set_base_display(base_visible, base_opacity);
        if self.overlay_revision == Some(state.revision) {
            return Ok(());
        }
        use wgpu::util::DeviceExt;
        let mut overlays = Vec::new();
        let mut fills = Vec::new();
        let shapes = annotation_shapes_from_state(&state);
        // Segmentation output is intentionally a separate read-only overlay:
        // it is visible and persisted by R, but it is never mistaken for an
        // editable tissue ROI in the annotation panel.
        let mut rendered_shapes = shapes.clone();
        rendered_shapes.extend(segmentation_shapes_from_state(&state));
        let mut line_data = overlay_lines_from_shapes(&state, &rendered_shapes, 160_000);
        line_data.extend(trajectory_lines_from_state(&state, 40_000));
        line_data.extend(measurement_lines_from_state(&state));
        for line in line_data {
            if line.vertices.len() < 12 {
                continue;
            }
            let vertices = self
                .device
                .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                    label: Some("wsiTools native annotation outline"),
                    contents: bytemuck::cast_slice(&line.vertices),
                    usage: wgpu::BufferUsages::VERTEX,
                });
            overlays.push(NativeOverlayLine {
                vertices,
                vertex_count: (line.vertices.len() / 6) as u32,
            });
        }
        for fill in overlay_fills_from_shapes(&state, &rendered_shapes, 240_000) {
            if fill.vertices.len() < 18 {
                continue;
            }
            let vertices = self
                .device
                .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                    label: Some("wsiTools native annotation fill"),
                    contents: bytemuck::cast_slice(&fill.vertices),
                    usage: wgpu::BufferUsages::VERTEX,
                });
            fills.push(NativeOverlayFill {
                vertices,
                vertex_count: (fill.vertices.len() / 6) as u32,
            });
        }
        self.overlays = overlays;
        self.overlay_fills = fills;
        self.annotation_shapes = shapes;
        self.trajectory_items = state.trajectories.as_array().cloned().unwrap_or_default();
        self.measurement_items = state.measurements.as_array().cloned().unwrap_or_default();
        self.overlay_revision = Some(state.revision);
        Ok(())
    }
    fn set_stain_display(&mut self, display: NativeStainDisplay) {
        self.base_style.stain_mode = display.shader_mode();
        self.queue.write_buffer(
            &self.base_style.buffer,
            0,
            bytemuck::cast_slice(&native_tile_style_values(
                if self.base_style.visible { self.base_style.opacity } else { 0.0 },
                self.base_style.colour,
                self.base_style.gain,
                self.base_style.contrast_min,
                self.base_style.contrast_max,
                self.base_style.channel_mode,
                self.base_style.stain_mode,
            )),
        );
    }
    fn set_base_display(&mut self, visible: bool, opacity: f32) {
        self.base_style.visible = visible;
        self.base_style.opacity = opacity.clamp(0.0, 1.0);
        self.queue.write_buffer(
            &self.base_style.buffer,
            0,
            bytemuck::cast_slice(&native_tile_style_values(
                if self.base_style.visible { self.base_style.opacity } else { 0.0 },
                self.base_style.colour,
                self.base_style.gain,
                self.base_style.contrast_min,
                self.base_style.contrast_max,
                self.base_style.channel_mode,
                self.base_style.stain_mode,
            )),
        );
    }
    fn apply_channel_settings(&mut self, settings: &serde_json::Value) {
        for patch in native_channel_settings_rows(settings) {
            let Some(style) = self.channel_styles.get_mut(&patch.id) else {
                continue;
            };
            if let Some(visible) = patch.visible {
                style.visible = visible;
            }
            if let Some(opacity) = patch.opacity {
                style.opacity = opacity.clamp(0.0, 1.0);
            }
            if let Some(colour) = patch.colour.as_ref() {
                style.colour = parse_hex_colour(colour);
            }
            if let Some(gain) = patch.gain {
                style.gain = gain.max(0.0);
            }
            if let Some(minimum) = patch.contrast_min {
                style.contrast_min = minimum.clamp(0.0, 1.0);
            }
            if let Some(maximum) = patch.contrast_max {
                style.contrast_max = maximum.clamp(0.0, 1.0);
            }
            self.queue.write_buffer(
                &style.buffer,
                0,
                bytemuck::cast_slice(&native_tile_style_values(
                    style.opacity,
                    style.colour,
                    style.gain,
                    style.contrast_min,
                    style.contrast_max,
                    style.channel_mode,
                    style.stain_mode,
                )),
            );
            if let Some(channel) = self.channel_sources.get_mut(&patch.id) {
                channel.visible = style.visible;
                channel.opacity = style.opacity;
                if let Some(colour) = patch.colour.as_ref() {
                    channel.colour = colour.clone();
                }
                channel.gain = style.gain;
                channel.contrast_min = style.contrast_min;
                channel.contrast_max = style.contrast_max;
            }
        }
    }
    fn set_dense_overlay(
        &mut self,
        source_id: &str,
        items: Vec<serde_json::Value>,
    ) -> Result<(), String> {
        use wgpu::util::DeviceExt;
        let mut overlays = Vec::new();
        for line in dense_lines_from_items(&items, 160_000) {
            if line.vertices.len() < 12 {
                continue;
            }
            let vertices = self
                .device
                .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                    label: Some("wsiTools native dense annotation outline"),
                    contents: bytemuck::cast_slice(&line.vertices),
                    usage: wgpu::BufferUsages::VERTEX,
                });
            overlays.push(NativeOverlayLine {
                vertices,
                vertex_count: (line.vertices.len() / 6) as u32,
            });
        }
        self.dense_overlays.insert(source_id.to_string(), overlays);
        Ok(())
    }
    fn set_point_overlay(
        &mut self,
        source_id: &str,
        points: Vec<NativePointItem>,
        _represented: usize,
        _total: usize,
        _stride: usize,
        opacity: f32,
    ) -> Result<(), String> {
        use wgpu::util::DeviceExt;
        let vertices = native_point_circle_vertices(&points, 50_000, 14, opacity);
        if vertices.len() < 18 {
            self.point_overlays.remove(source_id);
            return Ok(());
        }
        let buffer = self
            .device
            .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some("wsiTools native spatial points"),
                contents: bytemuck::cast_slice(&vertices),
                usage: wgpu::BufferUsages::VERTEX,
            });
        self.point_overlays.insert(
            source_id.to_string(),
            NativeOverlayFill {
                vertices: buffer,
                vertex_count: (vertices.len() / 6) as u32,
            },
        );
        Ok(())
    }
    fn set_draft_polygon(&mut self, points: &[(f32, f32)]) -> Result<(), String> {
        use wgpu::util::DeviceExt;
        self.draft_overlays.clear();
        if points.len() < 2 {
            return Ok(());
        }
        let mut ring = points.to_vec();
        if ring.len() > 2 {
            ring.push(ring[0]);
        }
        let mut vertices = Vec::with_capacity(ring.len() * 6);
        for (x, y) in ring {
            vertices.extend_from_slice(&[x, y, 0.13, 0.77, 0.37, 1.0]);
        }
        let buffer = self
            .device
            .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some("wsiTools native annotation draft"),
                contents: bytemuck::cast_slice(&vertices),
                usage: wgpu::BufferUsages::VERTEX,
            });
        self.draft_overlays.push(NativeOverlayLine {
            vertices: buffer,
            vertex_count: (vertices.len() / 6) as u32,
        });
        Ok(())
    }
    fn clear_draft_polygon(&mut self) {
        self.draft_overlays.clear();
    }
    fn roi_at_screen_in_view(
        &self,
        screen: (f64, f64),
        width: u32,
        height: u32,
    ) -> Option<serde_json::Value> {
        let display_point = self
            .camera_state
            .slide_at(screen.0, screen.1, width, height);
        let point = self
            .image_transform
            .inverse_map_point(&self.source, display_point);
        self.annotation_shapes
            .iter()
            .rev()
            .find(|shape| point_in_annotation(point, shape))
            .map(|shape| shape.feature.clone())
    }
    fn roi_at_screen(&self, screen: (f64, f64)) -> Option<serde_json::Value> {
        self.roi_at_screen_in_view(screen, self.config.width, self.config.height)
    }
    fn annotation_label_positions(&self, maximum: usize) -> Vec<(String, (f32, f32), [f32; 4])> {
        self.annotation_label_positions_for_pane(
            &NativeRenderPane {
                source: self.source.clone(),
                camera: self.camera_state,
                transform: self.image_transform,
                rect: NativePaneRect {
                    x: 0,
                    y: 0,
                    width: self.config.width,
                    height: self.config.height,
                },
            },
            maximum,
        )
    }

    fn annotation_label_positions_for_pane(
        &self,
        pane: &NativeRenderPane,
        maximum: usize,
    ) -> Vec<(String, (f32, f32), [f32; 4])> {
        let mut labels = Vec::new();
        let mut occupied = Vec::<(f32, f32)>::new();
        let shapes = if pane.source.id == self.source.id {
            self.annotation_shapes.as_slice()
        } else {
            self.saved_source_overlays
                .get(&pane.source.id)
                .map(|state| state.annotation_shapes.as_slice())
                .unwrap_or(&[])
        };
        for shape in shapes {
            if labels.len() >= maximum {
                break;
            }
            let Some(ring) = shape.rings.first() else {
                continue;
            };
            if ring.len() < 3 {
                continue;
            }
            let count = ring.len() as f32;
            let x = ring.iter().map(|point| point.0).sum::<f32>() / count;
            let y = ring.iter().map(|point| point.1).sum::<f32>() / count;
            let display = pane
                .transform
                .map_point(&pane.source, (x as f64, y as f64));
            let screen = pane.camera.screen_at(
                display.0,
                display.1,
                pane.rect.width,
                pane.rect.height,
            );
            let screen = (
                screen.0 as f32 + pane.rect.x as f32,
                screen.1 as f32 + pane.rect.y as f32,
            );
            // Prevent a crowd of nearly coincident labels from turning into an
            // unreadable dark patch. The ROI outlines remain visible either way.
            if occupied
                .iter()
                .any(|point| (point.0 - screen.0).abs() < 86.0 && (point.1 - screen.1).abs() < 25.0)
            {
                continue;
            }
            if screen.0 < 5.0
                || screen.1 < 5.0
                || screen.0 < pane.rect.x as f32 + 5.0
                || screen.1 < pane.rect.y as f32 + 5.0
                || screen.0 > (pane.rect.x + pane.rect.width) as f32 - 5.0
                || screen.1 > (pane.rect.y + pane.rect.height) as f32 - 5.0
            {
                continue;
            }
            occupied.push(screen);
            let colour = shape
                .feature
                .get("properties")
                .and_then(|properties| properties.get("color").or_else(|| properties.get("colour")))
                .and_then(serde_json::Value::as_str)
                .map(parse_hex_colour)
                .unwrap_or([0.13, 0.77, 0.37, 1.0]);
            labels.push((annotation_feature_label(&shape.feature), screen, colour));
        }
        labels
    }
    fn upload_tile(&mut self, tile: NativeDecodedTile) -> Result<(), String> {
        use wgpu::util::DeviceExt;
        let is_base_source = self.base_sources.contains_key(&tile.source_id);
        let source = if let Some(source) = self.base_sources.get(&tile.source_id) {
            source.clone()
        } else {
            self.channel_sources
                .get(&tile.source_id)
                .map(NativeChannelSource::tile_source)
                .ok_or_else(|| format!("Unknown native tile source `{}`.", tile.source_id))?
        };
        let style = if is_base_source {
            &self.base_style
        } else {
            self.channel_styles
                .get(&tile.source_id)
                .ok_or_else(|| format!("Missing native channel style `{}`.", tile.source_id))?
        };
        let texture = self.device.create_texture(&wgpu::TextureDescriptor {
            label: Some("wsiTools native dynamic tile"),
            size: wgpu::Extent3d {
                width: tile.width,
                height: tile.height,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Rgba8UnormSrgb,
            usage: wgpu::TextureUsages::COPY_DST | wgpu::TextureUsages::TEXTURE_BINDING,
            view_formats: &[],
        });
        self.queue.write_texture(
            wgpu::TexelCopyTextureInfo {
                texture: &texture,
                mip_level: 0,
                origin: wgpu::Origin3d::ZERO,
                aspect: wgpu::TextureAspect::All,
            },
            &tile.rgba,
            wgpu::TexelCopyBufferLayout {
                offset: 0,
                bytes_per_row: Some(4 * tile.width),
                rows_per_image: Some(tile.height),
            },
            wgpu::Extent3d {
                width: tile.width,
                height: tile.height,
                depth_or_array_layers: 1,
            },
        );
        let view = texture.create_view(&wgpu::TextureViewDescriptor::default());
        let sampler = self.device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("wsiTools native tile sampler"),
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            ..Default::default()
        });
        let bind_group = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("wsiTools native tile bind group"),
            layout: &self.tile_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(&view),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(&sampler),
                },
                wgpu::BindGroupEntry {
                    binding: 2,
                    resource: self.camera.as_entire_binding(),
                },
                wgpu::BindGroupEntry {
                    binding: 3,
                    resource: style.buffer.as_entire_binding(),
                },
            ],
        });
        let vertices = self
            .device
            .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some("wsiTools native tile quad"),
                contents: bytemuck::cast_slice(&tile_vertices(&source, tile.key)),
                usage: wgpu::BufferUsages::VERTEX,
            });
        let key = tile.key;
        let source_id = tile.source_id;
        let gpu_tile = NativeGpuTile {
            _texture: texture,
            bind_group,
            vertices,
        };
        if is_base_source {
            self.ensure_source_tile_cache(&source_id);
            if let Some(cache) = self.source_tiles.get_mut(&source_id) {
                cache.insert(key, gpu_tile);
            }
        } else if let Some(cache) = self.channel_tiles.get_mut(&source_id) {
            cache.insert(key, gpu_tile);
        }
        Ok(())
    }
    fn render(&mut self, egui_frame: Option<NativeEguiFrame>) -> Result<(), String> {
        self.render_background(
            wgpu::Color {
                r: 0.035,
                g: 0.06,
                b: 0.11,
                a: 1.0,
            },
            egui_frame,
        )
    }

    fn request_screenshot(&mut self, path: std::path::PathBuf) {
        self.pending_screenshot = Some(path);
    }

    fn begin_screenshot_capture(
        &mut self,
        encoder: &mut wgpu::CommandEncoder,
        texture: &wgpu::Texture,
    ) -> Option<NativeScreenshotCapture> {
        let path = self.pending_screenshot.take()?;
        let width = self.config.width.max(1);
        let height = self.config.height.max(1);
        let unpadded_bytes_per_row = width.saturating_mul(4);
        let alignment = wgpu::COPY_BYTES_PER_ROW_ALIGNMENT;
        let padded_bytes_per_row =
            ((unpadded_bytes_per_row + alignment - 1) / alignment) * alignment;
        let buffer = self.device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("wsiTools native screenshot readback"),
            size: u64::from(padded_bytes_per_row) * u64::from(height),
            usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
            mapped_at_creation: false,
        });
        encoder.copy_texture_to_buffer(
            wgpu::TexelCopyTextureInfo {
                texture,
                mip_level: 0,
                origin: wgpu::Origin3d::ZERO,
                aspect: wgpu::TextureAspect::All,
            },
            wgpu::TexelCopyBufferInfo {
                buffer: &buffer,
                layout: wgpu::TexelCopyBufferLayout {
                    offset: 0,
                    bytes_per_row: Some(padded_bytes_per_row),
                    rows_per_image: Some(height),
                },
            },
            wgpu::Extent3d {
                width,
                height,
                depth_or_array_layers: 1,
            },
        );
        Some(NativeScreenshotCapture {
            path,
            buffer,
            width,
            height,
            padded_bytes_per_row,
            bgra: matches!(self.config.format, wgpu::TextureFormat::Bgra8Unorm | wgpu::TextureFormat::Bgra8UnormSrgb),
        })
    }

    fn finish_screenshot_capture(&self, capture: NativeScreenshotCapture) -> Result<(), String> {
        let slice = capture.buffer.slice(..);
        let (sender, receiver) = mpsc::sync_channel(1);
        slice.map_async(wgpu::MapMode::Read, move |result| {
            let _ = sender.send(result);
        });
        self.device.poll(wgpu::Maintain::wait());
        receiver
            .recv_timeout(Duration::from_secs(10))
            .map_err(|error| format!("Screenshot GPU readback timed out: {error}"))?
            .map_err(|error| format!("Screenshot GPU readback failed: {error}"))?;
        let mapped = slice.get_mapped_range();
        let rgba = native_screenshot_rgba_bytes(
            &mapped,
            capture.width,
            capture.height,
            capture.padded_bytes_per_row,
            capture.bgra,
        )?;
        drop(mapped);
        capture.buffer.unmap();
        let image = image::RgbaImage::from_raw(capture.width, capture.height, rgba)
            .ok_or_else(|| "Could not create the screenshot image buffer.".to_string())?;
        native_save_screenshot(&image, &capture.path)
    }

    fn render_panes(
        &mut self,
        panes: &[NativeRenderPane],
        egui_frame: Option<NativeEguiFrame>,
    ) -> Result<(), String> {
        if panes.len() <= 1 {
            return self.render(egui_frame);
        }
        let frame = self
            .surface
            .get_current_texture()
            .map_err(|error| format!("Could not acquire native viewer frame: {error}"))?;
        let view = frame
            .texture
            .create_view(&wgpu::TextureViewDescriptor::default());
        // Clear once, then submit each pane after replacing the shared camera
        // uniform. Queue order keeps each pane's camera binding independent
        // without copying tile textures per viewport.
        let mut clear_encoder =
            self.device
                .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                    label: Some("wsiTools native multi-view clear"),
                });
        {
            let _pass = clear_encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("wsiTools native multi-view background"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &view,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color {
                            r: 0.035,
                            g: 0.06,
                            b: 0.11,
                            a: 1.0,
                        }),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
            });
        }
        self.queue.submit(Some(clear_encoder.finish()));
        for pane in panes {
            self.sync_camera_for(
                &pane.source,
                pane.transform,
                pane.camera,
                pane.rect.width,
                pane.rect.height,
            );
            let mut encoder = self
                .device
                .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                    label: Some("wsiTools native multi-view pane"),
                });
            {
                let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                    label: Some("wsiTools native multi-view tile pass"),
                    color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                        view: &view,
                        resolve_target: None,
                        ops: wgpu::Operations {
                            load: wgpu::LoadOp::Load,
                            store: wgpu::StoreOp::Store,
                        },
                    })],
                    depth_stencil_attachment: None,
                    timestamp_writes: None,
                    occlusion_query_set: None,
                });
                pass.set_viewport(
                    pane.rect.x as f32,
                    pane.rect.y as f32,
                    pane.rect.width as f32,
                    pane.rect.height as f32,
                    0.0,
                    1.0,
                );
                pass.set_scissor_rect(pane.rect.x, pane.rect.y, pane.rect.width, pane.rect.height);
                pass.set_pipeline(&self.pipeline);
                let desired_level = level_for_camera(&pane.source, pane.camera);
                let mut tiles = self
                    .source_tiles
                    .get(&pane.source.id)
                    .into_iter()
                    .flat_map(|cache| cache.values.iter())
                    .filter(|(key, _)| {
                        let visible = if pane.transform == NativeImageTransform::default() {
                            tile_intersects_viewport(
                                &pane.source,
                                **key,
                                pane.camera,
                                pane.rect.width,
                                pane.rect.height,
                            )
                        } else {
                            tile_intersects_viewport_transformed(
                                &pane.source,
                                pane.transform,
                                **key,
                                pane.camera,
                                pane.rect.width,
                                pane.rect.height,
                            )
                        };
                        key.level <= desired_level && visible
                    })
                    .collect::<Vec<_>>();
                tiles.sort_by_key(|(key, _)| key.level);
                for (_, tile) in tiles {
                    pass.set_bind_group(0, &tile.bind_group, &[]);
                    pass.set_vertex_buffer(0, tile.vertices.slice(..));
                    pass.draw(0..6, 0..1);
                }
                // Tiled channel layers follow the exact camera and clip
                // rectangle of their pane. This keeps H&E/mIHC overlays
                // locked to the correct slide when several sources are open.
                for (source_id, channel) in &self.channel_sources {
                    let Some(style) = self.channel_styles.get(source_id) else {
                        continue;
                    };
                    if !style.visible
                        || style.opacity <= 0.0
                        || !channel.matches_source(&pane.source)
                    {
                        continue;
                    }
                    let Some(cache) = self.channel_tiles.get(source_id) else {
                        continue;
                    };
                    let channel_source = channel.tile_source();
                    let desired_level = level_for_camera(&channel_source, pane.camera);
                    let mut channel_tiles = cache
                        .values
                        .iter()
                        .filter(|(key, _)| {
                            let visible = if pane.transform == NativeImageTransform::default() {
                                tile_intersects_viewport(
                                    &channel_source,
                                    **key,
                                    pane.camera,
                                    pane.rect.width,
                                    pane.rect.height,
                                )
                            } else {
                                tile_intersects_viewport_transformed(
                                    &channel_source,
                                    pane.transform,
                                    **key,
                                    pane.camera,
                                    pane.rect.width,
                                    pane.rect.height,
                                )
                            };
                            key.level <= desired_level && visible
                        })
                        .collect::<Vec<_>>();
                    channel_tiles.sort_by_key(|(key, _)| key.level);
                    for (_, tile) in channel_tiles {
                        pass.set_bind_group(0, &tile.bind_group, &[]);
                        pass.set_vertex_buffer(0, tile.vertices.slice(..));
                        pass.draw(0..6, 0..1);
                    }
                }
                // Source-scoped GPU snapshots keep existing annotations,
                // trajectories, dense geometry and spatial points visible in
                // their own panes while another pane is active.
                if pane.source.id == self.source.id {
                    pass.set_pipeline(&self.overlay_fill_pipeline);
                    pass.set_bind_group(0, &self.overlay_bind_group, &[]);
                    for points in self.point_overlays.values() {
                        pass.set_vertex_buffer(0, points.vertices.slice(..));
                        pass.draw(0..points.vertex_count, 0..1);
                    }
                    for fill in &self.overlay_fills {
                        pass.set_vertex_buffer(0, fill.vertices.slice(..));
                        pass.draw(0..fill.vertex_count, 0..1);
                    }
                    pass.set_pipeline(&self.overlay_pipeline);
                    for line in &self.overlays {
                        pass.set_vertex_buffer(0, line.vertices.slice(..));
                        pass.draw(0..line.vertex_count, 0..1);
                    }
                    for overlays in self.dense_overlays.values() {
                        for line in overlays {
                            pass.set_vertex_buffer(0, line.vertices.slice(..));
                            pass.draw(0..line.vertex_count, 0..1);
                        }
                    }
                    for line in &self.draft_overlays {
                        pass.set_vertex_buffer(0, line.vertices.slice(..));
                        pass.draw(0..line.vertex_count, 0..1);
                    }
                } else if let Some(state) = self.saved_source_overlays.get(&pane.source.id) {
                    pass.set_pipeline(&self.overlay_fill_pipeline);
                    pass.set_bind_group(0, &self.overlay_bind_group, &[]);
                    for points in state.points.values() {
                        pass.set_vertex_buffer(0, points.vertices.slice(..));
                        pass.draw(0..points.vertex_count, 0..1);
                    }
                    for fill in &state.fills {
                        pass.set_vertex_buffer(0, fill.vertices.slice(..));
                        pass.draw(0..fill.vertex_count, 0..1);
                    }
                    pass.set_pipeline(&self.overlay_pipeline);
                    for line in &state.overlays {
                        pass.set_vertex_buffer(0, line.vertices.slice(..));
                        pass.draw(0..line.vertex_count, 0..1);
                    }
                    for overlays in state.dense.values() {
                        for line in overlays {
                            pass.set_vertex_buffer(0, line.vertices.slice(..));
                            pass.draw(0..line.vertex_count, 0..1);
                        }
                    }
                }
            }
            self.queue.submit(Some(encoder.finish()));
        }
        // Multi-view exports are captured after every pane and scientific
        // overlay has been rendered, before the egui interface is composed.
        let screenshot = if self.pending_screenshot.is_some() {
            let mut encoder = self
                .device
                .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                    label: Some("wsiTools native multi-view screenshot"),
                });
            let capture = self.begin_screenshot_capture(&mut encoder, &frame.texture);
            self.queue.submit(Some(encoder.finish()));
            capture
        } else {
            None
        };
        if let Some(frame_ui) = &egui_frame {
            let mut encoder = self
                .device
                .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                    label: Some("wsiTools native multi-view UI"),
                });
            for (id, image_delta) in &frame_ui.textures_delta.set {
                self.egui_renderer
                    .update_texture(&self.device, &self.queue, *id, image_delta);
            }
            let descriptor = egui_wgpu::ScreenDescriptor {
                size_in_pixels: [self.config.width, self.config.height],
                pixels_per_point: frame_ui.pixels_per_point,
            };
            self.egui_renderer.update_buffers(
                &self.device,
                &self.queue,
                &mut encoder,
                &frame_ui.primitives,
                &descriptor,
            );
            let mut pass = encoder
                .begin_render_pass(&wgpu::RenderPassDescriptor {
                    label: Some("wsiTools native multi-view UI pass"),
                    color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                        view: &view,
                        resolve_target: None,
                        ops: wgpu::Operations {
                            load: wgpu::LoadOp::Load,
                            store: wgpu::StoreOp::Store,
                        },
                    })],
                    depth_stencil_attachment: None,
                    timestamp_writes: None,
                    occlusion_query_set: None,
                })
                .forget_lifetime();
            self.egui_renderer
                .render(&mut pass, &frame_ui.primitives, &descriptor);
            self.queue.submit(Some(encoder.finish()));
            for id in &frame_ui.textures_delta.free {
                self.egui_renderer.free_texture(id);
            }
        }
        if let Some(screenshot) = screenshot {
            self.finish_screenshot_capture(screenshot)?;
        }
        frame.present();
        Ok(())
    }
    fn render_background(
        &mut self,
        color: wgpu::Color,
        egui_frame: Option<NativeEguiFrame>,
    ) -> Result<(), String> {
        let frame = self
            .surface
            .get_current_texture()
            .map_err(|error| format!("Could not acquire native viewer frame: {error}"))?;
        let view = frame
            .texture
            .create_view(&wgpu::TextureViewDescriptor::default());
        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("wsiTools native frame"),
            });
        let screen_descriptor = egui_frame.as_ref().map(|frame_ui| {
            for (id, image_delta) in &frame_ui.textures_delta.set {
                self.egui_renderer
                    .update_texture(&self.device, &self.queue, *id, image_delta);
            }
            let descriptor = egui_wgpu::ScreenDescriptor {
                size_in_pixels: [self.config.width, self.config.height],
                pixels_per_point: frame_ui.pixels_per_point,
            };
            self.egui_renderer.update_buffers(
                &self.device,
                &self.queue,
                &mut encoder,
                &frame_ui.primitives,
                &descriptor,
            );
            descriptor
        });
        {
            let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("wsiTools native tile pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &view,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(color),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
            });
            pass.set_pipeline(&self.pipeline);
            let desired_level = level_for_camera(&self.source, self.camera_state);
            let mut drawable_tiles = self
                .source_tiles
                .get(&self.source.id)
                .into_iter()
                .flat_map(|cache| cache.values.iter())
                .filter(|(key, _)| {
                    let visible = if self.image_transform == NativeImageTransform::default() {
                        tile_intersects_viewport(
                            &self.source,
                            **key,
                            self.camera_state,
                            self.config.width,
                            self.config.height,
                        )
                    } else {
                        tile_intersects_viewport_transformed(
                            &self.source,
                            self.image_transform,
                            **key,
                            self.camera_state,
                            self.config.width,
                            self.config.height,
                        )
                    };
                    key.level <= desired_level && visible
                })
                .collect::<Vec<_>>();
            // Coarser tiles draw first, retaining a useful image during tile
            // replacement; sharper tiles cover them as soon as they arrive.
            drawable_tiles.sort_by_key(|(key, _)| key.level);
            for (_, tile) in drawable_tiles {
                pass.set_bind_group(0, &tile.bind_group, &[]);
                pass.set_vertex_buffer(0, tile.vertices.slice(..));
                pass.draw(0..6, 0..1);
            }
            for (source_id, channel) in &self.channel_sources {
                let Some(style) = self.channel_styles.get(source_id) else {
                    continue;
                };
                if !style.visible || style.opacity <= 0.0 || !channel.matches_source(&self.source) {
                    continue;
                }
                let Some(cache) = self.channel_tiles.get(source_id) else {
                    continue;
                };
                let channel_source = channel.tile_source();
                let mut tiles = cache
                    .values
                    .iter()
                    .filter(|(key, _)| {
                        let visible = if self.image_transform == NativeImageTransform::default() {
                            tile_intersects_viewport(
                                &channel_source,
                                **key,
                                self.camera_state,
                                self.config.width,
                                self.config.height,
                            )
                        } else {
                            tile_intersects_viewport_transformed(
                                &channel_source,
                                self.image_transform,
                                **key,
                                self.camera_state,
                                self.config.width,
                                self.config.height,
                            )
                        };
                        key.level <= level_for_camera(&channel_source, self.camera_state) && visible
                    })
                    .collect::<Vec<_>>();
                tiles.sort_by_key(|(key, _)| key.level);
                for (_, tile) in tiles {
                    pass.set_bind_group(0, &tile.bind_group, &[]);
                    pass.set_vertex_buffer(0, tile.vertices.slice(..));
                    pass.draw(0..6, 0..1);
                }
            }
            pass.set_pipeline(&self.overlay_fill_pipeline);
            pass.set_bind_group(0, &self.overlay_bind_group, &[]);
            for points in self.point_overlays.values() {
                pass.set_vertex_buffer(0, points.vertices.slice(..));
                pass.draw(0..points.vertex_count, 0..1);
            }
            for fill in &self.overlay_fills {
                pass.set_vertex_buffer(0, fill.vertices.slice(..));
                pass.draw(0..fill.vertex_count, 0..1);
            }
            pass.set_pipeline(&self.overlay_pipeline);
            for line in &self.overlays {
                pass.set_vertex_buffer(0, line.vertices.slice(..));
                pass.draw(0..line.vertex_count, 0..1);
            }
            for overlays in self.dense_overlays.values() {
                for line in overlays {
                    pass.set_vertex_buffer(0, line.vertices.slice(..));
                    pass.draw(0..line.vertex_count, 0..1);
                }
            }
            for line in &self.draft_overlays {
                pass.set_vertex_buffer(0, line.vertices.slice(..));
                pass.draw(0..line.vertex_count, 0..1);
            }
        }
        // Capture before native panels are painted: exports contain the tissue
        // plus visible scientific overlays, not temporary menu chrome.
        let screenshot = self.begin_screenshot_capture(&mut encoder, &frame.texture);
        if let (Some(frame_ui), Some(screen_descriptor)) = (&egui_frame, screen_descriptor.as_ref())
        {
            let mut pass = encoder
                .begin_render_pass(&wgpu::RenderPassDescriptor {
                    label: Some("wsiTools native UI pass"),
                    color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                        view: &view,
                        resolve_target: None,
                        ops: wgpu::Operations {
                            load: wgpu::LoadOp::Load,
                            store: wgpu::StoreOp::Store,
                        },
                    })],
                    depth_stencil_attachment: None,
                    timestamp_writes: None,
                    occlusion_query_set: None,
                })
                .forget_lifetime();
            self.egui_renderer
                .render(&mut pass, &frame_ui.primitives, screen_descriptor);
        }
        self.queue.submit(Some(encoder.finish()));
        if let Some(screenshot) = screenshot {
            self.finish_screenshot_capture(screenshot)?;
        }
        if let Some(frame_ui) = &egui_frame {
            for id in &frame_ui.textures_delta.free {
                self.egui_renderer.free_texture(id);
            }
        }
        frame.present();
        Ok(())
    }
}

fn native_screenshot_rgba_bytes(
    mapped: &[u8],
    width: u32,
    height: u32,
    padded_bytes_per_row: u32,
    bgra: bool,
) -> Result<Vec<u8>, String> {
    let unpadded_bytes_per_row = width as usize * 4;
    let expected = padded_bytes_per_row as usize * height as usize;
    if mapped.len() < expected || (padded_bytes_per_row as usize) < unpadded_bytes_per_row {
        return Err("Screenshot GPU readback buffer has an invalid row layout.".to_string());
    }
    let mut rgba = vec![0_u8; unpadded_bytes_per_row * height as usize];
    for row in 0..height as usize {
        let source_start = row * padded_bytes_per_row as usize;
        let target_start = row * unpadded_bytes_per_row;
        rgba[target_start..target_start + unpadded_bytes_per_row]
            .copy_from_slice(&mapped[source_start..source_start + unpadded_bytes_per_row]);
    }
    if bgra {
        for pixel in rgba.chunks_exact_mut(4) {
            pixel.swap(0, 2);
        }
    }
    Ok(rgba)
}

/// Persist a native viewport capture according to the filename selected in
/// the platform file dialog. SVG deliberately embeds the rendered PNG: a
/// whole-slide capture contains GPU-composited tiles as well as annotations,
/// so claiming a fully vector screenshot would be misleading. The SVG still
/// scales cleanly in reports and preserves the exact visible scientific view.
fn native_save_screenshot(
    image: &image::RgbaImage,
    path: &std::path::Path,
) -> Result<(), String> {
    let extension = path
        .extension()
        .and_then(|extension| extension.to_str())
        .unwrap_or("png")
        .to_ascii_lowercase();
    match extension.as_str() {
        "jpg" | "jpeg" => image::DynamicImage::ImageRgba8(image.clone())
            .to_rgb8()
            .save_with_format(path, image::ImageFormat::Jpeg)
            .map_err(|error| format!("Could not save {}: {error}", path.display())),
        "svg" => {
            let mut png = Vec::new();
            image::DynamicImage::ImageRgba8(image.clone())
                .write_to(
                    &mut std::io::Cursor::new(&mut png),
                    image::ImageFormat::Png,
                )
                .map_err(|error| format!("Could not encode the SVG screenshot: {error}"))?;
            let encoded = BASE64_STANDARD.encode(png);
            let svg = format!(
                concat!(
                    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
                    "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" ",
                    "width=\"{}\" height=\"{}\" viewBox=\"0 0 {} {}\">\n",
                    "  <image width=\"{}\" height=\"{}\" xlink:href=\"data:image/png;base64,{}\"/>\n",
                    "</svg>\n"
                ),
                image.width(),
                image.height(),
                image.width(),
                image.height(),
                image.width(),
                image.height(),
                encoded
            );
            std::fs::write(path, svg)
                .map_err(|error| format!("Could not save {}: {error}", path.display()))
        }
        _ => image
            .save_with_format(path, image::ImageFormat::Png)
            .map_err(|error| format!("Could not save {}: {error}", path.display())),
    }
}

fn tile_vertices(source: &NativeTileSource, key: TileKey) -> [f32; 24] {
    let downsample = 2f64.powi((source.max_level - key.level) as i32);
    let span = source.tile_size as f64 * downsample;
    let left = key.column as f64 * span;
    let top = key.row as f64 * span;
    let right = (left + span).min(source.width);
    let bottom = (top + span).min(source.height);
    [
        left as f32,
        top as f32,
        0.0,
        0.0,
        right as f32,
        top as f32,
        1.0,
        0.0,
        right as f32,
        bottom as f32,
        1.0,
        1.0,
        left as f32,
        top as f32,
        0.0,
        0.0,
        right as f32,
        bottom as f32,
        1.0,
        1.0,
        left as f32,
        bottom as f32,
        0.0,
        1.0,
    ]
}

struct NativeOverlayLineData {
    vertices: Vec<f32>,
}

/// Triangle fans keep points geometrically circular at every zoom level. The
/// renderer owns only the visible viewport slice, not a permanent full-slide
/// point buffer.
fn native_point_circle_vertices(
    points: &[NativePointItem],
    maximum_points: usize,
    segments: usize,
    opacity: f32,
) -> Vec<f32> {
    let segments = segments.clamp(8, 32);
    let mut vertices = Vec::with_capacity(points.len().min(maximum_points) * segments * 18);
    for point in points.iter().take(maximum_points) {
        if !point.x.is_finite() || !point.y.is_finite() {
            continue;
        }
        let radius = point.radius.max(0.25);
        let mut colour = parse_hex_colour(&point.colour);
        colour[3] = opacity.clamp(0.0, 1.0);
        for segment in 0..segments {
            let a = std::f32::consts::TAU * segment as f32 / segments as f32;
            let b = std::f32::consts::TAU * (segment + 1) as f32 / segments as f32;
            vertices.extend_from_slice(&[
                point.x,
                point.y,
                colour[0],
                colour[1],
                colour[2],
                colour[3],
                point.x + radius * a.cos(),
                point.y + radius * a.sin(),
                colour[0],
                colour[1],
                colour[2],
                colour[3],
                point.x + radius * b.cos(),
                point.y + radius * b.sin(),
                colour[0],
                colour[1],
                colour[2],
                colour[3],
            ]);
        }
    }
    vertices
}

/// Converts a freehand centreline into a stable closed ribbon. It is purposely
/// sampled by slide-space distance, so a fast mouse movement does not create a
/// dense, jagged GeoJSON payload merely because the display refreshes quickly.
fn native_brush_outline(points: &[(f32, f32)], radius: f32) -> Vec<(f32, f32)> {
    let radius = radius.max(0.5);
    if points.is_empty() {
        return Vec::new();
    }
    if points.len() == 1 {
        return (0..16)
            .map(|index| {
                let angle = std::f32::consts::TAU * index as f32 / 16.0;
                (
                    points[0].0 + radius * angle.cos(),
                    points[0].1 + radius * angle.sin(),
                )
            })
            .collect();
    }
    let mut left = Vec::with_capacity(points.len());
    let mut right = Vec::with_capacity(points.len());
    for index in 0..points.len() {
        let previous = points[index.saturating_sub(1)];
        let next = points[(index + 1).min(points.len() - 1)];
        let dx = next.0 - previous.0;
        let dy = next.1 - previous.1;
        let length = dx.hypot(dy).max(1e-4);
        let nx = -dy / length * radius;
        let ny = dx / length * radius;
        left.push((points[index].0 + nx, points[index].1 + ny));
        right.push((points[index].0 - nx, points[index].1 - ny));
    }
    right.reverse();
    left.extend(right);
    left
}

/// Builds a flat-capped corridor around a trajectory backbone. Unlike a paint
/// brush this deliberately has no semicircular end caps, which makes the
/// biological width of a sampled trajectory explicit and reproducible.
fn native_trajectory_area_outline(points: &[(f32, f32)], width: f32) -> Vec<(f32, f32)> {
    let points = points
        .iter()
        .copied()
        .filter(|(x, y)| x.is_finite() && y.is_finite())
        .fold(Vec::new(), |mut kept, point| {
            if kept.last().is_none_or(|last: &(f32, f32)| {
                (last.0 - point.0).hypot(last.1 - point.1) > 1e-3
            }) {
                kept.push(point);
            }
            kept
        });
    if points.len() < 2 {
        return Vec::new();
    }
    let radius = (width * 0.5).max(0.5);
    let mut left = Vec::with_capacity(points.len());
    let mut right = Vec::with_capacity(points.len());
    for index in 0..points.len() {
        let previous = points[index.saturating_sub(1)];
        let next = points[(index + 1).min(points.len() - 1)];
        let dx = next.0 - previous.0;
        let dy = next.1 - previous.1;
        let length = dx.hypot(dy).max(1e-4);
        let nx = -dy / length * radius;
        let ny = dx / length * radius;
        left.push((points[index].0 + nx, points[index].1 + ny));
        right.push((points[index].0 - nx, points[index].1 - ny));
    }
    right.reverse();
    left.extend(right);
    left
}

/// Applies native paint using true polygon Boolean operations while retaining
/// the original feature id and class used by R-side analyses.
fn native_apply_brush_to_feature(
    feature: &mut serde_json::Value,
    outline: &[(f32, f32)],
    operation: NativeBrushOperation,
) -> bool {
    if outline.len() < 3 || operation == NativeBrushOperation::New {
        return false;
    }
    let brush = MultiPolygon(vec![Polygon::new(
        LineString::new(
            outline
                .iter()
                .map(|&(x, y)| Coord { x: x as f64, y: y as f64 })
                .chain(std::iter::once(Coord { x: outline[0].0 as f64, y: outline[0].1 as f64 }))
                .collect(),
        ),
        Vec::new(),
    )]);
    let Some(root) = feature.as_object_mut() else { return false; };
    let existing = root
        .get("geometry")
        .and_then(native_geojson_multi_polygon)
        .filter(|geometry| !geometry.0.is_empty());
    let Some(existing) = existing else { return false; };
    let result = match operation {
        NativeBrushOperation::Extend => existing.union(&brush),
        NativeBrushOperation::Subtract => existing.difference(&brush),
        NativeBrushOperation::New => return false,
    };
    if result.0.is_empty() {
        return false;
    }
    root.insert("geometry".to_string(), native_multi_polygon_geojson(&result));
    if let Some(properties) = root
        .entry("properties")
        .or_insert_with(|| serde_json::json!({}))
        .as_object_mut()
    {
        properties.insert("brush_edited".to_string(), serde_json::json!(true));
        properties.insert(
            "brush_operation".to_string(),
            serde_json::json!(match operation {
                NativeBrushOperation::Extend => "extend",
                NativeBrushOperation::Subtract => "subtract",
                NativeBrushOperation::New => "new",
            }),
        );
    }
    true
}

fn native_geojson_multi_polygon(geometry: &serde_json::Value) -> Option<MultiPolygon<f64>> {
    let kind = geometry.get("type")?.as_str()?;
    let polygons = geometry.get("coordinates")?.as_array()?;
    let raw_polygons = match kind {
        "Polygon" => vec![polygons.clone()],
        "MultiPolygon" => polygons.iter().filter_map(serde_json::Value::as_array).cloned().collect(),
        _ => return None,
    };
    let mut output = Vec::new();
    for raw_polygon in raw_polygons {
        let mut rings = raw_polygon.into_iter().filter_map(|ring| {
            let ring = ring.as_array()?;
            let mut points = ring.iter().filter_map(|point| {
                let point = point.as_array()?;
                Some(Coord { x: point.first()?.as_f64()?, y: point.get(1)?.as_f64()? })
            }).collect::<Vec<_>>();
            if points.len() < 3 { return None; }
            if points.first() != points.last() {
                points.push(*points.first()?);
            }
            Some(LineString::new(points))
        });
        let exterior = rings.next()?;
        output.push(Polygon::new(exterior, rings.collect()));
    }
    Some(MultiPolygon(output))
}

fn native_feature_bounds(feature: &serde_json::Value) -> Option<(f64, f64, f64, f64)> {
    let geometry = feature.get("geometry")?;
    let polygons = native_geojson_multi_polygon(geometry)?;
    let mut xmin = f64::INFINITY;
    let mut ymin = f64::INFINITY;
    let mut xmax = f64::NEG_INFINITY;
    let mut ymax = f64::NEG_INFINITY;
    for polygon in polygons.0 {
        for coordinate in polygon.exterior().coords() {
            xmin = xmin.min(coordinate.x);
            ymin = ymin.min(coordinate.y);
            xmax = xmax.max(coordinate.x);
            ymax = ymax.max(coordinate.y);
        }
    }
    if xmin.is_finite() && ymin.is_finite() && xmax > xmin && ymax > ymin {
        Some((xmin, ymin, xmax, ymax))
    } else {
        None
    }
}

fn native_multi_polygon_geojson(geometry: &MultiPolygon<f64>) -> serde_json::Value {
    let polygon_to_coords = |polygon: &Polygon<f64>| {
        std::iter::once(polygon.exterior())
            .chain(polygon.interiors().iter())
            .map(|ring| serde_json::Value::Array(ring.0.iter().map(|point| serde_json::json!([point.x, point.y])).collect()))
            .collect::<Vec<_>>()
    };
    let polygons = geometry.0.iter().map(|polygon| serde_json::Value::Array(polygon_to_coords(polygon))).collect::<Vec<_>>();
    serde_json::json!({ "type": "MultiPolygon", "coordinates": polygons })
}

fn native_path_length(points: &[(f32, f32)]) -> f32 {
    points
        .windows(2)
        .map(|pair| (pair[1].0 - pair[0].0).hypot(pair[1].1 - pair[0].1))
        .sum()
}

fn selected_annotation_id(state: &NativeRendererState) -> Option<String> {
    state
        .selected_roi
        .as_ref()
        .and_then(|feature| feature.get("id"))
        .and_then(json_id)
        .or_else(|| {
            state
                .selected_roi
                .as_ref()
                .and_then(|feature| feature.get("properties"))
                .and_then(|properties| properties.get("roi_id"))
                .and_then(json_id)
        })
}

fn annotation_feature_label(feature: &serde_json::Value) -> String {
    let properties = feature
        .get("properties")
        .unwrap_or(&serde_json::Value::Null);
    properties
        .get("classification")
        .and_then(|classification| classification.get("name"))
        .and_then(serde_json::Value::as_str)
        .or_else(|| properties.get("name").and_then(serde_json::Value::as_str))
        .or_else(|| properties.get("class").and_then(serde_json::Value::as_str))
        .filter(|value| !value.trim().is_empty())
        .unwrap_or("Annotation")
        .to_string()
}

fn annotation_feature_colour(feature: &serde_json::Value) -> String {
    let properties = feature.get("properties").unwrap_or(&serde_json::Value::Null);
    properties.get("colour").and_then(serde_json::Value::as_str)
        .or_else(|| properties.get("color").and_then(serde_json::Value::as_str))
        .or_else(|| properties.get("classification").and_then(|x| x.get("color")).and_then(serde_json::Value::as_str))
        .filter(|value| !value.trim().is_empty())
        .unwrap_or("#22C55E").to_string()
}

/// Combines same-class annotation parts into one editable GeoJSON MultiPolygon
/// with a true geometric union. It intentionally refuses mixed classes and
/// non-polygonal features.
fn native_merge_annotation_features(
    features: &[serde_json::Value],
) -> Result<serde_json::Value, String> {
    if features.len() < 2 {
        return Err("Select at least two ROIs to merge.".to_string());
    }
    let class_name = annotation_feature_label(&features[0]);
    if features
        .iter()
        .any(|feature| annotation_feature_label(feature) != class_name)
    {
        return Err("Only annotations with the same category can be merged.".to_string());
    }
    let mut combined = features[0]
        .get("geometry")
        .and_then(native_geojson_multi_polygon)
        .ok_or_else(|| "Selected ROI has invalid polygon geometry.".to_string())?;
    for feature in features.iter().skip(1) {
        let other = feature
            .get("geometry")
            .and_then(native_geojson_multi_polygon)
            .ok_or_else(|| "Only Polygon or MultiPolygon ROIs can be merged.".to_string())?;
        combined = combined.union(&other);
    }
    if combined.0.is_empty() { return Err("Selected ROIs contain no polygon parts.".to_string()); }
    let mut merged = features[0].clone();
    merged["geometry"] = native_multi_polygon_geojson(&combined);
    if let Some(properties) = merged.get_mut("properties").and_then(serde_json::Value::as_object_mut) {
        properties.insert("native_wgpu_merged".to_string(), serde_json::Value::Bool(true));
        properties.insert("native_wgpu_merged_count".to_string(), serde_json::json!(features.len()));
    }
    Ok(merged)
}

/// Splits a MultiPolygon ROI into individually editable features. The original
/// class and properties stay intact while each part receives a stable child id.
fn native_split_annotation_feature(
    feature: &serde_json::Value,
) -> Result<Vec<serde_json::Value>, String> {
    let geometry = feature
        .get("geometry")
        .and_then(native_geojson_multi_polygon)
        .ok_or_else(|| "Selected ROI has invalid polygon geometry.".to_string())?;
    if geometry.0.len() < 2 {
        return Err("Selected ROI is not multipart.".to_string());
    }
    let original_id = feature.get("id").and_then(json_id).or_else(|| {
        feature.get("properties")
            .and_then(|properties| properties.get("roi_id"))
            .and_then(json_id)
    }).ok_or_else(|| "Selected ROI has no stable identifier.".to_string())?;
    Ok(geometry.0.into_iter().enumerate().map(|(index, polygon)| {
        let mut part = feature.clone();
        let part_id = format!("{original_id}__part_{}", index + 1);
        part["id"] = serde_json::Value::String(part_id.clone());
        if let Some(properties) = part.get_mut("properties").and_then(serde_json::Value::as_object_mut) {
            properties.insert("roi_id".to_string(), serde_json::Value::String(part_id));
            properties.insert("native_wgpu_split_from".to_string(), serde_json::Value::String(original_id.clone()));
        }
        part["geometry"] = native_multi_polygon_geojson(&MultiPolygon(vec![polygon]));
        part
    }).collect())
}

fn native_polygon_outer_ring(feature: &serde_json::Value) -> Vec<(f32, f32)> {
    geojson_rings(feature.get("geometry").unwrap_or(&serde_json::Value::Null))
        .into_iter()
        .next()
        .unwrap_or_default()
}

fn native_nearest_polygon_vertex(
    feature: &serde_json::Value,
    renderer: &NativeWindowRenderer,
    cursor: (f64, f64),
    rect: NativePaneRect,
    tolerance_px: f64,
) -> Option<usize> {
    let ring = native_polygon_outer_ring(feature);
    if ring.len() < 3 { return None; }
    let mut best = None;
    let mut distance = tolerance_px;
    // A closed GeoJSON ring repeats its first point; editing either endpoint
    // updates both, so expose only one drag handle for that corner.
    let limit = if ring.first() == ring.last() { ring.len().saturating_sub(1) } else { ring.len() };
    for (index, (x, y)) in ring.iter().take(limit).enumerate() {
        let display = renderer.image_transform.map_point(&renderer.source, (*x as f64, *y as f64));
        let screen = renderer.camera_state.screen_at(display.0, display.1, rect.width, rect.height);
        let candidate = (screen.0 - cursor.0).hypot(screen.1 - cursor.1);
        if candidate <= distance {
            distance = candidate;
            best = Some(index);
        }
    }
    best
}

fn native_nearest_polygon_segment(
    feature: &serde_json::Value,
    renderer: &NativeWindowRenderer,
    cursor: (f64, f64),
    rect: NativePaneRect,
    tolerance_px: f64,
) -> Option<usize> {
    let ring = native_polygon_outer_ring(feature);
    native_nearest_polygon_segment_in_ring(&ring, renderer, cursor, rect, tolerance_px)
}

fn native_nearest_polygon_segment_in_ring(
    ring: &[(f32, f32)],
    renderer: &NativeWindowRenderer,
    cursor: (f64, f64),
    rect: NativePaneRect,
    tolerance_px: f64,
) -> Option<usize> {
    let limit = if ring.len() > 3 && ring.first() == ring.last() {
        ring.len().saturating_sub(1)
    } else {
        ring.len()
    };
    if limit < 3 {
        return None;
    }
    let mut nearest = None;
    let mut best = tolerance_px;
    for index in 0..limit {
        let a = renderer.image_transform.map_point(
            &renderer.source,
            (ring[index].0 as f64, ring[index].1 as f64),
        );
        let b = renderer.image_transform.map_point(
            &renderer.source,
            (ring[(index + 1) % limit].0 as f64, ring[(index + 1) % limit].1 as f64),
        );
        let a = renderer.camera_state.screen_at(a.0, a.1, rect.width, rect.height);
        let b = renderer.camera_state.screen_at(b.0, b.1, rect.width, rect.height);
        let distance = native_point_segment_distance(cursor, a, b);
        if distance <= best {
            best = distance;
            nearest = Some(index);
        }
    }
    nearest
}

fn native_nearest_polygon_segment_by_slide_point(
    ring: &[(f32, f32)],
    point: (f32, f32),
) -> Option<usize> {
    let limit = if ring.len() > 3 && ring.first() == ring.last() {
        ring.len().saturating_sub(1)
    } else {
        ring.len()
    };
    if limit < 3 {
        return None;
    }
    (0..limit).min_by(|&left, &right| {
        native_point_segment_distance_f32(point, ring[left], ring[(left + 1) % limit])
            .total_cmp(&native_point_segment_distance_f32(
                point,
                ring[right],
                ring[(right + 1) % limit],
            ))
    })
}

fn native_point_segment_distance(point: (f64, f64), a: (f64, f64), b: (f64, f64)) -> f64 {
    let dx = b.0 - a.0;
    let dy = b.1 - a.1;
    let length_squared = dx * dx + dy * dy;
    if length_squared <= f64::EPSILON {
        return (point.0 - a.0).hypot(point.1 - a.1);
    }
    let t = (((point.0 - a.0) * dx + (point.1 - a.1) * dy) / length_squared).clamp(0.0, 1.0);
    (point.0 - (a.0 + t * dx)).hypot(point.1 - (a.1 + t * dy))
}

fn native_point_segment_distance_f32(point: (f32, f32), a: (f32, f32), b: (f32, f32)) -> f32 {
    native_point_segment_distance(
        (point.0 as f64, point.1 as f64),
        (a.0 as f64, a.1 as f64),
        (b.0 as f64, b.1 as f64),
    ) as f32
}

fn native_set_polygon_vertex(
    feature: &mut serde_json::Value,
    index: usize,
    x: f32,
    y: f32,
) -> bool {
    let Some(geometry) = feature.get_mut("geometry").and_then(serde_json::Value::as_object_mut) else { return false; };
    if geometry.get("type").and_then(serde_json::Value::as_str) != Some("Polygon") { return false; }
    let Some(rings) = geometry.get_mut("coordinates").and_then(serde_json::Value::as_array_mut) else { return false; };
    let Some(ring) = rings.first_mut().and_then(serde_json::Value::as_array_mut) else { return false; };
    if index >= ring.len() || !ring[index].is_array() { return false; }
    let closing = ring.len() > 2 && ring.first() == ring.last();
    ring[index] = serde_json::json!([x, y]);
    if closing && index == 0 {
        let last = ring.len() - 1;
        ring[last] = serde_json::json!([x, y]);
    }
    true
}

/// Smoothly displaces a local arc of a polygon ring. The original ring is kept
/// for the whole drag, avoiding cumulative distortion while the pointer moves.
fn native_soft_move_polygon_vertex(
    feature: &mut serde_json::Value,
    original_ring: &[(f32, f32)],
    vertex_index: usize,
    original_point: (f32, f32),
    target: (f32, f32),
    span: usize,
) -> bool {
    let closed = original_ring.len() > 3 && original_ring.first() == original_ring.last();
    let limit = if closed {
        original_ring.len().saturating_sub(1)
    } else {
        original_ring.len()
    };
    if limit < 3 || vertex_index >= limit {
        return false;
    }
    let span = span.max(1).min(limit.saturating_div(2).max(1));
    let dx = target.0 - original_point.0;
    let dy = target.1 - original_point.1;
    let mut ring = original_ring[..limit].to_vec();
    for (index, point) in ring.iter_mut().enumerate() {
        let direct = index.abs_diff(vertex_index);
        let distance = if closed { direct.min(limit - direct) } else { direct };
        if distance > span {
            continue;
        }
        let weight = if distance == 0 {
            1.0
        } else {
            0.5 * (1.0 + (std::f32::consts::PI * distance as f32 / span as f32).cos())
        };
        point.0 += dx * weight;
        point.1 += dy * weight;
    }
    if closed {
        ring.push(ring[0]);
    }
    let Some(geometry) = feature
        .get_mut("geometry")
        .and_then(serde_json::Value::as_object_mut)
    else {
        return false;
    };
    if geometry.get("type").and_then(serde_json::Value::as_str) != Some("Polygon") {
        return false;
    }
    let Some(rings) = geometry
        .get_mut("coordinates")
        .and_then(serde_json::Value::as_array_mut)
    else {
        return false;
    };
    let Some(first_ring) = rings.first_mut() else {
        return false;
    };
    *first_ring = serde_json::Value::Array(
        ring.into_iter()
            .map(|(x, y)| serde_json::json!([x, y]))
            .collect(),
    );
    true
}

fn native_replace_polygon_arc_with_smooth_curve(
    feature: &mut serde_json::Value,
    original_ring: &[(f32, f32)],
    start_after: usize,
    end_after: usize,
    stroke: &[(f32, f32)],
) -> bool {
    let closed = original_ring.len() > 3 && original_ring.first() == original_ring.last();
    let limit = if closed {
        original_ring.len().saturating_sub(1)
    } else {
        original_ring.len()
    };
    if limit < 3 || start_after >= limit || end_after >= limit || stroke.len() < 2 {
        return false;
    }
    let forward = (end_after + limit - start_after) % limit;
    let backward = (start_after + limit - end_after) % limit;
    if forward == 0 || backward == 0 {
        return false;
    }
    let open = &original_ring[..limit];
    let ring = if forward <= backward {
        let mut curve = Vec::with_capacity(stroke.len() + 2);
        curve.push(open[start_after]);
        curve.extend_from_slice(stroke);
        curve.push(open[(end_after + 1) % limit]);
        native_replace_ring_forward_arc(open, start_after, end_after, &native_smooth_open_curve(&curve))
    } else {
        let mut curve = Vec::with_capacity(stroke.len() + 2);
        curve.push(open[end_after]);
        curve.extend(stroke.iter().rev().copied());
        curve.push(open[(start_after + 1) % limit]);
        native_replace_ring_forward_arc(open, end_after, start_after, &native_smooth_open_curve(&curve))
    };
    let Some(mut ring) = ring else { return false; };
    if closed {
        ring.push(ring[0]);
    }
    let Some(geometry) = feature
        .get_mut("geometry")
        .and_then(serde_json::Value::as_object_mut)
    else { return false; };
    let Some(rings) = geometry
        .get_mut("coordinates")
        .and_then(serde_json::Value::as_array_mut)
    else { return false; };
    let Some(first_ring) = rings.first_mut() else { return false; };
    *first_ring = serde_json::Value::Array(
        ring.into_iter()
            .map(|(x, y)| serde_json::json!([x, y]))
            .collect(),
    );
    true
}

fn native_replace_ring_forward_arc(
    ring: &[(f32, f32)],
    start_after: usize,
    end_after: usize,
    curve: &[(f32, f32)],
) -> Option<Vec<(f32, f32)>> {
    let limit = ring.len();
    if limit < 3 || curve.len() < 2 {
        return None;
    }
    let end_offset = (end_after + 1 + limit - start_after) % limit;
    if end_offset == 0 || end_offset >= limit {
        return None;
    }
    let mut output = Vec::with_capacity(limit + curve.len());
    output.push(ring[start_after]);
    output.extend(curve.iter().skip(1).take(curve.len().saturating_sub(2)).copied());
    output.extend((end_offset..limit).map(|offset| ring[(start_after + offset) % limit]));
    (output.len() >= 3).then_some(output)
}

fn native_smooth_open_curve(points: &[(f32, f32)]) -> Vec<(f32, f32)> {
    if points.len() < 3 {
        return points.to_vec();
    }
    let mut current = points.to_vec();
    for _ in 0..2 {
        let mut next = Vec::with_capacity(current.len() * 2);
        next.push(current[0]);
        for pair in current.windows(2) {
            next.push((pair[0].0 * 0.75 + pair[1].0 * 0.25, pair[0].1 * 0.75 + pair[1].1 * 0.25));
            next.push((pair[0].0 * 0.25 + pair[1].0 * 0.75, pair[0].1 * 0.25 + pair[1].1 * 0.75));
        }
        next.push(*current.last().unwrap());
        current = native_dedupe_open_points(&next, 0.75);
    }
    current
}

fn native_dedupe_open_points(points: &[(f32, f32)], minimum_distance: f32) -> Vec<(f32, f32)> {
    points.iter().copied().fold(Vec::new(), |mut kept, point| {
        if kept.last().is_none_or(|last: &(f32, f32)| {
            (last.0 - point.0).hypot(last.1 - point.1) >= minimum_distance
        }) {
            kept.push(point);
        }
        kept
    })
}

fn trajectory_item_id(item: &serde_json::Value) -> Option<String> {
    item.get("id")
        .and_then(json_id)
        .or_else(|| item.get("trajectory_id").and_then(json_id))
}

fn trajectory_item_points(item: &serde_json::Value) -> Vec<(f32, f32)> {
    item.get("points")
        .or_else(|| item.get("control_points"))
        .and_then(serde_json::Value::as_array)
        .map(|points| {
            points
                .iter()
                .filter_map(|point| {
                    let x = point.get("x").and_then(serde_json::Value::as_f64)? as f32;
                    let y = point.get("y").and_then(serde_json::Value::as_f64)? as f32;
                    (x.is_finite() && y.is_finite()).then_some((x, y))
                })
                .collect()
        })
        .unwrap_or_default()
}

fn trajectory_area_trajectory_id(feature: &serde_json::Value) -> Option<String> {
    let properties = feature.get("properties")?;
    properties
        .get("trajectory_id")
        .and_then(json_id)
        .or_else(|| {
            properties
                .get("wsiToolsTrajectory")
                .and_then(|item| item.get("id"))
                .and_then(json_id)
        })
}

fn trajectory_item_label(item: &serde_json::Value) -> String {
    item.get("name")
        .or_else(|| item.get("label"))
        .and_then(serde_json::Value::as_str)
        .filter(|value| !value.trim().is_empty())
        .unwrap_or("Trajectory")
        .to_string()
}

fn annotation_shapes_from_state(state: &NativeRendererState) -> Vec<NativeAnnotationShape> {
    let features = state
        .annotations
        .get("features")
        .and_then(serde_json::Value::as_array)
        .cloned()
        .unwrap_or_default();
    features
        .into_iter()
        .filter_map(|feature| {
            let properties = feature
                .get("properties")
                .unwrap_or(&serde_json::Value::Null);
            if properties
                .get("visible")
                .and_then(serde_json::Value::as_bool)
                == Some(false)
            {
                return None;
            }
            let rings = geojson_rings(feature.get("geometry")?);
            (!rings.is_empty()).then_some(NativeAnnotationShape { feature, rings })
        })
        .collect()
}

fn segmentation_shapes_from_state(state: &NativeRendererState) -> Vec<NativeAnnotationShape> {
    let features = state
        .segmentation
        .get("features")
        .and_then(serde_json::Value::as_array)
        .cloned()
        .unwrap_or_default();
    features
        .into_iter()
        .filter_map(|mut feature| {
            let properties = feature
                .as_object_mut()
                .map(|object| object.entry("properties").or_insert_with(|| serde_json::json!({})))?;
            if let Some(properties) = properties.as_object_mut() {
                properties.entry("colour").or_insert_with(|| serde_json::Value::String("#E879F9".to_string()));
                properties.entry("visible").or_insert(serde_json::Value::Bool(true));
                properties.insert("native_read_only_segmentation".to_string(), serde_json::Value::Bool(true));
            }
            let rings = geojson_rings(feature.get("geometry")?);
            (!rings.is_empty()).then_some(NativeAnnotationShape { feature, rings })
        })
        .collect()
}

fn overlay_lines_from_shapes(
    state: &NativeRendererState,
    shapes: &[NativeAnnotationShape],
    maximum_vertices: usize,
) -> Vec<NativeOverlayLineData> {
    let selected_id = selected_annotation_id(state);
    let mut output = Vec::new();
    let mut used = 0usize;
    for shape in shapes {
        let properties = shape
            .feature
            .get("properties")
            .unwrap_or(&serde_json::Value::Null);
        let id = shape
            .feature
            .get("id")
            .and_then(json_id)
            .or_else(|| properties.get("roi_id").and_then(json_id));
        let selected = id.is_some() && id == selected_id;
        let colour = if selected {
            [1.0, 0.95, 0.25, 1.0]
        } else {
            parse_hex_colour(
                properties
                    .get("colour")
                    .or_else(|| properties.get("color"))
                    .and_then(serde_json::Value::as_str)
                    .unwrap_or("#22C55E"),
            )
        };
        for ring in &shape.rings {
            if ring.len() < 2 || used + ring.len() > maximum_vertices {
                continue;
            }
            let mut vertices = Vec::with_capacity(ring.len() * 6);
            for &(x, y) in ring {
                vertices.extend_from_slice(&[x, y, colour[0], colour[1], colour[2], colour[3]]);
            }
            used += vertices.len() / 6;
            output.push(NativeOverlayLineData { vertices });
        }
        if used >= maximum_vertices {
            break;
        }
    }
    output
}

fn overlay_lines_from_state(
    state: &NativeRendererState,
    maximum_vertices: usize,
) -> Vec<NativeOverlayLineData> {
    let shapes = annotation_shapes_from_state(state);
    overlay_lines_from_shapes(state, &shapes, maximum_vertices)
}

fn overlay_fills_from_shapes(
    state: &NativeRendererState,
    shapes: &[NativeAnnotationShape],
    maximum_vertices: usize,
) -> Vec<NativeOverlayLineData> {
    let selected_id = selected_annotation_id(state);
    let mut output = Vec::new();
    let mut used = 0usize;
    for shape in shapes {
        let properties = shape
            .feature
            .get("properties")
            .unwrap_or(&serde_json::Value::Null);
        let id = shape
            .feature
            .get("id")
            .and_then(json_id)
            .or_else(|| properties.get("roi_id").and_then(json_id));
        let selected = id.is_some() && id == selected_id;
        let mut colour = parse_hex_colour(
            properties
                .get("colour")
                .or_else(|| properties.get("color"))
                .and_then(serde_json::Value::as_str)
                .unwrap_or("#22C55E"),
        );
        colour[3] = if selected { 0.32 } else { 0.16 };
        let mut vertices_2d = Vec::<f64>::new();
        let mut holes = Vec::<usize>::new();
        let mut point_count = 0usize;
        for (ring_index, ring) in shape.rings.iter().enumerate() {
            let ring = without_closed_endpoint(ring);
            if ring.len() < 3 {
                continue;
            }
            if ring_index > 0 {
                holes.push(point_count);
            }
            point_count += ring.len();
            for &(x, y) in &ring {
                vertices_2d.push(x as f64);
                vertices_2d.push(y as f64);
            }
        }
        if point_count < 3 {
            continue;
        }
        let Ok(indices) = earcutr::earcut(&vertices_2d, &holes, 2) else {
            continue;
        };
        if indices.len() + used > maximum_vertices {
            continue;
        }
        let mut vertices = Vec::with_capacity(indices.len() * 6);
        for index in indices {
            let offset = index * 2;
            if offset + 1 >= vertices_2d.len() {
                continue;
            }
            vertices.extend_from_slice(&[
                vertices_2d[offset] as f32,
                vertices_2d[offset + 1] as f32,
                colour[0],
                colour[1],
                colour[2],
                colour[3],
            ]);
        }
        used += vertices.len() / 6;
        output.push(NativeOverlayLineData { vertices });
        if used >= maximum_vertices {
            break;
        }
    }
    output
}

fn without_closed_endpoint(ring: &[(f32, f32)]) -> Vec<(f32, f32)> {
    let mut output = ring.to_vec();
    if output.len() > 2 && output.first() == output.last() {
        output.pop();
    }
    output
}

fn trajectory_lines_from_state(
    state: &NativeRendererState,
    maximum_vertices: usize,
) -> Vec<NativeOverlayLineData> {
    let Some(trajectories) = state.trajectories.as_array() else {
        return Vec::new();
    };
    let mut output = Vec::new();
    let mut used = 0usize;
    for trajectory in trajectories {
        let points = trajectory
            .get("points")
            .or_else(|| trajectory.get("control_points"))
            .and_then(serde_json::Value::as_array)
            .map(|values| {
                values
                    .iter()
                    .filter_map(|point| {
                        let x = point.get("x")?.as_f64()? as f32;
                        let y = point.get("y")?.as_f64()? as f32;
                        (x.is_finite() && y.is_finite()).then_some((x, y))
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        if points.len() < 2 || used + points.len() > maximum_vertices {
            continue;
        }
        let mut vertices = Vec::with_capacity(points.len() * 6);
        for (x, y) in points {
            // Orange differentiates analytical trajectories from class ROIs.
            vertices.extend_from_slice(&[x, y, 0.98, 0.45, 0.09, 1.0]);
        }
        used += vertices.len() / 6;
        output.push(NativeOverlayLineData { vertices });
        if used >= maximum_vertices {
            break;
        }
    }
    output
}

fn measurement_lines_from_state(state: &NativeRendererState) -> Vec<NativeOverlayLineData> {
    let Some(measurements) = state.measurements.as_array() else { return Vec::new(); };
    measurements.iter().filter_map(|measurement| {
        let x0 = measurement.get("start_x")?.as_f64()? as f32;
        let y0 = measurement.get("start_y")?.as_f64()? as f32;
        let x1 = measurement.get("end_x")?.as_f64()? as f32;
        let y1 = measurement.get("end_y")?.as_f64()? as f32;
        (x0.is_finite() && y0.is_finite() && x1.is_finite() && y1.is_finite()).then_some(
            NativeOverlayLineData { vertices: vec![x0, y0, 0.20, 0.75, 1.0, 1.0, x1, y1, 0.20, 0.75, 1.0, 1.0] }
        )
    }).collect()
}

fn dense_lines_from_items(
    items: &[serde_json::Value],
    maximum_vertices: usize,
) -> Vec<NativeOverlayLineData> {
    let mut output = Vec::new();
    let mut used = 0usize;
    for item in items {
        let colour = parse_hex_colour(
            item.get("colour")
                .or_else(|| item.get("color"))
                .and_then(serde_json::Value::as_str)
                .unwrap_or("#F97316"),
        );
        let mut rings = dense_item_rings(item.get("rings").unwrap_or(&serde_json::Value::Null));
        rings.extend(dense_item_rings(
            item.get("add_groups").unwrap_or(&serde_json::Value::Null),
        ));
        for ring in rings {
            if ring.len() < 2 || used + ring.len() > maximum_vertices {
                continue;
            }
            let mut vertices = Vec::with_capacity(ring.len() * 6);
            for (x, y) in ring {
                vertices.extend_from_slice(&[x, y, colour[0], colour[1], colour[2], 0.95]);
            }
            used += vertices.len() / 6;
            output.push(NativeOverlayLineData { vertices });
        }
        if used >= maximum_vertices {
            break;
        }
    }
    output
}

fn dense_item_rings(value: &serde_json::Value) -> Vec<Vec<(f32, f32)>> {
    let Some(values) = value.as_array() else {
        return Vec::new();
    };
    let points = values
        .iter()
        .filter_map(|point| {
            let x = point.get("x")?.as_f64()? as f32;
            let y = point.get("y")?.as_f64()? as f32;
            (x.is_finite() && y.is_finite()).then_some((x, y))
        })
        .collect::<Vec<_>>();
    if points.len() == values.len() && points.len() >= 2 {
        return vec![close_ring(points)];
    }
    values.iter().flat_map(dense_item_rings).collect()
}

fn close_ring(mut ring: Vec<(f32, f32)>) -> Vec<(f32, f32)> {
    if ring.len() > 2 && ring.first() != ring.last() {
        ring.push(ring[0]);
    }
    ring
}

fn point_in_annotation(point: (f64, f64), shape: &NativeAnnotationShape) -> bool {
    let Some(outer_ring) = shape.rings.first() else {
        return false;
    };
    point_in_ring(point, outer_ring)
        && !shape
            .rings
            .iter()
            .skip(1)
            .any(|hole| point_in_ring(point, hole))
}

fn point_in_ring(point: (f64, f64), ring: &[(f32, f32)]) -> bool {
    if ring.len() < 3 {
        return false;
    }
    let (x, y) = point;
    let mut inside = false;
    for index in 0..ring.len() {
        let (x1, y1) = ring[index];
        let (x2, y2) = ring[(index + 1) % ring.len()];
        let (x1, y1, x2, y2) = (x1 as f64, y1 as f64, x2 as f64, y2 as f64);
        let crosses = (y1 > y) != (y2 > y);
        if crosses {
            let crossing_x = (x2 - x1) * (y - y1) / (y2 - y1) + x1;
            if x < crossing_x {
                inside = !inside;
            }
        }
    }
    inside
}

fn json_id(value: &serde_json::Value) -> Option<String> {
    value
        .as_str()
        .map(ToString::to_string)
        .or_else(|| value.as_i64().map(|number| number.to_string()))
        .or_else(|| value.as_u64().map(|number| number.to_string()))
}

fn parse_hex_colour(value: &str) -> [f32; 4] {
    let text = value.trim().trim_start_matches('#');
    if text.len() == 6 {
        let component = |range: std::ops::Range<usize>| {
            u8::from_str_radix(&text[range], 16)
                .ok()
                .map(|component| component as f32 / 255.0)
        };
        if let (Some(red), Some(green), Some(blue)) =
            (component(0..2), component(2..4), component(4..6))
        {
            return [red, green, blue, 1.0];
        }
    }
    [0.13, 0.77, 0.37, 1.0]
}

fn native_geojson_features(value: serde_json::Value) -> Vec<serde_json::Value> {
    match value.get("type").and_then(serde_json::Value::as_str) {
        Some("FeatureCollection") => value
            .get("features")
            .and_then(serde_json::Value::as_array)
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .filter(|feature| feature.get("geometry").is_some())
            .collect(),
        Some("Feature") if value.get("geometry").is_some() => vec![value],
        _ if value.get("geometry").is_some() => vec![serde_json::json!({
            "type": "Feature",
            "properties": value.get("properties").cloned().unwrap_or_else(|| serde_json::json!({})),
            "geometry": value.get("geometry").cloned().unwrap_or(serde_json::Value::Null)
        })],
        _ => Vec::new(),
    }
}

fn native_file_stem(value: &str) -> String {
    let compact = value.chars().fold(String::new(), |mut output, character| {
        if character.is_ascii_alphanumeric() {
            output.push(character);
        } else if !output.ends_with('_') {
            output.push('_');
        }
        output
    });
    let stem = compact.trim_matches('_').to_string();
    if stem.is_empty() { "wsiTools".to_string() } else { stem }
}

fn geojson_rings(geometry: &serde_json::Value) -> Vec<Vec<(f32, f32)>> {
    let geometry_type = geometry
        .get("type")
        .and_then(serde_json::Value::as_str)
        .unwrap_or_default();
    let coordinates = geometry
        .get("coordinates")
        .unwrap_or(&serde_json::Value::Null);
    let rings = match geometry_type {
        "Polygon" => coordinates
            .as_array()
            .map(|rings| rings.iter().collect::<Vec<_>>())
            .unwrap_or_default(),
        "MultiPolygon" => coordinates
            .as_array()
            .map(|polygons| {
                polygons
                    .iter()
                    .flat_map(|polygon| polygon.as_array().into_iter().flatten())
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default(),
        _ => Vec::new(),
    };
    rings
        .into_iter()
        .filter_map(|ring| {
            let mut points = ring
                .as_array()?
                .iter()
                .filter_map(|point| {
                    let values = point.as_array()?;
                    let x = values.first()?.as_f64()? as f32;
                    let y = values.get(1)?.as_f64()? as f32;
                    (x.is_finite() && y.is_finite()).then_some((x, y))
                })
                .collect::<Vec<_>>();
            if points.len() > 2 && points.first() != points.last() {
                points.push(points[0]);
            }
            (points.len() > 1).then_some(points)
        })
        .collect()
}

const NATIVE_TILE_SHADER: &str = r#"
struct In { @location(0) pos: vec2<f32>, @location(1) uv: vec2<f32> };
struct Out { @builtin(position) pos: vec4<f32>, @location(0) uv: vec2<f32> };
struct Camera {
  center: vec2<f32>,
  pixels_per_screen_pixel: f32,
  rotation: f32,
  viewport: vec2<f32>,
  flip: vec2<f32>,
  source_dimensions: vec2<f32>,
  display_dimensions: vec2<f32>,
  _pad1: vec4<f32>,
};
@group(0) @binding(2) var<uniform> camera: Camera;
fn transformed_position(pos: vec2<f32>) -> vec2<f32> {
  var out = pos;
  if (camera.rotation == 90.0) { out = vec2<f32>(camera.source_dimensions.y - pos.y, pos.x); }
  if (camera.rotation == 180.0) { out = vec2<f32>(camera.source_dimensions.x - pos.x, camera.source_dimensions.y - pos.y); }
  if (camera.rotation == 270.0) { out = vec2<f32>(pos.y, camera.source_dimensions.x - pos.x); }
  if (camera.flip.x > 0.5) { out.x = camera.display_dimensions.x - out.x; }
  if (camera.flip.y > 0.5) { out.y = camera.display_dimensions.y - out.y; }
  return out;
}
@vertex fn vs_main(v: In) -> Out {
  var o: Out;
  let relative = (transformed_position(v.pos) - camera.center) / (camera.pixels_per_screen_pixel * camera.viewport * 0.5);
  o.pos = vec4<f32>(relative.x, -relative.y, 0.0, 1.0);
  o.uv = v.uv;
  return o;
}
@group(0) @binding(0) var tile_tex: texture_2d<f32>;
@group(0) @binding(1) var tile_sampler: sampler;
struct TileStyle {
  colour_opacity: vec4<f32>,
  gain: f32,
  contrast_min: f32,
  contrast_max: f32,
  channel_mode: f32,
  stain_mode: f32,
};
@group(0) @binding(3) var<uniform> style: TileStyle;
@fragment fn fs_main(v: Out) -> @location(0) vec4<f32> {
  let sampled = textureSample(tile_tex, tile_sampler, v.uv);
  if (style.channel_mode < 0.5) {
    if (style.stain_mode < 0.5) { return sampled; }
    // Fast brightfield stain display derived from the RGB tile on the GPU.
    // This is intentionally local to the displayed tile: no whole-slide RGB
    // pixels are copied into R or retained by the native renderer.
    let od = -log(max(sampled.rgb, vec3<f32>(0.003)));
    let h = clamp(dot(od, vec3<f32>(0.650, 0.704, 0.286)) / 1.6, 0.0, 1.0);
    let e = clamp(dot(od, vec3<f32>(0.072, 0.990, 0.105)) / 1.6, 0.0, 1.0);
    let r = clamp(dot(od, vec3<f32>(0.268, 0.570, 0.776)) / 1.6, 0.0, 1.0);
    if (style.stain_mode < 1.5) {
      return vec4<f32>(vec3<f32>(1.0) - h * vec3<f32>(0.72, 0.47, 0.04), sampled.a);
    }
    if (style.stain_mode < 2.5) {
      return vec4<f32>(vec3<f32>(1.0) - e * vec3<f32>(0.06, 0.60, 0.45), sampled.a);
    }
    return vec4<f32>(vec3<f32>(1.0 - r), sampled.a);
  }
  let signal = dot(sampled.rgb, vec3<f32>(0.2126, 0.7152, 0.0722));
  let normalized = clamp((signal * style.gain - style.contrast_min) / max(0.00001, style.contrast_max - style.contrast_min), 0.0, 1.0);
  return vec4<f32>(style.colour_opacity.rgb * normalized, sampled.a * normalized * style.colour_opacity.a);
}
"#;

const NATIVE_OVERLAY_SHADER: &str = r#"
struct In { @location(0) pos: vec2<f32>, @location(1) color: vec4<f32> };
struct Out { @builtin(position) pos: vec4<f32>, @location(0) color: vec4<f32> };
struct Camera {
  center: vec2<f32>,
  pixels_per_screen_pixel: f32,
  rotation: f32,
  viewport: vec2<f32>,
  flip: vec2<f32>,
  source_dimensions: vec2<f32>,
  display_dimensions: vec2<f32>,
  _pad1: vec4<f32>,
};
@group(0) @binding(0) var<uniform> camera: Camera;
fn transformed_position(pos: vec2<f32>) -> vec2<f32> {
  var out = pos;
  if (camera.rotation == 90.0) { out = vec2<f32>(camera.source_dimensions.y - pos.y, pos.x); }
  if (camera.rotation == 180.0) { out = vec2<f32>(camera.source_dimensions.x - pos.x, camera.source_dimensions.y - pos.y); }
  if (camera.rotation == 270.0) { out = vec2<f32>(pos.y, camera.source_dimensions.x - pos.x); }
  if (camera.flip.x > 0.5) { out.x = camera.display_dimensions.x - out.x; }
  if (camera.flip.y > 0.5) { out.y = camera.display_dimensions.y - out.y; }
  return out;
}
@vertex fn vs_main(v: In) -> Out {
  var o: Out;
  let relative = (transformed_position(v.pos) - camera.center) / (camera.pixels_per_screen_pixel * camera.viewport * 0.5);
  o.pos = vec4<f32>(relative.x, -relative.y, 0.0, 1.0);
  o.color = v.color;
  return o;
}
@fragment fn fs_main(v: Out) -> @location(0) vec4<f32> { return v.color; }
"#;

fn fetch_native_tile(
    endpoint: &NativeTileEndpoint,
    source: &NativeTileSource,
    key: TileKey,
) -> Result<(u32, u32, Vec<u8>), String> {
    let url = endpoint.url(source, key);
    let mut bytes = Vec::new();
    ureq::get(&url)
        .timeout(std::time::Duration::from_secs(30))
        .call()
        .map_err(|error| format!("Could not fetch native tile `{url}`: {error}"))?
        .into_reader()
        .read_to_end(&mut bytes)
        .map_err(|error| format!("Could not read native tile `{url}`: {error}"))?;
    let rgba = image::load_from_memory(&bytes)
        .map_err(|error| format!("Could not decode native tile `{url}`: {error}"))?
        .to_rgba8();
    Ok((rgba.width(), rgba.height(), rgba.into_raw()))
}

/// A native surface deliberately shares Tauri's OS window handle. It is the
/// rendering primitive used by the next native UI phase; the Tauri webview
/// remains the compatibility shell until every interactive tool has a native
/// control implementation.
pub struct WgpuSurfaceRenderer<R: Runtime> {
    _window: WebviewWindow<R>,
    _instance: wgpu::Instance,
    surface: wgpu::Surface<'static>,
    device: wgpu::Device,
    queue: wgpu::Queue,
    config: wgpu::SurfaceConfiguration,
}

impl<R: Runtime> WgpuSurfaceRenderer<R> {
    pub fn attach(window: WebviewWindow<R>) -> Result<Self, String> {
        pollster::block_on(async move {
            let instance = wgpu::Instance::default();
            let surface = instance.create_surface(window.clone()).map_err(|error| {
                format!("Could not attach WGPU to the Tauri viewer window: {error}")
            })?;
            let adapter = instance
                .request_adapter(&wgpu::RequestAdapterOptions {
                    power_preference: wgpu::PowerPreference::HighPerformance,
                    force_fallback_adapter: false,
                    compatible_surface: Some(&surface),
                })
                .await
                .ok_or_else(|| "No compatible WGPU adapter was available.".to_string())?;
            let (device, queue) = adapter
                .request_device(
                    &wgpu::DeviceDescriptor {
                        label: Some("wsiTools native renderer"),
                        required_features: wgpu::Features::empty(),
                        required_limits: adapter.limits(),
                        memory_hints: wgpu::MemoryHints::Performance,
                    },
                    None,
                )
                .await
                .map_err(|error| format!("Could not create WGPU device: {error}"))?;
            let size = window
                .inner_size()
                .map_err(|error| format!("Could not obtain native viewer size: {error}"))?;
            let caps = surface.get_capabilities(&adapter);
            let format = caps
                .formats
                .iter()
                .copied()
                .find(|format| format.is_srgb())
                .or_else(|| caps.formats.first().copied())
                .ok_or_else(|| {
                    "The selected WGPU surface has no supported texture format.".to_string()
                })?;
            let present_mode = caps
                .present_modes
                .iter()
                .copied()
                .find(|mode| *mode == wgpu::PresentMode::AutoVsync)
                .or_else(|| caps.present_modes.first().copied())
                .ok_or_else(|| {
                    "The selected WGPU surface has no supported present mode.".to_string()
                })?;
            let config = wgpu::SurfaceConfiguration {
                usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
                format,
                width: size.width.max(1),
                height: size.height.max(1),
                present_mode,
                alpha_mode: caps
                    .alpha_modes
                    .first()
                    .copied()
                    .unwrap_or(wgpu::CompositeAlphaMode::Auto),
                view_formats: vec![],
                desired_maximum_frame_latency: 2,
            };
            surface.configure(&device, &config);
            Ok(Self {
                _window: window,
                _instance: instance,
                surface,
                device,
                queue,
                config,
            })
        })
    }

    pub fn resize(&mut self, width: u32, height: u32) {
        if width == 0 || height == 0 {
            return;
        }
        self.config.width = width;
        self.config.height = height;
        self.surface.configure(&self.device, &self.config);
    }

    pub fn render_background(&self, color: wgpu::Color) -> Result<(), String> {
        let frame = self
            .surface
            .get_current_texture()
            .map_err(|error| format!("Could not acquire WGPU render frame: {error}"))?;
        let view = frame
            .texture
            .create_view(&wgpu::TextureViewDescriptor::default());
        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("wsiTools native render frame"),
            });
        {
            let _pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("wsiTools native tile pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &view,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(color),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
            });
        }
        self.queue.submit(Some(encoder.finish()));
        frame.present();
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn source() -> NativeTileSource {
        NativeTileSource {
            id: "slide".to_string(),
            width: 8192.0,
            height: 4096.0,
            tile_size: 512,
            tile_overlap: 0,
            tile_format: "jpg".to_string(),
            min_level: 0,
            max_level: 13,
            label: "Slide".to_string(),
            mpp: serde_json::Value::Null,
            objective_power: None,
        }
    }

    #[test]
    fn manifest_url_preserves_local_authority() {
        assert_eq!(
            native_manifest_url("http://127.0.0.1:8788/viewer-state").unwrap(),
            "http://127.0.0.1:8788/native-renderer"
        );
    }

    #[test]
    fn overview_request_preserves_single_tile_preview_detail() {
        let mut slide = source();
        slide.min_level = 3;
        let key = native_overview_tile(&slide);
        assert_eq!(key.level, 9);
        assert_eq!(key.column, 0);
        assert_eq!(key.row, 0);
    }

    #[test]
    fn camera_screen_and_slide_coordinates_round_trip() {
        let camera = NativeCamera {
            center_x: 1200.0,
            center_y: 800.0,
            pixels_per_screen_pixel: 2.5,
        };
        let screen = camera.screen_at(1475.0, 925.0, 1600, 1000);
        let slide = camera.slide_at(screen.0, screen.1, 1600, 1000);
        assert!((slide.0 - 1475.0).abs() < 1e-8);
        assert!((slide.1 - 925.0).abs() < 1e-8);
    }

    #[test]
    fn spatial_gene_payload_maps_unpacked_and_packed_ids() {
        let unpacked = native_gene_response_from_json(serde_json::json!({
            "gene": "Mbp", "points": [{"id": "a", "barcode": "b", "colour": "#123456"}]
        })).unwrap();
        assert_eq!(unpacked.colours.get("a").unwrap(), "#123456");
        assert_eq!(unpacked.colours.get("b").unwrap(), "#123456");
        let packed = native_gene_response_from_json(serde_json::json!({
            "gene": "Mbp", "range": {"min": 0, "max": 10},
            "packed_points": {"keys": ["c"], "values": [5]}
        })).unwrap();
        assert!(packed.colours.get("c").unwrap().starts_with('#'));
    }

    #[test]
    fn measurement_snapshot_creates_a_native_line() {
        let state: NativeRendererState = serde_json::from_value(serde_json::json!({
            "protocol": "wsiTools-native-renderer-state/v1",
            "measurements": [{"start_x": 1, "start_y": 2, "end_x": 4, "end_y": 6}]
        })).unwrap();
        let lines = measurement_lines_from_state(&state);
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].vertices.len(), 12);
    }

    #[test]
    fn segmentation_snapshot_is_a_read_only_coloured_overlay() {
        let state: NativeRendererState = serde_json::from_value(serde_json::json!({
            "protocol": "wsiTools-native-renderer-state/v1",
            "segmentation": {"type":"FeatureCollection","features":[{
                "type":"Feature", "id":"cell_1", "properties":{},
                "geometry":{"type":"Polygon","coordinates":[[[1,1],[4,1],[4,4],[1,1]]]}
            }]}
        })).unwrap();
        let shapes = segmentation_shapes_from_state(&state);
        assert_eq!(shapes.len(), 1);
        assert_eq!(annotation_feature_colour(&shapes[0].feature), "#E879F9");
        assert_eq!(
            shapes[0].feature.pointer("/properties/native_read_only_segmentation").and_then(serde_json::Value::as_bool),
            Some(true)
        );
    }

    #[test]
    fn tile_endpoint_uses_the_route_advertised_by_r() {
        let mut manifest = NativeRendererManifest {
            protocol: NATIVE_RENDERER_PROTOCOL.to_string(),
            tile_route: "/session-tiles".to_string(),
            state_route: "/viewer-state".to_string(),
            state_snapshot_route: "/native-renderer-state".to_string(),
            dense_geojson_route: "/dense-geojson".to_string(),
            native_points_route: "/native-points".to_string(),
            spatial_gene_route: "/seurat-gene".to_string(),
            prediction_route: "/prediction".to_string(),
            prediction_enabled: false,
            prediction: serde_json::Value::Null,
            proximity_route: "/proximity".to_string(),
            image_export_route: "/image-export".to_string(),
            proximity_enabled: false,
            segmentation_run_url: String::new(),
            spatial_clusters: serde_json::Value::Null,
            spatial: serde_json::Value::Null,
            cellphenotyper: serde_json::Value::Null,
            source_count: 1,
            sources: vec![source()],
            dense_sources: Vec::new(),
            channel_sources: Vec::new(),
            point_sources: Vec::new(),
        };
        let key = TileKey {
            level: 9,
            column: 3,
            row: 7,
        };
        assert_eq!(
            tile_url(
                "http://127.0.0.1:8788/",
                &manifest,
                &manifest.sources[0],
                key
            ),
            "http://127.0.0.1:8788/session-tiles/slide/9/3/7.jpg"
        );
        manifest.tile_route = "tiles".to_string();
        let endpoint = NativeTileEndpoint::from_live_viewer(
            "http://127.0.0.1:8788/viewer-state?session=abc",
            &manifest,
        )
        .unwrap();
        assert_eq!(
            endpoint.url(&manifest.sources[0], key),
            "http://127.0.0.1:8788/tiles/slide/9/3/7.jpg"
        );
        assert_eq!(
            endpoint.state_snapshot_url().unwrap(),
            "http://127.0.0.1:8788/native-renderer-state"
        );
        assert_eq!(
            endpoint.native_points_url().unwrap(),
            "http://127.0.0.1:8788/native-points"
        );
    }

    #[test]
    fn viewport_points_become_bounded_circle_triangles() {
        let points = vec![
            NativePointItem {
                x: 100.0,
                y: 200.0,
                radius: 4.0,
                colour: "#38bdf8".to_string(),
                id: "one".to_string(),
                cluster_values: HashMap::new(),
            },
            NativePointItem {
                x: 300.0,
                y: 400.0,
                radius: 5.0,
                colour: "#f97316".to_string(),
                id: "two".to_string(),
                cluster_values: HashMap::new(),
            },
        ];
        let vertices = native_point_circle_vertices(&points, 1, 12, 1.0);
        // One point, twelve triangles, three vertices, six floats per vertex.
        assert_eq!(vertices.len(), 12 * 3 * 6);
        assert_eq!(vertices[0], 100.0);
        assert_eq!(vertices[1], 200.0);
    }

    #[test]
    fn native_cluster_fields_preserve_r_palette() {
        let fields = native_cluster_fields(&serde_json::json!({
            "fields": [{"field":"cluster", "label":"Cluster", "levels":[
                {"value":"Tumour", "colour":"#dc2626"}
            ]}]
        }));
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].0, "cluster");
        assert_eq!(fields[0].2.get("Tumour"), Some(&"#dc2626".to_string()));
    }

    #[test]
    fn native_reduction_plots_keep_only_selectable_two_dimensional_points() {
        let plots = native_spatial_reduction_plots(&serde_json::json!({
            "plots": [{
                "id": "pca", "label": "PCA",
                "points": [
                    {"label": "cell-a", "x": -1.5, "y": 2.5, "colour": "#22c55e"},
                    {"label": "broken", "x": "not-a-number", "y": 2}
                ]
            }]
        }));
        assert_eq!(plots.len(), 1);
        assert_eq!(plots[0].id, "pca");
        assert_eq!(plots[0].points.len(), 1);
        assert_eq!(plots[0].points[0].label, "cell-a");
    }

    #[test]
    fn native_grandqc_menu_keeps_only_addressable_project_files() {
        let items = native_grandqc_items(&serde_json::json!({
            "grandqc": {
                "enabled": true,
                "geojsons": [
                    {"id": "folds", "label": "Fold candidates", "path": "/tmp/folds.geojson"},
                    {"id": "missing-path", "label": "Ignore me"}
                ]
            }
        }));
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].id, "folds");
        assert_eq!(items[0].payload().get("path").and_then(serde_json::Value::as_str), Some("/tmp/folds.geojson"));
    }

    #[test]
    fn native_kodama_menu_keeps_registration_without_embedding_full_geojson() {
        let items = native_kodama_items(&serde_json::json!({
            "kodama": {
                "enabled": true,
                "geojsons": [{
                    "id": "medsam-fine",
                    "label": "Fine MedSAM refinement",
                    "profile": "fine",
                    "path": "/tmp/refinement.geojson",
                    "shift_dx": 12.5,
                    "shift_dy": -9.0,
                    "geojson": { "features": [{ "large": "browser-only" }] }
                }, { "id": "missing-path" }]
            }
        }));
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].id, "medsam-fine");
        assert_eq!(items[0].shift_dx, 12.5);
        assert_eq!(items[0].shift_dy, -9.0);
        let payload = items[0].payload();
        assert_eq!(payload.get("path").and_then(serde_json::Value::as_str), Some("/tmp/refinement.geojson"));
        assert!(payload.get("geojson").is_none());
    }

    #[test]
    fn brush_outline_is_closed_ribbon_with_bounded_geometry() {
        let path = vec![(10.0, 10.0), (30.0, 10.0), (50.0, 25.0)];
        let outline = native_brush_outline(&path, 8.0);
        assert_eq!(outline.len(), path.len() * 2);
        assert!(outline.iter().all(|(x, y)| x.is_finite() && y.is_finite()));
        assert!(native_path_length(&path) > 0.0);
    }

    #[test]
    fn trajectory_area_outline_uses_flat_caps() {
        let outline = native_trajectory_area_outline(&[(0.0, 0.0), (20.0, 0.0)], 10.0);
        assert_eq!(outline, vec![(0.0, 5.0), (20.0, 5.0), (20.0, -5.0), (0.0, -5.0)]);
    }

    #[test]
    fn trajectory_area_metadata_preserves_its_backbone_id() {
        let feature = serde_json::json!({
            "type": "Feature",
            "properties": {
                "trajectory_area": true,
                "wsiToolsTrajectory": { "id": "trajectory-7", "flat_caps": true }
            }
        });
        assert_eq!(
            trajectory_area_trajectory_id(&feature).as_deref(),
            Some("trajectory-7")
        );
    }

    #[test]
    fn soft_vertex_edit_preserves_closed_ring_and_smooths_neighbours() {
        let mut feature = serde_json::json!({
            "type": "Feature",
            "geometry": {"type": "Polygon", "coordinates": [[
                [0.0, 0.0], [10.0, 0.0], [20.0, 0.0], [20.0, 10.0], [0.0, 0.0]
            ]]}
        });
        let original = native_polygon_outer_ring(&feature);
        assert!(native_soft_move_polygon_vertex(
            &mut feature,
            &original,
            1,
            (10.0, 0.0),
            (10.0, 8.0),
            2,
        ));
        let moved = native_polygon_outer_ring(&feature);
        assert_eq!(moved.first(), moved.last());
        assert_eq!(moved[1], (10.0, 8.0));
        assert!(moved[0].1 > 0.0);
        assert!(moved[2].1 > 0.0);
    }

    #[test]
    fn curve_edit_replaces_a_closed_polygon_arc_with_a_smooth_ring() {
        let mut feature = serde_json::json!({
            "type": "Feature",
            "geometry": {"type": "Polygon", "coordinates": [[
                [0.0, 0.0], [20.0, 0.0], [20.0, 20.0], [0.0, 20.0], [0.0, 0.0]
            ]]}
        });
        let original = native_polygon_outer_ring(&feature);
        assert!(native_replace_polygon_arc_with_smooth_curve(
            &mut feature,
            &original,
            0,
            1,
            &[(5.0, -8.0), (15.0, -8.0)],
        ));
        let edited = native_polygon_outer_ring(&feature);
        assert_eq!(edited.first(), edited.last());
        assert!(edited.len() > original.len());
        assert!(edited.iter().any(|(_, y)| *y < 0.0));
    }

    #[test]
    fn brush_edits_keep_the_roi_id_and_apply_boolean_geometry() {
        let base = serde_json::json!({
            "type": "Feature", "id": "roi-1", "properties": {"classification": {"name": "Tumour"}},
            "geometry": {"type": "Polygon", "coordinates": [[
                [0.0, 0.0], [20.0, 0.0], [20.0, 20.0], [0.0, 20.0], [0.0, 0.0]
            ]]}
        });
        let outline = vec![(4.0, 4.0), (8.0, 4.0), (8.0, 8.0), (4.0, 8.0)];
        let mut added = base.clone();
        assert!(native_apply_brush_to_feature(
            &mut added,
            &outline,
            NativeBrushOperation::Extend
        ));
        assert_eq!(added.get("id").and_then(json_id).as_deref(), Some("roi-1"));
        assert_eq!(added.pointer("/geometry/type").and_then(serde_json::Value::as_str), Some("MultiPolygon"));
        assert_eq!(
            added.pointer("/geometry/coordinates").and_then(serde_json::Value::as_array).map(Vec::len),
            Some(1),
            "a brush fully inside the ROI must not create a duplicate polygon"
        );
        let mut subtracted = base;
        assert!(native_apply_brush_to_feature(
            &mut subtracted,
            &outline,
            NativeBrushOperation::Subtract
        ));
        assert_eq!(
            subtracted.pointer("/geometry/coordinates/0").and_then(serde_json::Value::as_array).map(Vec::len),
            Some(2)
        );
        assert_eq!(
            subtracted.pointer("/properties/brush_operation").and_then(serde_json::Value::as_str),
            Some("subtract")
        );
    }

    #[test]
    fn same_class_roi_merge_preserves_identity_and_polygon_parts() {
        let first = serde_json::json!({
            "type": "Feature", "id": "roi-1",
            "properties": {"classification": {"name": "Tumour"}},
            "geometry": {"type": "Polygon", "coordinates": [[[0,0],[2,0],[2,2],[0,2],[0,0]]]}
        });
        let second = serde_json::json!({
            "type": "Feature", "id": "roi-2",
            "properties": {"classification": {"name": "Tumour"}},
            "geometry": {"type": "Polygon", "coordinates": [[[5,0],[7,0],[7,2],[5,2],[5,0]]]}
        });
        let merged = native_merge_annotation_features(&[first, second]).unwrap();
        assert_eq!(merged.get("id").and_then(json_id).as_deref(), Some("roi-1"));
        assert_eq!(merged.pointer("/geometry/type").and_then(serde_json::Value::as_str), Some("MultiPolygon"));
        assert_eq!(merged.pointer("/geometry/coordinates").and_then(serde_json::Value::as_array).map(Vec::len), Some(2));
        assert_eq!(merged.pointer("/properties/native_wgpu_merged_count").and_then(serde_json::Value::as_u64), Some(2));
    }

    #[test]
    fn roi_merge_rejects_mixed_annotation_categories() {
        let tumour = serde_json::json!({
            "type": "Feature", "properties": {"classification": {"name": "Tumour"}},
            "geometry": {"type": "Polygon", "coordinates": [[[0,0],[1,0],[1,1],[0,0]]]}
        });
        let stroma = serde_json::json!({
            "type": "Feature", "properties": {"classification": {"name": "Stroma"}},
            "geometry": {"type": "Polygon", "coordinates": [[[2,0],[3,0],[3,1],[2,0]]]}
        });
        assert!(native_merge_annotation_features(&[tumour, stroma]).is_err());
    }

    #[test]
    fn multipart_roi_splits_to_individual_editable_features() {
        let feature = serde_json::json!({
            "type": "Feature", "id": "roi-1",
            "properties": {"classification": {"name": "Tumour"}},
            "geometry": {"type": "MultiPolygon", "coordinates": [
                [[[0,0],[2,0],[2,2],[0,2],[0,0]]],
                [[[5,0],[7,0],[7,2],[5,2],[5,0]]]
            ]}
        });
        let parts = native_split_annotation_feature(&feature).unwrap();
        assert_eq!(parts.len(), 2);
        assert_eq!(parts[0].get("id").and_then(json_id).as_deref(), Some("roi-1__part_1"));
        assert_eq!(parts[1].get("id").and_then(json_id).as_deref(), Some("roi-1__part_2"));
        assert_eq!(parts[0].pointer("/geometry/type").and_then(serde_json::Value::as_str), Some("MultiPolygon"));
        assert_eq!(parts[0].pointer("/geometry/coordinates").and_then(serde_json::Value::as_array).map(Vec::len), Some(1));
    }

    #[test]
    fn manifest_accepts_dynamic_channel_layers() {
        let manifest: NativeRendererManifest = serde_json::from_str(
            r##"{
              "protocol":"wsiTools-native-renderer/v1",
              "tile_route":"/tiles",
              "state_route":"/viewer-state",
              "state_snapshot_route":"/native-renderer-state",
              "spatial":{"enabled":true,"source_name":"Seurat","plots":[{"id":"seurat_pca","label":"Seurat PCA plot","points":[{"label":"spot-a","spot_id":"spot-a","x":-1,"y":2,"slide_x":101,"slide_y":202,"colour":"#22c55e"}]}]},
              "cellphenotyper":{"enabled":true,"kodama":{"plots":[{"id":"kodama_fine","label":"KODAMA Fine plot","points":[{"label":"cell-a","x":1,"y":2,"colour":"#e41a1c"}]}]}},
              "source_count":1,
              "sources":[{"id":"slide","width":8192,"height":4096,"tile_size":512,"tile_overlap":0,"tile_format":"jpg","min_level":0,"max_level":13,"label":"H&E"}],
              "channel_sources":[{"id":"hemo","width":8192,"height":4096,"tile_size":512,"tile_overlap":0,"tile_format":"png","min_level":0,"max_level":13,"label":"Hematoxylin","source_type":"dynamic","visible":true,"opacity":0.75,"colour":"#276FBF","gain":1.2,"contrast_min":0.05,"contrast_max":0.9}]
            }"##,
        )
        .unwrap();
        manifest.validate().unwrap();
        assert_eq!(manifest.channel_sources.len(), 1);
        let channel = &manifest.channel_sources[0];
        assert_eq!(channel.tile_source().tile_format, "png");
        assert_eq!(channel.opacity, 0.75);
        assert!(channel.matches_source(&manifest.sources[0]));
        assert_eq!(manifest.spatial.pointer("/plots/0/points/0/spot_id").and_then(serde_json::Value::as_str), Some("spot-a"));
        assert_eq!(manifest.cellphenotyper.pointer("/kodama/plots/0/id").and_then(serde_json::Value::as_str), Some("kodama_fine"));
    }

    #[test]
    fn channel_targeting_hides_unrelated_slide_layers() {
        let mut channel = NativeChannelSource {
            id: "mihc".to_string(),
            width: 8192.0,
            height: 4096.0,
            tile_size: 512,
            tile_overlap: 0,
            tile_format: "png".to_string(),
            min_level: 0,
            max_level: 13,
            label: "mIHC".to_string(),
            source_type: "dynamic".to_string(),
            visible: true,
            opacity: 1.0,
            colour: "#ffffff".to_string(),
            gain: 1.0,
            contrast_min: 0.0,
            contrast_max: 1.0,
            target_ids: vec!["other-slide".to_string()],
        };
        assert!(!channel.matches_source(&source()));
        channel.target_ids.clear();
        assert!(channel.matches_source(&source()));
    }

    #[test]
    fn channel_settings_accept_row_and_column_payloads() {
        let rows = serde_json::json!([
            {"id":"hemo","visible":false,"opacity":0.4,"colour":"#123456","gain":1.5,"contrast_min":0.1,"contrast_max":0.8}
        ]);
        let row_patches = native_channel_settings_rows(&rows);
        assert_eq!(row_patches.len(), 1);
        assert_eq!(row_patches[0].id, "hemo");
        assert_eq!(row_patches[0].visible, Some(false));
        assert_eq!(row_patches[0].opacity, Some(0.4));

        let columns = serde_json::json!({
            "id":["eosin"], "visible":[true], "opacity":[0.85],
            "colour":["#EE6677"], "gain":[0.9], "contrast_min":[0.2], "contrast_max":[0.95]
        });
        let column_patches = native_channel_settings_rows(&columns);
        assert_eq!(column_patches.len(), 1);
        assert_eq!(column_patches[0].id, "eosin");
        assert_eq!(column_patches[0].visible, Some(true));
        assert_eq!(column_patches[0].colour.as_deref(), Some("#EE6677"));
    }

    #[test]
    fn fit_camera_requests_tiles() {
        let source = source();
        let tiles = visible_tiles(&source, NativeCamera::fit(&source, 1200, 800), 1200, 800);
        assert!(!tiles.is_empty());
        assert!(tiles.iter().all(|key| key.level <= source.max_level));
    }

    #[test]
    fn metadata_drives_native_scale_and_magnification() {
        let mut slide = source();
        slide.mpp = serde_json::json!({ "x": 0.5, "y": 0.5 });
        assert_eq!(native_source_mpp(&slide), Some(0.5));
        assert_eq!(native_base_magnification(&slide), 20.0);
        assert_eq!(native_nice_scale_length(742.0), 500.0);
        assert_eq!(native_scale_label(500.0), "500 um");
        assert_eq!(native_scale_label(2000.0), "2 mm");
    }

    #[test]
    fn request_plan_is_bounded_and_prefers_the_camera_center() {
        let source = source();
        let camera = NativeCamera::fit(&source, 1200, 800);
        let plan = native_tile_request_plan(&source, camera, 1200, 800, 3);
        assert!(!plan.is_empty());
        assert!(plan.len() <= 3);
        assert!(plan
            .iter()
            .all(|key| key.level == level_for_camera(&source, camera)));
    }

    #[test]
    fn image_transform_round_trips_points_and_swaps_dimensions() {
        let source = source();
        let point = (1270.5, 891.25);
        for rotation in [0, 90, 180, 270] {
            for flip_x in [false, true] {
                for flip_y in [false, true] {
                    let transform = NativeImageTransform {
                        rotation,
                        flip_x,
                        flip_y,
                    };
                    let displayed = transform.map_point(&source, point);
                    let restored = transform.inverse_map_point(&source, displayed);
                    assert!((restored.0 - point.0).abs() < 1e-8);
                    assert!((restored.1 - point.1).abs() < 1e-8);
                    let dimensions = transform.display_dimensions(&source);
                    if rotation == 90 || rotation == 270 {
                        assert_eq!(dimensions, (source.height, source.width));
                    } else {
                        assert_eq!(dimensions, (source.width, source.height));
                    }
                }
            }
        }
    }

    #[test]
    fn transformed_tile_request_plan_stays_bounded() {
        let source = source();
        let transform = NativeImageTransform {
            rotation: 90,
            flip_x: true,
            flip_y: false,
        };
        let (width, height) = transform.display_dimensions(&source);
        let camera = NativeCamera::fit_dimensions(width, height, 1200, 800);
        let plan = native_tile_request_plan_transformed(&source, transform, camera, 1200, 800, 5);
        assert!(!plan.is_empty());
        assert!(plan.len() <= 5);
        assert!(plan
            .iter()
            .all(|key| key.level == level_for_camera(&source, camera)));
    }

    #[test]
    fn cache_evicts_oldest_tile_at_capacity() {
        let mut cache = NativeTileCache::new(2);
        let first = TileKey {
            level: 3,
            column: 0,
            row: 0,
        };
        let second = TileKey {
            level: 3,
            column: 1,
            row: 0,
        };
        let third = TileKey {
            level: 3,
            column: 2,
            row: 0,
        };
        cache.insert(first, 1u8);
        cache.insert(second, 2u8);
        cache.insert(third, 3u8);
        assert_eq!(cache.len(), 2);
        assert!(!cache.contains(&first));
        assert!(cache.contains(&second));
        assert!(cache.contains(&third));
    }

    #[test]
    fn multi_view_layouts_cover_the_image_area_without_overlapping() {
        for layout in 1_usize..=12 {
            let panes = native_pane_rects(layout, 1200, 900);
            assert_eq!(panes.len(), layout);
            assert!(panes.iter().all(|pane| pane.width > 0 && pane.height > 0));
            for (index, left) in panes.iter().enumerate() {
                for right in panes.iter().skip(index + 1) {
                    let separated = left.x + left.width <= right.x
                        || right.x + right.width <= left.x
                        || left.y + left.height <= right.y
                        || right.y + right.height <= left.y;
                    assert!(separated, "panes must not overlap");
                }
            }
        }
    }

    #[test]
    fn multi_view_rejects_duplicate_sources_in_other_panes() {
        let camera = NativeCamera::fit(&source(), 800, 600);
        let panes = vec![
            NativeViewPane {
                source_index: 0,
                camera,
                transform: NativeImageTransform::default(),
            },
            NativeViewPane {
                source_index: usize::MAX,
                camera,
                transform: NativeImageTransform::default(),
            },
        ];
        assert!(native_source_is_assigned_elsewhere(&panes, 1, 0));
        assert!(!native_source_is_assigned_elsewhere(&panes, 0, 0));
        assert!(!native_source_is_assigned_elsewhere(&panes, 1, 1));
    }

    #[test]
    fn native_table_rows_accept_r_column_oriented_data_frames() {
        let rows = native_json_table_rows(&serde_json::json!({
            "rank": [1, 2],
            "feature": ["MMP11", "NNMT"],
            "correlation": [-0.61, 0.42]
        }));
        assert_eq!(rows.len(), 2);
        assert_eq!(rows[0]["feature"], "MMP11");
        assert_eq!(rows[1]["rank"], 2);
    }

    #[test]
    fn roi_snapshot_becomes_closed_gpu_outline_geometry() {
        let snapshot: NativeRendererState = serde_json::from_str(
            r##"{
              "protocol":"wsiTools-native-renderer-state/v1",
              "event":"roi_created",
              "annotations_total":1,
              "annotations":{"type":"FeatureCollection","features":[{
                "type":"Feature","id":"roi-1",
                "properties":{"colour":"#FF0000","visible":true},
                "geometry":{"type":"Polygon","coordinates":[[[10,20],[30,20],[30,40],[10,40]]]}
              }]},
              "selected_roi":{"type":"Feature","id":"roi-1"}
            }"##,
        )
        .unwrap();
        let lines = overlay_lines_from_state(&snapshot, 100);
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].vertices.len() / 6, 5);
        // Selected native outlines are highlighted independently from their
        // stored class colour so selection remains visible over H&E tiles.
        assert_eq!(&lines[0].vertices[2..6], &[1.0, 0.95, 0.25, 1.0]);
    }

    #[test]
    fn native_geojson_import_accepts_feature_collections_and_single_features() {
        let collection = serde_json::json!({
            "type": "FeatureCollection",
            "features": [{
                "type": "Feature",
                "id": "roi-a",
                "properties": {"class": "Tumour"},
                "geometry": {"type": "Polygon", "coordinates": [[[0,0],[10,0],[10,10],[0,0]]]}
            }]
        });
        assert_eq!(native_geojson_features(collection).len(), 1);
        let feature = serde_json::json!({
            "type": "Feature",
            "geometry": {"type": "Point", "coordinates": [2, 3]}
        });
        assert_eq!(native_geojson_features(feature).len(), 1);
        assert_eq!(native_file_stem("A slide / section"), "A_slide_section");
    }

    #[test]
    fn native_state_accepts_live_dense_source_updates() {
        let snapshot: NativeRendererState = serde_json::from_str(
            r##"{
              "protocol":"wsiTools-native-renderer-state/v1",
              "dense_sources":[{
                "id":"dense_cells","name":"Cells","source_type":"cell_segmentation",
                "visible":true,"min_zoom":1,"colour":"#F97316"
              }]
            }"##,
        )
        .unwrap();
        assert_eq!(snapshot.dense_sources.len(), 1);
        assert_eq!(snapshot.dense_sources[0].id, "dense_cells");
        assert!(snapshot.dense_sources[0].visible);
    }

    #[test]
    fn native_polygon_vertex_edit_preserves_a_closed_ring() {
        let mut feature = serde_json::json!({
            "type":"Feature",
            "geometry":{"type":"Polygon","coordinates":[[[1,1],[4,1],[4,4],[1,1]]]}
        });
        assert!(native_set_polygon_vertex(&mut feature, 0, 2.0, 3.0));
        let ring = native_polygon_outer_ring(&feature);
        assert_eq!(ring.first(), Some(&(2.0, 3.0)));
        assert_eq!(ring.last(), Some(&(2.0, 3.0)));
    }

    #[test]
    fn roi_snapshot_becomes_translucent_gpu_fill_with_hole() {
        let snapshot: NativeRendererState = serde_json::from_str(
            r##"{
              "protocol":"wsiTools-native-renderer-state/v1",
              "annotations":{"type":"FeatureCollection","features":[{
                "type":"Feature","id":"roi-1",
                "properties":{"colour":"#FF0000","visible":true},
                "geometry":{"type":"Polygon","coordinates":[
                  [[0,0],[40,0],[40,40],[0,40],[0,0]],
                  [[10,10],[30,10],[30,30],[10,30],[10,10]]
                ]}
              }]}
            }"##,
        )
        .unwrap();
        let shapes = annotation_shapes_from_state(&snapshot);
        let fills = overlay_fills_from_shapes(&snapshot, &shapes, 200);
        assert_eq!(fills.len(), 1);
        assert!(fills[0].vertices.len() >= 18);
        assert_eq!(fills[0].vertices.len() % 18, 0);
        assert!(fills[0].vertices.chunks_exact(6).all(|vertex| {
            vertex[0].is_finite()
                && vertex[1].is_finite()
                && vertex[2] == 1.0
                && vertex[3] == 0.0
                && vertex[4] == 0.0
                && vertex[5] > 0.0
                && vertex[5] < 1.0
        }));
    }

    #[test]
    fn annotation_hit_testing_respects_outer_ring_and_holes() {
        let feature = serde_json::json!({
            "type": "Feature",
            "id": "roi-1",
            "geometry": {
                "type": "Polygon",
                "coordinates": [
                    [[0, 0], [20, 0], [20, 20], [0, 20], [0, 0]],
                    [[5, 5], [15, 5], [15, 15], [5, 15], [5, 5]]
                ]
            }
        });
        let shape = NativeAnnotationShape {
            rings: geojson_rings(feature.get("geometry").unwrap()),
            feature,
        };
        assert!(point_in_annotation((2.0, 2.0), &shape));
        assert!(!point_in_annotation((10.0, 10.0), &shape));
        assert!(!point_in_annotation((25.0, 10.0), &shape));
    }

    #[test]
    fn trajectory_snapshot_becomes_native_line_geometry() {
        let snapshot: NativeRendererState = serde_json::from_str(
            r#"{
              "protocol":"wsiTools-native-renderer-state/v1",
              "trajectories":[{
                "id":"trajectory-1",
                "points":[{"x":2,"y":3},{"x":7,"y":11},{"x":12,"y":5}]
              }]
            }"#,
        )
        .unwrap();
        let lines = trajectory_lines_from_state(&snapshot, 100);
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].vertices.len() / 6, 3);
        assert_eq!(&lines[0].vertices[2..6], &[0.98, 0.45, 0.09, 1.0]);
    }

    #[test]
    fn viewport_dense_items_become_closed_native_outlines() {
        let items = vec![serde_json::json!({
            "id": "cell-1",
            "colour": "#00FF00",
            "rings": [[
                {"x": 10, "y": 10}, {"x": 14, "y": 10}, {"x": 14, "y": 14}, {"x": 10, "y": 14}
            ]]
        })];
        let lines = dense_lines_from_items(&items, 100);
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].vertices.len() / 6, 5);
        assert_eq!(&lines[0].vertices[2..6], &[0.0, 1.0, 0.0, 0.95]);
    }

    #[test]
    fn screenshot_readback_removes_row_padding_and_converts_bgra() {
        // Two 1-pixel rows with 8-byte GPU row pitch. BGRA must become RGBA.
        let raw = vec![3, 2, 1, 255, 99, 99, 99, 99, 30, 20, 10, 128, 88, 88, 88, 88];
        let rgba = native_screenshot_rgba_bytes(&raw, 1, 2, 8, true).unwrap();
        assert_eq!(rgba, vec![1, 2, 3, 255, 10, 20, 30, 128]);
    }

    #[test]
    fn screenshot_export_uses_the_selected_image_format() {
        let image = image::RgbaImage::from_raw(1, 1, vec![12, 34, 56, 255]).unwrap();
        let stem = format!("wsitools-native-screenshot-{}", std::process::id());
        let temp = std::env::temp_dir();
        let png_path = temp.join(format!("{stem}.png"));
        let jpeg_path = temp.join(format!("{stem}.jpg"));
        let svg_path = temp.join(format!("{stem}.svg"));

        native_save_screenshot(&image, &png_path).unwrap();
        native_save_screenshot(&image, &jpeg_path).unwrap();
        native_save_screenshot(&image, &svg_path).unwrap();

        assert_eq!(&std::fs::read(&png_path).unwrap()[..8], b"\x89PNG\r\n\x1a\n");
        assert_eq!(&std::fs::read(&jpeg_path).unwrap()[..2], b"\xff\xd8");
        let svg = std::fs::read_to_string(&svg_path).unwrap();
        assert!(svg.contains("<svg"));
        assert!(svg.contains("data:image/png;base64,"));

        let _ = std::fs::remove_file(png_path);
        let _ = std::fs::remove_file(jpeg_path);
        let _ = std::fs::remove_file(svg_path);
    }

    #[test]
    fn native_prediction_sources_preserve_reduction_metadata() {
        let config = serde_json::json!({
            "sources": [
                {"id": "spatial:raw", "label": "Spatial raw expression", "type": "raw_expression"},
                {"id": "spatial:reduction:0", "label": "Spatial PCA", "type": "reduction", "dimension_count": 30}
            ]
        });
        assert_eq!(
            native_prediction_sources(&config),
            vec![
                ("spatial:raw".to_string(), "Spatial raw expression".to_string(), false, 0),
                ("spatial:reduction:0".to_string(), "Spatial PCA".to_string(), true, 30)
            ]
        );
    }

    #[test]
    fn native_proximity_annotations_include_categories_and_rois() {
        let shapes = vec![NativeAnnotationShape {
            feature: serde_json::json!({
                "type":"Feature", "id":"roi-1",
                "properties":{"classification":{"name":"Tumour"}},
                "geometry":{"type":"Polygon","coordinates":[[[0,0],[1,0],[0,1],[0,0]]]}
            }),
            rings: vec![],
        }];
        let choices = native_proximity_annotations(&shapes);
        assert!(choices.iter().any(|(id, _)| id == "roi:roi-1"));
        assert!(choices.iter().any(|(id, label)| id == "class:Tumour" && label == "All Tumour annotations"));
    }
}
