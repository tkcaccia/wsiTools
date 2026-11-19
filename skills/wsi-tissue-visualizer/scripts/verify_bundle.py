#!/usr/bin/env python3
"""Check the static viewer's data files before an agent publishes or embeds it."""

import argparse
import hashlib
import json
import math
from pathlib import Path
import sys
import xml.etree.ElementTree as ET


def sha256(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(8 * 1024 * 1024), b""):
            h.update(block)
    return h.hexdigest()


def check_remote(bundle: Path, manifest: dict, qa_report: Path | None) -> None:
    required = {"index.html", "viewer.css", "viewer.js", "remote.js", "manifest.json",
                "wsi-tissue-viewer.json", "vendor/geotiff.js", "vendor/LICENSE",
                "preview.jpg", "source-check.json"}
    overlay = manifest.get("overlay")
    if overlay:
        required.add("annotations.geojson")
        if overlay.get("display_url"):
            required.add("display-contours.geojson")
        if overlay.get("approved_for_publication") is not True:
            raise ValueError("Overlay is not approved for publication")
    mask = manifest.get("display_mask")
    if mask:
        required.add("display-mask.png")
        if mask.get("approved_for_publication") is not True or mask.get("label_resample") != "nearest":
            raise ValueError("Display mask approval or nearest label resampling is missing")
    files = set()
    for path in bundle.rglob("*"):
        if path.is_symlink():
            raise ValueError("Publishable bundle contains a symlink")
        if path.is_file():
            files.add(path.relative_to(bundle).as_posix())
    if files != required:
        raise ValueError(f"Unexpected or missing publishable files: {sorted(files ^ required)}")
    if manifest.get("mode") != "remote-tiff" or manifest.get("schema_version") != 2:
        raise ValueError("Invalid source-only manifest")
    image = manifest["image"]
    if not image["url"].startswith("https://") or image.get("remote_sha256_verified") is not False:
        raise ValueError("Remote URL or remote-hash statement is invalid")
    if image.get("rgb_resample") not in ("bilinear", "nearest") or image.get("label_resample") != "nearest":
        raise ValueError("Invalid RGB or label resampling")
    identity = manifest["source_identity"]
    if identity.get("publication_approved") is not True or identity.get("preview_approved") is not True:
        raise ValueError("Publication or preview is unapproved")
    if identity.get("coordinate_system") != "level0_pixel_xy":
        raise ValueError("Unsupported coordinate system")
    check = json.loads((bundle / "source-check.json").read_text())
    if (check.get("range_status") != 206 or check.get("boundary_ranges_match_local") is not True or
            check.get("remote_sha256_verified") is not False or
            check.get("local_source_sha256") != image.get("source_sha256")):
        raise ValueError("Remote range/CORS probe or source identity is missing")
    if check.get("cors_allow_origin") not in ("*", identity.get("publish_origin")):
        raise ValueError("Publication origin is not allowed by source CORS")
    expected = manifest.get("integrity", {})
    if set(expected) != required - {"manifest.json", "wsi-tissue-viewer.json"}:
        raise ValueError("Asset hash inventory does not match publishable files")
    for name, digest in expected.items():
        if sha256(bundle / name) != digest:
            raise ValueError(f"Asset SHA256 mismatch: {name}")
    if overlay:
        exact = json.loads((bundle / "annotations.geojson").read_text())
        if exact.get("type") != "FeatureCollection" or len(exact.get("features", [])) != overlay["feature_count"]:
            raise ValueError("Exact GeoJSON feature count differs")
        if overlay.get("display_url"):
            display = json.loads((bundle / "display-contours.geojson").read_text())
            if len(display.get("features", [])) != len(exact["features"]):
                raise ValueError("Display contour feature count differs")
    if mask and (mask.get("source_sha256") != image["source_sha256"] or
                 mask.get("coordinate_system") != "preview_pixel_xy" or
                 (mask.get("width"), mask.get("height")) != (image["preview_width"], image["preview_height"])):
        raise ValueError("Display mask source or preview dimensions differ")
    # Manifest data must not leak private local paths or credentials.
    raw = (bundle / "manifest.json").read_text()
    if any(token in raw for token in ("file://", "/home/", "/mnt/", "/Users/", "\\\\", "@")):
        raise ValueError("Manifest contains a local path or credential marker")
    if qa_report:
        report = json.loads((qa_report / "report.json").read_text())
        if report.get("status") != "passed" or report.get("bundle_manifest_sha256") != sha256(bundle / "manifest.json"):
            raise ValueError("Browser QA did not pass against this manifest")
        for mode in ("fit", "medium", "close", "narrow"):
            if report.get("screenshots", {}).get(mode) != sha256(qa_report / f"{mode}.png"):
                raise ValueError(f"QA screenshot SHA256 mismatch: {mode}")
    print(f"Verified source-only {image['width']} × {image['height']} TIFF viewer")


def check(bundle: Path, qa_report: Path | None = None) -> None:
    manifest_path = bundle / "manifest.json"
    if not manifest_path.is_file():
        raise ValueError("Missing manifest.json")
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    if manifest.get("schema_version") == 2:
        check_remote(bundle, manifest, qa_report)
        return
    required = ["index.html", "viewer.css", "viewer.js", "manifest.json", "wsi-tissue-viewer.json", "tiles/slide.dzi"]
    missing = [name for name in required if not (bundle / name).is_file()]
    if missing:
        raise ValueError(f"Missing files: {', '.join(missing)}")
    if manifest.get("schema_version") != 1:
        raise ValueError("Unexpected manifest schema version")
    image = manifest["image"]
    if image.get("dzi") != "tiles/slide.dzi":
        raise ValueError("Unexpected DZI path")
    root = ET.parse(bundle / image["dzi"]).getroot()
    size = next((child for child in root if child.tag.rsplit("}", 1)[-1] == "Size"), None)
    if size is None:
        raise ValueError("DZI has no Size element")
    width, height = int(size.attrib["Width"]), int(size.attrib["Height"])
    if (width, height) != (int(image["width"]), int(image["height"])):
        raise ValueError("DZI dimensions differ from manifest")
    max_level = math.ceil(math.log2(max(width, height)))
    tile_extension = root.attrib.get("Format", "jpg")
    top_level = bundle / "tiles" / "slide_files" / str(max_level)
    if not top_level.is_dir() or not any(top_level.glob(f"*.{tile_extension}")):
        raise ValueError("Top-resolution image tiles are missing")
    if (bundle / "tiles" / "slide.wsiTools.json").exists():
        raise ValueError("Publishable bundle contains source-path cache metadata")
    overlay = manifest.get("overlay")
    if overlay:
        geojson_path = bundle / overlay["url"]
        data = json.loads(geojson_path.read_text(encoding="utf-8"))
        if data.get("type") != "FeatureCollection" or not isinstance(data.get("features"), list):
            raise ValueError("Overlay is not a GeoJSON FeatureCollection")
        if len(data["features"]) != overlay["feature_count"]:
            raise ValueError("Overlay feature count differs from manifest")
        print(f"Verified {width} × {height} slide with {len(data['features'])} exact GeoJSON features")
    else:
        print(f"Verified {width} × {height} image-only viewer")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("bundle", type=Path, help="Directory built by build_viewer.R")
    parser.add_argument("--qa-report", type=Path, help="Require a passing browser QA report for source-only bundles")
    args = parser.parse_args()
    try:
        check(args.bundle, args.qa_report)
    except (OSError, ValueError, KeyError, ET.ParseError, json.JSONDecodeError) as error:
        print(f"Viewer verification failed: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
