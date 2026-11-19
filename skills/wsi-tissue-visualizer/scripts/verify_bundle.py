#!/usr/bin/env python3
"""Check the static viewer's data files before an agent publishes or embeds it."""

import argparse
import json
import math
from pathlib import Path
import sys
import xml.etree.ElementTree as ET


def check(bundle: Path) -> None:
    required = ["index.html", "viewer.css", "viewer.js", "manifest.json", "wsi-tissue-viewer.json", "tiles/slide.dzi"]
    missing = [name for name in required if not (bundle / name).is_file()]
    if missing:
        raise ValueError(f"Missing files: {', '.join(missing)}")
    manifest = json.loads((bundle / "manifest.json").read_text(encoding="utf-8"))
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
    args = parser.parse_args()
    try:
        check(args.bundle)
    except (OSError, ValueError, KeyError, ET.ParseError, json.JSONDecodeError) as error:
        print(f"Viewer verification failed: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
