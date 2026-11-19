#!/usr/bin/env python3
"""Check the tissue-viewer skill against wsiTools and package a versioned ZIP."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import zipfile


API_ARGS = {
    "R/slide.R": {"wsi_open": {"path", "backend"}, "wsi_info": {"slide"}},
    "R/viewer.R": {"wsi_viewer": {"slide", "mode", "output", "tile_dir", "tile_size", "tile_format", "quality", "open", "overwrite", "rebuild"}},
    "R/roi.R": {"wsi_read_geojson": {"path"}},
    "R/region.R": {"wsi_read_region": {"slide", "x", "y", "width", "height", "level", "format"}},
    "R/thumbnail-crop.R": {"wsi_thumbnail": {"slide", "width", "format"}},
}
CODE_DIRS = ("R", "inst/viewer", "src", "scripts", "tools")
CODE_SUFFIXES = {".R", ".r", ".js", ".mjs", ".cjs", ".ts", ".rs", ".c", ".cc", ".cpp", ".h", ".hpp", ".py", ".sh", ".toml"}
SKIP_PARTS = {"node_modules", "target", "dist", "__pycache__", ".git"}


def blob_sha(path: Path) -> str:
    data = path.read_bytes()
    return hashlib.sha1(b"blob " + str(len(data)).encode() + b"\0" + data).hexdigest()


def api_check(root: Path, lock_path: Path) -> dict[str, str]:
    if not lock_path.is_file():
        raise ValueError(f"Missing API review lock: {lock_path}")
    lock = json.loads(lock_path.read_text(encoding="utf-8"))
    current = {}
    for relative, functions in API_ARGS.items():
        path = root / relative
        source = path.read_text(encoding="utf-8")
        current[relative] = blob_sha(path)
        accepted = lock.get(relative, [])
        if current[relative] not in accepted:
            raise ValueError(f"{relative} changed since skill review. Review the skill and run --sync-lock.")
        for name, required in functions.items():
            match = re.search(rf"(?ms)^{re.escape(name)}\s*<-\s*function\((.*?)\)\s*\{{", source)
            if not match:
                raise ValueError(f"Expected API {name}() not found in {relative}")
            signature = match.group(1)
            found = set(re.findall(r"\b([A-Za-z][A-Za-z_0-9]*)\s*(?=,|=|$)", signature))
            missing = required - found
            if missing:
                raise ValueError(f"{name}() is missing arguments: {', '.join(sorted(missing))}")
    return current


def source_fingerprint(root: Path) -> tuple[str, int]:
    files = []
    for directory in CODE_DIRS:
        base = root / directory
        if base.is_dir():
            for path in base.rglob("*"):
                if path.is_file() and path.suffix in CODE_SUFFIXES and not (set(path.relative_to(root).parts) & SKIP_PARTS):
                    files.append(path)
    files += [root / "DESCRIPTION", root / "NAMESPACE"]
    digest = hashlib.sha256()
    for path in sorted(set(files)):
        relative = path.relative_to(root).as_posix()
        digest.update(relative.encode() + b"\0" + hashlib.sha256(path.read_bytes()).digest())
    return digest.hexdigest(), len(files)


def package(root: Path, output: Path, metadata_output: Path | None, api_blobs: dict[str, str]) -> None:
    skill = root / "skills" / "wsi-tissue-visualizer"
    if not (skill / "SKILL.md").is_file():
        raise ValueError("Skill directory is missing")
    version_match = re.search(r"(?m)^Version:\s*(\S+)", (root / "DESCRIPTION").read_text(encoding="utf-8"))
    version = version_match.group(1) if version_match else "unknown"
    fingerprint, file_count = source_fingerprint(root)
    revision = os.environ.get("GITHUB_SHA")
    if not revision:
        result = subprocess.run(["git", "rev-parse", "HEAD"], cwd=root, capture_output=True, text=True, check=False)
        revision = result.stdout.strip() if result.returncode == 0 else None
    metadata = {
        "wsiTools_version": version,
        "source_revision": revision,
        "source_sha256": fingerprint,
        "source_file_count": file_count,
        "reviewed_api_blobs": api_blobs,
    }
    output.parent.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(output, "w", compression=zipfile.ZIP_DEFLATED, compresslevel=9) as archive:
        for path in sorted(p for p in skill.rglob("*") if p.is_file() and "__pycache__" not in p.parts):
            name = path.relative_to(skill.parent).as_posix()
            archive.write(path, name)
        info = zipfile.ZipInfo("wsi-tissue-visualizer/SOURCE_VERSION.json", date_time=(2020, 1, 1, 0, 0, 0))
        info.compress_type = zipfile.ZIP_DEFLATED
        archive.writestr(info, json.dumps(metadata, indent=2, sort_keys=True) + "\n")
    if metadata_output:
        metadata_output.parent.mkdir(parents=True, exist_ok=True)
        metadata_output.write_text(json.dumps(metadata, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    print(f"Packaged {output} for wsiTools {version}; {file_count} source files fingerprinted")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument("--check", action="store_true", help="Verify reviewed APIs and exit")
    parser.add_argument("--sync-lock", action="store_true", help="After reviewing skill compatibility, accept current API blobs")
    parser.add_argument("--output", type=Path, help="ZIP output path")
    parser.add_argument("--metadata-output", type=Path, help="JSON source metadata output path")
    args = parser.parse_args()
    root = args.root.resolve()
    lock_path = root / "skills" / "wsi-tissue-visualizer" / "references" / "api-lock.json"
    try:
        if args.sync_lock:
            lock = json.loads(lock_path.read_text(encoding="utf-8")) if lock_path.exists() else {}
            for relative in API_ARGS:
                sha = blob_sha(root / relative)
                lock[relative] = sorted(set(lock.get(relative, []) + [sha]))
            lock_path.parent.mkdir(parents=True, exist_ok=True)
            lock_path.write_text(json.dumps(lock, indent=2, sort_keys=True) + "\n", encoding="utf-8")
            print(f"Updated reviewed API blobs in {lock_path}")
        current = api_check(root, lock_path)
        if args.output:
            package(root, args.output, args.metadata_output, current)
        elif not args.sync_lock:
            print("Tissue skill API review check passed")
        if not args.output and not args.check and not args.sync_lock:
            parser.print_help()
    except (OSError, ValueError, KeyError, json.JSONDecodeError) as error:
        print(f"Tissue skill check failed: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
