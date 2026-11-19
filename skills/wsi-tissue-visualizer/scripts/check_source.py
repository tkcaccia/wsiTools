#!/usr/bin/env python3
"""Validate a local TIFF against a publication identity and probe its remote URL."""

import argparse
import hashlib
import json
from pathlib import Path
import re
import sys
from urllib.request import Request, urlopen
from urllib.parse import urlsplit


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(8 * 1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def validate(identity: dict, slide: Path, url: str) -> dict:
    if identity.get("schema_version") != 1:
        raise ValueError("Source identity must have schema_version 1")
    if identity.get("remote_tiff_url") != url or not url.startswith("https://"):
        raise ValueError("Remote TIFF URL must match the HTTPS URL in the identity manifest")
    parsed = urlsplit(url)
    if not parsed.hostname or parsed.username or parsed.password or parsed.fragment or parsed.query:
        raise ValueError("Remote TIFF URL must have a public hostname and no credentials, query, or fragment")
    digest = identity.get("source_sha256", "")
    if not re.fullmatch(r"[0-9a-f]{64}", digest):
        raise ValueError("source_sha256 must be a lowercase SHA256 digest")
    if sha256(slide) != digest:
        raise ValueError("Local source SHA256 differs from the source identity manifest")
    if identity.get("coordinate_system") != "level0_pixel_xy":
        raise ValueError("Only level0_pixel_xy source coordinates are supported")
    registration = identity.get("registration", {})
    if registration.get("annotation_to_source_affine") != [1, 0, 0, 0, 1, 0]:
        raise ValueError("This viewer requires an explicit identity level-0 registration")
    origin = identity.get("publish_origin", "")
    if not re.fullmatch(r"https://[^/]+", origin):
        raise ValueError("publish_origin must be an HTTPS origin without a path")
    if identity.get("publication_approved") is not True:
        raise ValueError("Source identity must declare publication_approved: true")
    if identity.get("preview_approved") is not True:
        raise ValueError("Source identity must declare preview_approved: true")
    width, height = identity.get("width"), identity.get("height")
    if not all(isinstance(x, int) and x > 0 for x in (width, height)):
        raise ValueError("Source width and height must be positive integers")
    if not all(isinstance(identity.get(k), (float, int)) and identity[k] > 0 for k in ("mpp_x", "mpp_y")):
        raise ValueError("Positive mpp_x and mpp_y are required")

    size = slide.stat().st_size
    if size < 64:
        raise ValueError("Source TIFF is too small for range identity checks")

    def probe(start: int, end: int) -> tuple[bytes, str, str]:
        request = Request(url, headers={"Range": f"bytes={start}-{end}", "Origin": origin,
                                        "Accept-Encoding": "identity"})
        with urlopen(request, timeout=20) as response:
            status = response.status
            content_range = response.headers.get("Content-Range", "")
            allow_origin = response.headers.get("Access-Control-Allow-Origin", "")
            content_encoding = response.headers.get("Content-Encoding", "identity")
            redirected = response.geturl() != url
            data = response.read(end - start + 2)
        if redirected or status != 206 or content_range != f"bytes {start}-{end}/{size}" or len(data) != end - start + 1:
            raise ValueError("Remote TIFF did not honor a bounded, unredirected byte-range request")
        if allow_origin not in ("*", origin):
            raise ValueError("Remote TIFF CORS does not allow the publication origin")
        if content_encoding.lower() != "identity":
            raise ValueError("Remote TIFF range response uses content encoding")
        return data, content_range, allow_origin

    first, content_range, allow_origin = probe(0, 63)
    last, _, _ = probe(size - 64, size - 1)
    with slide.open("rb") as stream:
        local_first = stream.read(64)
        stream.seek(size - 64)
        local_last = stream.read(64)
    if first != local_first or last != local_last:
        raise ValueError("Remote TIFF boundary ranges differ from the locally hashed source")
    if first[:2] not in (b"II", b"MM"):
        raise ValueError("Remote source does not begin with a TIFF byte-order marker")
    return {
        "range_status": 206,
        "content_range": content_range,
        "cors_allow_origin": allow_origin,
        "remote_byte_length": size,
        "boundary_ranges_match_local": True,
        "local_source_sha256": digest,
        "remote_sha256_verified": False,
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--slide", required=True, type=Path)
    parser.add_argument("--identity", required=True, type=Path)
    parser.add_argument("--url", required=True)
    parser.add_argument("--out", required=True, type=Path)
    args = parser.parse_args()
    try:
        result = validate(json.loads(args.identity.read_text(encoding="utf-8")), args.slide, args.url)
        args.out.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
    except (OSError, ValueError, KeyError, json.JSONDecodeError) as error:
        print(f"Source identity check failed: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
