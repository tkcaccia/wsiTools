#!/usr/bin/env python3
"""Capture source-only viewer states and fail when source detail or controls fail."""

import argparse
import hashlib
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
import json
from pathlib import Path
import shutil
import sys
from threading import Thread
import time


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--bundle", type=Path, required=True)
    parser.add_argument("--out", type=Path, required=True)
    args = parser.parse_args()
    try:
        from playwright.sync_api import sync_playwright
    except ImportError as error:
        raise RuntimeError("Browser QA requires Python Playwright and an installed Chromium/Chrome browser") from error
    manifest = json.loads((args.bundle / "manifest.json").read_text())
    if manifest.get("mode") != "remote-tiff":
        raise ValueError("Browser QA supports source-only bundles")
    args.out.mkdir(parents=True, exist_ok=False)
    class Handler(SimpleHTTPRequestHandler):
        def __init__(self, *a, **kw):
            super().__init__(*a, directory=str(args.bundle), **kw)
        def log_message(self, *_):
            pass
    server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    thread = Thread(target=server.serve_forever, daemon=True)
    thread.start()
    base = f"http://127.0.0.1:{server.server_port}/index.html"
    report = {"schema_version": 1, "status": "failed", "source_sha256": manifest["image"]["source_sha256"],
              "bundle_manifest_sha256": digest(args.bundle / "manifest.json"),
              "cors_probe": "source-check.json; browser QA uses an isolated localhost origin",
              "screenshots": {}, "views": {}, "landmarks": []}
    try:
        with sync_playwright() as p:
            chrome = shutil.which("google-chrome") or shutil.which("chromium") or shutil.which("chromium-browser")
            browser = p.chromium.launch(executable_path=chrome, headless=True,
                                        args=["--disable-web-security", "--ignore-certificate-errors"])
            page = browser.new_page(viewport={"width": 1440, "height": 900}, device_scale_factor=1)
            errors = []
            page.on("pageerror", lambda error: errors.append(str(error)))
            page.goto(base, wait_until="domcontentloaded")
            page.wait_for_function("window.__wsiViewerQA && window.__wsiViewerQA.state().ready", timeout=30000)
            for mode in ("fit", "medium", "close"):
                page.evaluate("mode => window.__wsiViewerQA.focus(mode)", mode)
                if mode != "fit":
                    page.wait_for_function("window.__wsiViewerQA.state().detailSuccess > 0", timeout=45000)
                page.wait_for_function("window.__wsiViewerQA.state().detailRequests === 0", timeout=45000)
                page.wait_for_timeout(500)
                path = args.out / f"{mode}.png"
                page.screenshot(path=str(path), full_page=True)
                report["screenshots"][mode] = digest(path)
                report["views"][mode] = page.evaluate("window.__wsiViewerQA.state()")
            for landmark in manifest["qa"]["landmarks"][:2]:
                a = landmark["annotation_xy"]
                b = landmark["source_xy"]
                distance = sum((x - y) ** 2 for x, y in zip(a, b)) ** .5
                annotation_screen = page.evaluate("xy => window.__wsiViewerQA.point(...xy)", a)
                source_screen = page.evaluate("xy => window.__wsiViewerQA.point(...xy)", b)
                screen_distance = sum((x - y) ** 2 for x, y in zip(annotation_screen, source_screen)) ** .5
                report["landmarks"].append({"name": landmark.get("name", "landmark"),
                                            "source_pixel_error": distance, "screen_pixel_error": screen_distance})
                if distance > 3:
                    errors.append("Registration landmark exceeds 3 source pixels")
            narrow = browser.new_page(viewport={"width": 390, "height": 844}, device_scale_factor=1)
            narrow.on("pageerror", lambda error: errors.append(str(error)))
            narrow.goto(base, wait_until="domcontentloaded")
            narrow.wait_for_function("window.__wsiViewerQA && window.__wsiViewerQA.state().ready", timeout=30000)
            narrow.evaluate("window.__wsiViewerQA.focus('narrow')")
            narrow.wait_for_function("window.__wsiViewerQA.state().detailSuccess > 0", timeout=45000)
            narrow.wait_for_function("window.__wsiViewerQA.state().detailRequests === 0", timeout=45000)
            narrow.wait_for_timeout(500)
            path = args.out / "narrow.png"
            narrow.screenshot(path=str(path), full_page=True)
            report["screenshots"]["narrow"] = digest(path)
            report["views"]["narrow"] = narrow.evaluate("window.__wsiViewerQA.state()")
            controls = narrow.evaluate("""() => ['fit','zoom-in','zoom-out','opacity','outlines'].every(id => {
              const r = document.getElementById(id).getBoundingClientRect();
              return r.width > 0 && r.height > 0 && r.left >= 0 && r.right <= innerWidth;
            })""")
            if not controls:
                errors.append("Responsive controls are clipped or hidden")
            for mode, view in report["views"].items():
                if view["detailFallbacks"] or view["networkFailures"]:
                    errors.append(f"{mode}: source tile fallback/network failure")
            if any(report["views"][mode]["detailSuccess"] < 1 for mode in ("medium", "close", "narrow")):
                errors.append("Source detail did not replace preview at all required zoom levels")
            if len(report["landmarks"]) < 2:
                errors.append("Two registration landmarks are required")
            report["errors"] = errors
            report["status"] = "passed" if not errors else "failed"
            browser.close()
    except Exception as error:
        report["errors"] = report.get("errors", []) + [str(error)]
    finally:
        server.shutdown()
        report["generated_at_utc"] = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
        (args.out / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    print(f"Browser QA {report['status']}: {args.out / 'report.json'}")
    return 0 if report["status"] == "passed" else 1


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, ValueError, KeyError) as error:
        print(f"Browser QA failed: {error}", file=sys.stderr)
        raise SystemExit(1)
