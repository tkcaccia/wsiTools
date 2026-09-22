# Shared viewer runtime

These scripts are embedded in both static and live viewer HTML, including the
viewer opened by wsiTools Desktop. Geometry remains in level-0 slide pixels.

`clipper.js` is the unmodified JavaScript Clipper 6.4.2.2 distribution
(`clipper-lib@6.4.2` on npm). Source: https://sourceforge.net/projects/jsclipper/
Its Boost license and embedded JSBN notice are retained in `clipper.LICENSE`.
The geometry worker uses fixed-point clipping at 1/4096 slide-pixel precision,
restoring unchanged original vertices and retaining full-resolution boundaries.
