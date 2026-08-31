//! Tiny, dependency-free browser helper for dense vector-overlay culling.
//!
//! The module deliberately receives only numeric bounding boxes and returns
//! matching indices. Image IO, geometry editing, and R communication remain
//! in the normal wsiTools browser/R code paths.

use core::slice;

/// Allocate a buffer in Wasm linear memory for JavaScript to populate.
#[no_mangle]
pub extern "C" fn wsi_overlay_alloc(length: usize) -> *mut u8 {
    let mut buffer = Vec::<u8>::with_capacity(length);
    let pointer = buffer.as_mut_ptr();
    core::mem::forget(buffer);
    pointer
}

/// Release a buffer previously returned by [`wsi_overlay_alloc`].
///
/// `length` is the allocated capacity, not the number of initialized bytes.
#[no_mangle]
pub unsafe extern "C" fn wsi_overlay_dealloc(pointer: *mut u8, length: usize) {
    if !pointer.is_null() && length > 0 {
        drop(Vec::from_raw_parts(pointer, 0, length));
    }
}

/// Filter `count` axis-aligned bounding boxes against a viewport.
///
/// `boxes` is `[xmin, ymin, xmax, ymax]` repeated `count` times. Matching
/// box indices are written to `output`; the return value is their count.
#[no_mangle]
pub unsafe extern "C" fn wsi_filter_bboxes(
    boxes: *const f64,
    count: u32,
    viewport_xmin: f64,
    viewport_ymin: f64,
    viewport_xmax: f64,
    viewport_ymax: f64,
    output: *mut u32,
) -> u32 {
    if boxes.is_null() || output.is_null() || count == 0 {
        return 0;
    }

    let count = count as usize;
    let boxes = slice::from_raw_parts(boxes, count.saturating_mul(4));
    let output = slice::from_raw_parts_mut(output, count);
    let mut matches = 0usize;

    for index in 0..count {
        let offset = index * 4;
        let xmin = boxes[offset];
        let ymin = boxes[offset + 1];
        let xmax = boxes[offset + 2];
        let ymax = boxes[offset + 3];
        if xmin <= viewport_xmax
            && xmax >= viewport_xmin
            && ymin <= viewport_ymax
            && ymax >= viewport_ymin
        {
            output[matches] = index as u32;
            matches += 1;
        }
    }

    matches as u32
}
