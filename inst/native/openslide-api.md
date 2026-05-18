# Native OpenSlide API TODO

The first milestone uses command-line backends where available and keeps the R
API ready for native bindings.

The planned native OpenSlide layer should wrap:

- `openslide_open`
- `openslide_close`
- `openslide_get_level_count`
- `openslide_get_level_dimensions`
- `openslide_get_level_downsample`
- `openslide_get_property_names`
- `openslide_get_property_value`
- `openslide_get_associated_image_names`
- `openslide_read_region`

The R object should store an external pointer with a finalizer that calls
`openslide_close`. Region reads must remain bounded to explicit requests and
must never load the entire level-0 image by default.
