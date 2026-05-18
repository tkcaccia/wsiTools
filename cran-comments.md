## R CMD check results

0 errors | 0 warnings | 3 notes

`R CMD check --as-cran` on the local macOS machine reports expected NOTEs:

- New submission
- "unable to verify current time"
- HTML validation warnings from generated help-page templates

## System dependencies

wsiTools installs without OpenSlide, libvips, Bio-Formats, or ImageMagick.
These are optional runtime tools. Backend-dependent tests are skipped or mocked
when the relevant tools are not available.

## Test environments

- Local macOS, R release
