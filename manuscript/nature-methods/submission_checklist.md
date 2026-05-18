# Submission Checklist

## Nature Methods Fit

- [ ] Decide article type: Article, Brief Communication or Resource.
- [ ] Prepare a presubmission inquiry before full submission.
- [ ] Confirm that the final manuscript emphasizes method/tool novelty,
      validation, reproducibility, general applicability and biological utility.

Nature Methods states that Articles describing tools should include a full
technical description and strong validation data demonstrating performance,
reproducibility, general applicability and potential for discovering new
biology. The current manuscript draft therefore requires real benchmark and
case-study results before submission.

## Software

- [ ] Make GitHub repository public: https://github.com/tkcaccia/wsiTools.
- [ ] Create a versioned release, for example `v0.1.0`.
- [ ] Archive the release with Zenodo or another repository and add DOI.
- [ ] Add installation instructions for OpenSlide and libvips on Linux, macOS
      and Windows.
- [ ] Run `R CMD check --as-cran`.
- [ ] Add a small public test dataset or scripted public-data download.
- [ ] Add reproducible benchmark scripts.
- [ ] Add a `CITATION.cff` file.

## Validation Needed

- [ ] Benchmark opening/metadata extraction across representative WSI formats.
- [ ] Benchmark region reading at multiple pyramid levels.
- [ ] Benchmark tile-grid generation and tile export.
- [ ] Benchmark BTF/SVS-to-OME-TIFF conversion.
- [ ] Validate GeoJSON round-trip against QuPath.
- [ ] Validate stain deconvolution on known H-DAB examples.
- [ ] Validate measurements against hand-calculated synthetic geometries.
- [ ] Report failure modes and backend-dependent unsupported formats.

## Manuscript Items

- [ ] Final author list and affiliations.
- [ ] Abstract within target word count.
- [ ] Main text shortened to final format.
- [ ] Figures with final panels.
- [ ] Figure legends.
- [ ] Methods with enough detail to reproduce all benchmarks.
- [ ] Data availability statement.
- [ ] Code availability statement.
- [ ] Competing interests statement.
- [ ] Ethics statement if human tissue/images are used.
- [ ] Reporting summary if required.
- [ ] Supplementary software documentation.

## Submission

- [ ] Submit through the Springer Nature/Nature Methods submission system using
      the corresponding author's account.
- [ ] Upload manuscript PDF/Word file.
- [ ] Upload figures.
- [ ] Upload supplementary information.
- [ ] Include GitHub/Zenodo links.
- [ ] Include suggested reviewers if requested.

