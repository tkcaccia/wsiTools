# Deploy on Read the Docs

The repository is configured for Read the Docs Community with MkDocs. The build
uses `.readthedocs.yaml`, `mkdocs.yml`, and the pinned packages in
`docs/requirements.txt`.

## Build locally

```bash
python -m venv .venv
source .venv/bin/activate
python -m pip install --upgrade pip
python -m pip install -r docs/requirements.txt
mkdocs serve
```

On Windows PowerShell, activate the environment with:

```powershell
.venv\Scripts\Activate.ps1
```

Create a production build with:

```bash
mkdocs build
```

The generated `site/` directory is disposable and should not be committed.

## Import the project

1. Sign in to Read the Docs Community with GitHub.
2. Choose **Import a Project** and select `tkcaccia/wsiTools`.
3. Choose an available project slug, preferably `wsitools` when available.
4. Keep `main` as the default branch.
5. Trigger the first build; Read the Docs will discover `.readthedocs.yaml` at
   the repository root.
6. In the project settings, enable pull-request builds so documentation changes
   receive a rendered preview before merge.

After the final project URL is known, update the README documentation badge and
link to the canonical Read the Docs URL.

## Build design

The documentation build renders Markdown only. It does not execute R examples or
install OpenSlide, libvips, CZI, Bio-Formats, StarDist, Mesmer, or spatial-omics
packages. This keeps preview builds fast and avoids claiming that optional
runtime capabilities are available on the documentation worker.

The generated R API reference remains on the pkgdown site and is linked from the
MkDocs navigation.

## Versions

Read the Docs creates `latest` from the default branch. When release branches or
tags are introduced, activate the desired release version and use `stable` for
the recommended released documentation.

## Troubleshoot a build

- Confirm the build log uses `.readthedocs.yaml` from the intended branch.
- Confirm Python 3.12 is selected and both pinned packages install.
- Run `mkdocs build` locally and fix missing navigation pages or broken relative
  links reported by MkDocs.
- Keep file names and capitalization identical; Read the Docs builds on Linux.
- Enable pull-request builds after the project is imported to catch future
  documentation regressions.
