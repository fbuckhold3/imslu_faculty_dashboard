# IMSLU Faculty Dashboard

Internal Medicine faculty dashboard for the SSM Health / Saint Louis University
residency program. Built in R/Shiny, deployed to Posit Connect Cloud.

Faculty log in with a unique access code and see their own teaching evaluations,
assessment-completion stats, and feedback. Division administrators and department
leaders see aggregate views scoped to their division or the full department.

## Audience

- Active IMSLU Internal Medicine faculty (individual view)
- Division administrators / directors (division-scoped aggregate view)
- Department leadership (full or site-scoped aggregate view)

## Data sources

Two REDCap databases at `https://redcapsurvey.slu.edu`:

| Database | Token env var |
|---|---|
| IMSLUFaculty | `FAC_TOKEN` |
| RDM2_0 | `RDM_TOKEN` |

Base URL: `REDCAP_URL`.

## Local development

1. Clone the repo
2. Ensure `~/.Renviron` has `FAC_TOKEN`, `RDM_TOKEN`, and `REDCAP_URL` set
3. Open in Positron, run `renv::restore()`
4. `shiny::runApp()`

By default the login screen shows a faculty dropdown for testing. To force
access-code-only login, set `PRODUCTION_MODE=TRUE` in the environment before
launching.

## Deployment

Pushes to `main` auto-deploy to Posit Connect Cloud via the connected GitHub
integration. Connect environment variables required:

- `PRODUCTION_MODE=TRUE`
- `FAC_TOKEN`, `RDM_TOKEN`, `REDCAP_URL`

Regenerate `manifest.json` with `rsconnect::writeManifest()` after any package
changes before pushing.

## Documentation

- **Architecture, modules, conventions:** `Claude.md` in this repo
- **Project planning & design docs:** `~/Library/Cowork/faculty-dashboard-v1-launch/` (internal)
