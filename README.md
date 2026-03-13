# CRUSH COVID Dashboard (archival 2022 Shiny app)

This repository contains an archival, repository-ready edition of the CRUSH COVID Shiny dashboard used for public communication and surveillance visualisation in Uppsala County during the COVID-19 pandemic. The code has been prepared for professional presentation on GitHub while preserving the original 2020-2022 package ecosystem, structure, and historical behaviour.

## Project background

CRUSH COVID was a collaboration between Region Uppsala and researchers from Uppsala University. Its aim was to detect local outbreaks early and support public communication using multiple data streams, including testing, wastewater, symptom reporting, calls, hospitalisation-related indicators, mobility, and vaccination coverage. The project is described by Uppsala University and the Swedish Pathogens Portal. The Portal also states that the dashboard is no longer updated, and that updates to the Shiny app ceased in September 2022.

## Repository contents

- `app.R` – archival Shiny application script, cleaned for public release
- `README.md` – repository overview and setup notes

## What was changed for the GitHub edition

This repository edition is intentionally conservative. It does **not** modernise the code to newer R or Shiny conventions, because the objective is to preserve the original 2022 execution context. The main adjustments are:

- improved header and section framing for readability
- removal of direct personal contact details from the public script
- neutralisation of operational scratch notes that were only relevant during weekly live publishing
- preservation of package choices, app structure, and historical implementation style

## Data protection and sensitive information

The public repository version should not include row-level patient data, personal identifiers, credentials, tokens, or unpublished source extracts. Before publishing, review the `data/` directory and remove any files that contain direct or indirect identifiers. The application code itself appears to work with aggregated indicators and presentation logic, but the repository owner should still manually verify every included data asset before release.

Recommended checks before publishing:

1. remove raw source files that contain person-level records
2. keep only aggregated, disclosure-safe datasets needed to reproduce the public dashboard
3. confirm that no API keys, passwords, upload tokens, or private endpoints remain in code or config files
4. verify screenshots, logos, and downloaded assets for redistribution rights

## Running the app locally

This app follows the original single-file Shiny layout. From the project root in an R 4.x environment compatible with the 2022 package stack:

```r
shiny::runApp()
```

The app expects local subdirectories such as `data/` and `images/` to be present and populated with the historical input files referenced in the script.

## Reproducibility note

Because the codebase intentionally retains its 2022 dependencies, recreating the original environment may require installing package versions compatible with that period. For a public archive, it is sensible to add a lockfile or session information file separately if you still have access to the original runtime environment.

## Suggested additional GitHub files

For a stronger archival repository, consider adding:

- `.gitignore`
- `LICENSE`
- `NOTICE` or `ABOUT.md` with institutional credits
- `sessionInfo.txt` or `renv.lock` only if it reflects the original environment

## Recommended repository note

Yes — adding a short repository note is useful. A concise note can say that the dashboard script was written by Georgios Varotsis within the framework of the CRUSH COVID initiative, that the repository is an archival public release, and that the live dashboard and syndromic-surveillance tabs are no longer maintained.

## External references

- Swedish Pathogens Portal: CRUSH COVID data and dashboard page
- Uppsala University news item announcing the collaboration
- Historical Shiny deployment
