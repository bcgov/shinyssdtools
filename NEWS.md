<!-- NEWS.md is maintained by https://fledge.cynkra.com, contributors should not edit this file -->

# shinyssdtools 0.5.0 (2026-01-06)

- Added comprehensive testing infrastructure with shinytest2 and testthat.
- Updated to CRAN ssdtools version 2.4.0.
  - Changed `ssd_fit_dists()` to `ssd_fit_bcanz()` for BCANZ compliance.
  - Updated `ssd_gof()` and `ssd_hp()` for deprecated argument changes.
- Major UI/UX improvements:
  - Refactored app into modular architecture for better maintainability.
  - Switched to `bslib` for modern UI components and improved layouts.
  - Added "Update Fit" button to prevent unintended recalculations.
  - Improved client-side translation system for instant language switching.
  - Added option to include confidence limits on model-average plot with selectable ribbon/line style.
  - Added HTML preview for BCANZ reports before download.
  - Bootstrap samples now accept any value (not just preset options).
- Clarified minimum data requirements (6+ positive, non-missing values) with better error messages.
- Improved help text throughout app with `shinyhelper` icons.

# shinyssdtools 0.4.1 (2025-04-02)

- Updated TESTING.md file. 

# shinyssdtools 0.4.0 (2025-03-04)

- Update to CRAN ssdtools version 2.3.0.
  - Remove invpareto from distribution options. 
- Update citation to latest JOSS paper.

# shinyssdtools 0.3.5 (2024-12-18)

- Remove unused package dependencies of stringr and scales. 

# shinyssdtools 0.3.4 (2024-12-12)

- Update plotting functions to match new arguments in ssdtools.
- Code rendering is now simplified as most plot 'add-ons' have been incorporated into `ssd_plot()`.
- French big.mark fixed.

# shinyssdtools 0.3.3 (2024-12-05)

- Added label size to rendered code for prediction plot. 

# shinyssdtools 0.3.2 (2024-11-28)

- Adding TESTING.md file for steps on how to manually test app.
- Internal changes.

# shinyssdtools 0.3.1 (2024-11-08)

- Fix prediction plot x-axis label/breaks bugs 
  - incorrect breaks after transform
  - incorrect bold face if breaks outside limits
  - label function breaking if breaks outside limits and converted to NA

# shinyssdtools 0.3.0 (2024-10-01)

- More patches for compatibility with ssdtools CRAN version 2.0.0
- Removed at_boundary_ok and computable UI inputs
- Fixed bug in predict plot dotted line caused by rounding estimated hazard concentration 
- Improved plot axis labels (i.e., in both French and English versions)


# shinyssdtools 0.2.0 (2024-02-09)

- Updates for compatibility with ssdtools CRAN version 2.0.0
- Add BCANZ Report tab to generate PDF/HTML report from Rmd
- Tweaks to Fit/Predict plot formatting and user options

# shinyssdtools 0.1.1 (2022-04-14)

- Update deployment script.


# shinyssdtools 0.1.0.9000 (2022-04-13)

- Same as previous version.


# shinyssdtools 0.1.0 (2022-04-13)

- Updates for compatibility with ssdtools CRAN version 1.0.1


# shinyssdtools 0.0.3.9001 (2021-04-03)

- Add `run_app()` alternative to `run_ssdtools_app()`.


# shinyssdtools 0.0.3.9000 (2021-04-02)

- Internal changes only.
