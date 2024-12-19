<!-- NEWS.md is maintained by https://fledge.cynkra.com, contributors should not edit this file -->

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
