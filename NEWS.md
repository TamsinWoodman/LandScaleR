
# Version 1.1.0

Dependencies:
- `LandScaleR` now depends on `terra` version >=1.6-53

New functionality:
- LULC change maps can contain LULC change for each class as a proportion of a grid cell
- The LULC `LC_deltas_classes` input argument has been removed as the LULC change map classes are now derived from the `match_LC_classes` matrix

Bug fixes:
- The unit for calculating cell sizes is now provided to the `loadLCDeltas` function so that cell areas are calculated with the user-specified units

# Version 1.0.2

Documentation:
- Added README file
- Updated documentation for `downscaleLC()`
- Updated documentation for `assignRefMapCells()`
- Wrote 'Using landdownscaleR' vignette
- Changed name of package from `landdownscaleR` to `LandScaleR`
- Added GLP-3 license information

Bug fixes:
- The fuzzy method now randomly orders kernel densities if all kernel density values are equal to zero

# Version 1.0.1

Bug fixes:
- Mosiac code now knits together the whole of the UK instead of missing out cells
- The projection of reference map polygons is now set to be the same as the reference map
- A test was added to check that the `assignRefMap` function returns polygons with the same projection as the reference map
- All unit tests and the R CMD check were passing
- An input check was added to ensure that all row names of the `match_LC_classes` data frame are names of layers in the land cover change map
- An input check was added to ensure all column names of the `match_LC_classes` data frame are names of layers in the reference map

# Version 1.0.0

- First version of `landdownscaleR`
- Includes additional check that the projection of the reference and land cover change maps match

# Version 0.1.0

- Development version of `landdownscaleR`
