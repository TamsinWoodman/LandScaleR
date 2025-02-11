
# Version 1.1.0

Dependencies:
- `LandScaleR` now depends on `terra` version >=1.6-53
    - Note that `LandScaleR` is not currently compatible with versions 1.7-37 and 1.7-39 of `terra`

New functionality:
- LULC change maps can contain LULC change for each class as a proportion of a grid cell
    - LULC change proportions for one coarse resolution grid cell are multiplied by the total area of that cell to get LULC change areas for downscaling
- Removed the LULC `LC_deltas_classes` input argument as the LULC change map classes are now derived from the `match_LC_classes` matrix
- Added the `assign_ref_cells` input argument to allow for the individual assignment of reference map cells to LULC change map cells to be turned off
- Prints out time check for calculation of kernel densities
- Added a warning that layer names of area-based reference maps must not be numeric
- Added a warning if a coarse resolution grid cell has no fine resolution grid cells assigned to it
- New `harmonisation_radius` input argument which changes the search radius for harmonisation
- The proportion of a LULC change map class matched to each reference map class can now be calculated dynamically according to the proportions of reference map classes within each coarse resolution grid cell

Bug fixes:
- The unit for calculating cell sizes is now provided to the `loadLCDeltas` function so that cell areas are calculated with the user-specified units
- Updated the code to merge downscaled coarse resolution grid cells so that it can handle much larger maps

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
