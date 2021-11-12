# This script contains functions for pre-processing PLUMv2 output before running
# downscaling.

#' Calculate land-use delta values
#'
#' Calculates the change in each land cover type (delta) between two timesteps
#'   of PLUMv2 output.
#'
#' @param LC_map_1 Land cover data frame.
#' @param LC_map_2 Second land cover data frame from the subsequent timestep.
#' @param LC_classes Vector of land cover classes, which must be column names in
#'   both data frames.
#'
#' @return A data frame with the delta value for each land cover type between
#'   the two input timesteps.
calculateLCDeltas <- function(LC_map_1,
                              LC_map_2,
                              LC_classes,
                              x_col = "x",
                              y_col = "y") {

  calculateLCDeltasInputChecks(LC_map_1 = LC_map_1,
                               LC_map_2 = LC_map_2,
                               LC_classes = LC_classes)

  coord_cols <- c(x_col, y_col)

  # Order data frame by coordinates
  LC_map_1_sorted <- LC_map_1[order(LC_map_1[ , x_col],
                                    LC_map_1[ , y_col]), ]
  LC_map_2_sorted <- LC_map_2[order(LC_map_2[ , x_col],
                                    LC_map_2[ , y_col]), ]

  # Throw error if coordinates do not match
  if (!identical(LC_map_1_sorted[ , coord_cols], LC_map_2_sorted[ , coord_cols])) {
    stop("Coordinates differ between the two timesteps")
  }

  # Set up x and y in data frame
  LC_map_coords <- LC_map_1_sorted[ , coord_cols]

  # Set up two LC-only data frame
  LC_map_1_sorted_LC <- LC_map_1_sorted[ , LC_classes]
  LC_map_2_sorted_LC <- LC_map_2_sorted[ , LC_classes]

  # Calculate deltas
  LC_deltas <- LC_map_2_sorted_LC - LC_map_1_sorted_LC

  # Create output data frame
  LC_deltas_with_coords <- cbind(LC_map_coords,
                                 LC_deltas)

  return(LC_deltas_with_coords)
}
