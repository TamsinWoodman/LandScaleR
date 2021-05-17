# This script contains functions for pre-processing PLUMv2 output before running
# downscaling.

#' Calculate land-use delta values
#'
#' Calculates the change in each land cover type (delta) between two timesteps
#'   of PLUMv2 output.
#'
#' @param coarse_scale_df_time_1 Coarse-scale land cover data frame.
#' @param coarse_scale_df_time_2 Second coarse-scale land cover data frame from
#'   the subsequent timestep.
#' @param LC_types
#'
#' @return A data frame with the delta value for each land cover type between
#'   the two input timesteps.
calculateLCDeltas <- function(coarse_scale_df_time_1,
                              coarse_scale_df_time_2,
                              LC_types) {

  # Order data frame by coordinates
  coarse_scale_df_time_1_sorted <- coarse_scale_df_time_1[order(coarse_scale_df_time_1$x,
                                                                coarse_scale_df_time_1$y), ]
  coarse_scale_df_time_2_sorted <- coarse_scale_df_time_2[order(coarse_scale_df_time_2$x,
                                                                coarse_scale_df_time_2$y), ]

  # Throw error if coordinates do not match
  if (!identical(coarse_scale_df_time_1_sorted[ , c("x", "y")], coarse_scale_df_time_2_sorted[ , c("x", "y")])) {
    stop("Coordinates differ between the two timesteps")
  }

  # Set up x and y in data frame
  coarse_scale_df_coords <- coarse_scale_df_time_1_sorted[ , c("x", "y")]

  # Set up two LC-only data frame
  coarse_scale_df_time_1_sorted_LC <- coarse_scale_df_time_1_sorted[ , LC_types]
  coarse_scale_df_time_2_sorted_LC <- coarse_scale_df_time_2_sorted[ , LC_types]

  # Calculate deltas
  LC_deltas <- coarse_scale_df_time_2_sorted_LC - coarse_scale_df_time_1_sorted_LC

  # Create output data frame
  LC_deltas_with_coords <- cbind(coarse_scale_df_coords,
                                 LC_deltas)

  return(LC_deltas_with_coords)
}
