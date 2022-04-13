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
#'   the two input timesteps. The function works by subtracting land cover areas
#'   in `LC_map_1` from those in `LC_map_2`.
calculateLCDeltas <- function(LC_map_1,
                              LC_map_2,
                              LC_classes,
                              x_col = "x",
                              y_col = "y") {

  calculateLCDeltasInputChecks(LC_map_1 = LC_map_1,
                               LC_map_2 = LC_map_2,
                               LC_classes = LC_classes,
                               x_col = x_col,
                               y_col = y_col)

  coord_cols <- c(x_col, y_col)

  # Order data frame by coordinates
  LC_map_1_sorted <- LC_map_1[order(LC_map_1[ , x_col],
                                    LC_map_1[ , y_col]), ]
  LC_map_2_sorted <- LC_map_2[order(LC_map_2[ , x_col],
                                    LC_map_2[ , y_col]), ]

  # Remove row numbers for comparisons
  LC_map_1_coords <- LC_map_1_sorted[ , coord_cols]
  row.names(LC_map_1_coords) <- NULL
  LC_map_2_coords <- LC_map_2_sorted[ , coord_cols]
  row.names(LC_map_2_coords) <- NULL

  # Throw error if coordinates do not match
  if (!identical(LC_map_1_coords, LC_map_2_coords)) {
    stop("Coordinates differ between the two timesteps")
  }

  # Set up x and y in data frame
  LC_map_coords <- LC_map_1_sorted[ , !names(LC_map_1_sorted) %in% LC_classes]

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

calculateLCDeltasInputChecks <- function(LC_map_1,
                                         LC_map_2,
                                         LC_classes,
                                         x_col,
                                         y_col) {
  if (!is.data.frame(LC_map_1)) {
    stop("Land cover map 1 is not a data frame.")
  }

  if (!is.data.frame(LC_map_2)) {
    stop("Land cover map 2 is not a data frame.")
  }

  if (!nrow(LC_map_1) == nrow(LC_map_2)) {
    stop("Land cover maps contain different numbers of rows.")
  }

  if (!is.vector(LC_classes)) {
    stop("Land cover classes are not in the form of a vector.")
  }

  if (!(all(LC_classes %in% colnames(LC_map_1)))) {
    stop("Land cover class columns are missing from land cover map 1.")
  }

  if(!(all(LC_classes %in% colnames(LC_map_2)))) {
    stop("Land cover class columns are missing from land cover map 2.")
  }

  if(!(all(colnames(LC_map_1) %in% c(LC_classes, x_col, y_col)))) {
    warning("There are one or more extra land cover classes in land cover map 1. These will not be included in output land cover change.")
  }

  if(!(all(colnames(LC_map_2) %in% c(LC_classes, x_col, y_col)))) {
    warning("There are one or more extra land cover classes in land cover map 2. These will not be included in output land cover change.")
  }

  if(!x_col %in% colnames(LC_map_1)) {
    stop("X-coordinate column name is missing from land cover map 1.")
  }

  if(!x_col %in% colnames(LC_map_2)) {
    stop("X-coordinate column name is missing from land cover map 2.")
  }

  if(!y_col %in% colnames(LC_map_1)) {
    stop("Y-coordinate column name is missing from land cover map 1.")
  }

  if(!y_col %in% colnames(LC_map_2)) {
    stop("Y-coordinate column name is missing from land cover map 2.")
  }
}
