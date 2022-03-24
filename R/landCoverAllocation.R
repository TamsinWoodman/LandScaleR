
runLCAllocation <- function(LC_allocation_params) {

  print(paste0("Starting land cover allocation..."))
  allocation_start_time <- Sys.time()
  intensify_start_time <- Sys.time()

  # Extract variables from object
  LC_deltas <- slot(LC_allocation_params,
                    "LC_deltas")
  ref_map <- slot(LC_allocation_params,
                  "ref_map")
  kernel_radius <- slot(LC_allocation_params,
                        "kernel_radius")

  updated_LC_deltas_df <- slot(LC_deltas,
                               "LC_map")
  updated_ref_map_df <- slot(ref_map,
                             "LC_map")
  LC_classes <- slot(ref_map,
                     "LC_classes")
  ref_map_cell_resolution <- slot(ref_map,
                                  "cell_resolution")
  ref_map_cell_area <- slot(ref_map,
                            "cell_area")

  kernel_xy_dist <- calculateXYKernelDistances(ref_map_cell_resolution = ref_map_cell_resolution,
                                               kernel_radius = kernel_radius)

  # Loop through the rows in the updated_LC_deltas_df data frame
  for (i in 1:nrow(updated_LC_deltas_df)) {

    # get cell ID
    coarse_ID <- updated_LC_deltas_df$coarse_ID[i]

    # Get transition matrix showing area of LU conversion between each LC class
    transition_matrix <- getTransitionMatrix(LC_deltas_cell = updated_LC_deltas_df[i, ],
                                             LC_classes = LC_classes)

    # Loop through increasing land covers
    for (j in 1:nrow(transition_matrix)) {
      LC_to_name <- rownames(transition_matrix)[j]

        # Loop through decreasing land cover classes
        for (k in 1:ncol(transition_matrix)) {
          LC_from_name <- colnames(transition_matrix)[k]
          LC_conversion_area <- abs(transition_matrix[j, k])

          # Get cells for land cover allocation
          cells_for_allocation <- getCellsForAllocation(ref_map_df = updated_ref_map_df,
                                                        LC_from_name = LC_from_name,
                                                        coarse_ID = coarse_ID)

          if (nrow(cells_for_allocation) >= 1) {

            # Calculate kernel densities
            cells_for_allocation[ , "kernel_density"] <- apply(cells_for_allocation,
                                                               1,
                                                               calculateKernelDensitiesForOneCell,
                                                               ref_map_df = updated_ref_map_df,
                                                               LC_class = LC_to_name,
                                                               kernel_xy_dist = kernel_xy_dist)

            cells_for_allocation_sorted <- sortKernelDensities(kernel_density_df = cells_for_allocation)

            # Set up for LC allocation
            cells_for_allocation_sorted$actual_conversion <- 0
            LC_conversion_area_remaining <- LC_conversion_area

            for (m in 1:nrow(cells_for_allocation_sorted)) {

              cells_for_allocation_sorted$actual_conversion[m] <- min(cells_for_allocation_sorted[m, LC_from_name],
                                                               LC_conversion_area_remaining)

              LC_conversion_area_remaining <- LC_conversion_area_remaining - cells_for_allocation_sorted$actual_conversion[m]

              if (round(LC_conversion_area_remaining, 8) == 0) {
                break
              }

            }

            # Update the reference map with new land cover areas
            updated_ref_map_df <- updateRefMapWithLCConversions(ref_map_df = updated_ref_map_df,
                                                                LC_conversion_df = cells_for_allocation_sorted,
                                                                LC_from_name = LC_from_name,
                                                                LC_to_name = LC_to_name)
            # Calculate total conversion value
            total_conversion <- sum(cells_for_allocation_sorted$actual_conversion)

            # Update LC_to_delta in this for loop and in data frame
            LC_to_delta <- updated_LC_deltas_df[i, LC_to_name]
            updated_LC_deltas_df[i, LC_to_name] <- updateLCToDelta(LC_to_delta,
                                                                   total_conversion)

            # Update LC_from_delta in data frame
            LC_from_delta <- updated_LC_deltas_df[i, LC_from_name]
            updated_LC_deltas_df[i, LC_from_name] <- updateLCFromDelta(LC_from_delta,
                                                                       total_conversion)
          }
        }
    }
  }

  # Run checks on LC deltas and new reference map
  LCAllocationChecks(LC_deltas_df = updated_LC_deltas_df,
                     ref_map_df = updated_ref_map_df,
                     LC_classes = LC_classes,
                     ref_map_cell_area = ref_map_cell_area)

  # Time check
  allocation_end_time <- Sys.time()
  timeCheckMessage(allocation_start_time,
                   allocation_end_time,
                   "Completed land cover allocation in ")

  return(updated_ref_map_df)
}

getTransitionMatrix <- function(LC_deltas_cell,
                                LC_classes) {

  # Get delta values only
  LC_deltas_vector <- unlist(LC_deltas_cell)
  LC_deltas_only <- LC_deltas_vector[LC_classes]

  # Get increasing and decreasing LC deltas
  inc_LC_deltas <- LC_deltas_only[LC_deltas_only > 0]
  dec_LC_deltas <- LC_deltas_only[LC_deltas_only < 0]

  # Sort increasing and decreasing LC deltas
  inc_LC_deltas_sorted <- sort(inc_LC_deltas,
                               decreasing = TRUE)
  dec_LC_deltas_sorted <- sort(dec_LC_deltas)

  # Sum LC deltas
  total_cell_LC_deltas <- sum(inc_LC_deltas_sorted)

  transition_matrix <- matrix(data = NA,
                              nrow = length(inc_LC_deltas_sorted),
                              ncol = length(dec_LC_deltas_sorted),
                              dimnames = list(names(inc_LC_deltas_sorted),
                                              names(dec_LC_deltas_sorted)))

  # Get vector of LU transitions
  for (i in 1:length(inc_LC_deltas_sorted)) {
    inc_LC_delta <- inc_LC_deltas_sorted[i]
    inc_LC_delta_proportion <- inc_LC_delta / total_cell_LC_deltas

    for (j in 1:length(dec_LC_deltas_sorted)) {
      dec_LC_delta <- dec_LC_deltas_sorted[j]
      dec_LC_delta_area <- dec_LC_delta * inc_LC_delta_proportion

      transition_matrix[i, j] <- dec_LC_delta_area
    }
  }

  return(transition_matrix)
}

getCellsForAllocation <- function(ref_map_df,
                                  LC_from_name,
                                  coarse_ID) {

  cells_for_allocation <- ref_map_df[which(ref_map_df[ , LC_from_name] > 0 &
                                           ref_map_df[ , "coarse_ID"] == coarse_ID), ]

  return(cells_for_allocation)
}

#' Update reference map with land cover conversion values
#'
#' Updates a reference map with the actual land cover conversion areas for one
#'   timestep and land cover class.
#'
#' @inheritParams assignRefMapCells
#' @param LC_conversion_df Data frame with one column for actual land cover
#'   conversion areas and a second with reference map cell IDs. Output from the
#'   `getLCConversions` function.
#' @inheritParams getCellsToIntensifyLC
#'
#' @return Reference map data frame with updated land cover areas in the cells
#'   specified in the `LC_conversion_df` data frame.
updateRefMapWithLCConversions <- function(ref_map_df,
                                          LC_conversion_df,
                                          LC_from_name,
                                          LC_to_name) {

  updated_ref_map_df <- ref_map_df

  for (i in 1:nrow(LC_conversion_df)) {
    ref_ID <- LC_conversion_df[i, "ref_ID"]
    actual_conversion <- LC_conversion_df[i, "actual_conversion"]
    ref_map_cell <- updated_ref_map_df[updated_ref_map_df$ref_ID == ref_ID, ]

    # Update LCs in this cell
    updated_ref_map_df[updated_ref_map_df$ref_ID == ref_ID, ] <- updateOneRefMapCellWithLCConversions(ref_map_cell,
                                                                                                      LC_to_name,
                                                                                                      LC_from_name,
                                                                                                      actual_conversion)
  }

  return(updated_ref_map_df)
}

#' Update one reference map cell with land cover conversion areas
#'
#' Updates one cell from a reference map with land cover conversion areas from
#'   the conversion of one land cover class to another.
#'
#' @param ref_map_cell One row from a reference map data frame, which is
#'   equivalent to one grid cell.
#' @inheritParams getCellsToIntensifyLC
#' @param actual_conversion The actual area of land cover to be converted from
#'   the decreasing land cover (`LC_from_name`) to the increasing land cover
#'   (`LC_to_name`) in this reference map cell.
#'
#' @return A one row data frame with updated land cover areas.
updateOneRefMapCellWithLCConversions <- function(ref_map_cell,
                                                 LC_to_name,
                                                 LC_from_name,
                                                 actual_conversion) {

  updated_ref_map_cell <- ref_map_cell

  # Update area of the increasing LC
  current_LC_to_area <-  updated_ref_map_cell[ , LC_to_name]
  new_LC_to_area <- current_LC_to_area + actual_conversion
  updated_ref_map_cell[, LC_to_name] <- new_LC_to_area

  # Update area of the decreasing LC
  current_LC_from_area <-  updated_ref_map_cell[ , LC_from_name]
  new_LC_from_area <- current_LC_from_area - actual_conversion
  updated_ref_map_cell[ , LC_from_name] <- new_LC_from_area

  return(updated_ref_map_cell)
}

#' Update land cover change (delta) area of an increasing land cover class
#'
#' Updates the land cover delta value of an increasing land cover class, by
#'   subtracting the total increase of that land cover within one coarse-scale
#'   cell from the initial delta value.
#'
#' @inheritParams getLCConversions
#' @param total_conversion The total area of land converted to the given land
#'   cover class.
#'
#' @return The new land cover delta value for one land cover class.
updateLCToDelta <- function(LC_to_delta,
                            total_conversion) {

  LC_to_delta_updated <- LC_to_delta - total_conversion
  LC_to_delta_updated_rounded <- round(LC_to_delta_updated,
                                       8)

  return(LC_to_delta_updated_rounded)
}

#' Update land cover change (delta) area of a decreasing land cover class
#'
#' Updates the land cover delta value of a decreasing land cover class, by
#'   adding the total decrease of that land cover within one coarse-scale cell
#'   to the initial delta value.
#'
#' @inheritParams getLCConversions
#' @param total_conversion The total area of land converted from the given land
#'   cover class.
#'
#' @return The new land cover delta value for one land cover class.
updateLCFromDelta <- function(LC_from_delta,
                              total_conversion) {

  LC_from_delta_updated <- LC_from_delta + total_conversion
  LC_from_delta_updated_rounded <- round(LC_from_delta_updated,
                                         8)

  return(LC_from_delta_updated_rounded)
}

#' Check unallocated land cover change and the sum of land cover areas in each
#'   reference map cell
#'
#' @param finalLCs `LCAllocationParams` object containing an updated reference
#'   map and unallocated land cover change areas after land cover allocation for
#'   one timestep.
#'
#' @return Returns nothing. Checks for unallocated land cover change and that
#'   the sum of land cover areas in each reference map cell is equal to the
#'   original area of the cell.
LCAllocationChecks <- function(LC_deltas_df,
                               ref_map_df,
                               LC_classes,
                               ref_map_cell_area) {

  # Check for unallocated land cover
  apply(LC_deltas_df,
        1,
        checkForUnallocatedLandCover,
        LC_types = LC_classes)

  # Check land cover areas in the fine-scale map
  apply(ref_map_df,
        1,
        checkLandCoverAreasInOneCell,
        total_area = ref_map_cell_area,
        LC_types = LC_classes)

}
