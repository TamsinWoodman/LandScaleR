
downscaleLCForOneCoarseCell <- function(coarse_cell,
                                        match_LC_classes,
                                        kernel_xy_dist,
                                        random_seed) {

  coarse_cell <- reconcileLCDeltas(x = coarse_cell,
                                   match_LC_classes = match_LC_classes)

  # Get transition matrix showing area of LU conversion between each LC class
  LC_transitions <- getLCTransitions(LC_deltas = lcDeltas(coarse_cell))

  updated_LC_maps <- allocateLCTransitions(coarse_cell = coarse_cell,
                                           LC_transitions = LC_transitions,
                                           kernel_xy_dist = kernel_xy_dist,
                                           random_seed = random_seed)

  return(updated_LC_maps)
}

#' Get the transition matrix for one cell
#'
#' The transition matrix determines the area of land cover change that is
#'   converted to each land cover class.
#'
#' @param LC_deltas
#'
#' @return Data frame giving order and magnitude of land cover transitions.
getLCTransitions <- function(LC_deltas) {

  # Get increasing and decreasing LC deltas
  inc_LC_deltas <- LC_deltas[LC_deltas > 0]
  dec_LC_deltas <- LC_deltas[LC_deltas < 0]

  # Can't allocated any land cover if the LC deltas are 0, so return NULL
  if (length(inc_LC_deltas) == 0 | length(dec_LC_deltas) == 0) {
    return(NULL)
  }

  # Sort increasing and decreasing LC deltas
  inc_LC_deltas_sorted <- sort(inc_LC_deltas,
                               decreasing = TRUE)
  dec_LC_deltas_sorted <- sort(dec_LC_deltas)

  # Sum LC deltas
  total_cell_LC_deltas <- sum(inc_LC_deltas_sorted)

  # Get vectors of LC classes and LC deltas
  LC_to_names <- rep(names(inc_LC_deltas_sorted),
                     each = length(dec_LC_deltas_sorted))
  LC_to_values <- rep(inc_LC_deltas_sorted,
                      each = length(dec_LC_deltas_sorted))
  LC_from_names <- rep(names(dec_LC_deltas_sorted),
                       times = length(inc_LC_deltas_sorted))
  LC_from_values <- rep(dec_LC_deltas_sorted,
                        times = length(inc_LC_deltas_sorted))

  # Calculate LC conversions
  LC_conversions <- LC_from_values * (LC_to_values / total_cell_LC_deltas)

  # Create data frame with conversions for each LC combination
  LC_transitions <- data.frame(LC_to_name = LC_to_names,
                               LC_from_name = LC_from_names,
                               LC_conversion = LC_conversions,
                               row.names = NULL)

  return(LC_transitions)
}

allocateLCTransitions <- function(coarse_cell,
                                  LC_transitions,
                                  kernel_xy_dist,
                                  random_seed) {

  # updated_assigned_ref_map_cells <- assigned_ref_map_cells
  # updated_agg_ref_map_cell <- agg_ref_map_cell
  # updated_LC_deltas_cell <- LC_deltas_cell

  if (!is.null(LC_transitions)) {

    for (i in 1:nrow(LC_transitions)) {

      LC_to_name <- LC_transitions[i , "LC_to_name"]
      LC_from_name <- LC_transitions[i , "LC_from_name"]
      LC_conversion_area <- abs(LC_transitions[i , "LC_conversion"])

      sum_ref_cells <- sumRefCells(coarse_cell)
      updated_ref_cells <- refCells(coarse_cell)

      # Check if aggregated reference map cell contains the land cover class to be converted
      if (sum_ref_cells[ , LC_from_name] > 0) {

        # Get cells for land cover allocation
        cells_for_allocation <- getCellsForAllocation(ref_map_df = updated_ref_cells,
                                                      LC_from_name = LC_from_name)

        if (nrow(cells_for_allocation) >= 1) {

          # Calculate kernel densities
          cells_for_allocation[ , "kernel_density"] <- apply(cells_for_allocation,
                                                             1,
                                                             calculateKernelDensitiesForOneCell,
                                                             ref_map_df = initial_ref_map_df,
                                                             LC_class = LC_to_name,
                                                             kernel_xy_dist = kernel_xy_dist)

          # Sort for cells for allocation depending on whether kernel density > 0 or kernel density == 0
          cells_for_allocation_sorted <- sortCellsForAllocation(cells_for_allocation = cells_for_allocation,
                                                                random_seed = random_seed)

          # Allocate land cover change
          cells_with_allocation <- getActualConversions(cells_for_allocation = cells_for_allocation_sorted,
                                                        LC_from_name = LC_from_name,
                                                        LC_conversion_area = LC_conversion_area)

          # Update the coarse-scale cell and all corresponding fine-scale cells
          # Update the reference map with new land cover areas
          updated_assigned_ref_map_cells <- updateRefMapWithLCConversions(ref_map_df = updated_assigned_ref_map_cells,
                                                                          LC_conversion_df = cells_with_allocation,
                                                                          LC_from_name = LC_from_name,
                                                                          LC_to_name = LC_to_name)

          # Calculate total conversion value
          total_conversion <- sum(cells_with_allocation$actual_conversion)

          # Update aggregated reference map
          updated_agg_ref_map_cell <- updateOneAggRefMapCellWithLCConversions(agg_ref_map_cell = updated_agg_ref_map_cell,
                                                                              total_conversion = total_conversion,
                                                                              LC_from_name = LC_from_name,
                                                                              LC_to_name = LC_to_name)

          # Update LC deltas
          updated_LC_deltas_cell <- updateOneLCDeltasCellWithLCConversions(LC_deltas_cell = updated_LC_deltas_cell,
                                                                           LC_to_name = LC_to_name,
                                                                           LC_from_name = LC_from_name,
                                                                           total_conversion = total_conversion)

        }
      }
    }
  }

  updated_LC_deltas_cell_df <- as.data.frame(t(updated_LC_deltas_cell))

  updated_LC_map_cells <- list(ref_map_cells = updated_assigned_ref_map_cells,
                               agg_ref_map_cell = updated_agg_ref_map_cell,
                               LC_deltas_cell = updated_LC_deltas_cell_df)

  return(coarse_cell)
}

#' Get a data frame of candidate fine-scale cells for land cover allocation
#'
#' Fine-scale cells are selected for allocation if they contain the land cover
#'   class that is being converted to a different class.
#'
#' @inheritParams assignRefMapCells
#' @param LC_from_name Name of the land cover class that is being converted to
#'   `LC_to_name`.
#'
#' @return Data frame of fine-scale cells that contain `LC_from_name`.
getCellsForAllocation <- function(ref_map_df,
                                  LC_from_name) {

  cells_for_allocation <- ref_map_df[ref_map_df[ , LC_from_name] > 0, ]

  return(cells_for_allocation)
}

#' Sort cells before land cover allocation
#'
#' @param cells_for_allocation Data frame of fine-scale cells selected to
#'   receive a given land cover class.
#' @inheritParams downscaleLC
#'
#' @return Data frame of fine-scale cells ordered from highest to lowest kernel
#'   density. Cells with a kernel density value of zero are randomly sorted at
#'   the end of the data frame.
sortCellsForAllocation <- function(cells_for_allocation,
                                   random_seed) {

  if (any(cells_for_allocation$kernel_density > 0) & any(cells_for_allocation$kernel == 0)) {
    cells_for_allocation_kd_not_zero <- getCellsForAllocationKdNotZero(cells_for_allocation = cells_for_allocation)
    cells_for_allocation_kd_zero <- getCellsForAllocationKdZero(cells_for_allocation = cells_for_allocation,
                                                                random_seed = random_seed)
    cells_for_allocation_sorted <- rbind(cells_for_allocation_kd_not_zero,
                                         cells_for_allocation_kd_zero)

  } else if (any(cells_for_allocation$kernel_density > 0) & !any(cells_for_allocation$kernel == 0)) {
    cells_for_allocation_sorted <- getCellsForAllocationKdNotZero(cells_for_allocation = cells_for_allocation)

  } else if (!any(cells_for_allocation$kernel_density > 0) & any(cells_for_allocation$kernel == 0)) {
    cells_for_allocation_sorted <- getCellsForAllocationKdZero(cells_for_allocation = cells_for_allocation,
                                                               random_seed = random_seed)

  }

  return(cells_for_allocation_sorted)
}

#' Return a data frame of fine-scale cells with kernel density greater than 0
#'
#' @inheritParams sortCellsForAllocation
#'
#' @return Data frame of fine-scale cells with kernel density greater than 0,
#'   sorted from highest to lowest kernel density.
getCellsForAllocationKdNotZero <- function(cells_for_allocation) {

  cells_for_allocation_kd_not_zero <- cells_for_allocation[cells_for_allocation$kernel_density > 0, ]
  cells_for_allocation_kd_not_zero_sorted <- sortKernelDensities(kernel_density_df = cells_for_allocation_kd_not_zero)

  return(cells_for_allocation_kd_not_zero_sorted)
}

#' Return a data frame of fine-scale cells with kernel density equal to zero
#'
#' @inheritParams sortCellsForAllocation
#' @inheritParams downscaleLC
#'
#' @return Data frame of fine-scale cells with kernel density equal to 0 that
#'   have been randomly sorted.
getCellsForAllocationKdZero <- function(cells_for_allocation,
                                        random_seed) {

  cells_for_allocation_kd_zero <- cells_for_allocation[cells_for_allocation$kernel_density == 0, ]
  cells_for_allocation_kd_zero_random <- randomiseDataFrame(input_df = cells_for_allocation_kd_zero,
                                                            random_seed = random_seed)

  return(cells_for_allocation_kd_zero_random)
}

#' Calculate actual conversion areas for a set of grid cells
#'
#' For each grid cell, the actual conversion from a decreasing land cover class
#'   to an increasing land cover class is the minimum of the area of the
#'   decreasing land cover class in that cell and the remaining area of land to
#'   be converted within the coarse-scale cell.
#'
#' @inheritParams sortCellsForAllocation
#' @inheritParams getCellsForAllocation
#' @param LC_conversion_area Remaining area of land in the coarse-scale cell to
#'   be converted from `LC_from_name` (decreasing land cover class) to
#'   `LC_to_name` (increasing land cover class).
#'
#' @return Data frame of fine-scale cells with an additional column named
#'   `actual_conversion` that contains the actual conversion areas for each grid
#'   cell.
getActualConversions <- function(cells_for_allocation,
                                 LC_from_name,
                                 LC_conversion_area) {

  cells_for_allocation$actual_conversion <- 0
  LC_conversion_area_remaining <- LC_conversion_area

  for (m in 1:nrow(cells_for_allocation)) {

    cells_for_allocation$actual_conversion[m] <- min(cells_for_allocation[m, LC_from_name],
                                                     LC_conversion_area_remaining)

    LC_conversion_area_remaining <- LC_conversion_area_remaining - cells_for_allocation$actual_conversion[m]

    if (round(LC_conversion_area_remaining, 8) == 0) {
      break
    }
  }

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
#' @inheritParams getCellsForAllocation
#' @param LC_to_name Name of the land cover class that is increasing in the
#'   coarse-scale cell.
#'
#' @return Reference map data frame with updated land cover areas in the cells
#'   specified in the `LC_conversion_df` data frame.
updateRefMapWithLCConversions <- function(ref_map_df,
                                          LC_conversion_df,
                                          LC_from_name,
                                          LC_to_name) {

  updated_ref_map_df <- ref_map_df
  LC_conversion_df_increase <- LC_conversion_df[LC_conversion_df$actual_conversion > 0, ]

  for (i in 1:nrow(LC_conversion_df_increase)) {
    ref_ID <- LC_conversion_df_increase[i, "ref_ID"]
    actual_conversion <- LC_conversion_df_increase[i, "actual_conversion"]
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
#' @inheritParams updateRefMapWithLCConversions
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

# Update one aggregated reference map cell with land cover conversions
updateOneAggRefMapCellWithLCConversions <- function(agg_ref_map_cell,
                                                    total_conversion,
                                                    LC_from_name,
                                                    LC_to_name) {

  agg_ref_map_cell_updated <- agg_ref_map_cell

  # Add total conversion to increasing LC
  agg_ref_map_cell_updated[ , LC_to_name] <- agg_ref_map_cell[ , LC_to_name] + total_conversion

  # Substract total conversion from decresing LC
  agg_ref_map_cell_updated[ , LC_from_name] <- agg_ref_map_cell[ , LC_from_name] - total_conversion

  return(agg_ref_map_cell_updated)
}

# Update one LC deltas cell with land cover conversions
updateOneLCDeltasCellWithLCConversions <- function(LC_deltas_cell,
                                                   LC_to_name,
                                                   LC_from_name,
                                                   total_conversion) {

  updated_LC_deltas_cell <- LC_deltas_cell

  # Update LC_to_delta in this for loop and in data frame
  LC_to_delta <-  updated_LC_deltas_cell[LC_to_name]
  updated_LC_deltas_cell[LC_to_name] <- updateLCToDelta(LC_to_delta,
                                                           total_conversion)

  # Update LC_from_delta in data frame
  LC_from_delta <- LC_deltas_cell[LC_from_name]
  updated_LC_deltas_cell[LC_from_name] <- updateLCFromDelta(LC_from_delta,
                                                               total_conversion)

  return(updated_LC_deltas_cell)
}

#' Update land cover change (delta) area of an increasing land cover class
#'
#' Updates the land cover delta value of an increasing land cover class, by
#'   subtracting the total increase of that land cover within one coarse-scale
#'   cell from the initial delta value.
#'
#' @param LC_to_delta Initial delta value for the increasing land cover class.
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
#' @param LC_from_delta Initial delta value for the decreasing land cover class.
#' @inheritParams updateLCToDelta
#'
#' @return The new land cover delta value for one land cover class.
updateLCFromDelta <- function(LC_from_delta,
                              total_conversion) {

  LC_from_delta_updated <- LC_from_delta + total_conversion
  LC_from_delta_updated_rounded <- round(LC_from_delta_updated,
                                         8)

  return(LC_from_delta_updated_rounded)
}
