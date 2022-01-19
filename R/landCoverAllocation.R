#' Run allocation of land covers to reference map by intensification and
#'   expansion
#'
#' This function will run the allocation of all land cover change (delta) values
#'   from a coarse-scale map, such as the output from one timestep of a global
#'   land-use model, to a fine-scale reference map. The method employed is from
#'   West et al. (2014) and Le Page et al. (2016). Land cover change is
#'   allocated in three steps: a first round of intensification, meaning land
#'   cover is allocated to cells where it already occurs; a round of expansion,
#'   where land cover is allocated to cells where it does not exist; and a final
#'   round of intensification to make sure all land cover change has been
#'   allocated.
#'
#'   The amount of land cover change that is allocated by intensification versus
#'   expansion is determined by the `intensification_ratio` parameter. An
#'   intensification ratio of 0.8 gives a target of 80 percent of land cover to
#'   be allocated via intensification and 20 percent by expansion.
#'
#'   The amount of land cover change for a single land cover type that is
#'   allocated to a fine-scale reference cell is currently determined by the
#'   kernel density value of that cell. A kernel density value is a relative
#'   measure of the amount of a land cover type in neighbouring cells.
#'
#' @inheritParams reconcileLCDeltas
#' @inheritParams calculateKernelDensities
#' @param LC_deltas Data frame of adjusted and aggregated land cover change
#'   (delta) values from the coarse-scale input map for every cell and land
#'   cover type.
#' @param transition_priorities Matrix containing transition priorities for land
#'   cover allocation. Each row of the matrix should give the order in which one
#'   land cover type is converted to others.
#' @param intensification_ratio Ratio of land intensification versus land
#'   expansion for the allocation algorithm.
#'
#' @return New land cover data frame with the given land cover change allocated.
runLCAllocation <- function(assigned_ref_map,
                            ref_map_cell_resolution,
                            kernel_radius,
                            LC_deltas,
                            transition_priorities,
                            intensification_ratio,
                            ref_map_cell_area) {

  print(paste0("Starting land cover allocation..."))
  allocation_start_time <- Sys.time()
  intensify_start_time <- Sys.time()

  LC_types <- row.names(transition_priorities)

  # Calculate intensification LC deltas
  LC_deltas_intensify <- multiplyLCDeltas(LC_deltas,
                                          LC_types,
                                          intensification_ratio)

  # Run first round of intensification
  intensified_LCs <- allocateLCs(assigned_ref_map = assigned_ref_map,
                                 ref_map_cell_resolution = ref_map_cell_resolution,
                                 kernel_radius = kernel_radius,
                                 LC_deltas = LC_deltas_intensify,
                                 transition_priorities = transition_priorities,
                                 allocation_type = "intensify")

  # Time check
  intensify_end_time <- Sys.time()
  timeCheckMessage(intensify_start_time,
                   intensify_end_time,
                   "Completed land cover intensification in ")
  expand_start_time <- Sys.time()

  # Calculate expansion LC deltas
  expansion_factor <- 1 - intensification_ratio
  LC_deltas_expand <- multiplyLCDeltas(LC_deltas,
                                       LC_types,
                                       expansion_factor)

  # Run expansion
  expanded_LCs <- allocateLCs(assigned_ref_map = intensified_LCs@ref_map_df,
                              ref_map_cell_resolution = ref_map_cell_resolution,
                              kernel_radius = kernel_radius,
                              LC_deltas = LC_deltas_expand,
                              transition_priorities = transition_priorities,
                              allocation_type = "expand")

  # Time check
  expand_end_time <- Sys.time()
  timeCheckMessage(expand_start_time,
                   expand_end_time,
                   "Completed land cover expansion in ")
  intensify_two_start_time <- Sys.time()

  # LC deltas for second round of intensification are the remaining LC delta
  # that were not allocated in either the intensification or expansion round
  LC_deltas_intensify_two <- sumLCDeltas(intensified_LCs@LC_deltas,
                                         expanded_LCs@LC_deltas,
                                         LC_types)

  # Run second round of intensification
  final_LCs <- allocateLCs(assigned_ref_map = expanded_LCs@ref_map_df,
                           ref_map_cell_resolution = ref_map_cell_resolution,
                           kernel_radius = kernel_radius,
                           LC_deltas = LC_deltas_intensify_two,
                           transition_priorities = transition_priorities,
                           allocation_type = "intensify")

  # Time check
  intensify_two_end_time <- Sys.time()
  timeCheckMessage(intensify_two_start_time,
                   intensify_two_end_time,
                   "Completed second land cover intensification round in ")

  # Check for unallocated land cover
  apply(final_LCs@LC_deltas,
        1,
        checkForUnallocatedLandCover,
        LC_types = LC_types)

  # Check land cover areas in the fine-scale map
  apply(final_LCs@ref_map_df,
        1,
        checkLandCoverAreasInOneCell,
        total_area = ref_map_cell_area,
        LC_types = LC_types)

  # Time check
  allocation_end_time <- Sys.time()
  timeCheckMessage(allocation_start_time,
                   allocation_end_time,
                   "Completed land cover allocation in ")

  return(final_LCs@ref_map_df)
}

#' Multiply land cover change values
#'
#' Multiples land cover change (delta) values by a user-specified multiplication
#'   factor. This allows the implementation of an intensification versus
#'   expansion factor that determines the amount of land cover which is
#'   intensified versus expanded during land cover allocation.
#'
#' @inheritParams runLCAllocation
#' @param LC_types Vector of land cover types in the `LC_deltas` data frame.
#' @param multiplication_factor Factor by which to multiply the land cover
#'   change values in the `LC_deltas` data frame.
#'
#' @return Data frame containing land cover change values multiplied by the
#'   user-specified `multiplication_factor` parameter.
multiplyLCDeltas <- function(LC_deltas,
                             LC_types,
                             multiplication_factor) {

  multiplied_LC_deltas <- LC_deltas
  multiplied_LC_deltas[ , LC_types] <- LC_deltas[ , LC_types] * multiplication_factor

  return(multiplied_LC_deltas)
}

#' Sum land cover change values
#'
#' Sums the land cover change values in two data frames. The land cover types
#'   and grid cells must be the same in each data frame.
#'
#' @param LC_deltas_one Data frame of land cover change values from a
#'   coarse-scale map.
#' @param LC_deltas_two Second data frame of land cover change values from a
#'   coarse-scale map.
#' @inheritParams multiplyLCDeltas
#'
#' @return Data frame containing the sum of land cover change from the two input
#'   land cover change data frames.
sumLCDeltas <- function(LC_deltas_one,
                        LC_deltas_two,
                        LC_types) {

  summed_LC_deltas <- LC_deltas_one
  summed_LC_deltas[ , LC_types] <- LC_deltas_one[ , LC_types] + LC_deltas_two[ , LC_types]

  return(summed_LC_deltas)
}

#' Allocate land cover change to reference map
#'
#' Allocates land cover change (delta) values to the given reference map. Land
#'   cover types can be allocated to cells where they already exist
#'   (intensified) or to cells where they do not exist (expanded). The type of
#'   land cover allocation can be set with the `allocation_type` argument.
#'
#' @inheritParams runLCAllocation
#' @param allocation_type The type of land cover allocation. Can be one of
#'   `intensify` or `expand`, which mean that land covers will be allocated to
#'   cells where they do or do not already occur, respectively.
#'
#' @return `LCDataClass` with a new reference map with land cover change
#'   allocated, data frame with land cover delta values, and the final land
#'   cover types in the reference map.
allocateLCs <- function(assigned_ref_map,
                        ref_map_cell_resolution,
                        kernel_radius,
                        LC_deltas,
                        transition_priorities,
                        allocation_type = "intensify") {

  updated_assigned_ref_map <- assigned_ref_map
  updated_LC_deltas <- LC_deltas

  # Loop through the rows in the updated_LC_deltas data frame
  for (i in 1:nrow(updated_LC_deltas)) {
    coarse_ID <- updated_LC_deltas$coarse_ID[i]

    # Loop through the land-uses
    for (j in 1:nrow(transition_priorities)) {
      LC_to_name <- row.names(transition_priorities)[j]
      LC_to_delta <- updated_LC_deltas[i, LC_to_name]

      # Allocate the land cover if it increases in the cell
      if (LC_to_delta > 0) {

        # Get sorted list of priorities
        sorted_priority_LCs <- getSortedTransitionPriorities(transition_priorities,
                                                             j)

        # Loop through the other land covers in priority order
        for (k in 1:length(sorted_priority_LCs)) {
          LC_from_name <- sorted_priority_LCs[k]
          LC_from_delta <- updated_LC_deltas[i, LC_from_name]

          if (LC_from_delta < 0) {

            # Get cells to intensify land-use one in
            ### Getting the cells in which to expand/intensify LCs will be a
            ### switch statement --> then the rest of the function is the same
            ### irrespective of whether you're intensifying or expanding LCs
            cells_for_allocation <- switch(allocation_type,
                                           "intensify" = getCellsToIntensifyLC(updated_assigned_ref_map,
                                                                 LC_to_name,
                                                                 LC_from_name,
                                                                 coarse_ID),
                                           "expand" = getCellsToExpandLC(updated_assigned_ref_map,
                                                              LC_to_name,
                                                              LC_from_name,
                                                              coarse_ID))

            if (nrow(cells_for_allocation) >= 1) {

              # Get LC increasing to LC decreasing conversions
              LC_conversion_df <- getLCConversions(cells_for_allocation = cells_for_allocation,
                                                   assigned_ref_map = assigned_ref_map,
                                                   ref_map_cell_resolution = ref_map_cell_resolution,
                                                   kernel_radius = kernel_radius,
                                                   LC_to_name = LC_to_name,
                                                   LC_from_name = LC_from_name,
                                                   LC_to_delta = LC_to_delta,
                                                   LC_from_delta = LC_from_delta)

              # Update the reference map with new land cover areas
              updated_assigned_ref_map <- updateRefMapWithLCConversions(updated_assigned_ref_map,
                                                                           LC_conversion_df,
                                                                           LC_from_name,
                                                                           LC_to_name)

              # Calculate total conversion value
              total_conversion <- sum(LC_conversion_df$actual_conversion)

              # Update LC_to_delta in this for loop and in data frame
              LC_to_delta <- updateLCToDelta(LC_to_delta,
                                               total_conversion)
              updated_LC_deltas[i, LC_to_name] <- LC_to_delta

              # Update LC_from_delta in data frame
              updated_LC_deltas[i, LC_from_name] <- updateLCFromDelta(LC_from_delta,
                                                                    total_conversion)
            }
          }
        }

      }

    }
  }

  allocated_LCs <- new("LCDataClass",
                       ref_map_df = updated_assigned_ref_map,
                       LC_deltas = updated_LC_deltas,
                       LC_types = row.names(transition_priorities))

  return(allocated_LCs)
}

#' Get ordered vector of transition priorities for allocation of one land cover
#'   type
#'
#' Takes a land cover transition priorities matrix and a row number, and returns
#'   an vector with the order of land cover types in that row.
#'
#' @inheritParams runLCAllocation
#' @param row_number Row number of the `transition_priorities` matrix containing
#'   the land use for which you want to return an ordered vector of transition
#'   priorities.
#'
#' @return Sorted vector of land cover transition priorities for the land cover
#'   in the given row.
getSortedTransitionPriorities <- function(transition_priorities,
                                          row_number) {

  all_sorted_priorities <- sort(transition_priorities[row_number, ])
  sorted_priorities <- all_sorted_priorities[-which(all_sorted_priorities == 0)]
  sorted_priority_LCs <- names(sorted_priorities)

  return(sorted_priority_LCs)
}

#' Get cells in which to intensify one land cover type
#'
#' Find cells which match the conditions for intensifying one land cover type
#'   into a second. Cells meet the criteria for intensification if both land
#'   cover types are greater than 0 within the cell.
#'
#' @inheritParams reconcileLCDeltas
#' @param LC_to_name Name of the land cover that is increasing.
#' @param LC_from_name Name of the land cover that is being converted to
#'   `LC_to_name`.
#' @param coarse_ID Identification number of the coarse-scale cell within which
#'   `LC_to_name` has increased.
#'
#' @return Data frame of cells from the reference map that meet the criteria for
#'   intensification of one land cover type into another.
getCellsToIntensifyLC <- function(assigned_ref_map,
                                  LC_to_name,
                                  LC_from_name,
                                  coarse_ID) {

  cells_to_intensify <- assigned_ref_map[which(assigned_ref_map[ , LC_to_name] > 0 &
                                                    assigned_ref_map[ , LC_from_name] > 0 &
                                                    assigned_ref_map[ , "coarse_ID"] == coarse_ID), ]

  return(cells_to_intensify)
}

#' Get cells in which to expand one land cover type
#'
#' Find cells which match the conditions for expanding one land cover type into
#'   a second. Cells meet the criteria for expansion if the increasing land
#'   cover is 0 and the decreasing land cover is greater than 0 within the cell.
#'
#' @inheritParams reconcileLCDeltas
#' @inheritParams getCellsToIntensifyLC
#'
#' @return Data frame of cells from the reference map that meet the criteria for
#'   expansion of one land cover type into another.
getCellsToExpandLC <- function(assigned_ref_map,
                               LC_to_name,
                               LC_from_name,
                               coarse_ID) {

  cells_to_expand <- assigned_ref_map[which(assigned_ref_map[ , LC_to_name] == 0 &
                                                 assigned_ref_map[ , LC_from_name] > 0 &
                                                 assigned_ref_map[ , "coarse_ID"] == coarse_ID), ]

  return(cells_to_expand)
}

#' Get amount of one land cover type converted into another
#'
#' Calculates the amount of one land cover type that is converted into a second
#'   land cover type for each reference map cell assigned to a coarse-scale
#'   cell.
#'
#' @inheritParams runLCAllocation
#' @inheritParams getCellsToIntensifyLC
#' @param cells_for_allocation Data frame of reference cells to which the
#'   increasing land cover type will be allocated.
#' @param LC_to_delta The land cover change amount (delta) for the increasing
#'   land cover type, which is equivalent to the amount that this land cover
#'   type increased in the coarse-scale cell during one timestep.
#' @param LC_from_delta The land cover delta value for the decreasing land cover
#'   type, which is the amount that this land cover type decreased in the
#'   coarse-scale cell during one timestep.
#'
#' @return Two column data frame. First column contains IDs of reference map
#'   cells, second column is the amount of the increasing land cover type
#'   allocated to each cell.
getLCConversions <- function(cells_for_allocation,
                             assigned_ref_map,
                             ref_map_cell_resolution,
                             kernel_radius,
                             LC_to_name,
                             LC_from_name,
                             LC_to_delta,
                             LC_from_delta) {

  # Get kernel densities for cells to allocate LC conversion to
  ### Want to calculate kernel densities here instead of just returning them
  ### from a data frame
  LC_conversion_df_tmp <- calculateKernelDensities(ref_map_cells_df = cells_for_allocation,
                                                   assigned_ref_map = assigned_ref_map,
                                                   LC_class = LC_to_name,
                                                   ref_map_cell_resolution = ref_map_cell_resolution,
                                                   kernel_radius = kernel_radius)

  # Calculate tentative conversion values
  # Max amount of LC change used for tentative conversion is the min
  # out of LC_to_delta and LC_to_delta
  LC_delta_for_tc <- getLCDeltaForTentativeConversion(LC_to_delta,
                                                      LC_from_delta)

  # Calculate tentative conversion values
  LC_conversion_df_tmp$tentative_conversion <- calculateTentativeConversionValues(LC_delta_for_tc,
                                                                                  LC_conversion_df_tmp[ , LC_to_name])

  # Calculate the actual conversion values
  LC_conversion_df_tmp$actual_conversion <- calculateActualConversion(cells_for_allocation[ , LC_from_name],
                                                                      LC_conversion_df_tmp$tentative_conversion)

  LC_conversion_df <- LC_conversion_df_tmp[ , c("ref_ID", "actual_conversion")]

  return(LC_conversion_df)
}

#' Get land cover delta area for tentative land cover conversions
#'
#' Returns land cover change area (delta) to be used in tentative land cover
#'   conversions, which is the minimum of the absolute increasing and decreasing
#'   land cover type delta values.
#'
#' @inheritParams getLCConversions
#'
#' @return Value of land cover delta to be used in tentative land cover
#'   conversions.
getLCDeltaForTentativeConversion <- function(LC_to_delta,
                                             LC_from_delta) {

  LC_delta_for_tc <- min(abs(LC_to_delta), abs(LC_from_delta))

  return(LC_delta_for_tc)
}

#' Calculate tentative conversion values
#'
#' Calculates tentative land cover conversion values based on the change in area
#'   of one
#'   land cover type in a coarse-scale cell, and the kernel density values of
#'   reference map cells assigned to the coarse-scale cell.
#'
#' @param LC_delta_for_tc Change in area of one land cover type (delta) within
#'   one coarse-scale grid cell.
#' @param kernel_densities Vector of kernel densities within reference map cells
#'   assigned to the coarse-scale cell, for the land cover type of interest.
#'
#' @return Vector with a tentative land cover conversion area for each reference
#'   map cell.
calculateTentativeConversionValues <- function(LC_delta_for_tc,
                                               kernel_densities) {

  sum_of_kernel_densities <- sum(kernel_densities, na.rm = TRUE)

  if (sum_of_kernel_densities > 0) {
    tentative_conversions <- LC_delta_for_tc * (kernel_densities / sum_of_kernel_densities)

  } else {
    tentative_conversions <- rep(0, length(kernel_densities))
  }

  return(tentative_conversions)
}

#' Calculate actual conversion values
#'
#' Calculates actual land cover conversion areas for each reference map cell,
#'   which is the minimum of the tentative conversion value for a cell and the
#'   amount of the decreasing land cover currently in that cell.
#'
#' @param LC_from_values Vector of the area of the land cover type to be
#'   decreased that is currently in a set of reference map cells.
#' @param tentative_conversion Vector with the tentative land cover conversion
#'   value for each reference map cell.
#'
#' @return Vector with the actual land cover conversion area for the given set
#'   of reference map cells.
calculateActualConversion <- function(LC_from_values,
                                      tentative_conversion) {

  actual_conversion <- pmin(LC_from_values,
                            tentative_conversion)

  return(actual_conversion)
}

#' Update reference map with land cover conversion values
#'
#' Updates a reference map with the actual land cover conversion areas for one
#'   timestep and land cover type.
#'
#' @inheritParams reconcileLCDeltas
#' @param LC_conversion_df Data frame with one column for actual land cover
#'   conversion areas and a second with reference map IDs. Output from the
#'   `getLCConversions` function.
#' @inheritParams getCellsToIntensifyLC
#'
#' @return Reference map data frame with updated land cover areas in the cells
#'   specified in the `LC_conversion_df` data frame.
updateRefMapWithLCConversions <- function(assigned_ref_map,
                                          LC_conversion_df,
                                          LC_from_name,
                                          LC_to_name) {

  updated_assigned_ref_map <- assigned_ref_map

  for (i in 1:nrow(LC_conversion_df)) {
    ref_ID <- LC_conversion_df[i, "ref_ID"]
    actual_conversion <- LC_conversion_df[i, "actual_conversion"]
    ref_map_cell <- updated_assigned_ref_map[updated_assigned_ref_map$ref_ID == ref_ID, ]

    # Update LCs in this cell
    updated_assigned_ref_map[updated_assigned_ref_map$ref_ID == ref_ID, ] <- updateOneRefMapCellWithLCConversions(ref_map_cell,
                                                                                              LC_to_name,
                                                                                              LC_from_name,
                                                                                              actual_conversion)
  }

  return(updated_assigned_ref_map)
}

#' Update one reference map cell with land cover conversion areas
#'
#' Updates one cell from a reference map with land cover conversion areas from
#'   the conversion of one land cover type to another.
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

#' Update land cover change (delta) area of an increasing land cover type
#'
#' Updates the land cover delta value of an increasing land cover type, by
#'   subtracting the total increase of that land cover within one coarse-scale
#'   cell from the initial delta value.
#'
#' @inheritParams getLCConversions
#' @param total_conversion The total area of land converted to the given land
#'   cover type.
#'
#' @return The new land cover delta value for one land cover type.
updateLCToDelta <- function(LC_to_delta,
                             total_conversion) {

  LC_to_delta_updated <- LC_to_delta - total_conversion
  LC_to_delta_updated_rounded <- round(LC_to_delta_updated,
                                        8)

  return(LC_to_delta_updated_rounded)
}

#' Update land cover change (delta) area of a decreasing land cover type
#'
#' Updates the land cover delta value of a decreasing land cover type, by
#'   adding the total decrease of that land cover within one coarse-scale cell
#'   to the initial delta value.
#'
#' @inheritParams getLCConversions
#' @param total_conversion The total area of land converted from the given land
#'   cover type.
#'
#' @return The new land cover delta value for one land cover type.
updateLCFromDelta <- function(LC_from_delta,
                             total_conversion) {

  LC_from_delta_updated <- LC_from_delta + total_conversion
  LC_from_delta_updated_rounded <- round(LC_from_delta_updated,
                                        8)

  return(LC_from_delta_updated_rounded)
}
