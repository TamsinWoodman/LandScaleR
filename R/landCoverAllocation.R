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
#' @param LC_allocation_params `LCAllocationParams` object containing all the
#'   parameters required to allocate land cover change to a reference map for a
#'   single timestep.
#'
#' @return New land cover data frame at the same resolution as the given
#'   reference map with the specified land cover change areas allocated.
runLCAllocation <- function(LC_allocation_params) {

  print(paste0("Starting land cover allocation..."))
  allocation_start_time <- Sys.time()
  intensify_start_time <- Sys.time()

  # Extract required parameters from object
  LC_deltas <- slot(LC_allocation_params,
                    "LC_deltas")
  intensification_ratio <- slot(LC_allocation_params,
                                "intensification_ratio")

  # Calculate intensification LC deltas
  LC_deltas_intensify <- multiplyLCDeltas(LC_deltas = LC_deltas,
                                          multiplication_factor = intensification_ratio)

  # Run first round of intensification
  LC_intensify_params <- LC_allocation_params
  slot(LC_intensify_params,
       "LC_deltas") <- LC_deltas_intensify

  intensified_LCs <- allocateLCs(LC_allocation_params = LC_intensify_params,
                                 allocation_type = "intensify")

  # Time check
  intensify_end_time <- Sys.time()
  timeCheckMessage(intensify_start_time,
                   intensify_end_time,
                   "Completed land cover intensification in ")
  expand_start_time <- Sys.time()

  # Calculate expansion LC deltas
  expansion_factor <- 1 - intensification_ratio
  LC_deltas_expand <- multiplyLCDeltas(LC_deltas = LC_deltas,
                                       multiplication_factor = expansion_factor)

  # Run expansion
  LC_expand_params <- intensified_LCs
  slot(LC_expand_params,
       "LC_deltas") <- LC_deltas_expand

  expanded_LCs <- allocateLCs(LC_allocation_params = LC_expand_params,
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
                                         expanded_LCs@LC_deltas)

  # Run second round of intensification
  LC_intensify_two_params <- expanded_LCs
  slot(LC_intensify_two_params,
       "LC_deltas") <- LC_deltas_intensify_two

  final_LCs <- allocateLCs(LC_allocation_params = LC_intensify_two_params,
                           allocation_type = "intensify")

  # Time check
  intensify_two_end_time <- Sys.time()
  timeCheckMessage(intensify_two_start_time,
                   intensify_two_end_time,
                   "Completed second land cover intensification round in ")

  # Run checks on LC deltas and new reference map
  LCAllocationChecks(final_LCs = final_LCs)

  # Extract new reference map
  final_LCs_ref_map <- slot(final_LCs,
                            "ref_map")
  final_LCs_ref_map_df <- slot(final_LCs_ref_map,
                               "LC_map")

  # Time check
  allocation_end_time <- Sys.time()
  timeCheckMessage(allocation_start_time,
                   allocation_end_time,
                   "Completed land cover allocation in ")

  return(final_LCs_ref_map_df)
}

#' Multiply land cover change values
#'
#' Multiplies land cover change (delta) values by a user-specified multiplication
#'   factor. This allows the implementation of an intensification versus
#'   expansion factor that determines the amount of land cover which is
#'   intensified versus expanded during land cover allocation.
#'
#' @inheritParams loadRefMap
#' @param multiplication_factor Factor by which to multiply the land cover
#'   change values in the `LC_deltas` object.
#'
#' @return `LCMap` object containing land cover change values multiplied by the
#'   user-specified `multiplication_factor` parameter.
multiplyLCDeltas <- function(LC_deltas,
                             multiplication_factor) {

  LC_deltas_df <- slot(LC_deltas,
                       "LC_map")
  LC_classes <- slot(LC_deltas,
                     "LC_classes")

  multiplied_LC_deltas_df <- LC_deltas_df
  multiplied_LC_deltas_df[ , LC_classes] <- LC_deltas_df[ , LC_classes] * multiplication_factor

  multiplied_LC_deltas <- LC_deltas
  slot(multiplied_LC_deltas, "LC_map") <- multiplied_LC_deltas_df

  return(multiplied_LC_deltas)
}

#' Sum land cover change values
#'
#' Sums the land cover change values in two data frames. The land cover types
#'   and grid cells must be the same in each data frame.
#'
#' @param LC_deltas_one An `LCMap` object containing land cover change areas.
#' @param LC_deltas_two A second `LCMap` object containing land cover change
#'   areas.
#'
#' @return `LCMap` object containing the sum of land cover change from the two
#'   input objects.
sumLCDeltas <- function(LC_deltas_one,
                        LC_deltas_two) {

  # Extract variables from objects
  LC_deltas_one_df <- slot(LC_deltas_one,
                           "LC_map")
  LC_deltas_two_df <- slot(LC_deltas_two,
                           "LC_map")
  LC_classes <- slot(LC_deltas_two,
                     "LC_classes")

  # Sum LC deltas
  summed_LC_deltas_df <- LC_deltas_one_df
  summed_LC_deltas_df[ , LC_classes] <- LC_deltas_one_df[ , LC_classes] + LC_deltas_two_df[ , LC_classes]

  # Create new object
  summed_LC_deltas <- LC_deltas_one
  slot(summed_LC_deltas,
       "LC_map") <- summed_LC_deltas_df

  return(summed_LC_deltas)
}

#' Allocate land cover change to reference map
#'
#' Allocates land cover change (delta) values to the given reference map. Land
#'   cover classes can be allocated to cells where they already exist
#'   (intensified) or to cells where they do not exist (expanded). The type of
#'   land cover allocation can be set with the `allocation_type` argument.
#'
#' @inheritParams runLCAllocation
#' @param allocation_type The type of land cover allocation. Can be one of
#'   `intensify` or `expand`, which mean that land covers will be allocated to
#'   cells where they do or do not already occur, respectively.
#'
#' @return `LCAllocationParams` object with an updated reference map after land
#'   cover change allocation and a data frame with any unallocated land cover
#'   change areas.
allocateLCs <- function(LC_allocation_params,
                        allocation_type = "intensify") {

  # Extract variables from objects
  LC_deltas <- slot(LC_allocation_params,
                    "LC_deltas")
  ref_map <- slot(LC_allocation_params,
                  "ref_map")
  transition_priorities <- slot(LC_allocation_params,
                                "transition_priorities")
  kernel_radius <- slot(LC_allocation_params,
                        "kernel_radius")
  intensification_ratio <- slot(LC_allocation_params,
                                "intensification_ratio")

  updated_LC_deltas_df <- slot(LC_deltas,
                               "LC_map")
  updated_ref_map_df <- slot(ref_map,
                             "LC_map")
  ref_map_cell_resolution <- slot(ref_map,
                                  "cell_resolution")


  # Loop through the rows in the updated_LC_deltas_df data frame
  for (i in 1:nrow(updated_LC_deltas_df)) {
    coarse_ID <- updated_LC_deltas_df$coarse_ID[i]

    # Loop through the land-uses
    for (j in 1:nrow(transition_priorities)) {
      LC_to_name <- row.names(transition_priorities)[j]
      LC_to_delta <- updated_LC_deltas_df[i, LC_to_name]

      # Allocate the land cover if it increases in the cell
      if (LC_to_delta > 0) {

        # Get sorted list of priorities
        sorted_priority_LCs <- getSortedTransitionPriorities(transition_priorities,
                                                             j)

        # Loop through the other land covers in priority order
        for (k in 1:length(sorted_priority_LCs)) {
          LC_from_name <- sorted_priority_LCs[k]
          LC_from_delta <- updated_LC_deltas_df[i, LC_from_name]

          if (LC_from_delta < 0) {

            # Get cells to intensify land-use one in
            ### Getting the cells in which to expand/intensify LCs will be a
            ### switch statement --> then the rest of the function is the same
            ### irrespective of whether you're intensifying or expanding LCs
            cells_for_allocation <- switch(allocation_type,
                                           "intensify" = getCellsToIntensifyLC(ref_map_df = updated_ref_map_df,
                                                                               LC_to_name = LC_to_name,
                                                                               LC_from_name = LC_from_name,
                                                                               coarse_ID = coarse_ID),
                                           "expand" = getCellsToExpandLC(ref_map_df = updated_ref_map_df,
                                                                         LC_to_name = LC_to_name,
                                                                         LC_from_name = LC_from_name,
                                                                         coarse_ID = coarse_ID))

            if (nrow(cells_for_allocation) >= 1) {

              # Get LC increasing to LC decreasing conversions
              LC_conversion_df <- getLCConversions(cells_for_allocation = cells_for_allocation,
                                                   ref_map_df = updated_ref_map_df,
                                                   ref_map_cell_resolution = ref_map_cell_resolution,
                                                   kernel_radius = kernel_radius,
                                                   LC_to_name = LC_to_name,
                                                   LC_from_name = LC_from_name,
                                                   LC_to_delta = LC_to_delta,
                                                   LC_from_delta = LC_from_delta)

              # Update the reference map with new land cover areas
              updated_ref_map_df <- updateRefMapWithLCConversions(ref_map_df = updated_ref_map_df,
                                                                  LC_conversion_df = LC_conversion_df,
                                                                  LC_from_name = LC_from_name,
                                                                  LC_to_name = LC_to_name)

              # Calculate total conversion value
              total_conversion <- sum(LC_conversion_df$actual_conversion)

              # Update LC_to_delta in this for loop and in data frame
              LC_to_delta <- updateLCToDelta(LC_to_delta,
                                               total_conversion)
              updated_LC_deltas_df[i, LC_to_name] <- LC_to_delta

              # Update LC_from_delta in data frame
              updated_LC_deltas_df[i, LC_from_name] <- updateLCFromDelta(LC_from_delta,
                                                                    total_conversion)
            }
          }
        }

      }

    }
  }

  # Return LCAllocationParams object
  updated_ref_map <- ref_map
  slot(updated_ref_map,
       "LC_map") <- updated_ref_map_df

  updated_LC_deltas <- LC_deltas
  slot(updated_LC_deltas,
       "LC_map") <- updated_LC_deltas_df

  allocated_LCs <- new("LCAllocationParams",
                       ref_map = updated_ref_map,
                       LC_deltas = updated_LC_deltas,
                       transition_priorities = transition_priorities,
                       kernel_radius = kernel_radius,
                       intensification_ratio = intensification_ratio)

  return(allocated_LCs)
}

#' Get ordered vector of transition priorities for allocation of one land cover
#'   class
#'
#' Takes a land cover transition priorities matrix and a row number, and returns
#'   an vector with the order of land cover classes in that row.
#'
#' @inheritParams downscaleLC
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

#' Get cells in which to intensify one land cover class
#'
#' Find cells which match the conditions for intensifying one land cover class
#'   into a second. Cells meet the criteria for intensification if both land
#'   cover classes are greater than 0 within the cell.
#'
#' @inheritParams assignRefMapCells
#' @param LC_to_name Name of the land cover class that is increasing.
#' @param LC_from_name Name of the land cover class that is being converted to
#'   `LC_to_name`.
#' @param coarse_ID Identification number of the coarse-scale cell within which
#'   `LC_to_name` has increased.
#'
#' @return Data frame of cells from the reference map that meet the criteria for
#'   intensification of one land cover class into another.
getCellsToIntensifyLC <- function(ref_map_df,
                                  LC_to_name,
                                  LC_from_name,
                                  coarse_ID) {

  cells_to_intensify <- ref_map_df[which(ref_map_df[ , LC_to_name] > 0 &
                                           ref_map_df[ , LC_from_name] > 0 &
                                           ref_map_df[ , "coarse_ID"] == coarse_ID), ]

  return(cells_to_intensify)
}

#' Get cells in which to expand one land cover class
#'
#' Find cells which match the conditions for expanding one land cover class into
#'   a second. Cells meet the criteria for expansion if the increasing land
#'   cover class is 0 and the decreasing land cover class is greater than 0
#'   within the cell.
#'
#' @inheritParams assignRefMapCells
#' @inheritParams getCellsToIntensifyLC
#'
#' @return Data frame of cells from the reference map that meet the criteria for
#'   expansion of one land cover class into another.
getCellsToExpandLC <- function(ref_map_df,
                               LC_to_name,
                               LC_from_name,
                               coarse_ID) {

  cells_to_expand <- ref_map_df[which(ref_map_df[ , LC_to_name] == 0 &
                                        ref_map_df[ , LC_from_name] > 0 &
                                        ref_map_df[ , "coarse_ID"] == coarse_ID), ]

  return(cells_to_expand)
}

#' Get amount of one land cover type converted into another
#'
#' Calculates the amount of one land cover type that is converted into a second
#'   land cover type for each reference map cell assigned to a coarse-scale
#'   cell.
#'
#' @inheritParams assignRefMapCells
#' @inheritParams getCellsToIntensifyLC
#' @inheritParams downscaleLC
#' @param cells_for_allocation Data frame of reference map cells to which the
#'   increasing land cover class will be allocated.
#' @param LC_to_delta The area by which the increasing land cover class grew in
#'   the coarse-scale cell the current timestep.
#' @param LC_from_delta The area by which the decreasing land cover class
#'   contracted in the coarse-scale cell in the current timestep.
#'
#' @return Two column data frame. First column contains IDs of reference map
#'   cells, second column is the amount of the increasing land cover class
#'   allocated to each cell.
getLCConversions <- function(cells_for_allocation,
                             ref_map_df,
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
                                                   ref_map_df = ref_map_df,
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
#'   land cover class delta values.
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
#'   of one land cover class in a coarse-scale cell and the kernel density
#'   values of reference map cells selected for land cover allocation.
#'
#' @param LC_delta_for_tc Change in area of one land cover class (delta) within
#'   one coarse-scale grid cell.
#' @param kernel_densities Vector of kernel densities for the land cover class
#'   of interest within reference map cells selected for land cover allocation.
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
#' @param LC_from_values Vector of the current area of the decreasing land cover
#'   class within reference cells selected for land cover allocation.
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
LCAllocationChecks <- function(final_LCs) {

  # Extract params
  final_LCs_deltas <- slot(final_LCs,
                           "LC_deltas")
  final_LCs_deltas_df <- slot(final_LCs_deltas,
                              "LC_map")
  final_LCs_deltas_classes <- slot(final_LCs_deltas,
                                   "LC_classes")
  final_LCs_ref_map <- slot(final_LCs,
                            "ref_map")
  final_LCs_ref_map_df <- slot(final_LCs_ref_map,
                               "LC_map")
  final_LCs_ref_map_LC_classes <- slot(final_LCs_ref_map,
                                       "LC_classes")
  final_LCs_ref_map_cell_area <- slot(final_LCs_ref_map,
                                      "cell_area")

  # Check for unallocated land cover
  apply(final_LCs_deltas_df,
        1,
        checkForUnallocatedLandCover,
        LC_types = final_LCs_deltas_classes)

  # Check land cover areas in the fine-scale map
  apply(final_LCs_ref_map_df,
        1,
        checkLandCoverAreasInOneCell,
        total_area = final_LCs_ref_map_cell_area,
        LC_types = final_LCs_ref_map_LC_classes)

}
