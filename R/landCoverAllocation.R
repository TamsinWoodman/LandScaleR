
downscaleLCForOneCoarseCell <- function(coarse_cell,
                                        match_LC_classes,
                                        LC_deltas_type,
                                        simulation_type,
                                        fuzzy_multiplier) {
  
  if (is.na(refCellsArea(coarse_cell))) {
    
    updated_coarse_cell <- coarse_cell
    
    warning("There are no reference map cells in coarse cell: ", 
            lcDeltasAndCoords(coarse_cell)[1],
            ", ",
            lcDeltasAndCoords(coarse_cell)[2])
    
  } else {
    
    # Get cell-specific match_LC_classes matrix
    cell_match_LC_classes <- getCellMatchLCClasses(match_LC_classes = match_LC_classes, 
                                                   coarse_cell = coarse_cell)
    
    coarse_cell <- reconcileLCDeltas(x = coarse_cell,
                                     match_LC_classes = cell_match_LC_classes,
                                     LC_deltas_type = LC_deltas_type)
    
    # Get transition matrix showing area of LU conversion between each LC class
    LC_transitions <- getLCTransitions(LC_deltas = lcDeltas(coarse_cell))
    
    updated_coarse_cell <- allocateLCTransitions(coarse_cell = coarse_cell,
                                                 LC_transitions = LC_transitions,
                                                 simulation_type = simulation_type,
                                                 fuzzy_multiplier = fuzzy_multiplier)
    
  }

  return(updated_coarse_cell)
}

# Updates the match_LC_classes matrix with any cell-specific ratios
getCellMatchLCClasses <- function(match_LC_classes, 
                                  coarse_cell) {
  
  cell_match_LC_classes <- match_LC_classes
  
  # Update match LC classes matrix
  if (any(rowSums(cell_match_LC_classes) > 1)) {
    
    agg_ref_cells <- coarse_cell@agg_ref_cells
    
    for (i in 1:nrow(cell_match_LC_classes)) {
      
      if (sum(cell_match_LC_classes[i, ]) > 1) {
        
        row_to_update <- cell_match_LC_classes[i, ]
        
        proportional_ref_classes <- names(row_to_update)[which(row_to_update == 1)]
        
        proportional_ref_areas <- agg_ref_cells[proportional_ref_classes]
        proportional_ref_areas <- round(proportional_ref_areas, 8)
        
        if (all(proportional_ref_areas == 0)) {
          
          # This if statement sets the proportions to be equal if none of the 
          # LULC classes to be split proportionally are present in the reference
          # map grid cells
          proportions_for_matching <- proportional_ref_areas
          proportions_for_matching[proportional_ref_classes] <- 1 / length(proportional_ref_classes)
          
        } else {
          
          proportions_for_matching <- proportional_ref_areas / sum(proportional_ref_areas)
        }
        
        cell_match_LC_classes[i, proportional_ref_classes] <- proportions_for_matching
      }
    }
  }
  
  return(cell_match_LC_classes)
}

#' Get the transition matrix for one cell
#'
#' The transition matrix determines the area of land cover change that is
#'   converted to each land cover class.
#'
#' @param LC_deltas Vector of land-use change areas.
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
                                  simulation_type,
                                  fuzzy_multiplier) {

  if (!is.null(LC_transitions)) {

    updated_ref_cells <- refCells(coarse_cell)
    updated_LC_deltas <- lcDeltas(coarse_cell)
    updated_agg_ref_cells <- aggRefCells(coarse_cell)
    kernel_densities <- kernelDensities(coarse_cell)

    for (i in 1:nrow(LC_transitions)) {

      LC_from_name <- LC_transitions[i, "LC_from_name"]

      # Check if aggregated reference map cell contains the land cover class to be converted
      if (updated_agg_ref_cells[LC_from_name] > 0 & !is.na(updated_agg_ref_cells[LC_from_name])) {

        LC_to_name <- LC_transitions[i, "LC_to_name"]
        LC_conversion_area <- abs(LC_transitions[i, "LC_conversion"])

        # Get cells for land cover allocation
        cell_numbers_for_allocation <- getCellsForAllocation(ref_map = updated_ref_cells,
                                                             LC_from_name = LC_from_name)

        if (length(cell_numbers_for_allocation) >= 1) {

          cells_for_allocation <- getAllocationDF(cell_numbers_for_allocation = cell_numbers_for_allocation,
                                                  updated_ref_cells = updated_ref_cells,
                                                  LC_from_name = LC_from_name,
                                                  LC_to_name = LC_to_name,
                                                  kernel_densities = kernel_densities)

          # print("Original:")
          # print(cells_for_allocation$kernel_density)
          # hist(cells_for_allocation$kernel_density)#

          # Sort the cells for allocation and add deviations if simulation type is fuzzy
          cells_for_allocation <- switch(simulation_type,
                                         "null_model" = randomiseDataFrame(input_df = cells_for_allocation),
                                         "deterministic" = sortCellsForAllocation(cells_for_allocation = cells_for_allocation),
                                         "fuzzy" = getFuzzyKernelDensities(cells_for_allocation = cells_for_allocation,
                                                                           fuzzy_multiplier = fuzzy_multiplier))

          # print("New:")
          # print(cells_for_allocation$kernel_density)
          # hist(cells_for_allocation$kernel_density)

          # Allocate land cover change
          cells_for_allocation <- getActualConversions(cells_for_allocation = cells_for_allocation,
                                                       LC_conversion_area = LC_conversion_area)

          # Update the coarse-scale cell and all corresponding fine-scale cells
          # Update the reference map with new land cover areas
          updated_ref_cells <- updateRefCells(cells_for_allocation = cells_for_allocation,
                                              updated_ref_cells = updated_ref_cells,
                                              LC_to_name = LC_to_name,
                                              LC_from_name = LC_from_name)

          # Calculate total conversion value
          total_conversion <- getTotalConversion(cells_for_allocation = cells_for_allocation)

          # Update aggregated reference map
          updated_agg_ref_cells <- updateAggRefCells(updated_agg_ref_cells = updated_agg_ref_cells,
                                                     LC_to_name = LC_to_name,
                                                     total_conversion = total_conversion,
                                                     LC_from_name = LC_from_name)

          # Update LC deltas
          updated_LC_deltas <- updateLCDeltas(updated_LC_deltas = updated_LC_deltas,
                                              LC_to_name = LC_to_name,
                                              total_conversion = total_conversion,
                                              LC_from_name = LC_from_name)

        }
      }
    }

    lcDeltas(coarse_cell) <- updated_LC_deltas
    refCells(coarse_cell) <- updated_ref_cells
    aggRefCells(coarse_cell) <- updated_agg_ref_cells
  }

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
getCellsForAllocation <- function(ref_map,
                                  LC_from_name) {

  cells_for_allocation_raster <- terra::ifel(ref_map[[LC_from_name]] > 0,
                                             1,
                                             NA)
  cells_for_allocation <- terra::cells(cells_for_allocation_raster)

  return(cells_for_allocation)
}

getAllocationDF <- function(cell_numbers_for_allocation,
                            updated_ref_cells,
                            LC_from_name,
                            LC_to_name,
                            kernel_densities) {

  # Set up data frame
  cells_for_allocation <- data.frame(cell_number = cell_numbers_for_allocation,
                                     dec_LC_area = updated_ref_cells[cell_numbers_for_allocation][LC_from_name],
                                     kernel_density = kernel_densities[cell_numbers_for_allocation][LC_to_name],
                                     actual_conversion = 0)
  colnames(cells_for_allocation)[2] <- "dec_LC_area"
  colnames(cells_for_allocation)[3] <- "kernel_density"

  # Put fix in to convert NaN kernel density values to 0
  # A kernel density of NaN can occur if all the cells surrounding a
  # focal cell are empty (NaN or NA)
  cells_for_allocation$kernel_density <- ifelse(is.na(cells_for_allocation$kernel_density),
                                                0,
                                                cells_for_allocation$kernel_density)

  return(cells_for_allocation)
}

#' Sort cells before land cover allocation
#'
#' @param cells_for_allocation Data frame of fine-scale cells selected to
#'   receive a given land cover class.
#'
#' @return Data frame of fine-scale cells ordered from highest to lowest kernel
#'   density. Cells with a kernel density value of zero are randomly sorted at
#'   the end of the data frame.
sortCellsForAllocation <- function(cells_for_allocation) {

  if (any(cells_for_allocation$kernel_density > 0) & any(cells_for_allocation$kernel == 0)) {
    cells_for_allocation_kd_not_zero <- getCellsForAllocationKdNotZero(cells_for_allocation = cells_for_allocation)
    cells_for_allocation_kd_zero <- getCellsForAllocationKdZero(cells_for_allocation = cells_for_allocation)
    cells_for_allocation_sorted <- rbind(cells_for_allocation_kd_not_zero,
                                         cells_for_allocation_kd_zero)

  } else if (any(cells_for_allocation$kernel_density > 0) & !any(cells_for_allocation$kernel == 0)) {
    cells_for_allocation_sorted <- getCellsForAllocationKdNotZero(cells_for_allocation = cells_for_allocation)

  } else if (!any(cells_for_allocation$kernel_density > 0) & any(cells_for_allocation$kernel == 0)) {
    cells_for_allocation_sorted <- getCellsForAllocationKdZero(cells_for_allocation = cells_for_allocation)

  }

  return(cells_for_allocation_sorted)
}

#' @importFrom stats sd
getFuzzyKernelDensities <- function(cells_for_allocation,
                                    fuzzy_multiplier) {

  if (length(cells_for_allocation$kernel_density) > 1) {

    if (sd(cells_for_allocation$kernel_density) == 0) {

      # If all kernel densities are zero then data frame won't be shuffled by the fuzzy method,
      # so we randomise the data frame here instead
      cells_for_allocation <- randomiseDataFrame(input_df = cells_for_allocation)

    } else {

      # Add deviations drawn from a Normal distribution to the kernel density values
      # This introduces stochasticity to the simulation
      cells_for_allocation$kernel_density <- cells_for_allocation$kernel_density + stats::rnorm(length(cells_for_allocation$kernel_density),
                                                                                                mean = 0,
                                                                                                sd = fuzzy_multiplier * stats::sd(cells_for_allocation$kernel_density,
                                                                                                                                  na.rm = TRUE))

      # Sort cells
      cells_for_allocation <- sortKernelDensities(kernel_density_df = cells_for_allocation)

    }
  }

  return(cells_for_allocation)
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
getCellsForAllocationKdZero <- function(cells_for_allocation) {

  cells_for_allocation_kd_zero <- cells_for_allocation[cells_for_allocation$kernel_density == 0, ]
  cells_for_allocation_kd_zero_random <- randomiseDataFrame(input_df = cells_for_allocation_kd_zero)

  return(cells_for_allocation_kd_zero_random)
}

#' Sort a data frame with a 'kernel_density' column from highest to lowest
#'   kernel density
#'
#' @param kernel_density_df Data frame of grid cells with a column named
#'   `kernel_density` that contains a kernel density for each cell.
#'
#' @return Input data frame sorted from highest to lowest kernel density value.
sortKernelDensities <- function(kernel_density_df) {

  sorted_kernel_density_df <- kernel_density_df[order(kernel_density_df[ , "kernel_density"],
                                                      decreasing = TRUE), ]

  return(sorted_kernel_density_df)
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
                                 LC_conversion_area) {

  LC_conversion_area_remaining <- LC_conversion_area

  for (m in 1:nrow(cells_for_allocation)) {

    cells_for_allocation$actual_conversion[m] <- min(cells_for_allocation[m, "dec_LC_area"],
                                                     LC_conversion_area_remaining)

    LC_conversion_area_remaining <- LC_conversion_area_remaining - cells_for_allocation$actual_conversion[m]

    if (round(LC_conversion_area_remaining, 8) == 0) {
      break
    }
  }

  return(cells_for_allocation)
}

updateRefCells <- function(cells_for_allocation,
                           updated_ref_cells,
                           LC_to_name,
                           LC_from_name) {

  cell_numbers_for_allocation <- cells_for_allocation$cell_number
  actual_conversions <- cells_for_allocation$actual_conversion

  updated_ref_cells[[LC_to_name]][cell_numbers_for_allocation] <- unlist(updated_ref_cells[[LC_to_name]][cell_numbers_for_allocation]) + actual_conversions
  updated_ref_cells[[LC_from_name]][cell_numbers_for_allocation] <- unlist(updated_ref_cells[[LC_from_name]][cell_numbers_for_allocation]) - actual_conversions

  return(updated_ref_cells)
}

getTotalConversion <- function(cells_for_allocation) {

  total_conversion <- sum(cells_for_allocation$actual_conversion)

  return(total_conversion)
}

updateAggRefCells <- function(updated_agg_ref_cells,
                              LC_to_name,
                              total_conversion,
                              LC_from_name) {

  updated_agg_ref_cells[LC_to_name] <- updated_agg_ref_cells[LC_to_name] + total_conversion
  updated_agg_ref_cells[LC_from_name] <- updated_agg_ref_cells[LC_from_name] - total_conversion

  return(updated_agg_ref_cells)
}

updateLCDeltas <- function(updated_LC_deltas,
                           LC_to_name,
                           total_conversion,
                           LC_from_name) {

  updated_LC_deltas[LC_to_name] <- updated_LC_deltas[LC_to_name] - total_conversion
  updated_LC_deltas[LC_from_name] <- updated_LC_deltas[LC_from_name] + total_conversion

  return(updated_LC_deltas)
}
