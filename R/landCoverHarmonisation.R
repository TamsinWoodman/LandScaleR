# Harmonisation functions
# During harmonisation, the algorithm looks for nearby cells in which to put any
# land cover change that was not allocated to a specific coarse-scale cell

harmoniseUnallocatedLC <- function(coarse_cell_list) {

  for (i in 1:length(coarse_cell_list)) {

    if (isUnallocatedLC(coarse_cell_list[[i]])) {

      # Get the unallocated land cover change and neighbour cells
      updated_LC_deltas <- lcDeltas(coarse_cell_list[[i]])
      neighbour_cells <- neighbourCells(coarse_cell_list[[i]])

      for (j in 1:length(neighbour_cells)) {

        # Calculate transition matrix
        LC_transitions <- getLCTransitions(LC_deltas = updated_LC_deltas)

        # A NULL value for LC_transitions can occur if the land area was unequal
        # between two time steps
        if (!is.null(LC_transitions) &
            !is.na(neighbour_cells[j]) &
            as.character(neighbour_cells[j]) %in% names(coarse_cell_list)) {

          # Set the cell where you will try to allocated LC change
          neighbour_cell <- coarse_cell_list[[as.character(neighbour_cells[j])]]

          updated_ref_cells <- refCells(neighbour_cell)
          updated_agg_ref_cells <- aggRefCells(neighbour_cell)
          kernel_densities <- kernelDensities(neighbour_cell)

          for (k in 1:nrow(LC_transitions)) {

            LC_from_name <- LC_transitions[k, "LC_from_name"]

            # Check if aggregated reference map cell contains the land cover class to be converted
            if (updated_agg_ref_cells[LC_from_name] > 0 & !is.na(updated_agg_ref_cells[LC_from_name])) {

              LC_to_name <- LC_transitions[k, "LC_to_name"]
              LC_conversion_area <- abs(LC_transitions[k, "LC_conversion"])

              # Get cells for land cover allocation
              cell_numbers_for_allocation <- getCellsForAllocation(ref_map = updated_ref_cells,
                                                                   LC_from_name = LC_from_name)

              if (length(cell_numbers_for_allocation) >= 1) {

                cells_for_allocation <- getAllocationDF(cell_numbers_for_allocation = cell_numbers_for_allocation,
                                                        updated_ref_cells = updated_ref_cells,
                                                        LC_from_name = LC_from_name,
                                                        LC_to_name = LC_to_name,
                                                        kernel_densities = kernel_densities)

                # Sort for cells for allocation depending on whether kernel density > 0 or kernel density == 0
                cells_for_allocation <- sortCellsForAllocation(cells_for_allocation = cells_for_allocation)

                # Allocate land cover change
                cells_for_allocation <- getActualConversions(cells_for_allocation = cells_for_allocation,
                                                             LC_from_name = LC_from_name,
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

          lcDeltas(coarse_cell_list[[i]]) <- updated_LC_deltas
          refCells(neighbour_cell) <- updated_ref_cells
          aggRefCells(neighbour_cell) <- updated_agg_ref_cells
          coarse_cell_list[[as.character(neighbour_cells[j])]] <- neighbour_cell

        }

        if (all(round(updated_LC_deltas, 8) == 0)) {
          break
        }

      }

    }
  }

  ## Print warnings for unallocated land cover change
  for (m in 1:length(coarse_cell_list)) {

    if (isUnallocatedLC(coarse_cell_list[[m]])) {

      cell_LC_deltas <- lcDeltas(coarse_cell_list[[m]])

      warning("There is ",
              sum(cell_LC_deltas[cell_LC_deltas > 0]),
              " and ",
              sum(cell_LC_deltas[cell_LC_deltas < 0]),
              " unallocated land cover change in grid cell: ")
      print(lcDeltasAndCoords(coarse_cell_list[[m]]))

    }
  }

  return(coarse_cell_list)
}
