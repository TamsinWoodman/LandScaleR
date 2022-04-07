
#' Check unallocated land cover change and the sum of land cover areas in each
#'   reference map cell
#'
LCAllocationChecks <- function(LC_deltas_df,
                               LC_classes,
                               ref_map_df,
                               ref_map_cell_area) {

  # Check for unallocated land cover
  unallocated_LC_deltas <- apply(LC_deltas_df,
                                 1,
                                 checkForUnallocatedLandCover,
                                 LC_types = LC_classes)

  # The apply function can return three objects:
  #     1. NULL if no cells have unallocated land cover
  #     2. A list if some cells have unallocated land cover
  #     3. A matrix if all cells have unallocated land cover
  # Handle all three cases below
  if (is.null(unallocated_LC_deltas)) {

    unallocated_LC_deltas_df <- unallocated_LC_deltas

  } else if (is.list(unallocated_LC_deltas)) {

    unallocated_LC_deltas_df <- do.call(rbind,
                                        unallocated_LC_deltas)
    unallocated_LC_deltas_df <- as.data.frame(unallocated_LC_deltas_df)

  } else if (is.matrix(unallocated_LC_deltas)) {

    unallocated_LC_deltas_df <- as.data.frame(t(unallocated_LC_deltas))

  }

  # Check land cover areas in the fine-scale map
  apply(ref_map_df,
        1,
        checkLandCoverAreasInOneCell,
        total_area = ref_map_cell_area,
        LC_types = LC_classes)

  return(unallocated_LC_deltas_df)
}

#' Check if sum of land cover area in one grid cell equals the expected total
#'   area
#'
#' Checks if the sum of all land cover types in one grid cell is within 0.1\% of
#'   the expected total area of that grid cell.
#'
#' @param grid_cell Single row from a land cover data frame as a named vector,
#'   corresponding to one grid cell in a map.
#' @param total_area The expected total area in the given grid cell.
#' @param LC_types Vector of land cover types in the grid cell. Land cover types
#'   should all be names of the `grid_cell` vector.
#'
#' @return Invisible `NULL`. Throws a warning if the sum of land cover in the
#'   grid cell is > 0.1\% different to the expected total area of land in the
#'   cell.
checkLandCoverAreasInOneCell <- function(grid_cell,
                                         total_area,
                                         LC_types) {

  # Can throw error if land cover types are not present in grid_cell names,
  # and if grid_cell is not a named vector

  # Set the tolerance for land cover differences
  percent_tolerance <- 0.1
  area_tolerance <- (total_area / 100) * percent_tolerance

  # Find the difference between current land cover area and expected total area
  LC_areas <- sum(grid_cell[LC_types])
  LC_areas_diff <- total_area - LC_areas

  if (abs(LC_areas_diff) > area_tolerance) {
    x_coord <- grid_cell["x"]
    y_coord <- grid_cell["y"]

    warning(paste0("Total land cover in grid cell ", x_coord, ", ", y_coord,
                   " is >0.1% different to expected land cover area"))
  }

  return(invisible(NULL))
}

#' Check for unallocated land cover change after land cover allocation
#'
#' Checks whether there is any unallocated land cover change after all three
#'   steps of the land cover allocation algorithm (intensification, expansion
#'   and second round of intensification). Unallocated land cover change is
#'   defined as remaining land cover change which is more than 0.1\% of the
#'   total area of the coarse-scale grid cell.
#'
#' @inheritParams checkLandCoverAreasInOneCell
#'
#' @return Invisible 'NULL'. Throws a warning if a grid cell contains
#'   unallocated land cover change and prints the grid cell to the console.
checkForUnallocatedLandCover <- function(grid_cell,
                                         LC_types) {

  # Set the tolerance for unallocated land cover change
  percent_tolerance <- 0.1
  area_tolerance <- (grid_cell["ref_map_area"] / 100) * percent_tolerance

  # Check if any LC_deltas are more than or equal to 0
  if (any(abs(grid_cell[LC_types]) > area_tolerance)) {

    return(grid_cell)

  } else {
    return(invisible(NULL))
  }

}
