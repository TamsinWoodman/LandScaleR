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
#'   should all be columns in the `grid_cell` data frame.
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
  percent_tolerance <- 0.001
  area_tolerance <- total_area * percent_tolerance

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
