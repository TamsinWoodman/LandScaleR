
#' Calculates the x- and y- distances for the kernel density radius
#'
#' @inheritParams downscaleLC
#'
#' @return Vector in the format `c(x, y)` that gives the radius of
#'   the cell neighbourhood in the x- and y- directions.
calculateXYKernelDistances <- function(ref_map_cell_resolution,
                                       kernel_radius) {

  # Set up the radius in the x and y directions
  x_size <- ref_map_cell_resolution[1]
  y_size <- ref_map_cell_resolution[2]

  cell_x_dist <- kernel_radius * x_size
  cell_y_dist <- kernel_radius * y_size

  kernel_xy_dist <- c(cell_x_dist,
                      cell_y_dist)

  return(kernel_xy_dist)
}

#' Calculate kernel densities for one land cover class in one cell
#'
#' Calculates the kernel density values for one land cover class in a single
#'   grid cell.
#'
#' @param grid_cell Single row from the `ref_map_cells_df` data frame with x-
#'   and y-coordinates and the area of each land cover class in that cell.
#' @param LC_class Single land cover class for which to calculate kernel
#'   densities. Must be a column name in `ref_map_cells_df` and
#'   `ref_map`.
#' @param kernel_xy_dist Vector in the format `c(x, y)` that gives the radius of
#'   the cell neighbourhood in the x- and y- directions.
#' @inheritParams assignRefMapCells
#'
#' @return The kernel density value for the user-specified land cover class and
#'   grid cell.
calculateKernelDensitiesForOneCell <- function(grid_cell,
                                               ref_map_df,
                                               LC_class,
                                               kernel_xy_dist) {

  # Find neighbour cells for kernel density calculation
  neighbour_cells <- findNeighbourCells(kernel_xy_dist = kernel_xy_dist,
                                        grid_cell = grid_cell,
                                        ref_map_df = ref_map_df)

  # Set the coordinates of the cell
  cell_x_coord <- grid_cell["x"]
  cell_y_coord <- grid_cell["y"]
  cell_coords <- c(cell_x_coord, cell_y_coord)

  # Calculate distance between focal cell and neighbour cells
  neighbour_cells[ , "distance"] <- raster::pointDistance(cell_coords,
                                                    neighbour_cells[ , c("x", "y")],
                                                    lonlat = FALSE)

  # Find kernel density for each land-use type
  grid_cell_kernel_densities <- kernelDensityFunction(LC_areas = neighbour_cells[ , LC_class],
                                                      distance_values = neighbour_cells[ , "distance"],
                                                      number_of_neighbour_cells = nrow(neighbour_cells))

  return(grid_cell_kernel_densities)
}

findNeighbourCells <- function(kernel_xy_dist,
                               grid_cell,
                               ref_map_df) {

  # Get x- and y- kernel distances
  cell_x_dist <- kernel_xy_dist[1]
  cell_y_dist <- kernel_xy_dist[2]

  # Set the coordinates of the cell
  cell_x_coord <- grid_cell["x"]
  cell_y_coord <- grid_cell["y"]

  # Find neighbour cells
  neighbour_cells <- ref_map_df[which(ref_map_df["x"] >= (cell_x_coord - cell_x_dist) &
                                        ref_map_df["x"] <= (cell_x_coord + cell_x_dist) &
                                        ref_map_df["y"] >= (cell_y_coord - cell_y_dist) &
                                        ref_map_df["y"] <= (cell_y_coord + cell_y_dist)), ]

  # Remove focal cell from neighbour cells df
  neighbour_cells <- neighbour_cells[-which(neighbour_cells["x"] == cell_x_coord &
                                              neighbour_cells["y"] == cell_y_coord), ]

  return(neighbour_cells)
}

#' Calculate kernel density value for a single land cover class in a single cell
#'
#' Calculates the kernel density value for a single land cover class in a single
#'   reference map grid cell.
#'
#' @param LC_areas Vector of areas for one land cover class within each
#'   neighbour cell.
#' @param distance_values Vector giving the distance of each neighbour cell
#'   from the focal cell, should be the same length as `LC_values`.
#' @param number_of_neighbour_cells Number of neighbour cells.
#'
#' @return The kernel density of the given land-use in the focal cell.
kernelDensityFunction <- function(LC_areas,
                                  distance_values,
                                  number_of_neighbour_cells) {

  # Fix for NA values included here
  kernel_density <- (1 / number_of_neighbour_cells) * sum(LC_areas / distance_values^2, na.rm = TRUE)

  return(kernel_density)
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
