#' Calculate kernel densities
#'
#' Calculates kernel densities for one land cover class for a subset of
#'   reference map cells.
#'
#' @inheritParams assignRefMapCells
#' @param ref_map_cells_df Subset of rows from the `ref_map_df` data frame
#'   that contains the grid cells for which you want to calculate kernel
#'   densities.
#' @param LC_class Single land cover class for which to calculate kernel
#'   densities. Must be a column name in `ref_map_cells_df` and
#'   `ref_map`.
#' @inheritParams downscaleLC
#'
#' @return Data frame with the kernel density value for the given land cover
#'   class in each cell.
calculateKernelDensities <- function(ref_map_cells_df,
                                     ref_map_df,
                                     LC_class,
                                     ref_map_cell_resolution,
                                     kernel_radius = 1) {

  # Set up the radius in the x and y directions
  x_size <- ref_map_cell_resolution[1]
  y_size <- ref_map_cell_resolution[2]
  cell_x_dist <- kernel_radius * x_size
  cell_y_dist <- kernel_radius * y_size

  # Set up new data frame containing kernel densities
  kernel_density_df <- ref_map_cells_df[ , c("x", "y")]

  # Fill kernel density data frame
  kernel_density_df[ , LC_class] <- apply(ref_map_cells_df,
                                                  1,
                                                  calculateKernelDensitiesForOneCell,
                                                  ref_map_df = ref_map_df,
                                                  LC_class = LC_class,
                                                  cell_x_dist = cell_x_dist,
                                                  cell_y_dist = cell_y_dist)

  # Add coarse-scale cell IDs to kernel density data frame
  kernel_density_df$ref_ID <- ref_map_cells_df$ref_ID
  kernel_density_df$coarse_ID <- ref_map_cells_df$coarse_ID

  return(kernel_density_df)
}

#' Calculate kernel densities for one land cover class in one cell
#'
#' Calculates the kernel density values for one land cover class in a single
#'   grid cell.
#'
#' @param grid_cell Single row from the `ref_map_cells_df` data frame with x-
#'   and y-coordinates and the area of each land cover class in that cell.
#' @param cell_x_dist Radius of the cell neighbourhood in the x-direction.
#' @param cell_y_dist Radius of the cell neighbourhood in the y-direction.
#' @inheritParams calculateKernelDensities
#' @inheritParams assignRefMapCells
#'
#' @return The kernel density value for the user-specified land cover class and
#'   grid cell.
calculateKernelDensitiesForOneCell <- function(grid_cell,
                                               ref_map_df,
                                               LC_class,
                                               cell_x_dist,
                                               cell_y_dist) {

  # Set the coordinates of the cell
  cell_x_coord <- grid_cell["x"]
  cell_y_coord <- grid_cell["y"]
  cell_coords <- c(cell_x_coord, cell_y_coord)

  # Find neighbour cells
  neighbour_cells <- ref_map_df[which(ref_map_df["x"] >= (cell_x_coord - cell_x_dist) &
                                                    ref_map_df["x"] <= (cell_x_coord + cell_x_dist) &
                                                    ref_map_df["y"] >= (cell_y_coord - cell_y_dist) &
                                                    ref_map_df["y"] <= (cell_y_coord + cell_y_dist)), ]

  # Remove focal cell from neighbour cells df
  neighbour_cells <- neighbour_cells[-which(neighbour_cells["x"] == cell_x_coord &
                                              neighbour_cells["y"] == cell_y_coord), ]

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



