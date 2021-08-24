#' Calculate kernel densities
#'
#' Calculates kernel densities for all cell and land cover types in a reference
#'   map data frame.
#'
#' @inheritParams processLCDeltas
#' @param ref_map_LC_types Vector of land cover types in the reference map. All
#'   land cover types should be column names in the reference map.
#' @param ref_map_cell_resolution Resolution of one cell in the reference map,
#'   in the form `c(x, y)`.
#' @param kernel_radius Radius of cells to include in the kernel density
#'   calculation. A value of 1 means that the neighbour cells used to calculate
#'   kernel density will be 1 cell in every direction around the focal cell.
#'   Defaults to 1.
#'
#' @return Data frame with the kernel density value for each land cover type in
#'   each cell.
calculateKernelDensities <- function(assigned_ref_map,
                                     ref_map_LC_types,
                                     ref_map_cell_resolution,
                                     kernel_radius = 1) {

  start_time <- Sys.time()

  # Set up the radius in the x and y directions
  x_size <- ref_map_cell_resolution[1]
  y_size <- ref_map_cell_resolution[2]
  cell_x_dist <- kernel_radius * x_size
  cell_y_dist <- kernel_radius * y_size

  # Set up new data frame containing kernel densities
  kernel_density_df <- assigned_ref_map[ , c("x", "y")]

  # Fill kernel density data frame
  kernel_density_df[ , ref_map_LC_types] <- t(apply(assigned_ref_map,
                                                  1,
                                                  calculateKernelDensitiesForOneCell,
                                                  assigned_ref_map = assigned_ref_map,
                                                  ref_map_LC_types = ref_map_LC_types,
                                                  cell_x_dist = cell_x_dist,
                                                  cell_y_dist = cell_y_dist))

  # Add coarse-scale cell IDs to kernel density data frame
  kernel_density_df$ref_ID <- assigned_ref_map$ref_ID
  kernel_density_df$coarse_ID <- assigned_ref_map$coarse_ID

  end_time <- Sys.time()

  time_taken <- end_time - start_time
  print(paste0("Time taken: ", time_taken))

  return(kernel_density_df)
}

#' Calculate kernel densities for all land cover types in one cell
#'
#' Calculates the kernel density values for all land cover types in a single
#'   grid cell.
#'
#' @param grid_cell Single row from reference map data frame with x and y values
#'   and the area of each land cover type in that cell.
#' @param cell_x_dist Radius of the cell neighbourhood in the x-direction.
#' @param cell_y_dist Radius of the cell neighbourhood in the y-direction.
#' @inheritParams calculateKernelDensities
#'
#' @return Named vector of the kernel density for each land-use within the
#'   grid cell.
calculateKernelDensitiesForOneCell <- function(grid_cell,
                                               assigned_ref_map,
                                               ref_map_LC_types,
                                               cell_x_dist,
                                               cell_y_dist) {

  # Set the coordinates of the cell
  cell_x_coord <- grid_cell["x"]
  cell_y_coord <- grid_cell["y"]
  cell_coords <- c(cell_x_coord, cell_y_coord)

  # Find neighbour cells
  neighbour_cells <- assigned_ref_map[which(assigned_ref_map["x"] >= (cell_x_coord - cell_x_dist) &
                                                    assigned_ref_map["x"] <= (cell_x_coord + cell_x_dist) &
                                                    assigned_ref_map["y"] >= (cell_y_coord - cell_y_dist) &
                                                    assigned_ref_map["y"] <= (cell_y_coord + cell_y_dist)), ]

  # Remove focal cell from neighbour cells df
  neighbour_cells <- neighbour_cells[-which(neighbour_cells["x"] == cell_x_coord &
                                              neighbour_cells["y"] == cell_y_coord), ]

  # Calculate distance between focal cell and neighbour cells
  neighbour_cells$distance <- raster::pointDistance(cell_coords,
                                                    neighbour_cells[ , c("x", "y")],
                                                    lonlat = FALSE)

  # Find kernel density for each land-use type
  grid_cell_kernel_densities <- apply(neighbour_cells[ , ref_map_LC_types],
                                                      2,
                                                      kernelDensityFunction,
                                                      distance_values = neighbour_cells$distance,
                                                      number_of_neighbour_cells = nrow(neighbour_cells))

  return(grid_cell_kernel_densities)
}

#' Calculate kernel density value for a single land cover in a single cell
#'
#' Calculates the kernel density value for a single land cover in a single
#'   fine-scale grid cell.
#'
#' @param LC_values Vector of land cover areas within each neighbour cell.
#' @param distance_values Vector giving the distance of each neighbour cell
#'   from the focal cell, should be the same length as `LC_values`.
#' @param number_of_neighbour_cells Number of neighbour cells.
#'
#' @return The kernel density of the given land-use in the focal cell.
kernelDensityFunction <- function(LC_values,
                                  distance_values,
                                  number_of_neighbour_cells) {

  # Fix for NA values included here
  kernel_density <- (1 / number_of_neighbour_cells) * sum(LC_values / distance_values^2, na.rm = TRUE)

  return(kernel_density)
}
