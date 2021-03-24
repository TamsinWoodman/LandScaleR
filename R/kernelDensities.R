#' Calculate kernel densities
#'
#' Calculates kernel densities for all cell and land-use types in a data frame.
#'
#' @param fine_scale_df_with_IDs A data frame of fine-scale cells including the
#'   the ID of the coarse-scale cell that each fine-scale cell is assigned to.
#' @param land_use_types The names of columns containing land-use values in the
#'   `final_scale_df_with_IDs` data frame.
#' @param cell_resolution The resolution of one cell in the landscape. Should be
#'   in the form `c(x, y)`.
#' @param moving_window The radius of cells to include in the kernel density
#'   calculation. A value of 1 means that the neighbour cells for calculating
#'   kernel density will be 1 cell in every direction around the focal cell.
#'   Defaults to 1.
#'
#' @return A data frame with the kernel density value for each land-use type in
#'   each cell.
calculateKernelDensities <- function(fine_scale_df_with_IDs,
                                     land_use_types,
                                     cell_resolution,
                                     moving_window = 1) {

  start_time <- Sys.time()

  # Set up the radius in the x and y directions
  x_size <- cell_resolution[1]
  y_size <- cell_resolution[2]
  cell_x_dist <- moving_window * x_size
  cell_y_dist <- moving_window * y_size

  # Set up new data frame containing kernel densities
  kernel_density_df <- fine_scale_df_with_IDs[ , c("x", "y")]

  # Fill kernel density data frame
  kernel_density_df[ , land_use_types] <- t(apply(fine_scale_df_with_IDs,
                             1,
                             calculateKernelDensitiesForOneCell,
                             fine_scale_df_with_IDs = fine_scale_df_with_IDs,
                             land_use_types = land_use_types,
                             cell_x_dist = cell_x_dist,
                             cell_y_dist = cell_y_dist))

  # Add coarse-scale cell IDs to kernel density data frame
  kernel_density_df$coarse_ID <- fine_scale_df_with_IDs$coarse_ID

  end_time <- Sys.time()

  time_taken <- end_time - start_time
  print(paste0("Time taken: ", time_taken))

  return(kernel_density_df)
}

#' Calculate kernel densities for all land-use types in one cell
#'
#' Calculates the kernel density values for all land-use types in a single grid
#'   cell.
#'
#' @param grid_cell Single row from a fine-scale cell data frame with x and y
#'   values and the area of each land-use type in that cell.
#' @param cell_x_dist The radius of the cell neighbourhood in the x-direction.
#' @param cell_y_dist The radius of the cell neighbourhood in the y-direction.
#' @inheritParams calculateKernelDensities
#'
#' @return A named vector of the kernel density for each land-use within the
#'   grid cell.
calculateKernelDensitiesForOneCell <- function(grid_cell,
                                               fine_scale_df_with_IDs,
                                               land_use_types,
                                               cell_x_dist,
                                               cell_y_dist) {

  # Set the coordinates of the cell
  cell_x_coord <- grid_cell["x"]
  cell_y_coord <- grid_cell["y"]
  cell_coords <- c(cell_x_coord, cell_y_coord)

  # Find neighbour cells
  neighbour_cells <- fine_scale_df_with_IDs[which(fine_scale_df_with_IDs["x"] >= (cell_x_coord - cell_x_dist) &
                                                    fine_scale_df_with_IDs["x"] <= (cell_x_coord + cell_x_dist) &
                                                    fine_scale_df_with_IDs["y"] >= (cell_y_coord - cell_y_dist) &
                                                    fine_scale_df_with_IDs["y"] <= (cell_y_coord + cell_y_dist)), ]

  # Remove focal cell from neighbour cells df
  neighbour_cells <- neighbour_cells[-which(neighbour_cells["x"] == cell_x_coord &
                                              neighbour_cells["y"] == cell_y_coord), ]

  # Calculate distance between focal cell and neighbour cells
  neighbour_cells$distance <- raster::pointDistance(cell_coords,
                                                    neighbour_cells[ , c("x", "y")],
                                                    lonlat = FALSE)

  # Find kernel density for each land-use type
  grid_cell_kernel_densities <- apply(neighbour_cells[ , land_use_types],
                                                      2,
                                                      kernelDensityFunction,
                                                      distance_values = neighbour_cells$distance,
                                                      number_of_neighbour_cells = nrow(neighbour_cells))

  return(grid_cell_kernel_densities)
}

#' Calculate kernel density value for a single land-use in a single cell
#'
#' Calculates the kernel density value for a single land-use in a single
#'   fine-scale grid cell.
#'
#' @param land_use_values A vector of land-use values within each neighbouring
#'   cell.
#' @param distance_values A vector giving the distance of each neighbouring cell
#'   from the focal cell, should be the same length as `land_use_values`
#'   parameter.
#' @param number_of_neighbour_cells The number of neighbour cells.
#'
#' @return The kernel density of the given land-use in the focal cell.
kernelDensityFunction <- function(land_use_values,
                                  distance_values,
                                  number_of_neighbour_cells) {

  # Fix for NA values included here
  kernel_density <- (1 / number_of_neighbour_cells) * sum(land_use_values / distance_values^2, na.rm = TRUE)

  return(kernel_density)
}
