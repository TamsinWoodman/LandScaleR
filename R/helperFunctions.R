# Time check messages
timeCheckMessage <- function(start_time,
                             end_time,
                             message) {

  time_taken <- difftime(end_time,
                         start_time,
                         units = "auto")
  time_taken <- format(time_taken)

  print(paste0(message,
               time_taken))
}

# Randomly sorts a data frame
randomiseDataFrame <- function(input_df) {

  df_rows <- sample(nrow(input_df))

  random_input_df <- input_df[df_rows, ]

  return(random_input_df)
}

# Create a coarse cell for testing
createCoarseCellForTests <- function() {
  
  cell_number <- 1
  cell_LC_deltas <- c("urban" = 0, 
                      "managedForest" = -2,
                      "unmanagedForest" = -4,
                      "otherNatural" = 0,
                      "cropland" = 3,
                      "pasture" = 3,
                      "barren" = 0)
  cell_area <- 100
  cell_coords <- matrix(data = c(5, 5),
                        nrow = 1, 
                        ncol = 2)
  colnames(cell_coords) <- c("x", "y")
  
  first_neighbours <- c(NA, 4, 5,
                        NA, 8,
                        NA, NA, NA)
  second_neighbours <- c(NA, NA, 1, 2, 3, 
                         NA, 6, 
                         NA, 9, 
                         NA, NA, 
                         NA, NA, NA, NA, NA)
  neighbour_cells <- c(first_neighbours,
                       second_neighbours)
  
  ref_cells <- terra::rast(data.frame(x = c(2.5, 2.5, 7.5, 7.5),
                                      y = c(2.5, 7.5, 2.5, 7.5),
                                      pri = c(8, 5, 6, 5), 
                                      sec = c(4, 8, 8, 8),
                                      crp = c(2, 5, 2, 3),
                                      pas = c(5, 2, 6, 4),
                                      urb = c(6, 5, 3, 5)))
  kernel_densities <- calculateKernelDensities(ref_cells, 
                                               distance_mat = getDistMatrix(1))
  agg_ref_cells <- unlist(terra::global(ref_cells,
                                        fun = "sum",
                                        na.rm = TRUE))
  names(agg_ref_cells) <- names(ref_cells)
  ref_cells_area <- sum(agg_ref_cells)
  
  coarse_cell <- new("CoarseCell",
                     cell_number = cell_number,
                     LC_deltas = cell_LC_deltas,
                     cell_coords = cell_coords,
                     neighbour_cells = neighbour_cells,
                     cell_area = cell_area,
                     ref_cells = ref_cells,
                     kernel_densities = kernel_densities,
                     agg_ref_cells = agg_ref_cells,
                     ref_cells_area = ref_cells_area)
  
  return(coarse_cell)
}
