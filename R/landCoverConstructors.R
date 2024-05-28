
CoarseCellFromRaster <- function(cell_number,
                                 LC_deltas,
                                 LC_deltas_classes,
                                 harmonisation_radius,
                                 ref_map_polygons,
                                 ref_map,
                                 kernel_densities) {

  cell_LC_deltas <- unlist(LC_deltas[cell_number])
  
  cell_area <- cell_LC_deltas["cell_area"]
  
  # The line of code below means that the order of LULC classes will be the same
  # as the order of LULC classes in the match_LC_classes matrix 
  cell_LC_deltas <- cell_LC_deltas[LC_deltas_classes]
  cell_coords <- terra::xyFromCell(LC_deltas,
                                   cell_number)
  
  neighbour_cells <- getNeighbourCells(LC_deltas = LC_deltas, 
                                       cell_number = cell_number, 
                                       harmonisation_radius = harmonisation_radius)
  ref_cells <- terra::crop(ref_map,
                           ref_map_polygons[ref_map_polygons$coarse_ID == cell_number],
                           mask = TRUE)
  kernel_densities <- terra::crop(kernel_densities,
                                  ref_map_polygons[ref_map_polygons$coarse_ID == cell_number],
                                  mask = TRUE)
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

getNeighbourCells <- function(LC_deltas, 
                              cell_number, 
                              harmonisation_radius) {
  
  all_neighbour_cells <- as.vector(terra::adjacent(LC_deltas,
                                                   cell_number,
                                                   directions = "8"))
  
  if (harmonisation_radius > 1) {
    
    # Start the for loop from second neighbours as first neighbours have already
    # been generated above
    for (i in 2:harmonisation_radius) {
      
      tmp_neighbour_matrix <- matrix(data = 0, 
                                     nrow = (i * 2) + 1,
                                     ncol = (i * 2) + 1)
      
      tmp_neighbour_matrix[1, ] <- 1
      tmp_neighbour_matrix[ , 1] <- 1
      tmp_neighbour_matrix[nrow(tmp_neighbour_matrix), ] <- 1
      tmp_neighbour_matrix[ , ncol(tmp_neighbour_matrix)] <- 1
      
      tmp_neighbour_cells <- as.vector(terra::adjacent(LC_deltas, 
                                                       cell_number, 
                                                       directions = tmp_neighbour_matrix))
      
      all_neighbour_cells <- c(all_neighbour_cells, 
                               tmp_neighbour_cells)
    }
  }
  
  return(all_neighbour_cells)
}

