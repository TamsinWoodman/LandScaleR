
CoarseCellFromRaster <- function(cell_number,
                                 LC_deltas,
                                 LC_deltas_classes,
                                 ref_map_polygons,
                                 ref_map,
                                 kernel_densities) {

  cell_LC_deltas <- unlist(LC_deltas[cell_number])
  cell_area <- cell_LC_deltas["cell_area"]
  cell_LC_deltas <- cell_LC_deltas[LC_deltas_classes]
  neighbour_cells <- as.vector(terra::adjacent(LC_deltas,
                                               cell_number,
                                               directions = "16"))
  ref_cells <- terra::crop(ref_map,
                           ref_map_polygons[ref_map_polygons$coarse_ID == cell_number],
                           mask = TRUE)
  kernel_densities <- terra::crop(kernel_densities,
                                  ref_map_polygons[ref_map_polygons$coarse_ID == cell_number],
                                  mask = TRUE)
  agg_ref_cells <- unlist(global(ref_cells,
                                 fun = "sum",
                                 na.rm = TRUE))
  names(agg_ref_cells) <- names(ref_cells)
  ref_cells_area <- sum(agg_ref_cells)

  coarse_cell <- new("CoarseCell",
                     cell_number = cell_number,
                     LC_deltas = cell_LC_deltas,
                     neighbour_cells = neighbour_cells,
                     cell_area = cell_area,
                     ref_cells = ref_cells,
                     kernel_densities = kernel_densities,
                     agg_ref_cells = agg_ref_cells,
                     ref_cells_area = ref_cells_area)

  print(cell_number)

  return(coarse_cell)
}
