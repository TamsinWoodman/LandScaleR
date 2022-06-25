
CoarseCellFromDFRow <- function(grid_cell,
                                cell_resolution,
                                LC_deltas_classes,
                                LC_deltas_cell_area,
                                ref_map,
                                ref_map_LC_classes) {

  coarse_ID <- unname(grid_cell["coarse_ID"])
  mid_coords <- MidCoords(x = grid_cell["x"],
                          y = grid_cell["y"])
  outer_coords <- OuterCoordsFromMidCoords(mid_coords = mid_coords,
                                           cell_resolution = cell_resolution)

  if (is.na(LC_deltas_cell_area)) {
    cell_area <- grid_cell["cell_area"]
  } else {
    cell_area <- LC_deltas_cell_area
  }

  ref_cells <- ref_map[ref_map$coarse_ID == coarse_ID, ]
  sum_ref_cells <- sumLCAreasInRefCells(ref_cells = ref_cells,
                                        LC_classes = ref_map_LC_classes)
  ref_cells_area <- sum(sum_ref_cells)

  coarse_cell <- new("CoarseCell",
                     id = coarse_ID,
                     mid_coords = mid_coords,
                     outer_coords = outer_coords,
                     LC_deltas = grid_cell[LC_deltas_classes],
                     cell_area = cell_area,
                     ref_cells = ref_cells,
                     sum_ref_cells = sum_ref_cells,
                     ref_cells_area = ref_cells_area)

  return(coarse_cell)
}

OuterCoordsFromMidCoords <- function(mid_coords,
                                     cell_resolution) {

  half_cell_resolution <- cell_resolution / 2

  mid_x <- midX(mid_coords)
  mid_y <- midY(mid_coords)

  outer_coords <- new("OuterCoords",
                      min_x = mid_x - half_cell_resolution,
                      max_x = mid_x + half_cell_resolution,
                      min_y = mid_y - half_cell_resolution,
                      max_y = mid_y + half_cell_resolution)

  return(outer_coords)
}

MidCoords <- function(x, y) {

  x <- as.numeric(x)
  y <- as.numeric(y)

  mid_coords <- new("MidCoords",
                    mid_x = x,
                    mid_y = y)

  return(mid_coords)
}
