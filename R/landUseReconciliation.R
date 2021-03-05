#' Assign fine-scale cells to coarse-scale grid cells
#'
#' Assigns fine-scale cells on an initial land-use map to coarse-scale grid
#' cells from a land-use model.
#'
#' @param fine_scale_raster A raster of fine-scale cells.
#' @param coarse_scale_raster A raster of coarse-scale cells to which to assign
#'   the fine-scale cells. Must have be on same projection as the fine-scale
#'   cells.
#'
#' @return A list, where each element corresponds to one coarse-scale grid cell.
#'   Within each element is a data frame with the cell numbers of fine-scale
#'   cells and their values.
assignFineScaleCells <- function(fine_scale_raster,
                                 coarse_scale_raster) {

  coarse_scale_polygons <- rasterToPolygons(coarse_scale_raster)

  assigned_fine_scale_cells <- extract(fine_scale_raster,
                                       coarse_scale_polygons,
                                       cellnumbers = TRUE)

}
