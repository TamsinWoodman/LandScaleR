#' Land-use reconciliation
#'
#' Performs the following actions: assigns fine-scale cells to coarse-scale
#'   cells, reconciles the coarse-scale areas with fine-scale areas, and
#'   aggregates land-uses.
#'
reconcileLandUses <- function(coarse_scale_raster,
                              fine_scale_raster) {

  # Convert coarse-scale raster to data frame and add cell ID column
  coarse_scale_df <- as.data.frame(rasterToPoints(coarse_scale_raster))
  coarse_scale_df$coarse_ID <- seq(1:nrow(coarse_scale_df))

  # Convert fine-scale raster to data frame
  fine_scale_df <- as.data.frame(rasterToPoints(fine_scale_raster))

  return(coarse_scale_df)
}

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

  return(assigned_fine_scale_cells)
}
