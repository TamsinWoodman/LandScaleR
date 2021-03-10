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

  # Assign fine-scale cells
  fine_scale_df_with_IDs <- assignFineScaleCells(fine_scale_df,
                                                 coarse_scale_df)

  return(fine_scale_df_with_IDs)
}

#' Assign fine-scale cells to coarse-scale grid cells
#'
#' Assigns fine-scale cells on an initial land-use map to coarse-scale grid
#' cells from a land-use model using a nearest-neighbour method.
#'
#' @param fine_scale_raster A data frame of fine-scale cells.
#' @param coarse_scale_raster A data frame of coarse-scale cells to which to
#'   assign the fine-scale cells. Must have be on same projection as the fine-scale
#'   cells.
#'
#' @return A copy of the fine-scale cells data frame with an extra column
#'   containing the ID of the coarse-scale cell to which each fine-scale cell
#'   belongs.
assignFineScaleCells <- function(fine_scale_df,
                                 coarse_scale_df) {

  # Calculate nearest neighbours
  nearest_neighbours <- get.knnx(coarse_scale_df[ , 1:2],
                                 fine_scale_df[ , 1:2],
                                 1)

  # Add nearest neighbours to fine-scale cell df
  fine_scale_df_with_nn <- fine_scale_df
  fine_scale_df_with_nn$coarse_ID <- nearest_neighbours$nn.index

  return(fine_scale_df_with_nn)
}
