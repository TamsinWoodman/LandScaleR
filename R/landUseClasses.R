#' S4 class to hold land-use data frames
#'
#' An S4 class with slots for fine-scale and coarse-scale land-use data frames.
#'
#' @slot fine_scale_df A data frame of fine-scale land-uses, where each row is
#'   one raster cell.
#' @slot coarse_scale_df A data frame of coarse-scale land-uses, where each row
#'   is one raster cell.
#'
#' @export
setClass("ReconciledLandUses",
         representation = list(fine_scale_df = "data.frame",
                               coarse_scale_df = "data.frame"))
