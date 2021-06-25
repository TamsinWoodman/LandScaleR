#' S4 class to hold coarse-scale and fine-scale land cover data for one timestep
#'
#' An S4 class with slots for the reference map data frame and a data frame of
#'   coarse-scale land cover delta values.
#'
#' @slot ref_map_df Data frame of the reference map, where each row is one
#'   raster cell.
#' @slot LC_deltas Data frame of land cover changes between two coarse-scale
#'   timesteps. Must be the same projection as the `ref_map_df`.
#' @slot LC_types Character vector with the land-use types. The land-use
#'   types should be the same in each data frame.
#'
#' @export
setClass("LCDataClass",
         representation = list(ref_map_df = "data.frame",
                               LC_deltas = "data.frame",
                               LC_types = "character"))
