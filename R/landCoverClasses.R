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
         slots = c(ref_map = "LCMap",
                   LC_deltas = "LCMap",
                   LC_types = "character"))

#' S4 class to hold a land cover map with associated properties
#'
#' @slot LC_map Data frame containing a land cover map. First two columns must
#'   contain x- and y-coordinates for the map. Each subsequent column contains
#'   the area of a given land cover class per cell.
#' @slot LC_classes Vector of land cover classes in the `LC_map` slot. Must be
#'   the same as land cover class column names in the `LC_map` data frame.
#' @slot grid_cell_area Gives the area of each grid cell in the `LC_map` data
#'   frame where an equal-area projection is used.
#' @slot resolution Vector with the x- and y-dimensions of each grid cell where
#'   all grid cells have the same resolution. Should be provided in the format
#'   `c(x, y)`.
#'
#' @export
setClass("LCMap",
         slots = c(LC_map = "data.frame",
                   LC_classes = "character",
                   cell_area = "numeric",
                   cell_resolution = "numeric"),
         prototype = c(LC_map = data.frame(),
                       LC_classes = NA_character_,
                       cell_area = NA_real_,
                       cell_resolution = NA_real_))
