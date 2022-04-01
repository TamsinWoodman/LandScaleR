#' S4 class to hold a land cover map with associated properties
#'
#' @slot LC_map Data frame containing a land cover map. First two columns must
#'   contain x- and y-coordinates for the map. Each subsequent column contains
#'   the area of a given land cover class per cell.
#' @slot agg_LC_map For fine-scale maps only, contains a copy of `LC_map` that
#'   has been aggregated into coarse-scale grid cells.
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
                   agg_LC_map = "data.frame",
                   LC_classes = "character",
                   cell_area = "numeric",
                   cell_resolution = "numeric"),
         prototype = c(LC_map = data.frame(),
                       agg_LC_map = data.frame(),
                       LC_classes = NA_character_,
                       cell_area = NA_real_,
                       cell_resolution = NA_real_))

#' S4 class to hold parameters required for land cover allocation
#'
#' @slot LC_deltas `LCMap` object containing the LC deltas data frame and
#'   associated information.
#' @slot ref_map `LCMap` object containing a reference map data frame and
#'   associated information.
#' @slot transition_priorities Matrix with the transition priorities for land
#'   cover allocation.
#' @slot kernel_radius Value for radius of cells used to calculate kernel
#'   densities for each land cover and grid cell.
#' @slot intensification_ratio Value giving the ratio of land cover to be
#'   allocated via intensification.
#'
#' @export
setClass("LCAllocationParams",
         slots = c(LC_deltas = "LCMap",
                   ref_map = "LCMap",
                   kernel_radius = "numeric",
                   random_seed = "numeric"),
         prototype = c(LC_deltas = new("LCMap"),
                       ref_map = new("LCMap"),
                       kernel_radius = NA_real_,
                       random_seed = NA_real_))

