#' Calculate land-use delta values
#'
#' Calculates the change in each land-use type (delta) in every coarse-scale
#'   cell between two time steps. The fine-scale data frame is used as the
#'   baseline for the previous timestep, and the coarse-scale data frame gives
#'   the new land-use areas for the subsequent timestep.
#'
#' @inheritParams calculateKernelDensities
#' @param agg_adj_coarse_scale_df Coarse-scale land-use data frame containing
#'   the aggregated and adjusted land-use areas.
#'
#' @return A data frame with the delta value for each land-use type in each
#'   coarse-scale grid cell.
calculateLandUseDeltas <- function(fine_scale_df_with_IDs,
                                   agg_adj_coarse_scale_df,
                                   land_use_types) {

  land_use_deltas <- agg_adj_coarse_scale_df[ , c("x", "y")]

  land_use_deltas[ , land_use_types] <- t(apply(agg_adj_coarse_scale_df,
                                              1,
                                              calculateLandUseDeltasForOneCell,
                                              fine_scale_df_with_IDs = fine_scale_df_with_IDs,
                                              land_use_types = land_use_types))

  land_use_deltas$coarse_ID <- agg_adj_coarse_scale_df$coarse_ID

  return(land_use_deltas)
}

#' Calculate land-use delta values for one grid cell
#'
#' Calculates the change in land-use types in a single coarse-scale grid cell
#'   between two time steps.
#'
#' @param coarse_grid_cell The reconciled land-use areas within a single
#'   coarse-scale grid cell.
#' @inheritParams calculateLandUseDeltas
#'
#' @return A one-row data frame giving the delta value for each land-use type in
#'   the given grid cell.
calculateLandUseDeltasForOneCell <- function(coarse_grid_cell,
                                             fine_scale_df_with_IDs,
                                             land_use_types) {

  coarse_ID <- coarse_grid_cell["coarse_ID"]

  matched_fine_scale_cells <- fine_scale_df_with_IDs[which(fine_scale_df_with_IDs$coarse_ID == coarse_ID), ]

  fine_scale_land_use_areas <- apply(matched_fine_scale_cells[ , land_use_types],
                                     2,
                                     calculateLandUseDeltasForOneCellAndLandUseType)

  grid_cell_land_use_deltas <- coarse_grid_cell[land_use_types] - fine_scale_land_use_areas

  return(grid_cell_land_use_deltas)
}

#' Calculate area of one land-use type in a coarse-scale cell
#'
#' Sums the area of one land-use type in all fine-scale cells allocated to a
#'   single coarse-scale cell.
#'
#' @param matched_fine_scale_cells_land_use_areas Vector with the area of one
#'   land-use type in each fine-scale cells allocated to a single coarse-scale
#'   cell.
#'
#' @return A single value giving the area of one land-use type in all fine-scale
#'   cells allocated to a single coarse-scale cell.
calculateLandUseDeltasForOneCellAndLandUseType <- function(matched_fine_scale_cells_land_use_areas) {

  land_use_area <- sum(matched_fine_scale_cells_land_use_areas,
                       na.rm = TRUE)

  return(land_use_area)
}
