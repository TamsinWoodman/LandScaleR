#' Land-use reconciliation
#'
#' Performs the following actions: assigns fine-scale cells to coarse-scale
#'   cells, reconciles the coarse-scale areas with fine-scale areas, and
#'   aggregates land-uses.
#'
#' @param coarse_scale_raster A raster of land-use areas at a coarse-scale, i.e.
#'   output from PLUMv2.
#' @param fine_scale_raster A raster of land-use areas at the scale to which you
#'   want to downscale the `coarse_scale_raster`. Must be in the same projection
#'   as the `coarse_scale_raster`.
#' @param coarse_cell_area The area of a grid cell in the
#'   `coarse_scale_raster`.
#' @param fine_cell_area The area of a grid cell in the `fine_scale_raster`.
#'
#' @return
reconcileLandUses <- function(coarse_scale_raster,
                              fine_scale_raster,
                              coarse_cell_area,
                              fine_cell_area) {

  # Convert coarse-scale raster to data frame and add cell ID column
  coarse_scale_df <- as.data.frame(rasterToPoints(coarse_scale_raster))
  coarse_scale_df$coarse_ID <- seq(1:nrow(coarse_scale_df))

  # Convert fine-scale raster to data frame
  fine_scale_df <- as.data.frame(rasterToPoints(fine_scale_raster))

  # Assign fine-scale cells
  fine_scale_df_with_IDs <- assignFineScaleCells(fine_scale_df,
                                                 coarse_scale_df)

  # Adjust coarse-scale land-use areas according to fine-scale areas
  adj_coarse_scale_df <- reconcileLandUseAreas(coarse_scale_df,
                                               fine_scale_df_with_IDs,
                                               coarse_cell_area,
                                               fine_cell_area)

  return(adj_coarse_scale_df)
}

#' Assign fine-scale cells to coarse-scale grid cells
#'
#' Assigns fine-scale cells on an initial land-use map to coarse-scale grid
#' cells from a land-use model using a nearest-neighbour method.
#'
#' @param fine_scale_df A data frame of fine-scale cells.
#' @param coarse_scale_df A data frame of coarse-scale cells to which to assign
#'   the fine-scale cells. Must have be on same projection as the
#'   fine-scale cells.
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

#' Reconcile land-use areas
#'
#' Reconcile land-use areas for each coarse-scale grid cell with the area of
#'   fine-scale cells assigned to that grid cell.
#'
#' This function uses equation 1 from the Le Page et al. (2016) paper to
#'   reconcile coarse- and fine-scale land-use areas.
#'
#' @inheritParams assignFineScaleCells
#' @param fine_scale_df_assigned A fine-scale cells data frame with a column
#'   containing the ID of the coarse grid cell to which each fine-scale cell
#'   belongs. This is output by the `assignFineScaleCells` function.
#' @inheritParams reconcileLandUses
#'
#' @return A data frame of land-use areas within coarse-scale cells, with every
#'   area adjusted for the area of fine-scale cells associated with that coarse
#'   grid cell.
reconcileLandUseAreas <- function(coarse_scale_df,
                                  fine_scale_df_assigned,
                                  coarse_cell_area,
                                  fine_cell_area) {

  # Set names of columns with land-use values
  land_use_columns <- colnames(coarse_scale_df[ , 3:(ncol(coarse_scale_df) - 1)])

  # Calculate area of fine-scale cells assigned to each coarse grid cell
  fine_scale_areas <- apply(coarse_scale_df,
                            1,
                            calculateFineScaleAreaForCoarseCell,
                            fine_scale_df_assigned = fine_scale_df_assigned,
                            fine_cell_area = fine_cell_area)

  # Adjust coarse land-use areas using equation 1 from Le Page et al. (2016)
  adj_coarse_scale_df <- coarse_scale_df[ , 1:2]
  adj_coarse_scale_df[ , land_use_columns] <- apply(coarse_scale_df[ , land_use_columns],
                                                    2,
                                                    adjustCoarseLandUseAreas,
                                                    fine_scale_areas = fine_scale_areas,
                                                    coarse_cell_area = coarse_cell_area)

  # Add coarse IDs and fine-scale areas to output df
  adj_coarse_scale_df$coarse_ID <- coarse_scale_df$coarse_ID
  adj_coarse_scale_df$fine_scale_area <- fine_scale_areas

  ### Need to add some kind of check in here to make sure that the sum of the
  ### new land-use areas is equal to the fine-scale area for that cell
  # adj_coarse_scale_df$total_area <- apply(adj_coarse_scale_df[ , land_use_columns],
  #                                         1,
  #                                         sum)

  return(adj_coarse_scale_df)
}

#' Calculate area of fine-scale cells assigned to a coarse grid cell
#'
#' Calculates the area of fine-scale cells that are assigned to a single coarse
#'   grid cell
#'
#' @param coarse_cell A row containing land-use areas and ID number for a single
#'   coarse grid cell.
#' @inheritParams reconcileLandUseAreas
#'
#' @return The area of fine-scale cells that are assigned to the given coarse
#'   grid cell.
calculateFineScaleAreaForCoarseCell <- function(coarse_cell,
                                                fine_scale_df_assigned,
                                                fine_cell_area) {

  # Set coarse cell ID
  coarse_ID <- coarse_cell["coarse_ID"]

  # Get number of fine-scale cells assigned to coarse cell
  number_fine_cells <- nrow(fine_scale_df_assigned[which(fine_scale_df_assigned["coarse_ID"] == coarse_ID), ])

  # Calculate area of fine-scale cells assigned to coarse cell
  area_fine_cells <- number_fine_cells * fine_cell_area

  return(area_fine_cells)
}

#' Adjust land-use areas for each coarse grid cell by the area of fine-scale
#'   grid cells
#'
#' Adjusts the areas in coarse-scale grid cells for a single land-use according
#'   to the area of fine-scale cells assigned to the coarse cell.
#'
#' Implements equation 1 from Le Page et al. (2016) to adjust the area of a
#'   given land-use within a coarse-scale grid cell.
#'
#' @param coarse_scale_land_use_areas A vector containing values of a single
#'   land-use category in all coarse-scale grid cells.
#' @param fine_scale_areas A vector containing the area of fine-scale cells
#'   within each coarse-scale grid cells.
#' @inheritParams reconcileLandUses
#'
#' @return A vector of adjusted land-use areas.
adjustCoarseLandUseAreas <- function(coarse_scale_land_use_areas,
                                     fine_scale_areas,
                                     coarse_cell_area) {

  adj_coarse_scale_land_use_areas <- coarse_scale_land_use_areas * (fine_scale_areas / coarse_cell_area)

  return(adj_coarse_scale_land_use_areas)
}
