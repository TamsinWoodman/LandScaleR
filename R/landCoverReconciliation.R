#' Land cover change processing
#'
#' Performs the following actions: assigns fine-scale cells to coarse-scale
#'   cells, reconciles the land cover change values for each coarse-scale cell
#'   with fine-scale areas, and aggregates land cover types.
#'
#' @param LC_deltas Data frame of land cover changes between two coarse-scale
#'   timesteps. Must be the same projection as `ref_map_df`.
#' @param ref_map_df Data frame of the reference map, which is the scale at
#'   which you want to downscale the land cover data.
#' @param coarse_cell_area Area of a coarse-scale grid cell.
#' @param ref_map_cell_area Area of a reference map grid cell.
#' @param final_LC_types A matrix containing the fraction of each coarse-scale
#'   land cover type that contributes to each reference map land cover type.
#'   Columns should contain reference map land cover types, and rows are the
#'   coarse-scale land cover types. Each cell should contain the proportion of
#'   the coarse-scale land cover type that contributes to the fine-scale type in
#'   the output map.
#'
#' @return ReconciledLandUses class containing a data frame of reference map
#'   cells assigned to coarse-scale cells, and a second data frame of
#'   coarse-scale cells with adjusted and aggregated land-use areas.
processLCDeltas <- function(LC_deltas,
                            ref_map_df,
                            coarse_cell_area,
                            ref_map_cell_area,
                            final_LC_types) {

  # Add cell ID column to LC deltas df
  LC_deltas$coarse_ID <- seq(1:nrow(LC_deltas))

  # Set land cover types
  coarse_LC_types <- rownames(final_LC_types)
  ref_map_LC_types <- colnames(final_LC_types)

  # Assign fine-scale cells
  ref_map_df_with_IDs <- assignRefMapCells(ref_map_df,
                                           LC_deltas)

  # Adjust coarse-scale land-use areas according to fine-scale areas
  adj_LC_deltas <- reconcileLCDeltas(LC_deltas,
                                     ref_map_df_with_IDs,
                                     coarse_LC_types,
                                     coarse_cell_area,
                                     ref_map_cell_area)

  # Aggregate to final land-use types
  agg_adj_LC_deltas <- aggregateToFinalLCTypes(final_LC_types,
                                               adj_LC_deltas)

  processed_LC_deltas <- new("ProcessedLCDeltas",
                              ref_map_df = ref_map_df_with_IDs,
                              LC_deltas = agg_adj_LC_deltas,
                              LC_types = ref_map_LC_types)

  return(processed_LC_deltas)
}

#' Assign reference map cells to coarse-scale grid cells
#'
#' Assigns fine-scale cells on an reference land cover map to coarse-scale grid
#'   cells from a land-use model using a nearest-neighbour method.
#'
#' @inheritParams processLCDeltas
#'
#' @return A copy of the fine-scale cells data frame with an extra column
#'   containing the ID of the coarse-scale cell to which each fine-scale cell
#'   belongs.
assignRefMapCells <- function(ref_map_df,
                              LC_deltas) {

  # Calculate nearest neighbours
  nearest_neighbours <- FNN::get.knnx(LC_deltas[ , 1:2],
                                      ref_map_df[ , 1:2],
                                      1)

  # Add nearest neighbours to fine-scale cell df
  ref_map_df_with_nn <- ref_map_df
  ref_map_df_with_nn$coarse_ID <- nearest_neighbours$nn.index

  return(ref_map_df_with_nn)
}

#' Reconcile land cover deltas
#'
#' Reconcile land cover deltas for each coarse-scale grid cell with the area of
#'   reference map cells assigned to that grid cell.
#'
#' This function uses equation 1 from the Le Page et al. (2016) paper to
#'   reconcile coarse- and fine-scale land cover areas.
#'
#' @inheritParams processLCDeltas
#' @param ref_map_df_with_IDs Reference map data frame with a column containing
#'   the ID of the coarse grid cell to which each reference map cell was
#'   assigned to. This is output from the `assignFineScaleCells` function.
#' @param coarse_LC_types Vector of land cover types in the land cover deltas
#'   data frame.
#'
#' @return A data frame of land-use areas within coarse-scale cells, with every
#'   area adjusted for the area of fine-scale cells associated with that coarse
#'   grid cell.
reconcileLCDeltas <- function(LC_deltas,
                              ref_map_df_with_IDs,
                              coarse_LC_types,
                              coarse_cell_area,
                              ref_map_cell_area) {

  # Calculate area of fine-scale cells assigned to each coarse grid cell
  ref_map_areas <- apply(LC_deltas,
                         1,
                         calculateRefMapAreaForCoarseCell,
                         ref_map_df_with_IDs = ref_map_df_with_IDs,
                         ref_map_cell_area = ref_map_cell_area)

  # Adjust coarse land-use areas using equation 1 from Le Page et al. (2016)
  adj_LC_deltas <- LC_deltas[ , 1:2]
  adj_LC_deltas[ , coarse_LC_types] <- apply(LC_deltas[ , coarse_LC_types],
                                                    2,
                                                    adjustCoarseLCAreas,
                                                    ref_map_areas = ref_map_areas,
                                                    coarse_cell_area = coarse_cell_area)

  # Add coarse IDs and fine-scale areas to output df
  adj_LC_deltas$coarse_ID <- LC_deltas$coarse_ID
  adj_LC_deltas$ref_map_area <- ref_map_areas

  ### Need to add some kind of check in here to make sure that the sum of the
  ### new land-use areas is equal to the fine-scale area for that cell
  # adj_coarse_scale_df$total_area <- apply(adj_coarse_scale_df[ , coarse_land_use_types],
  #                                         1,
  #                                         sum)

  return(adj_LC_deltas)
}

#' Calculate area of reference map cells assigned to a coarse grid cell
#'
#' Calculates the area of reference map cells that are assigned to a single
#'   coarse grid cell.
#'
#' @param coarse_cell A row containing land-use areas and ID number for a single
#'   coarse grid cell.
#' @inheritParams reconcileLCDeltas
#'
#' @return Area of reference map cells that are assigned to the given coarse
#'   grid cell.
calculateRefMapAreaForCoarseCell <- function(coarse_cell,
                                             ref_map_df_with_IDs,
                                             ref_map_cell_area) {

  # Set coarse cell ID
  coarse_ID <- coarse_cell["coarse_ID"]

  # Get number of fine-scale cells assigned to coarse cell
  number_ref_map_cells <- nrow(ref_map_df_with_IDs[which(ref_map_df_with_IDs["coarse_ID"] == coarse_ID), ])

  # Calculate area of fine-scale cells assigned to coarse cell
  area_ref_map_cells <- number_ref_map_cells * ref_map_cell_area

  return(area_ref_map_cells)
}

#' Adjust land cover areas for each coarse grid cell by the area of reference
#'   map grid cells
#'
#' Adjusts the land cover delta value for a single land cover type according to
#'   the area of reference map cells assigned to the coarse cell.
#'
#' Implements equation 1 from Le Page et al. (2016) to adjust the area of a
#'   given land-use within a coarse-scale grid cell.
#'
#' @param coarse_scale_land_use_areas Vector containing values of a single land
#'   cover type in all coarse-scale grid cells.
#' @param ref_map_areas Vector containing the area of reference map cells within
#'   each coarse-scale grid cell.
#' @inheritParams processLCDeltas
#'
#' @return Vector of adjusted land cover deltas.
adjustCoarseLCAreas <- function(coarse_scale_land_use_areas,
                                ref_map_areas,
                                coarse_cell_area) {

  adj_LC_deltas <- coarse_scale_land_use_areas * (ref_map_areas / coarse_cell_area)

  return(adj_LC_deltas)
}

#' Aggregate land cover types
#'
#' Aggregate land cover types from the land cover deltas data frame to the
#'   land cover types in the reference map.
#'
#' @inheritParams processLCDeltas
#' @param adj_LC_deltas Data frame of land cover deltas, with every value
#'   adjusted for the area of reference map cells associated with that coarse
#'   grid cell. This data frame is output from `reconcileLCDeltas`.
#'
#' @return Data frame of land cover deltas in coarse-scale cells with land cover
#'   types aggregated to the reference map land cover types.
aggregateToFinalLCTypes <- function(final_LC_types,
                                    adj_LC_deltas) {

  new_LC_types <- colnames(final_LC_types)
  agg_adj_LC_deltas <- adj_LC_deltas[ , 1:2]

  for(i in 1:length(new_LC_types)) {
    new_LC_type <- new_LC_types[i]
    agg_rules <- final_LC_types[ , new_LC_type]

    agg_adj_LC_deltas[ , new_LC_type] <- apply(adj_LC_deltas,
                                               1,
                                               aggregateLCTypeInOneCell,
                                               agg_rules = agg_rules)
  }

  agg_adj_LC_deltas$coarse_ID <- adj_LC_deltas$coarse_ID
  agg_adj_LC_deltas$ref_map_area <- adj_LC_deltas$ref_map_area

  ### Need to add some kind of check in here to make sure that the sum of the
  ### new land-use areas is equal to the fine-scale area for that cell
  # agg_adj_coarse_scale_df$total_area <- apply(agg_adj_coarse_scale_df[ , new_land_use_types],
  #                                             1,
  #                                             sum)

  return(agg_adj_LC_deltas)
}

#' Aggregate a single land cover type in one grid cell
#'
#' @param grid_cell Single row from `adj_LC_deltas` containing adjusted land
#'   cover deltas for one coarse-scale grid cell.
#' @param agg_rules One column from the `final_LC_types` matrix, which gives
#'   rules for aggregating coarse-scale land cover types to a single reference
#'   map land cover type.
#'
#' @return Area of the reference map land cover type in the given grid cell.
aggregateLCTypeInOneCell <- function(grid_cell,
                                     agg_rules) {

  new_land_use_value <- 0

  for (i in 1:length(agg_rules)) {
    old_land_use_type <- names(agg_rules)[i]

    tmp_land_use_value <- as.numeric(grid_cell[old_land_use_type]) * agg_rules[old_land_use_type]

    new_land_use_value <- new_land_use_value + tmp_land_use_value
  }

  names(new_land_use_value) <- NULL

  return(new_land_use_value)
}
