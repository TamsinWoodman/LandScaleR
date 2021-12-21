#' Land cover change processing
#'
#' Reconciles the land cover change values for each coarse-scale cell
#'   with fine-scale areas and aggregates land cover types to specified final
#'   land cover types.
#'
#' @param LC_deltas Data frame of land cover changes between two coarse-scale
#'   timesteps. Must be the same projection as `assigned_ref_map`.
#' @param assigned_ref_map Data frame of the reference map which should be the
#'   scale at which you want to downscale the land cover data. The data frame
#'   must have a column called `coarse_ID` which assigns each cell in the
#'   reference map to the nearest coarse-scale cell.
#' @param LC_deltas_cell_area Area of a coarse-scale grid cell.
#' @param ref_map_cell_area Area of a reference map grid cell.
#' @param LC_delta_types Column names of land cover types in the land cover
#'   change data frame `LC_deltas`.
#' @param final_LC_types A matrix containing the fraction of each coarse-scale
#'   land cover type that contributes to each reference map land cover type.
#'   Columns should contain reference map land cover types, and rows are the
#'   coarse-scale land cover types. Each cell should contain the proportion of
#'   the coarse-scale land cover type that contributes to the fine-scale type in
#'   the output map.
#'
#' @return Data frame of coarse-scale cells with adjusted land cover changes
#'   aggregated to final land cover types.
processLCDeltas <- function(LC_deltas,
                            assigned_ref_map,
                            LC_deltas_cell_area,
                            ref_map_cell_area,
                            LC_delta_types,
                            final_LC_types) {

  print(paste0("Starting land cover deltas processing..."))

  # Adjust coarse-scale land-use areas according to fine-scale areas
  adj_LC_deltas <- reconcileLCDeltas(LC_deltas = LC_deltas,
                                     assigned_ref_map = assigned_ref_map,
                                     LC_delta_types = LC_delta_types,
                                     LC_deltas_cell_area = LC_deltas_cell_area,
                                     ref_map_cell_area = ref_map_cell_area)

  # Aggregate to final land-use types
  processed_LC_deltas <- aggregateToFinalLCTypes(final_LC_types = final_LC_types,
                                                 adj_LC_deltas = adj_LC_deltas)

  print(paste0("Completed land cover deltas processing"))

  return(processed_LC_deltas)
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
#'
#' @return A data frame of land-use areas within coarse-scale cells, with every
#'   area adjusted for the area of fine-scale cells associated with that coarse
#'   grid cell.
reconcileLCDeltas <- function(LC_deltas,
                              assigned_ref_map,
                              LC_delta_types,
                              LC_deltas_cell_area,
                              ref_map_cell_area) {

  start_time <- Sys.time()

  # Calculate area of fine-scale cells assigned to each coarse grid cell
  ref_map_areas <- apply(LC_deltas,
                         1,
                         calculateRefMapAreaForCoarseCell,
                         assigned_ref_map = assigned_ref_map,
                         ref_map_cell_area = ref_map_cell_area)

  # Adjust coarse land-use areas using equation 1 from Le Page et al. (2016)
  adj_LC_deltas <- LC_deltas[ , 1:2]
  adj_LC_deltas[ , LC_delta_types] <- apply(LC_deltas[ , LC_delta_types],
                                                    2,
                                                    adjustCoarseLCAreas,
                                                    ref_map_areas = ref_map_areas,
                                                    LC_deltas_cell_area = LC_deltas_cell_area)

  # Add coarse IDs and fine-scale areas to output df
  adj_LC_deltas$coarse_ID <- LC_deltas$coarse_ID
  adj_LC_deltas$ref_map_area <- ref_map_areas

  ### Need to add some kind of check in here to make sure that the sum of the
  ### new land-use areas is equal to the fine-scale area for that cell
  # adj_coarse_scale_df$total_area <- apply(adj_coarse_scale_df[ , coarse_land_use_types],
  #                                         1,
  #                                         sum)

  # Time check
  end_time <- Sys.time()
  timeCheckMessage(start_time,
                   end_time,
                   "Land cover areas reconciled in ")

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
                                             assigned_ref_map,
                                             ref_map_cell_area) {

  # Set coarse cell ID
  coarse_ID <- coarse_cell["coarse_ID"]

  # Get number of fine-scale cells assigned to coarse cell
  number_ref_map_cells <- nrow(assigned_ref_map[which(assigned_ref_map["coarse_ID"] == coarse_ID), ])

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
                                LC_deltas_cell_area) {

  adj_LC_deltas <- coarse_scale_land_use_areas * (ref_map_areas / LC_deltas_cell_area)

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

  start_time <- Sys.time()

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

  # Time check
  end_time <- Sys.time()
  timeCheckMessage(start_time,
                   end_time,
                   "Coarse-scale land cover classes aggregated to reference map land cover classes in ")

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
