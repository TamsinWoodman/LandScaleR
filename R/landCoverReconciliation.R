#' Land cover change processing
#'
#' Reconciles the land cover change values for each coarse-scale cell
#'   with fine-scale areas and aggregates land cover classes to specified final
#'   land cover classes.
#'
#' @inheritParams loadRefMap
#' @param ref_map An `LCMap` object containing the reference map for the current
#'   timestep.
#' @inheritParams downscaleLC
#'
#' @return `LCMap` object containing land cover change from `LC_deltas` that has
#'   been adjusted according to areas in the reference map and aggregated to
#'   final land cover classes.
processLCDeltas <- function(LC_deltas,
                            ref_map,
                            final_LC_classes) {

  print(paste0("Starting land cover deltas processing..."))

  # Adjust coarse-scale land-use areas according to fine-scale areas
  adj_LC_deltas <- reconcileLCDeltas(LC_deltas = LC_deltas,
                                     ref_map = ref_map)

  # Aggregate to final land-use classes
  processed_LC_deltas <- aggregateToFinalLCClasses(final_LC_classes = final_LC_classes,
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
#' @return `LCMap` object of land cover change in `LC_deltas` where every area
#'   is adjusted for the area of fine-scale cells associated with that coarse
#'   grid cell.
reconcileLCDeltas <- function(LC_deltas,
                              ref_map) {

  start_time <- Sys.time()

  # Extract variables
  LC_deltas_df <- slot(LC_deltas,
                       "LC_map")
  LC_deltas_classes <- slot(LC_deltas,
                            "LC_classes")
  LC_deltas_cell_area <- slot(LC_deltas,
                              "cell_area")
  LC_deltas_cell_resolution <- slot(LC_deltas,
                                    "cell_resolution")
  ref_map_df <- slot(ref_map,
                     "LC_map")
  ref_map_cell_area <- slot(ref_map,
                            "cell_area")

  # Calculate area of fine-scale cells assigned to each coarse grid cell
  ref_map_areas <- apply(LC_deltas_df,
                         1,
                         calculateRefMapAreaForCoarseCell,
                         ref_map_df = ref_map_df,
                         ref_map_cell_area = ref_map_cell_area)

  # Adjust coarse land-use areas using equation 1 from Le Page et al. (2016)
  adj_LC_deltas_df <- LC_deltas_df[ , 1:2]
  adj_LC_deltas_df[ , LC_deltas_classes] <- apply(LC_deltas_df[ , LC_deltas_classes],
                                                 2,
                                                 adjustCoarseLCAreas,
                                                 ref_map_areas = ref_map_areas,
                                                 LC_deltas_cell_area = LC_deltas_cell_area)

  # Add coarse IDs and fine-scale areas to output df
  adj_LC_deltas_df$coarse_ID <- LC_deltas_df$coarse_ID
  adj_LC_deltas_df$ref_map_area <- ref_map_areas

  ### Need to add some kind of check in here to make sure that the sum of the
  ### new land-use areas is equal to the fine-scale area for that cell
  # adj_coarse_scale_df$total_area <- apply(adj_coarse_scale_df[ , coarse_land_use_classes],
  #                                         1,
  #                                         sum)

  # Create new LCMap object with adjusted land-use deltas
  adj_LC_deltas <- new("LCMap",
                       LC_map = adj_LC_deltas_df,
                       LC_classes = LC_deltas_classes,
                       cell_area = LC_deltas_cell_area,
                       cell_resolution = LC_deltas_cell_resolution)

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
#' @inheritParams assignRefMapCells
#' @inheritParams downscaleLC
#'
#' @return Area of reference map cells that are assigned to the given coarse
#'   grid cell.
calculateRefMapAreaForCoarseCell <- function(coarse_cell,
                                             ref_map_df,
                                             ref_map_cell_area) {

  # Set coarse cell ID
  coarse_ID <- coarse_cell["coarse_ID"]

  # Get number of fine-scale cells assigned to coarse cell
  number_ref_map_cells <- nrow(ref_map_df[which(ref_map_df["coarse_ID"] == coarse_ID), ])

  # Calculate area of fine-scale cells assigned to coarse cell
  area_ref_map_cells <- number_ref_map_cells * ref_map_cell_area

  return(area_ref_map_cells)
}

#' Adjust land cover areas for each coarse grid cell by the area of reference
#'   map grid cells
#'
#' Adjusts the land cover delta value for a single land cover class according to
#'   the area of reference map cells assigned to the coarse cell.
#'
#' Implements equation 1 from Le Page et al. (2016) to adjust the area of a
#'   given land-use within a coarse-scale grid cell.
#'
#' @param coarse_scale_land_use_areas Vector containing areas of a single land
#'   cover class in all coarse-scale grid cells.
#' @param ref_map_areas Vector containing the area of reference map cells within
#'   each coarse-scale grid cell.
#' @inheritParams downscaleLC
#'
#' @return Vector of adjusted land cover deltas.
adjustCoarseLCAreas <- function(coarse_scale_land_use_areas,
                                ref_map_areas,
                                LC_deltas_cell_area) {

  adj_LC_deltas <- coarse_scale_land_use_areas * (ref_map_areas / LC_deltas_cell_area)

  return(adj_LC_deltas)
}

#' Aggregate land cover classes
#'
#' Aggregate land cover classes from the land cover deltas data frame to the
#'   land cover classes in the reference map.
#'
#' @inheritParams downscaleLC
#' @param adj_LC_deltas `LCMap` object containing land cover change areas
#'   adjusted for the area of reference map cells associated with each coarse
#'   grid cell. This object is output from `reconcileLCDeltas`.
#'
#' @return `LCMap` object of land cover change in `adj_LC_deltas` where land
#'   cover classes have been aggregated to the reference map land cover classes.
aggregateToFinalLCClasses <- function(final_LC_classes,
                                      adj_LC_deltas) {

  start_time <- Sys.time()

  # Get adjusted LC deltas from LCMap object
  adj_LC_deltas_df <- slot(adj_LC_deltas,
                           "LC_map")

  new_LC_classes <- colnames(final_LC_classes)
  agg_adj_LC_deltas_df <- adj_LC_deltas_df[ , 1:2]

  for(i in 1:length(new_LC_classes)) {
    new_LC_class <- new_LC_classes[i]
    agg_rules <- final_LC_classes[ , new_LC_class]

    agg_adj_LC_deltas_df[ , new_LC_class] <- apply(adj_LC_deltas_df,
                                                   1,
                                                   aggregateLCClassInOneCell,
                                                   agg_rules = agg_rules)
  }

  agg_adj_LC_deltas_df$coarse_ID <- adj_LC_deltas_df$coarse_ID
  agg_adj_LC_deltas_df$ref_map_area <- adj_LC_deltas_df$ref_map_area

  ### Need to add some kind of check in here to make sure that the sum of the
  ### new land-use areas is equal to the fine-scale area for that cell
  # agg_adj_coarse_scale_df$total_area <- apply(agg_adj_coarse_scale_df[ , new_land_use_classes],
  #                                             1,
  #                                             sum)

  # Create new LCMap object with aggregated, adjusted LC deltas
  agg_adj_LC_deltas <- new("LCMap",
                           LC_map = agg_adj_LC_deltas_df,
                           LC_classes = new_LC_classes,
                           cell_area = slot(adj_LC_deltas,
                                            "cell_area"),
                           cell_resolution = slot(adj_LC_deltas,
                                                  "cell_resolution"))

  # Time check
  end_time <- Sys.time()
  timeCheckMessage(start_time,
                   end_time,
                   "Coarse-scale land cover classes aggregated to reference map land cover classes in ")

  return(agg_adj_LC_deltas)
}

#' Aggregate a single land cover class in one grid cell
#'
#' @param grid_cell Single row from a data frame containing adjusted land cover
#'   deltas for one coarse-scale grid cell.
#' @param agg_rules One column from the `final_LC_classes` matrix, which gives
#'   rules for aggregating coarse-scale land cover classes to a single reference
#'   map land cover class.
#'
#' @return Area of the reference map land cover class in the given grid cell.
aggregateLCClassInOneCell <- function(grid_cell,
                                      agg_rules) {

  new_land_use_value <- 0

  for (i in 1:length(agg_rules)) {
    old_land_use_class <- names(agg_rules)[i]

    tmp_land_use_value <- as.numeric(grid_cell[old_land_use_class]) * agg_rules[old_land_use_class]

    new_land_use_value <- new_land_use_value + tmp_land_use_value
  }

  names(new_land_use_value) <- NULL

  return(new_land_use_value)
}
