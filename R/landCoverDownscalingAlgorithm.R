# This script contains only high-level functions for running the entire land
# cover downscaling algorithm

#' Downscale land cover data
#'
#' Downscales land cover by applying land cover change from coarse-scale input
#'   maps to a fine-scale reference map. The method used for downscaling land
#'   cover is based on that in West et al. (2014) and Le Page et al. (2016).
#'   Briefly, all fine-scale reference map cells are assigned to a single
#'   coarse-scale cell using a nearest neighbour approach. Kernel density values
#'   are calculated for each fine-scale cell and land cover type based on the
#'   amount of each land cover type in neighbouring cells. The kernel density
#'   values are used during the land cover allocation process to determine how
#'   much of a land cover type should be allocated to each grid cell; cells with
#'   higher kernel density values will be allocated more land cover. Land cover
#'   change is allocated in three steps: intensification, expansion, followed by
#'   a second round of intensification.
#'
#'   Land cover allocation is performed in three steps: a first round of
#'   intensification, where land cover is allocated to cells where it already
#'   occurs; a round of expansion, where land cover is allocated to cells where
#'   it does not exist; and a final round of intensification to ensure that all
#'   land cover change has been allocated. The `intensification_ratio` parameter
#'   specifies how much land cover the algorithm should attempt to allocate via
#'   intensification versus expansion. An intensification ratio of 0.8 gives a
#'   target of 80\% of land cover to be allocated via intensification and 20\%
#'   by expansion.
#'
#' @param ref_map_file_name Path of file name for the reference map for
#'   downscaling. The reference map file should be in tab-separated format with
#'   the extension `.txt`. The resolution of the reference map will determine
#'   the resolution of the output downscaled land cover maps.
#' @param LC_deltas_file_list List of file names for land cover change (delta).
#'   The algorithm expects one land cover change file per time step. All land
#'   cover change files must be in tab-separated format with the `.txt` file
#'   extension. The list must be ordered by time, with the first time step being
#'   the first file in the list, the second time step the second file, and so
#'   on. Land cover change files can be generated using the `calculateLCDeltas`
#'   function from this package.
#' @inheritParams processLCDeltas
#' @inheritParams calculateKernelDensities
#' @inheritParams runLCAllocation
#' @param output_file_prefix Prefix for output downscaled land cover map files.
#' @param output_dir_path Path to directory in which to saved the downscaled
#'   land cover map files.
#'
#' @return Tab-separated text files containing downscaled land cover maps, with
#'   one file per input land cover change file. In each output file, the first
#'   two columns are x and y coordinates for the map. Subsequent columns contain
#'   land cover for each cell. The 'ref_ID' column gives an identification
#'   number for each grid cell, and the 'coarse_ID' column specifies the
#'   coarse-scale cell to which each fine-scale cell was assigned to during
#'   downscaling.
downscaleLC <- function(ref_map_file_name,
                        LC_deltas_file_list,
                        LC_delta_types,
                        LC_deltas_cell_area,
                        ref_map_LC_types,
                        ref_map_cell_area,
                        ref_map_cell_resolution,
                        final_LC_types,
                        kernel_radius,
                        transition_priorities,
                        intensification_ratio,
                        output_file_prefix,
                        output_dir_path) {

  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir_path)) {
    dir.create(output_dir_path)
  }

  # Loop through LC delta files
  for (i in 1:length(LC_deltas_file_list)) {

    # Read one timestep of LC delta values from file
    LC_deltas_file_name <- LC_deltas_file_list[[i]]
    LC_deltas_raw <- read.table(LC_deltas_file_name,
                                header = TRUE,
                                sep = "\t")

    # Add the coarse ID values here
    LC_deltas <- addCellIDs(LC_deltas_raw,
                            "coarse_ID")

    # If this is the first timestep
    if (i == 1) {

      # Read in the reference map from file
      ref_map_raw <- read.table(ref_map_file_name,
                                header = TRUE,
                                sep = "\t")

      # Add IDs column
      ref_map <- addCellIDs(ref_map_raw,
                            "ref_ID")

      # Assign fine-scale cells
      assigned_ref_map <- assignRefMapCells(ref_map = ref_map,
                                            LC_deltas = LC_deltas)
    } else {

      # Read in fine-scale map from previous timestep
      previous_ref_map_file_path <- generateOutputFilePath(dir_path = output_dir_path,
                                                           file_prefix = output_file_prefix,
                                                           time_step = i - 1)

      assigned_ref_map <- read.table(previous_ref_map_file_path,
                                     header = TRUE,
                                     sep = "\t")
    }

    # Reconcile areas of LC deltas and aggregate to final LC types
    processed_LC_deltas <- processLCDeltas(LC_deltas = LC_deltas,
                                           assigned_ref_map = assigned_ref_map,
                                           LC_deltas_cell_area = LC_deltas_cell_area,
                                           ref_map_cell_area = ref_map_cell_area,
                                           LC_delta_types = LC_delta_types,
                                           final_LC_types = final_LC_types)

    # Calculate suitability of ref map cells
    ## This currently uses a kernel density method, but could be changed to any
    ## other method for calculating cell suitability for each land cover type
    ref_map_suitability <- calculateKernelDensities(assigned_ref_map = assigned_ref_map,
                                                    ref_map_LC_types = ref_map_LC_types,
                                                    ref_map_cell_resolution = ref_map_cell_resolution,
                                                    kernel_radius = kernel_radius)

    # Allocate land cover change
    new_LC_map <- runLCAllocation(assigned_ref_map = assigned_ref_map,
                                  kernel_density_df = ref_map_suitability,
                                  LC_deltas = processed_LC_deltas,
                                  transition_priorities = transition_priorities,
                                  intensification_ratio = intensification_ratio,
                                  ref_map_cell_area = ref_map_cell_area)

    # Save land cover map
    saveLandCoverMapAsTable(new_LC_map,
                            file_prefix = output_file_prefix,
                            dir_path = output_dir_path,
                            time_step = i)

  }

}

#' Add cell IDs to a data frame
#'
#' Sorts a data frame by x-coordinates, followed by y-coordinates. Adds a
#'   column with a user-defined name to the data frame containing an
#'   identification number for each grid cell. A grid cell corresponds to one
#'   row in the data frame.
#'
#' @param LC_df Data frame of land cover map. Must have a minimum of two
#'   columns. First column must be called 'x' and contain x-coordinates of the
#'   map. Second column must be name 'y' and contain y-coordinates of the map.
#' @param ID_column_name Character string giving the name of the column that
#'   will be added to the data frame.
#'
#' @return The input data frame with an added column called 'ID_column_name'
#'   that contains an identification number for each grid cell.
addCellIDs <- function(LC_df,
                       ID_column_name) {

  LC_df_sorted <- LC_df[order(LC_df[ , "x"], LC_df[ , "y"]), ]
  LC_df_sorted[ , ID_column_name] <- seq(1:nrow(LC_df))

  return(LC_df_sorted)
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
assignRefMapCells <- function(ref_map,
                              LC_deltas) {

  # Calculate nearest neighbours
  nearest_neighbours <- FNN::get.knnx(LC_deltas[ , 1:2],
                                      ref_map[ , 1:2],
                                      1,
                                      algorithm = "kd_tree")

  # Add nearest neighbours to fine-scale cell df
  ref_map_with_nn <- ref_map
  ref_map_with_nn$coarse_ID <- as.integer(nearest_neighbours$nn.index)

  return(ref_map_with_nn)
}

