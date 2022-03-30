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
#' @param LC_deltas_classes Vector of land cover classes in the land cover change
#'   maps. Each land cover class should be a column name in every land cover
#'   change file.
#' @param LC_deltas_cell_area Area of a single coarse-scale grid cell.
#' @param ref_map_type Specifies whether the reference map is discrete (contains
#'   one land cover class per cell) or area-based (provides the area of each
#'   land cover class in each cell). Must be one of "areas" or "discrete".
#' @param LC_column_name For a discrete reference map, gives the name of the
#'   column containing the land cover class for each grid cell.
#' @param ref_map_LC_classes Vector of land cover types in the reference map. For
#'   an area-based reference map, all land cover types should be column names in
#'   the reference map.
#' @param ref_map_cell_area Area of a single reference map grid cell.
#' @param ref_map_cell_resolution Resolution of one cell in the reference map,
#'   in the form `c(x, y)`.
#' @param final_LC_classes A matrix containing the fraction of each coarse-scale
#'   land cover class that contributes to each reference map land cover class.
#'   Columns should contain reference map land cover classes, and rows are the
#'   coarse-scale land cover classes. Each cell should contain the proportion of
#'   the coarse-scale land cover class that contributes to the fine-scale class in
#'   the output map.
#' @param kernel_radius Radius of cells to include in the kernel density
#'   calculation. A value of 1 means that the neighbour cells used to calculate
#'   kernel density will be 1 cell in every direction around the focal cell.
#'   Defaults to 1.
#' @param discrete_output_map Output discrete land cover as well as area-based
#'   land cover. Default is `FALSE`.
#' @param random_seed Numeric random seed for ordering fine-scale cells with a
#'   kernel density value of zero during land cover allocation. Defaults to the
#'   date and time when the function is called.
#' @param output_file_prefix Prefix for output downscaled land cover map files.
#' @param output_dir_path Path to directory in which to save the downscaled
#'   land cover map files. Will be created if it does not already exist.
#'
#' @return Tab-separated text files containing downscaled land cover maps, with
#'   one file per input land cover change file. In each output file, the first
#'   two columns are x and y coordinates for the map. Subsequent columns contain
#'   land cover for each cell. The 'ref_ID' column gives an identification
#'   number for each grid cell, and the 'coarse_ID' column specifies the
#'   coarse-scale cell to which each fine-scale cell was assigned to during
#'   downscaling. If `discrete_output_map = FALSE`, a column named
#'   'Discrete_LC_class' containing the discrete land cover class for each cell
#'   will be appended to the output.
downscaleLC <- function(ref_map_file_name,
                        LC_deltas_file_list,
                        LC_deltas_classes,
                        LC_deltas_cell_area,
                        ref_map_type = "areas",
                        LC_column_name = "Land_cover",
                        ref_map_LC_classes,
                        ref_map_cell_area,
                        ref_map_cell_resolution,
                        final_LC_classes,
                        kernel_radius,
                        discrete_output_map = FALSE,
                        random_seed = as.numeric(Sys.time()),
                        output_file_prefix,
                        output_dir_path) {

  start_time <- Sys.time()

  # Create output directory if it doesn't exist
  createOutputDir(output_dir_path = output_dir_path)

  # Loop through LC delta files
  for (i in 1:length(LC_deltas_file_list)) {

    LC_deltas <- loadLCDeltas(LC_deltas_file_list = LC_deltas_file_list,
                              timestep = i,
                              LC_deltas_classes = LC_deltas_classes,
                              LC_deltas_cell_area = LC_deltas_cell_area)

    ref_map <- loadRefMap(timestep = i,
                          ref_map_file_name = ref_map_file_name,
                          ref_map_type = ref_map_type,
                          ref_map_LC_classes = ref_map_LC_classes,
                          ref_map_cell_area = ref_map_cell_area,
                          ref_map_cell_resolution = ref_map_cell_resolution,
                          LC_column_name = LC_column_name,
                          LC_deltas = LC_deltas,
                          discrete_output_map = discrete_output_map,
                          output_dir_path = output_dir_path,
                          output_file_prefix = output_file_prefix)

    # Reconcile areas of LC deltas and aggregate to final LC types
    processed_LC_deltas <- processLCDeltas(LC_deltas = LC_deltas,
                                           ref_map = ref_map,
                                           final_LC_classes = final_LC_classes)

    # Allocate land cover change
    # Set up object for LC allocation
    LC_allocation_params <- new("LCAllocationParams",
                                LC_deltas = processed_LC_deltas,
                                ref_map = ref_map,
                                kernel_radius = kernel_radius,
                                random_seed = random_seed)

    new_LC_map <- runLCAllocation(LC_allocation_params)

    # Add discrete land cover if specified
    if (discrete_output_map) {
      new_LC_map <- getDiscreteLC(LC_map = new_LC_map,
                                  ref_map_LC_classes = ref_map_LC_classes)
    }

    # Save land cover map
    saveLandCoverMapAsTable(new_LC_map,
                            file_prefix = output_file_prefix,
                            dir_path = output_dir_path,
                            time_step = i)

    print(paste0("Completed downscaling timestep ",
                 i))
  }

  end_time <- Sys.time()
  timeCheckMessage(start_time,
                   end_time,
                   "Completed downscaling in ")
}

#' Create the output directory for land cover downscaling
#'
#' @inheritParams downscaleLC
#'
#' @return Creates an output directory with the path given in `output_dir_path`.
createOutputDir <- function(output_dir_path) {

  if (!dir.exists(output_dir_path)) {
    dir.create(output_dir_path)

    print(paste0("Created output directory: ",
                 output_dir_path))
  }
}

#' Load an LC deltas data frame
#'
#' Loads an LC deltas data frame for the given timestep.
#'
#' @inheritParams downscaleLC
#' @param timestep Timestep of land cover change to be downscaled.
#'
#' @return An `LCMap` object containing the LC deltas data frame and associated
#'   information.
loadLCDeltas <- function(LC_deltas_file_list,
                         timestep,
                         LC_deltas_classes,
                         LC_deltas_cell_area) {

  # Read one timestep of LC delta values from file
  LC_deltas_file_name <- LC_deltas_file_list[[timestep]]

  LC_deltas_raw <- read.table(LC_deltas_file_name,
                              header = TRUE,
                              sep = "\t",
                              check.names = FALSE)

  # Add the coarse ID values here
  LC_deltas_df <- addCellIDs(LC_df = LC_deltas_raw,
                             ID_column_name = "coarse_ID")

  LC_deltas <- new("LCMap",
                   LC_map = LC_deltas_df,
                   LC_classes = LC_deltas_classes,
                   cell_area = LC_deltas_cell_area)

  print(paste0("Loaded LC deltas file: ",
               LC_deltas_file_name))

  return(LC_deltas)
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

#' Load a reference map into R
#'
#' @inheritParams downscaleLC
#' @inheritParams loadLCDeltas
#' @param LC_deltas An `LCMap` object containing land cover change areas for the
#'   given timestep.
#'
#' @return An `LCMap` object containing the reference map data frame for the
#'   given timestep and associated information.
loadRefMap <- function(timestep,
                       ref_map_file_name,
                       ref_map_type,
                       ref_map_LC_classes,
                       ref_map_cell_area,
                       ref_map_cell_resolution,
                       LC_column_name,
                       LC_deltas,
                       discrete_output_map,
                       output_dir_path,
                       output_file_prefix) {

  LC_deltas_df <- slot(LC_deltas,
                       "LC_map")

  # If this is the first timestep
  if (timestep == 1) {

    # Read in the reference map from file
    ref_map_raw <- read.table(ref_map_file_name,
                              header = TRUE,
                              sep = "\t")

    # Convert discrete map into fractional map
    if (ref_map_type == "discrete") {
      ref_map_raw <- convertDiscreteLCToLCAreas(ref_map_df = ref_map_raw,
                                                ref_map_LC_classes = ref_map_LC_classes,
                                                ref_map_cell_area = ref_map_cell_area,
                                                LC_column_name = LC_column_name)
    }

    # Add IDs column
    ref_map_df <- addCellIDs(LC_df = ref_map_raw,
                             ID_column_name = "ref_ID")

    print(paste0("Loaded reference map: ",
                 ref_map_file_name))

    # Assign fine-scale cells
    assigned_ref_map <- assignRefMapCells(ref_map_df = ref_map_df,
                                          LC_deltas_df = LC_deltas_df)
  } else {

    # Read in fine-scale map from previous timestep
    previous_ref_map_file_path <- generateOutputFilePath(dir_path = output_dir_path,
                                                         file_prefix = output_file_prefix,
                                                         time_step = timestep - 1)

    assigned_ref_map <- read.table(previous_ref_map_file_path,
                                   header = TRUE,
                                   sep = "\t",
                                   check.names = FALSE)

    # Remove discrete LC column if discrete_output_map is TRUE
    if (discrete_output_map) {
      assigned_ref_map <- assigned_ref_map[ , !names(assigned_ref_map) == "Discrete_LC_class"]
    }

    print(paste0("Loaded reference map: ",
                 previous_ref_map_file_path))
  }

  new_ref_map <- new("LCMap",
                     LC_map = assigned_ref_map,
                     LC_classes = ref_map_LC_classes,
                     cell_area = ref_map_cell_area,
                     cell_resolution = ref_map_cell_resolution)

  return(new_ref_map)
}

#' Assign reference map cells to coarse-scale grid cells
#'
#' Assigns fine-scale cells on an reference land cover map to coarse-scale grid
#'   cells from a land cover change map using a nearest-neighbour method.
#'
#' @param ref_map_df Data frame of the reference map for the current timestep.
#' @param LC_deltas_df Data frame of coarse-scale land cover change areas for
#'   the current timestep.
#'
#' @return A copy of the reference map data frame with an extra column
#'   containing the ID of the coarse-scale cell to which each reference map cell
#'   belongs.
assignRefMapCells <- function(ref_map_df,
                              LC_deltas_df) {

  start_time <- Sys.time()

  # Calculate nearest neighbours
  nearest_neighbours <- FNN::get.knnx(LC_deltas_df[ , 1:2],
                                      ref_map_df[ , 1:2],
                                      1,
                                      algorithm = "kd_tree")

  # Add nearest neighbours to fine-scale cell df
  ref_map_with_nn <- ref_map_df
  ref_map_with_nn$coarse_ID <- as.integer(nearest_neighbours$nn.index)

  # Time check
  end_time <- Sys.time()
  timeCheckMessage(start_time,
                   end_time,
                   "Assigned reference map cells to coarse-scale map cells in ")

  return(ref_map_with_nn)
}

#' Convert a discrete land cover map to one containing the area of each land
#'   cover class per cell
#'
#' Takes a land cover map with one land cover class per grid cell, and converts
#'   it to a map with the area of each land cover class per cell.
#'
#' @inheritParams assignRefMapCells
#' @inheritParams downscaleLC
#'
#' @return A data frame of land cover areas per grid cell. The first two columns
#'   give the x- and y-coordinates of each cell. Remaining columns give the area
#'   of each land cover class in each cell.
convertDiscreteLCToLCAreas <- function(ref_map_df,
                                       ref_map_LC_classes,
                                       ref_map_cell_area,
                                       LC_column_name) {

  if (!LC_column_name %in% colnames(ref_map_df)) {
    stop(paste0(LC_column_name,
                " was not found as a column name in the reference map"))
  }

  ref_map_LC_areas <- ref_map_df[ , c("x", "y")]

  ref_map_LC_areas[ , ref_map_LC_classes] <- t(sapply(ref_map_df[ , LC_column_name],
                                                      convertDiscreteLCToLCAreasOneCell,
                                                      ref_map_LC_classes = ref_map_LC_classes,
                                                      ref_map_cell_area = ref_map_cell_area))

  return(ref_map_LC_areas)
}

#' Convert discrete land cover in one grid cell to a vector with the area of
#'   land cover classes
#'
#' Converts a discrete land cover class for one grid cell to a vector containing
#'   the area of user-specified land cover classes in that cell.
#'
#' @param LC_class Discrete land cover class that the grid cell contains.
#' @inheritParams downscaleLC
#'
#' @return A vector with the area of each of the specified land cover classes in
#'   the given grid cell.
convertDiscreteLCToLCAreasOneCell <- function(LC_class,
                                              ref_map_LC_classes,
                                              ref_map_cell_area) {

  if (!LC_class %in% ref_map_LC_classes) {
    stop(paste0(LC_class, " is not a land cover class in the ref_map_LC_classes vector"))
  }

  # Set up new data frame
  LC_areas <- rep(0,
                  length(ref_map_LC_classes))
  names(LC_areas) <- ref_map_LC_classes

  LC_areas[which(names(LC_areas) == LC_class)] <- ref_map_cell_area

  return(LC_areas)
}

#' Get discrete land cover classes from a land cover data frame containing the
#'   area of each land cover class per grid cell
#'
#' Takes a land cover data frame containing the area of each land cover class
#'   per grid cell and adds a column with a discrete land cover class for each
#'   grid cell. The discrete land cover class for a cell is calculated as the
#'   land cover class with the highest area in that cell.
#'
#' @param LC_map Land cover data frame with the area of each land cover class
#'   per grid cell.
#' @inheritParams downscaleLC
#'
#' @return The land cover data frame with an additional column called
#'   'Discrete_LC_class' that gives a discrete land cover class for each grid
#'   cell.
getDiscreteLC <- function(LC_map,
                          ref_map_LC_classes) {

  if (!all(ref_map_LC_classes %in% colnames(LC_map))) {
    stop("Not all land cover classes are columns in the land cover data frame.")
  }

  LC_map$Discrete_LC_class <- apply(LC_map[ , ref_map_LC_classes],
                                        1,
                                        getMaxLCClassInOneCell)

  return(LC_map)
}

#' Get the land cover class with highest area in one grid cell
#'
#' Takes a named vector with the area of each land cover class in one grid cell,
#'   and returns the land cover class with the highest area.
#'
#' @param grid_cell_LC Named vector containing area of land cover classes in one
#'   grid cell. Names should correspond to the land cover classes in that grid
#'   cell.
#'
#' @return Character string of the land cover class with the greatest coverage
#'   in the given grid cell.
getMaxLCClassInOneCell <- function(grid_cell_LC) {

  max_LC_class <- names(grid_cell_LC)[which(grid_cell_LC == max(grid_cell_LC))]

  if (length(max_LC_class) > 1) {
    max_LC_class <- sample(max_LC_class, 1)
  }

  return(max_LC_class)
}
