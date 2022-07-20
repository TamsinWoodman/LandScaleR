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
#' @param equal_area Logical. 'TRUE' indicates that input maps are in an equal
#'   area projection, 'FALSE' means they are not.
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
#' @importFrom methods new slot slot<-
#' @importFrom utils read.table stack write.table
#' @export
downscaleLC <- function(ref_map_file_name,
                        LC_deltas_file_list,
                        LC_deltas_classes,
                        LC_deltas_cell_area,
                        ref_map_type = "areas",
                        ref_map_LC_classes,
                        match_LC_classes,
                        kernel_radius,
                        discrete_output_map = FALSE,
                        random_seed = as.numeric(Sys.time()),
                        output_file_prefix,
                        output_dir_path) {

  start_time <- Sys.time()

  #### Set up
  # Create output directory if it doesn't exist
  createOutputDir(output_dir_path = output_dir_path)

  # Calculate distance matrix
  distance_mat <- getDistMatrix(kernel_radius = kernel_radius)

  #### Loop through time points
  for (timestep in 1:length(LC_deltas_file_list)) {

    #### Set up within timestep 1
    if (timestep == 1) {

      # Load LC_deltas file
      LC_deltas <- loadLCDeltas(LC_deltas_file_list = LC_deltas_file_list,
                                timestep = timestep)

      # Load ref map file
      ref_map <- loadRefMap(ref_map_file_name = ref_map_file_name,
                            ref_map_type = ref_map_type,
                            ref_map_LC_classes = ref_map_LC_classes)

      # Assign ref map cells to LC_deltas cells
      LC_deltas_coords <- crds(LC_deltas)
      LC_deltas_cell_numbers <- cellFromXY(LC_deltas,
                                           LC_deltas_coords)

      # Get polygons of ref map cells for each coarse cell
      ref_map_polygons <- assignRefMapCells(ref_map = ref_map,
                                            LC_deltas_coords = LC_deltas_coords,
                                            LC_deltas_cell_numbers = LC_deltas_cell_numbers)

      # Calculate kernel densities
      kernel_densities <- calculateKernelDensities(ref_map = ref_map,
                                                   distance_mat = distance_mat)

      # Get a list of coarse-scale cells
      coarse_cell_list <- lapply(LC_deltas_cell_numbers,
                                 FUN = CoarseCellFromRaster,
                                 LC_deltas = LC_deltas,
                                 LC_deltas_classes = LC_deltas_classes,
                                 ref_map_polygons = ref_map_polygons,
                                 ref_map = ref_map,
                                 kernel_densities = kernel_densities)
    } else {

      # Load LC_deltas file
      LC_deltas <- loadLCDeltas(LC_deltas_file_list = LC_deltas_file_list,
                                timestep = timestep)

      # Calculate kernel densities
      kernel_densities <- calculateKernelDensities(ref_map = ref_map,
                                                   distance_mat = distance_mat)

      # Get list of coarse-scale cells
      coarse_cell_list <- lapply(LC_deltas_cell_numbers,
                                 FUN = updateCoarseCell,
                                 LC_deltas = LC_deltas,
                                 LC_deltas_classes = LC_deltas_classes,
                                 kernel_densities = kernel_densities,
                                 ref_map_polygons = ref_map_polygons)

    }

    #### Run downscaling
    ####### Next step is to fix the code to run downscaling with SpatRasters
    ds_coarse_cell_list <- lapply(coarse_cell_list,
                               downscaleLCForOneCoarseCell,
                               match_LC_classes = match_LC_classes,
                               random_seed = random_seed)

    # Run harmonisation with unallocated land cover change
    ref_map <- harmoniseUnallocatedLCDeltas(new_LC_map)
    #ref_map <- harmoniseUnallocatedLCDeltas(LC_allocation_params)

    # Save land cover map
    saveLandCoverMapAsTable(LC_map = slot(ref_map,
                                          "LC_map"),
                            file_prefix = output_file_prefix,
                            dir_path = output_dir_path,
                            time_step = i,
                            discrete_output_map = discrete_output_map,
                            LC_classes = ref_map_LC_classes)

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

loadLCDeltas <- function(LC_deltas_file_list,
                         timestep) {

  # Load LC_deltas file
  LC_deltas <- rast(LC_deltas_file_list[[timestep]])

  # If cell_area layer is not present, calculate the area of each cell
  if (!"cell_area" %in% names(LC_deltas)) {
    LC_deltas <- c(LC_deltas,
                   cellSize(LC_deltas))
    names(LC_deltas)[nlyr(LC_deltas)] <- "cell_area"
  }

  return(LC_deltas)
}

loadRefMap <- function(ref_map_file_name,
                       ref_map_type,
                       ref_map_LC_classes) {

  ref_map <- rast(ref_map_file_name)

  if (ref_map_type == "discrete") {
    ref_map <- segregate(ref_map) * cellSize(ref_map,
                                             unit = "km")
  }

  names(ref_map) <- ref_map_LC_classes

  return(ref_map)
}

assignRefMapCells <- function(ref_map,
                              LC_deltas_coords,
                              LC_deltas_cell_numbers) {

  start_time <- Sys.time()

  ref_map_coords <- crds(ref_map)
  ref_map_assigned <- FNN::get.knnx(LC_deltas_coords,
                                    ref_map_coords,
                                    1,
                                    algorithm = "kd_tree")
  ref_map_assigned <- cbind(ref_map_coords,
                            LC_deltas_cell_numbers[ref_map_assigned$nn.index])
  colnames(ref_map_assigned) <- c("x",
                                  "y",
                                  "coarse_ID")
  ref_map_assigned_raster <- c(rast(ref_map_assigned,
                               type = "xyz"))
  ref_map_polygons <- as.polygons(ref_map_assigned_raster)

  # Time check
  end_time <- Sys.time()
  timeCheckMessage(start_time,
                   end_time,
                   "Assigned reference map cells to coarse-scale map cells in ")

  return(ref_map_polygons)
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
