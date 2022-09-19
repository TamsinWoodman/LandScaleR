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
#' @param ref_map_type Specifies whether the reference map is discrete (contains
#'   one land cover class per cell) or area-based (provides the area of each
#'   land cover class in each cell). Must be one of "areas" or "discrete".
#' @param ref_map_LC_classes Vector of land cover types in the reference map. For
#'   an area-based reference map, all land cover types should be column names in
#'   the reference map.
#' @param cell_size_unit The unit that cell areas should be calculated in. Must
#'   be one of "km", "m", or "ha".
#' @param match_LC_classes A matrix containing the fraction of each coarse-scale
#'   land cover class that contributes to each reference map land cover class.
#'   Columns should contain reference map land cover classes, and rows are the
#'   coarse-scale land cover classes. Each cell should contain the proportion of
#'   the coarse-scale land cover class that contributes to the fine-scale class in
#'   the output map.
#' @param kernel_radius Radius of cells to include in the kernel density
#'   calculation. A value of 1 means that the neighbour cells used to calculate
#'   kernel density will be 1 cell in every direction around the focal cell.
#'   Defaults to 1.
#' @param simulation_type Specifies the method of land-use change allocation to
#'   use. Can be 'deterministic', 'fuzzy', or 'null_model'. 'deterministic is
#'   a deterministic simulation where land-use change is only randomly allocated
#'   if a cell has a kernel density of 0. The fuzzy' option gives a stochastic
#'   simulation where each kernel density is modified by adding a modifier that
#'   is drawn from a Normal distribution with mean of 0 and standard deviation
#'   equal to the standard deviation of kernel density values in cells available
#'   for land-use allocation. The 'null_model' option allocations land-use
#'   change entirely randomly.
#' @param fuzzy_multiplier A value by which the standard deviation of the Normal
#'   distribution used to draw modifiers in the 'fuzzy' method will be
#'   multiplied. Specifying a value of 2 would mean the Normal distribution has
#'   a mean of 0 and standard deviation of 2 times the standard deviation of the
#'   kernel density values in cells available for land-use change allocation.
#' @param discrete_output_map Output discrete land cover as well as area-based
#'   land cover. Default is `FALSE`.
#' @param random_seed Numeric random seed for ordering fine-scale cells with a
#'   kernel density value of zero during land cover allocation. Defaults to the
#'   date and time when the function is called.
#' @param output_file_prefix Prefix for output downscaled land cover map files.
#' @param output_dir_path Path to directory in which to save the downscaled
#'   land cover map files. Will be created if it does not already exist.
#'
#' @return TIFF files containing downscaled land-use maps. If
#'   `discrete_output_map = TRUE` then maps with both the area of each land-use
#'   class per cell and the largest land-use class per cell will be generated.
#' @importFrom methods new slot slot<-
#' @importFrom utils read.table stack write.table
#' @export
downscaleLC <- function(ref_map_file_name,
                        LC_deltas_file_list,
                        LC_deltas_classes,
                        ref_map_type = "areas",
                        ref_map_LC_classes,
                        cell_size_unit = "m",
                        match_LC_classes,
                        kernel_radius,
                        simulation_type = "deterministic",
                        fuzzy_multiplier = 1,
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

  # set the seed
  set.seed(random_seed)

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
                            ref_map_LC_classes = ref_map_LC_classes,
                            cell_size_unit = cell_size_unit)

      # Assign ref map cells to LC_deltas cells
      LC_deltas_coords <- terra::crds(LC_deltas)
      LC_deltas_cell_numbers <- terra::cellFromXY(LC_deltas,
                                                  LC_deltas_coords)

      # Get polygons of ref map cells for each coarse cell
      ref_map_polygons <- assignRefMapCells(ref_map = ref_map,
                                            LC_deltas_coords = LC_deltas_coords,
                                            LC_deltas_cell_numbers = LC_deltas_cell_numbers)

      # Calculate kernel densities
      kernel_densities <- calculateKernelDensities(ref_map = ref_map,
                                                   distance_mat = distance_mat)
      print("Calculated kernel densities")

      # Get a list of coarse-scale cells
      coarse_cell_list <- lapply(LC_deltas_cell_numbers,
                                 FUN = CoarseCellFromRaster,
                                 LC_deltas = LC_deltas,
                                 LC_deltas_classes = LC_deltas_classes,
                                 ref_map_polygons = ref_map_polygons,
                                 ref_map = ref_map,
                                 kernel_densities = kernel_densities)
      names(coarse_cell_list) <- LC_deltas_cell_numbers

    } else {

      # Load LC_deltas file
      LC_deltas <- loadLCDeltas(LC_deltas_file_list = LC_deltas_file_list,
                                timestep = timestep)

      # Calculate kernel densities
      kernel_densities <- calculateKernelDensities(ref_map = downscaled_map,
                                                   distance_mat = distance_mat)

      # Get list of coarse-scale cells
      coarse_cell_list <- lapply(coarse_cell_list,
                                 FUN = updateCoarseCell,
                                 LC_deltas = LC_deltas,
                                 LC_deltas_classes = LC_deltas_classes,
                                 kernel_densities = kernel_densities,
                                 ref_map_polygons = ref_map_polygons)

    }

    #### Run downscaling
    allocation_start <- Sys.time()
    print("Starting land cover allocation...")

    coarse_cell_list <- lapply(coarse_cell_list,
                               downscaleLCForOneCoarseCell,
                               match_LC_classes = match_LC_classes,
                               simulation_type = simulation_type,
                               fuzzy_multiplier = fuzzy_multiplier)

    allocation_end <- Sys.time()
    timeCheckMessage(allocation_start,
                     allocation_end,
                     "Completed land-use allocation in ")

    # Run harmonisation with unallocated land cover change
    harmonisation_start <- Sys.time()
    print("Starting harmonisation...")

    coarse_cell_list <- harmoniseUnallocatedLC(coarse_cell_list = coarse_cell_list,
                                               simulation_type = simulation_type,
                                               fuzzy_multiplier = fuzzy_multiplier)

    harmonisation_end <- Sys.time()
    timeCheckMessage(harmonisation_start,
                     harmonisation_end,
                     "Completed harmonisation in ")

    # Check whether total area of reference cells assigned to each coarse cell
    # matches before and after downscaling
    lapply(coarse_cell_list,
           FUN = areasMatch)

    # Create the downscaled map
    ## if there are more than 100 tiles, pplit the list of tiles into chunks
    ## which each contain 100 tiles
    ## Mosaic each chunk, then mosaic the resulting chunks to speed up/reduce
    ## memory requirements
    downscaled_tiles <- lapply(coarse_cell_list,
                               FUN = refCells)
    mosaic_chunks <- round(length(downscaled_tiles) / 100)

    if (mosaic_chunks == 0) {

      downscaled_map <- terra::mosaic(terra::sprc(downscaled_tiles))

    } else if (mosaic_chunks > 0) {

      downscaled_chunks <- vector(mode = "list",
                                  length = length(mosaic_chunks))

      for (chunk in 1:mosaic_chunks) {

        end_pos <- chunk * 100
        start_pos <- end_pos - 99
        end_pos <- ifelse(end_pos > length(downscaled_tiles),
                          length(downscaled_tiles),
                          end_pos)

        downscaled_chunks[[chunk]] <- terra::mosaic(terra::sprc(downscaled_tiles[start_pos:end_pos]))
      }

      downscaled_map <- terra::mosaic(terra::sprc(downscaled_chunks))
    }

    # Save land cover map
    terra::writeRaster(downscaled_map,
                       filename = paste0(output_dir_path,
                                         output_file_prefix,
                                         "_Time",
                                         timestep,
                                         ".tif"),
                       overwrite = TRUE)

    if (discrete_output_map) {
      cat_downscaled_map <- terra::which.max(downscaled_map)
      levels(cat_downscaled_map) <- data.frame(id = 1:terra::nlyr(ref_map),
                                               Land_cover = names(ref_map))
      terra::writeRaster(cat_downscaled_map,
                         filename = paste0(output_dir_path,
                                           output_file_prefix,
                                           "_Discrete_Time",
                                           timestep,
                                           ".tif"),
                         overwrite = TRUE)
    }

    print(paste0("Completed downscaling timestep ",
                 timestep))
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
  LC_deltas <- terra::rast(LC_deltas_file_list[[timestep]])

  # If cell_area layer is not present, calculate the area of each cell
  if (!"cell_area" %in% names(LC_deltas)) {
    LC_deltas <- c(LC_deltas,
                   terra::cellSize(LC_deltas))
    names(LC_deltas)[terra::nlyr(LC_deltas)] <- "cell_area"
  }

  return(LC_deltas)
}

loadRefMap <- function(ref_map_file_name,
                       ref_map_type,
                       ref_map_LC_classes,
                       cell_size_unit) {

  ref_map <- terra::rast(ref_map_file_name)

  if (ref_map_type == "discrete") {
    ref_map <- terra::segregate(ref_map) * terra::cellSize(ref_map,
                                                           unit = cell_size_unit)
  }

  names(ref_map) <- ref_map_LC_classes

  return(ref_map)
}

#' Assign fine resolution cells to a coarse resolution map
#'
#' @param ref_map 'SpatRaster' object with fine resolution cells.
#' @param LC_deltas_coords Matrix containing coordinates of coarse resolution
#'   cells. Can be obtained from a 'SpatRaster' using the 'crds' function from
#'   the 'terra' package.
#' @param LC_deltas_cell_numbers Vector of cell numbers from the coarse
#'   resolution map. Can be obtained from a 'SpatRaster' object using the
#'   'cells' function from the 'terra' package
#'
#' @return 'SpatVector' object of numbered polygons. Each polygon encapsulates a
#'   set of fine resolution cells that are assigned to the same coarse
#'   resolution cell.
#'
#' @export
assignRefMapCells <- function(ref_map,
                              LC_deltas_coords,
                              LC_deltas_cell_numbers) {

  start_time <- Sys.time()

  ref_map_coords <- terra::crds(ref_map)
  ref_map_assigned <- FNN::get.knnx(LC_deltas_coords,
                                    ref_map_coords,
                                    1,
                                    algorithm = "kd_tree")
  ref_map_assigned <- cbind(ref_map_coords,
                            LC_deltas_cell_numbers[ref_map_assigned$nn.index])
  colnames(ref_map_assigned) <- c("x",
                                  "y",
                                  "coarse_ID")
  ref_map_assigned_raster <- terra::rast(ref_map_assigned,
                                         type = "xyz")
  ref_map_polygons <- terra::as.polygons(ref_map_assigned_raster)

  # Time check
  end_time <- Sys.time()
  timeCheckMessage(start_time,
                   end_time,
                   "Assigned reference map cells to coarse-scale map cells in ")

  return(ref_map_polygons)
}
