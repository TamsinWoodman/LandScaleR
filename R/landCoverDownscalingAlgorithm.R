# This script contains only high-level functions for running the entire land
# cover downscaling algorithm

#' Downscale land use and land cover projections
#'
#' Implements the 'LandScaleR' downscaling algorithm, which converts coarse
#'   resolution land use and land cover (LULC) change maps to a finer resolution
#'   by applying LULC change from a time series of maps to a fine resolution
#'   reference map.
#'
#' @param ref_map_file_name Character, file path to the fine resolution
#'   reference map. The reference map must be at the resolution to which you
#'   want to downscale the input LULC change maps. File format of the reference
#'   map must be one that can be read by the [terra::rast()] function from the
#'   'terra' R package. It is recommended to use the GeoTIFF file format because
#'   [downscaleLC()] outputs downscaled LULC maps as GeoTIFF files.
#' @param LC_deltas_file_list List, gives the file paths of a time series of
#'   LULC change maps. Each file should contain one timestep of LULC change, and
#'   the list must be in the order of maps in the time series. There must be one
#'   layer per LULC class in every file. An additional layer specifying the
#'   total land area per coarse resolution cell can optionally be included in
#'   every LULC change map; this layer must be named "cell_area". Each LULC
#'   change map must be in a file format that can be read by the [terra::rast()]
#'   function, although the GeoTIFF format is recommended.
#' @param LC_deltas_classes Character vector of LULC classes that occur in the
#'   LULC change maps.
#' @param ref_map_type Character, one of "areas" or "discrete". If the reference
#'   map contains one LULC class per cell use "areas". If the reference map has
#'   one layer per LULC class containing the area of each LULC class per cell
#'   use "discrete".
#' @param cell_size_unit Character, unit for calculating grid cell areas. Must
#'   be one of "km", "m", or "ha". Cell areas are calculated using the
#'   [terra::cellSize()] function.
#' @param match_LC_classes Matrix, specifies the proportion of each LULC class
#'   from the LULC change maps that corresponds to every reference map LULC
#'   class. Columns contain reference map LULC classes and rows LULC change map
#'   classes. See details for more information.
#' @param  kernel_radius Numeric, radius of cells to include in the kernel
#'   density calculation. Defaults to 1, which means all first neighbour cells
#'   will be used to calculate kernel density (eight cells in total).
#' @param simulation_type Character, the method of LULC allocation to be used in
#'   LandScaleR. One of "deterministic", "fuzzy", or "null_model". See details
#'   for more information.
#' @param fuzzy_multiplier Numeric, the \eqn{f} parameter for the "fuzzy" method
#'   of LULC allocation. See details for more information.
#' @param discrete_output_map Logical, output a discrete downscaled LULC map as
#'   well as an area-based LULC map if `discrete_output_map = TRUE`.
#' @param random_seed Numeric, random seed for the simulation.
#' @param output_file_prefix Character, prefix for the output downscaled LULC
#'   map files.
#' @param output_dir_path Character, path to the directory in which to saved the
#'   downscaled LULC map files. The directory will be created by [downscaleLC()]
#'   if it does not already exist.
#'
#' @details ## Input maps
#' The fine resolution reference map must be at the resolution to which the
#'   LULC change maps are to be downscaled. The reference and LULC maps must be
#'   in the same geographic projection but they do not have to cover exactly the
#'   same extent. The reference map can either contain one LULC class per cell
#'   (`ref_map_type = "discrete`) or the area of each LULC class per cell
#'   (`ref_map_type = "areas"`). Note that if `ref_map_type = discrete` is
#'   specified then the LULC classes in the reference map will be prepended with
#'   "LC" to ensure that they are treated as characters by LandScaleR. The LULC
#'   change maps and reference map do not have to contain the same LULC classes,
#'   as these can be matched using the `match_LC_classes` argument.
#'
#' ## Grid cell areas
#'   The total terrestrial area of every coarse and fine resolution grid cell is
#'   required in LandScaleR to ensure that the areas of coarse and fine
#'   resolution grid cells match. The algorithm uses the [terra::cellSize()]
#'   function to calculate grid cell areas in the reference map and LULC change
#'   maps. Alternatively, for the LULC change maps an extra layer called
#'   "cell_area" can be added to each file giving the area of every coarse
#'   resolution grid cell. This can be useful in situations where only a small
#'   area of a coarse resolution grid cell is covered by land, and the rest is
#'   water or ocean that is not to be included in the downscaling process.
#'
#' ## Matching LULC classes
#'   The `match_LC_classes` argument allows for cases where the input LULC
#'   change maps and reference map do not contain the same LULC classes. Columns
#'   of the matrix must contain the reference map LULC classes and rows the LULC
#'   change map classes. Values specify the proportion of each LULC change map
#'   class that should be matched to each reference map LULC class. For example:
#'
#'   | | Primary vegetation | Secondary vegetation | Forest plantation | Cropland | Pasture | Urban |
#'   | --- | --- | --- | --- | --- | --- | --- |
#'   | Managed forest | 0 | 0 | 1 | 0 | 0 | 0 |
#'   | Unmanaged forest | 0.5 | 0.5 | 0 | 0 | 0 | 0 |
#'   | Other natural | 0.5 | 0.5 | 0 | 0 | 0 | 0 |
#'   | Cropland | 0 | 0 | 0 | 1 | 0 | 0 |
#'   | Pasture | 0 | 0 | 0 | 0 | 1 | 0 |
#'   | Urban | 0 | 0 | 0 | 0 | 0 | 1 |
#'
#'   In this case, if unmanaged forest were to increase by 100
#'   \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}} in an LULC change map, 50
#'   \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}} of the increase would be
#'   allocated to the reference map as primary vegetation and a further 50
#'   \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}} would be allocated as
#'   secondary vegetation.
#'
#' ## Kernel density
#'   Kernel densities are calculated by LandScaleR for every reference map grid
#'   cell and LULC class. A kernel density gives the distance-weighted sum of
#'   the area of each LULC class surrounding a focal cell
#'   \insertCite{LePage2016}{LandScaleR}. Kernel densities are used later in
#'   LandScaleR to decide where to place LULC change on the reference map.
#'
#' ## LULC allocation methods
#'   Three methods are provided in LandScaleR to allocate LULC change to the
#'   reference map, which vary the order in which reference map grid cells are
#'   selected to receive new LULC. The three methods are:
#'
#'   * Quasi-deterministic ("deterministic")
#'   * Fuzzy ("fuzzy")
#'   * Random ("null_model")
#'
#'   In the quasi-deterministic method, grid cells are allocated new LULC in
#'   order from the cell with the highest kernel density for the increasing LULC
#'   class to the cell with the lowest. Any cells with a kernel density equal to
#'   zero are allocated new LULC randomly after LandScaleR has attempted to
#'   allocate new LULC in all cells with a kernel density greater than zero.
#'
#'   Similarly, in the fuzzy method of LULC allocation new areas of LULC are
#'   allocated in order from the cell with the highest to the one with the
#'   lowest kernel density. However, in the fuzzy method kernel densities are
#'   adjusted to alter the likelihood of each grid cell receiving new LULC. For
#'   a single reference map grid cell, the kernel density is adjusted by summing
#'   the kernel density and an adjustment drawn from the Normal distribution
#'   with mean of zero. The standard deviation for the Normal distribution is
#'   the standard deviation of kernel densities for all cells eligible to
#'   receive the increasing LULC class, multiplied by the user-specified \eqn{f}
#'   parameter (`fuzzy_multiplier`). The higher the \eqn{f} parameter, the more
#'   the order in which grid cells are allocated new LULC is shuffled. Increased
#'   shuffling of the order of grid cells leads to more random patterns of LULC
#'   in the output downscaled maps.
#'
#'   In the random method of LULC allocation new areas of LULC are randomly
#'   placed in eligible grid cells on the reference map.
#'
#'
#' @return GeoTIFF files containing downscaled land-use maps. If
#'   `discrete_output_map = TRUE` two output files will be generated per
#'   timestep: one with the area of each LULC class per cell and one with the
#'   largest LULC class per cell.
#' @references
#' \insertRef{LePage2016}{LandScaleR}
#' @importFrom Rdpack reprompt
#' @importFrom methods new slot slot<-
#' @importFrom utils read.table stack write.table
#' @export
downscaleLC <- function(ref_map_file_name,
                        LC_deltas_file_list,
                        LC_deltas_classes,
                        ref_map_type = "areas",
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

  ## Check inputs
  inputChecks(ref_map_file_name = ref_map_file_name,
              LC_deltas_file_list = LC_deltas_file_list,
              LC_deltas_classes = LC_deltas_classes,
              ref_map_type = ref_map_type,
              cell_size_unit = cell_size_unit,
              match_LC_classes = match_LC_classes,
              kernel_radius = kernel_radius,
              simulation_type = simulation_type,
              discrete_output_map = discrete_output_map,
              random_seed = random_seed,
              output_file_prefix = output_file_prefix,
              output_dir_path = output_dir_path,
              fuzzy_multiplier = fuzzy_multiplier)

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
                                timestep = timestep,
                                cell_size_unit = cell_size_unit)

      # Load ref map file
      ref_map <- loadRefMap(ref_map_file_name = ref_map_file_name,
                            ref_map_type = ref_map_type,
                            cell_size_unit = cell_size_unit)

      # Input checks
      inputMapChecks(LC_deltas,
                     ref_map,
                     match_LC_classes)

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
                                timestep = timestep,
                                cell_size_unit = cell_size_unit)

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
    mosaic_chunks <- ceiling(length(downscaled_tiles) / 100)

    if (mosaic_chunks == 1) {

      downscaled_map <- terra::mosaic(terra::sprc(downscaled_tiles))

    } else if (mosaic_chunks > 1) {

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

    # Set names of downscaled map as these are lost during mosaic function
    names(downscaled_map) <- names(ref_map)

    # Save land cover map
    terra::writeRaster(downscaled_map,
                       filename = file.path(output_dir_path,
                                            paste0(output_file_prefix,
                                                   "_Time",
                                                   timestep,
                                                   ".tif")),
                       overwrite = TRUE)

    if (discrete_output_map) {
      cat_downscaled_map <- terra::which.max(downscaled_map)
      levels(cat_downscaled_map) <- data.frame(id = 1:terra::nlyr(ref_map),
                                               Land_cover = names(ref_map))
      terra::writeRaster(cat_downscaled_map,
                         filename = file.path(output_dir_path,
                                              paste0(output_file_prefix,
                                                     "_Discrete_Time",
                                                     timestep,
                                                     ".tif")),
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
                         timestep,
                         cell_size_unit) {

  # Load LC_deltas file
  LC_deltas <- terra::rast(LC_deltas_file_list[[timestep]])

  # If cell_area layer is not present, calculate the area of each cell
  if (!"cell_area" %in% names(LC_deltas)) {
    LC_deltas <- c(LC_deltas,
                   terra::cellSize(LC_deltas[[1]],
                                   unit = cell_size_unit))
    names(LC_deltas)[terra::nlyr(LC_deltas)] <- "cell_area"
  }

  return(LC_deltas)
}

loadRefMap <- function(ref_map_file_name,
                       ref_map_type,
                       cell_size_unit) {

  ref_map <- terra::rast(ref_map_file_name)

  if (ref_map_type == "discrete") {
    ref_map <- terra::segregate(ref_map) * terra::cellSize(ref_map,
                                                           unit = cell_size_unit)

    # Add LC to the start of names so that they are treated as characters and
    # not as numeric during the downscaling algorithm
    ref_map_names <- names(ref_map)
    names(ref_map) <- paste0("LC",
                             ref_map_names)
  }

  return(ref_map)
}

#' Assign fine resolution grid cells to a coarse resolution map
#'
#' Assigns grid cells from a fine resolution map to their nearest neighbour grid
#'   cell in a coarse resolution map using the [FNN::get.knnx()] function.
#'
#' @param ref_map [`terra::SpatRaster`] object at fine resolution.
#' @param LC_deltas_coords Matrix, coordinates of coarse resolution grid cells.
#'   Can be obtained from a [`terra::SpatRaster`] object using the
#'   [terra::crds()] function.
#' @param LC_deltas_cell_numbers Numeric, cell numbers that uniquely identify
#'   each cell in the coarse resolution map. Should correspond to the order of
#'   coordinates in `LC_deltas_coords`. Can be obtained from a
#'   [`terra::SpatRaster`] object using the [terra::cells()] function.
#'
#' @return [`terra::SpatVector`] object of numbered polygons. The number of each
#'   polygon corresponds to the unique identifier of a coarse resolution grid
#'   cell. Each polygon encapsulates a set of fine resolution cells that have
#'   been assigned to that coarse resolution cell.
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
                                         type = "xyz",
                                         crs = terra::crs(ref_map))
  ref_map_polygons <- terra::as.polygons(ref_map_assigned_raster)

  # Time check
  end_time <- Sys.time()
  timeCheckMessage(start_time,
                   end_time,
                   "Assigned reference map cells to coarse-scale map cells in ")

  return(ref_map_polygons)
}
