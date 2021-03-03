#' Set up a matrix for land-use data filled with NA.
#'
#' @param land_use_min_lat The minimum latitude in the `land_use_df`.
#' @param land_use_max_lat The maximum latitude in the `land_use_df`.
#' @param land_use_min_lon The minimum longitude in the `land_use_df`.
#' @param land_use_max_lon The maximum longitude in the `land_use_df`.
#' @inheritParams makeLandUseRaster
#'
#' @return A matrix filled with NAs. Columns will be from minimum longitude to
#'   maximum longitude with step size `grid_cell_size`. Rows will be from
#'   maximum latitude to minimum latitude with step size `grid_cell_size`.
setUpLandUseMatrix <- function(grid_cell_size,
                               land_use_min_lat,
                               land_use_max_lat,
                               land_use_min_lon,
                               land_use_max_lon) {

  # Set the latitude and longitude values
  land_use_lat_values <- seq(land_use_min_lat, land_use_max_lat, grid_cell_size)
  land_use_lon_values <- seq(land_use_min_lon, land_use_max_lon, grid_cell_size)

  # Create empty matrix
  land_use_matrix <- matrix(nrow = length(land_use_lat_values),
                            ncol = length(land_use_lon_values))

  # Set matrix row and column names
  colnames(land_use_matrix) <- land_use_lon_values
  rownames(land_use_matrix) <- rev(land_use_lat_values)

  return(land_use_matrix)
}

#' Fill land-use matrix with land-use fractions.
#'
#' @param land_use_matrix An empty land-use matrix created by
#'   `set_up_land_use_matrix`.
#' @inheritParams makeLandUseRaster
#'
#' @return A land-use matrix filled with fractions of the given `land_use_type`
#'   in each grid cell.
fillLandUseMatrix <- function(land_use_df,
                              land_use_type,
                              land_use_matrix) {

  tmp_land_use_matrix <- land_use_matrix

  for (i in 1:nrow(land_use_df)) {

    # Set latitude, longitude and land-use fraction values
    lon_value <- as.character(land_use_df$Lon[i])
    lat_value <- as.character(land_use_df$Lat[i])
    land_use_value <- land_use_df[i, land_use_type]

    # Fill corresponding cell in matrix
    tmp_land_use_matrix[which(rownames(tmp_land_use_matrix) == lat_value), which(colnames(tmp_land_use_matrix) == lon_value)] <- land_use_value
  }

  return(tmp_land_use_matrix)

}

#' Make raster of land-use from PLUMv2 land-use data frame.
#'
#' @param land_use_df Data frame of land-use output from one timestep of a
#'   PLUMv2 run.
#' @param land_use_type The land-use type for which to generate a raster. Must
#'   be one of the column names in `land_use_df`. Default is "cropland".
#' @param grid_cell_size Resolution of the land-use data. Default is 0.5.
#' @param project_string A string that gives the projection for input to the
#'   `raster` function. Default is "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs".
#'
#' @return A raster of the given land-use type. Default is a raster of 0.5
#'   degree grid cells, each one containing the fraction of cropland in that
#'   cell for the PLUMv2 timestep.
#' @export
makeLandUseRaster <- function(land_use_df,
                              land_use_type = "cropland",
                              grid_cell_size = 0.5,
                              project_string = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") {

  if(!is.character(land_use_type)) {
    stop("Land use type must be a string")
  }

  if(!land_use_type %in% colnames(land_use_df)) {
    stop("Land use type must be a column name in the land-use data frame")
  }

  # Find the minimum and maximum latitude values
  land_use_min_lat <- min(land_use_df$Lat)
  land_use_max_lat <- max(land_use_df$Lat)

  # Find the minimum and maximum longitude values
  land_use_min_lon <- min(land_use_df$Lon)
  land_use_max_lon <- max(land_use_df$Lon)

  # Set up an empty matrix to hold the land-use fractions
  tmp_land_use_matrix <- setUpLandUseMatrix(grid_cell_size = grid_cell_size,
                                            land_use_min_lat = land_use_min_lat,
                                            land_use_max_lat = land_use_max_lat,
                                            land_use_min_lon = land_use_min_lon,
                                            land_use_max_lon = land_use_max_lon)

  # Fill the land-use matrix with fractions of the given land_use_type
  land_use_matrix <- fillLandUseMatrix(land_use_df = land_use_df,
                                       land_use_type = land_use_type,
                                       land_use_matrix = tmp_land_use_matrix)

  # Convert the land-use matrix to a raster
  land_use_raster <- raster(land_use_matrix,
                            xmn = land_use_min_lon,
                            xmx = land_use_max_lon + grid_cell_size,
                            ymn = land_use_min_lat,
                            ymx = land_use_max_lat + grid_cell_size,
                            crs = project_string)

  return(land_use_raster)

}

#' Make raster stack of land-uses from a PLUMv2 land-use data frame.
#'
#' @param land_use_df Data frame of land-use output from one timestep of a
#'   PLUMv2 run.
#' @param raster_layers List of land-uses to include in the raster stack. Must
#'   be one of the column names in `land_use_df`.
#'
#' @return A raster stack with each layer corresponding to one land-use.
makeLandUseRasterStack <- function(land_use_df,
                                   raster_layers = c("suitable_fraction",
                                                     "pa_fraction",
                                                     "managedForest_fract",
                                                     "unmanagedForest_fract",
                                                     "otherNatural_fract",
                                                     "cropland_fract",
                                                     "pasture_fract",
                                                     "barren_fract",
                                                     "urban_fract")) {

  if(!all(raster_layers %in% colnames(land_use_df))) {
    stop("All raster layers must be column names in the land-use data frame")
  }

  land_use_rasters <- list()

  for (i in 1:length(raster_layers)) {
    land_use_rasters[[i]] <- makeLandUseRaster(land_use_df = land_use_df,
                                               land_use_type = raster_layers[i])
  }

  land_use_raster_stack <- stack(land_use_rasters)
  names(land_use_raster_stack) <- raster_layers

  return(land_use_raster_stack)
}
