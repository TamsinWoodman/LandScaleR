#' Select cell from land-use data frame
#'
#' Selects a cell from the `raw_land_use` data frame using latitude and
#'   longitude values.
#'
#' @param land_use_df Data frame of raw land use values from PLUMv2 containing
#'   columns named 'Lat' and 'Lon'.
#' @param lat Value of latitude for the cell you want to select.
#' @param lon Value of longitude for the cell you want to select.
#'
#' @return A row from the `land_use_df` that contains a single grid cell.
selectCell <- function(land_use_df,
                       lat,
                       lon) {

  cell_row <- land_use_df[land_use_df$Lat == lat & land_use_df$Lon == lon, ]

  return(cell_row)

}

#' Select region from land-use data frame
#'
#' Returns a rectangular region from the `raw_land_use` data frame using
#'   latitude and longitude values.
#'
#' @param land_use_df Data frame of raw land use values from PLUMv2 containing
#'   columns named 'Lat' and 'Lon'.
#' @param min_lat Minimum latitude value of the region to select.
#' @param max_lat Maximum latitude value of the region to select.
#' @param min_lon Minimum longitude value of the region to select.
#' @param max_lon Maximum value of longitude for the region to select.
#'
#' @return A data frame containing rows with latitude and longitude values
#'   within the specified limits.
selectRegion <- function(land_use_df,
                         min_lat,
                         max_lat,
                         min_lon,
                         max_lon) {

  new_land_use_df <- land_use_df[(land_use_df$Lat >= min_lat & land_use_df$Lat <= max_lat) & (land_use_df$Lon >= min_lon & land_use_df$Lon <= max_lon), ]

  return(new_land_use_df)

}

#' Calculate land-use fraction
#'
#' Calculates land-use fraction when given the area of a specific land-use
#'   within a cell and the total area of the cell.
#'
#' @param land_use_area The area of a specific land-use, for example cropland,
#'   within a grid cell.
#' @param total_area The total area of the grid cell.
#'
#' @return The fraction of the area within the cell that is covered by the
#'   given land-use.
calculateLandUseFraction <- function(land_use_area, total_area) {
  land_use_fraction <- land_use_area / total_area

  return(land_use_fraction)
}
