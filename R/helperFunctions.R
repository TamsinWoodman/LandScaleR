#' Save land cover maps as .txt files
#'
#' Saves output land cover maps as tab-delimited text files with the `.txt` file
#'   extension.
#'
#' @param LC_map Land cover map data frame.
#' @param file_prefix Prefix for land cover map output file.
#' @param dir_path Path to directory in which to save the output file.
#' @param time_step Time step for the land cover map.
#'
#' @return Tab-delimited text file of the given land cover map.
saveLandCoverMapAsTable <- function(LC_map,
                                    file_prefix = "",
                                    dir_path = "",
                                    time_step) {

  file_path <- generateOutputFilePath(dir_path = dir_path,
                                      file_prefix = file_prefix,
                                      time_step = time_step)

  write.table(LC_map,
              file = file_path,
              sep = "\t",
              row.names = FALSE,
              quote = FALSE)

  print(paste0("Saved downscaled land cover map to file: ",
               file_path))
}

#' Get output file path string
#'
#' @inheritParams saveLandCoverMapAsTable
#'
#' @return String giving the path to an output file. The path is made up of the
#'   directory path, file prefix, timestep, and the suffix 'DownscaledLC.txt'.
generateOutputFilePath <- function(dir_path,
                                   file_prefix,
                                   time_step) {

  LC_file_path <- paste0(dir_path,
                        file_prefix,
                        "_Time",
                        time_step,
                        "_DownscaledLC",
                        ".txt")

  return(LC_file_path)
}

#' Simulate land cover data
#'
#' Simulates land cover data of a given resolution and size using random
#'   sampling. For each grid cell, the `sample` function is used to sample from
#'   the number of input land cover types. The cell size determines the number
#'   of items sampled. The number of times each land cover type occurs in the
#'   sample is the area of that land cover type in the output map.
#'
#' @param LC_types Vector of land cover types to simulate.
#' @param res_x Size of one grid cell in the x-direction.
#' @param res_y Size of one grid cell in the y-direction.
#' @param number_cells_x Number of cells in the x-direction.
#' @param number_cells_y Number of cells in the y-direction.
#' @param map_type Simulate a map with area of each land cover class per cell
#'   ("areas") or one land cover class per cell ("discrete").
#'
#' @return Data frame of land cover. The first two columns give the x- and
#'   y-coordinates of grid cells. If `map_type = "areas"` is specified the
#'   remaining columns contain the simulated area of each land cover type. If
#'   `map_type = "discrete"` is used, there is one other column with a discrete
#'   land cover class for each cell.
simulateLandCoverData <- function(LC_types,
                                  res_x,
                                  res_y,
                                  number_cells_x,
                                  number_cells_y,
                                  map_type = "areas") {

  number_LC_types <- length(LC_types)
  cell_size <- res_x * res_y
  number_cells <- number_cells_x * number_cells_y

  ## Set up land cover data
  simulated_LC_matrix <- switch(map_type,
                                "areas" = simulateAreaBasedLC(number_cells = number_cells,
                                                              number_LC_types = number_LC_types,
                                                              cell_size = cell_size),
                                "discrete" = simulateDiscreteLC(number_cells = number_cells,
                                                                LC_types = LC_types))

  ## Set up x and y
  x_start <- res_x / 2
  x_end <- (res_x * number_cells_x) - (res_x / 2)
  y_start <- res_y / 2
  y_end <- (res_y * number_cells_y) - (res_y / 2)

  cell_coords <- data.frame(x = rep(seq(x_start,
                                        x_end,
                                        res_x),
                                    number_cells_x),
                              y = rep(seq(y_start,
                                          y_end,
                                          res_y),
                                      each = number_cells_y))

  ## Combine x and y and land cover types
  simulated_LC_df <- cbind(cell_coords,
                           simulated_LC_matrix)

  if(map_type == "areas") {
    colnames(simulated_LC_df)[3:ncol(simulated_LC_df)] <- LC_types

  } else if(map_type == "discrete") {
    colnames(simulated_LC_df)[3] <- "Land_cover"
  }

  return(simulated_LC_df)
}

simulateAreaBasedLC <- function(number_cells,
                                number_LC_types,
                                cell_size) {
  simulated_LC <- c()
  for (i in 1:number_cells) {

    simulated_LC <- c(simulated_LC,
                      as.vector(table(factor(sample(1:number_LC_types,
                                                    size = cell_size,
                                                    replace = TRUE),
                                             levels = 1:number_LC_types))))
  }

  simulated_LC_matrix <- matrix(data = simulated_LC,
                                nrow = number_cells,
                                ncol = number_LC_types,
                                byrow = TRUE)

  return(simulated_LC_matrix)
}

simulateDiscreteLC <- function(number_cells,
                               LC_types) {

  simulated_LC <- sample(LC_types,
                         size = number_cells,
                         replace = TRUE)

  return(simulated_LC)
}

timeCheckMessage <- function(start_time,
                             end_time,
                             message) {

  time_taken <- end_time - start_time

  print(paste0(message,
               time_taken,
               " hours"))
}
