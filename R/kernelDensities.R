
getDistMatrix <- function(kernel_radius) {

  ncol_and_nrow <- (kernel_radius * 2) + 1
  distance_res <- 1
  distance_raster <- terra::rast(nrows = ncol_and_nrow,
                          ncols = ncol_and_nrow,
                          xmin = -(ncol_and_nrow * distance_res) / 2,
                          xmax = (ncol_and_nrow * distance_res) / 2,
                          ymin = -(ncol_and_nrow * distance_res) / 2,
                          ymax = (ncol_and_nrow * distance_res) / 2)
  central_coords <- kernel_radius + 1
  distance_raster[central_coords, central_coords] <- 1
  distance_vec <- terra::distance(terra::crds(distance_raster),
                                  terra::crds(distance_raster,
                                  na.rm = FALSE),
                           lonlat = FALSE)
  distance_mat <- matrix(distance_vec,
                         nrow = ncol_and_nrow,
                         ncol = ncol_and_nrow,
                         byrow = TRUE)
  distance_mat[central_coords, central_coords] <- NA
  distance_mat <- (distance_mat ^ 2)

  return(distance_mat)
}

kernelDensitiesEquation <- function(x,
                                    cell_distances,
                                    ...) {

  focal_vals_matrix <- matrix(x,
                              nrow = nrow(cell_distances),
                              ncol = ncol(cell_distances),
                              byrow = TRUE)
  values_over_distance <- focal_vals_matrix / cell_distances
  kernel_density <- sum(values_over_distance, na.rm = TRUE) / sum(!is.na(values_over_distance))

  return(kernel_density)
}

calculateKernelDensities <- function(ref_map,
                                     distance_mat) {
  
  kd_start <- Sys.time()

  kernel_densities <- ref_map

  for (i in 1:terra::nlyr(kernel_densities)) {
    kernel_densities[[i]] <- terra::focal(kernel_densities[[i]],
                                          w = ncol(distance_mat),
                                          fun = kernelDensitiesEquation,
                                          cell_distances = distance_mat,
                                          na.policy = "omit")
  }
  
  kd_end <- Sys.time()
  timeCheckMessage(kd_start,
                   kd_end,
                   "Calculated kernel densities in ")

  return(kernel_densities)
}
