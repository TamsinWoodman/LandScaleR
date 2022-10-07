
inputMapChecks <- function(LC_deltas,
                           ref_map) {

  if (!terra::crs(LC_deltas) == terra::crs(ref_map)) {
    stop("Projections of land cover change and reference maps do not match")
  }

}
