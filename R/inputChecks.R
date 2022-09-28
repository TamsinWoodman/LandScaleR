
inputMapChecks <- function(LC_deltas,
                           ref_map) {

  if (!crs(LC_deltas) == crs(ref_map)) {
    stop("Projections of land cover change and reference maps do not match")
  }

}
