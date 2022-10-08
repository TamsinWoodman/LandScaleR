
inputMapChecks <- function(LC_deltas,
                           ref_map,
                           match_LC_classes) {

  if (!terra::crs(LC_deltas) == terra::crs(ref_map)) {
    stop("Projections of land cover change and reference maps do not match")
  }

  if(!all(rownames(match_LC_classes) %in% names(LC_deltas))) {
    stop("Row names of the match_LC_classes matrix are not all present in the land-use change map")
  }

  if(!all(colnames(match_LC_classes) %in% names(LC_deltas))) {
    stop("Column names of the match_LC_classes matrix are not all present in the reference map")
  }

}
