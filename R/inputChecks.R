
inputChecks <- function(ref_map_file_name,
                        LC_deltas_file_list,
                        LC_deltas_classes,
                        ref_map_type) {
  
  if (class(ref_map_file_name) != "character") {
    stop("Name of the reference map must be of class character")
  }
  
  if (class(LC_deltas_file_list) != "list") {
    stop("The names of LULC change maps must be provided as a list")
  }
  
  if (class(LC_deltas_classes) != "character") {
    stop("The names of LULC change map classes must be of class character")
  }
  
  if (!ref_map_type %in% c("areas", "discrete")) {
    stop("Reference map type must be one of \"areas\" or \"discrete\"")
  }
  
  if (class(ref_map_LC_classes) != "character") {
    stop("The names of reference map classes must be of class character")
  }
  
  if (!cell_size_unit %in% c("km", "m", "ha")) {
    stop("The cell size unit must be one of \"km\", \"m\", or \"ha\"")
  }
  
  
  
  
}

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
