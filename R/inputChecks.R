
inputChecks <- function(ref_map_file_name,
                        LC_deltas_file_list,
                        LC_deltas_classes,
                        ref_map_type,
                        cell_size_unit,
                        match_LC_classes,
                        kernel_radius,
                        simulation_type,
                        discrete_output_map,
                        random_seed,
                        output_file_prefix,
                        output_dir_path,
                        fuzzy_multiplier) {
  
  if (class(ref_map_file_name) != "character") {
    stop("Name of the reference map must be of class character")
  }
  
  if (class(LC_deltas_file_list) != "list") {
    stop("The names of LULC change maps must be provided as a list")
  }
  
  if (class(LC_deltas_classes) != "character") {
    stop("The names of LULC change map classes must be of class character")
  }
  
  if (!LC_deltas_type %in% c("areas", "proportions")) {
    stop("LULC change map type must be one of \"areas\" or \"proportions\"")
  }
  
  if (!ref_map_type %in% c("areas", "discrete")) {
    stop("Reference map type must be one of \"areas\" or \"discrete\"")
  }
  
  if (!cell_size_unit %in% c("km", "m", "ha")) {
    stop("The cell size unit must be one of \"km\", \"m\", or \"ha\"")
  }
  
  if (class(match_LC_classes)[1] != "matrix") {
    stop("Match_LC_classes must be of class matrix")
  }
  
  if (!all(LC_deltas_classes == rownames(match_LC_classes))) {
    stop("Row names of the match_LC_classes matrix must be the same as the LC_deltas_classes argument")
  }
  
  if (class(kernel_radius) != "numeric") {
    stop("Kernel radius must be numeric")
  }
  
  if (!simulation_type %in% c("deterministic", "fuzzy", "null_model")) {
    stop("Simulation type must be one of \"deterministic\", \"fuzzy\", or \"null_model\"")
  }
  
  if (class(discrete_output_map) != "logical") {
    stop("Discrete_output_map must be logical")
  }
  
  if (class(random_seed) != "numeric") {
    stop("Random seed must be numeric")
  }
  
  if (class(output_file_prefix) != "character") {
    stop("Output file prefix must be of class character")
  }
  
  if (class(output_dir_path) != "character") {
    stop("Output directory path must be of class character")
  }
  
  if (class(fuzzy_multiplier) != "numeric") {
    stop("Fuzzy multiplier must be numeric")
  }
}

inputMapChecks <- function(LC_deltas,
                           ref_map,
                           LC_deltas_type,
                           match_LC_classes) {

  if (!terra::crs(LC_deltas) == terra::crs(ref_map)) {
    stop("Projections of land cover change and reference maps do not match")
  }

  if (!all(rownames(match_LC_classes) %in% names(LC_deltas))) {
    stop("Row names of the match_LC_classes matrix are not all present in the land-use change map")
  }

  if (!all(colnames(match_LC_classes) %in% names(ref_map))) {
    stop("Column names of the match_LC_classes matrix are not all present in the reference map")
  }
  
  if (LC_deltas_type == "proportions" & "cell_area" %in% names(LC_deltas)) {
    warning("Cell areas do not need to be supplied if LC_deltas_type is \"proportions\"")
  }

}
