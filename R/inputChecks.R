
inputChecks <- function(ref_map_file_name,
                        LC_deltas_file_list,
                        LC_deltas_type,
                        ref_map_type,
                        cell_size_unit,
                        match_LC_classes,
                        kernel_radius,
                        harmonisation_radius,
                        simulation_type,
                        discrete_output_map,
                        random_seed,
                        output_file_prefix,
                        output_dir_path,
                        fuzzy_multiplier) {
  
  if (!inherits(ref_map_file_name, 
                what = "character")) {
    stop("Name of the reference map must be of class character")
  }
  
  if (!inherits(LC_deltas_file_list, 
                what = "list")) {
    stop("The names of LULC change maps must be provided as a list")
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
  
  if (!inherits(match_LC_classes, 
                what = "matrix")) {
    stop("Match_LC_classes must be of class matrix")
  }
  
  apply(match_LC_classes, 
        MARGIN = 1, 
        FUN = checkMatchLCClassesRows)
  
  if (kernel_radius %% 1 != 0) {
    stop("Kernel radius must be an integer")
  }
  
  if (harmonisation_radius %% 1 != 0) {
    stop("Harmonisation radius must be an integer")
  }
  
  if(harmonisation_radius < 1 | harmonisation_radius > 5) {
    stop("Harmonisation radius must be between 1 and 5")
  }
  
  if (!simulation_type %in% c("deterministic", "fuzzy", "null_model")) {
    stop("Simulation type must be one of \"deterministic\", \"fuzzy\", or \"null_model\"")
  }
  
  if (!inherits(discrete_output_map, 
                what = "logical")) {
    stop("Discrete_output_map must be logical")
  }
  
  if (!inherits(random_seed, 
                what = "numeric")) {
    stop("Random seed must be numeric")
  }
  
  if (!inherits(output_file_prefix, 
                what = "character")) {
    stop("Output file prefix must be of class character")
  }
  
  if (!inherits(output_dir_path, 
                what = "character")) {
    stop("Output directory path must be of class character")
  }
  
  if (!inherits(fuzzy_multiplier, 
                what = "numeric")) {
    stop("Fuzzy multiplier must be numeric")
  }
}

checkMatchLCClassesRows <- function(tmp_row) {
  
  tolerance <- 1e-8
  one_upper_limit <- 1 + tolerance
  one_lower_limit <- 1 - tolerance
  
  # All numbers must be between zero and one
  if (all(tmp_row >= 0 & tmp_row <= one_upper_limit)) {
    
    # For fixed proportions
    if (any(tmp_row > 0 + tolerance & tmp_row < one_lower_limit)) {
      
      # Must sum to 1
      if (!(sum(tmp_row) > one_lower_limit & sum(tmp_row) < one_upper_limit)) {
        stop("Rows in match_LC_classes must sum to 1 if matching LULC classes using fixed proportions")
      }
    }
    
  } else {
    stop("All numbers in match_LC_classes matrix must be between 0 and 1")
  }
}

inputMapChecks <- function(LC_deltas,
                           ref_map,
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

}
