
test_that("inputChecks returns error if match_LC_classes has values more than 1", {
  
  ref_map_file_name <- "data/test/ref_file.tif"
  LC_deltas_file_list <- list(c("data/test/LUC_file.tif"))
  LC_deltas_type <- "areas"
  ref_map_type <- "areas"
  cell_size_unit <- "m"
  coarse_LC_classes <- c("managedForest",
                         "unmanagedForest",
                         "otherNatural",
                         "cropland",
                         "pasture",
                         "barren",
                         "urban")
  ref_LC_classes <- c("pri",
                      "sec",
                      "crp",
                      "pas",
                      "urb")
  match_LC_classes <- matrix(data = c(0, 1, 0.5, 0, 0, 0, 0,
                                      2, 0, 0.5, 0, 0, 1, 0,
                                      0, 0, 0, 1, 0, 0, 0,
                                      0, 0, 0, 0, 1, 0, 0,
                                      0, 0, 0, 0, 0, 0, 1),
                             nrow = 7,
                             ncol = 5,
                             dimnames = list(coarse_LC_classes,
                                             ref_LC_classes))
  kernel_radius <- 1
  simulation_type <- "fuzzy"
  fuzzy_multiplier <- 1
  discrete_output_map <- TRUE
  random_seed <- 34
  output_file_prefix <- "outputs"
  output_dir_path <- "test/"
  
  expect_error(inputChecks(ref_map_file_name = ref_map_file_name,
                           LC_deltas_file_list = LC_deltas_file_list,
                           LC_deltas_type = LC_deltas_type,
                           ref_map_type = ref_map_type,
                           cell_size_unit = cell_size_unit,
                           match_LC_classes = match_LC_classes,
                           kernel_radius = kernel_radius,
                           simulation_type = simulation_type,
                           discrete_output_map = discrete_output_map,
                           random_seed = random_seed,
                           output_file_prefix = output_file_prefix,
                           output_dir_path = output_dir_path,
                           fuzzy_multiplier = fuzzy_multiplier),
               "All values in match_LC_classes must be between 0 and 1")
  
})

test_that("inputChecks returns error if match_LC_classes has values less than 0", {
  
  ref_map_file_name <- "data/test/ref_file.tif"
  LC_deltas_file_list <- list(c("data/test/LUC_file.tif"))
  LC_deltas_type <- "areas"
  ref_map_type <- "areas"
  cell_size_unit <- "m"
  coarse_LC_classes <- c("managedForest",
                         "unmanagedForest",
                         "otherNatural",
                         "cropland",
                         "pasture",
                         "barren",
                         "urban")
  ref_LC_classes <- c("pri",
                      "sec",
                      "crp",
                      "pas",
                      "urb")
  match_LC_classes <- matrix(data = c(0, 1, 0.5, 0, 0, 0, 0,
                                      1, 0, 0.5, 0, 0, 1, 0,
                                      0, 0, 0, 1, 0, 0, 0,
                                      0, 0, 0, 0, 1, 0, 0,
                                      0, 0, 0, 0, 0, 0, -1),
                             nrow = 7,
                             ncol = 5,
                             dimnames = list(coarse_LC_classes,
                                             ref_LC_classes))
  kernel_radius <- 1
  simulation_type <- "fuzzy"
  fuzzy_multiplier <- 1
  discrete_output_map <- TRUE
  random_seed <- 34
  output_file_prefix <- "outputs"
  output_dir_path <- "test/"
  
  expect_error(inputChecks(ref_map_file_name = ref_map_file_name,
                           LC_deltas_file_list = LC_deltas_file_list,
                           LC_deltas_type = LC_deltas_type,
                           ref_map_type = ref_map_type,
                           cell_size_unit = cell_size_unit,
                           match_LC_classes = match_LC_classes,
                           kernel_radius = kernel_radius,
                           simulation_type = simulation_type,
                           discrete_output_map = discrete_output_map,
                           random_seed = random_seed,
                           output_file_prefix = output_file_prefix,
                           output_dir_path = output_dir_path,
                           fuzzy_multiplier = fuzzy_multiplier),
               "All values in match_LC_classes must be between 0 and 1")
  
})

test_that("inputChecks returns error if match_LC_classes has values less than 0", {
  
  ref_map_file_name <- "data/test/ref_file.tif"
  LC_deltas_file_list <- list(c("data/test/LUC_file.tif"))
  LC_deltas_type <- "areas"
  ref_map_type <- "areas"
  cell_size_unit <- "m"
  coarse_LC_classes <- c("managedForest",
                         "unmanagedForest",
                         "otherNatural",
                         "cropland",
                         "pasture",
                         "barren",
                         "urban")
  ref_LC_classes <- c("pri",
                      "sec",
                      "crp",
                      "pas",
                      "urb")
  match_LC_classes <- matrix(data = c(0, 1, 0.5, 0, 0, 0, 0,
                                      1, 0, 0.5, 0, 0, 1, 0,
                                      0, 0, 0, 1, 0, 0, 0,
                                      0, 0, 0, 0, 1, 0, 0,
                                      0, 0, 0, 0, 0, 0, -1),
                             nrow = 7,
                             ncol = 5,
                             dimnames = list(coarse_LC_classes,
                                             ref_LC_classes))
  kernel_radius <- 1
  simulation_type <- "fuzzy"
  fuzzy_multiplier <- 1
  discrete_output_map <- TRUE
  random_seed <- 34
  output_file_prefix <- "outputs"
  output_dir_path <- "test/"
  
  expect_error(inputChecks(ref_map_file_name = ref_map_file_name,
                           LC_deltas_file_list = LC_deltas_file_list,
                           LC_deltas_type = LC_deltas_type,
                           ref_map_type = ref_map_type,
                           cell_size_unit = cell_size_unit,
                           match_LC_classes = match_LC_classes,
                           kernel_radius = kernel_radius,
                           simulation_type = simulation_type,
                           discrete_output_map = discrete_output_map,
                           random_seed = random_seed,
                           output_file_prefix = output_file_prefix,
                           output_dir_path = output_dir_path,
                           fuzzy_multiplier = fuzzy_multiplier),
               "All values in match_LC_classes must be between 0 and 1")
  
})

test_that("inputChecks returns error if match_LC_classes has rows that don't sum to an integer", {
  
  ref_map_file_name <- "data/test/ref_file.tif"
  LC_deltas_file_list <- list(c("data/test/LUC_file.tif"))
  LC_deltas_type <- "areas"
  ref_map_type <- "areas"
  cell_size_unit <- "m"
  coarse_LC_classes <- c("managedForest",
                         "unmanagedForest",
                         "otherNatural",
                         "cropland",
                         "pasture",
                         "barren",
                         "urban")
  ref_LC_classes <- c("pri",
                      "sec",
                      "crp",
                      "pas",
                      "urb")
  match_LC_classes <- matrix(data = c(0, 1, 0.5, 0, 0, 0, 0,
                                      1, 0, 0.4, 0, 0, 1, 0,
                                      0, 0, 0, 1, 0, 0, 0,
                                      0, 0, 0, 0, 1, 0, 0,
                                      0, 0, 0, 0, 0, 0, 1),
                             nrow = 7,
                             ncol = 5,
                             dimnames = list(coarse_LC_classes,
                                             ref_LC_classes))
  kernel_radius <- 1
  simulation_type <- "fuzzy"
  fuzzy_multiplier <- 1
  discrete_output_map <- TRUE
  random_seed <- 34
  output_file_prefix <- "outputs"
  output_dir_path <- "test/"
  
  expect_error(inputChecks(ref_map_file_name = ref_map_file_name,
                           LC_deltas_file_list = LC_deltas_file_list,
                           LC_deltas_type = LC_deltas_type,
                           ref_map_type = ref_map_type,
                           cell_size_unit = cell_size_unit,
                           match_LC_classes = match_LC_classes,
                           kernel_radius = kernel_radius,
                           simulation_type = simulation_type,
                           discrete_output_map = discrete_output_map,
                           random_seed = random_seed,
                           output_file_prefix = output_file_prefix,
                           output_dir_path = output_dir_path,
                           fuzzy_multiplier = fuzzy_multiplier),
               "All rows in match_LC_classes must sum to an integer")
  
})
