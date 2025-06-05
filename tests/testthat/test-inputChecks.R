
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
               "All numbers in match_LC_classes matrix must be between 0 and 1")

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
               "All numbers in match_LC_classes matrix must be between 0 and 1")

})

test_that("inputChecks returns error if match_LC_classes has rows that don't sum to 1", {

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
               "Rows in match_LC_classes must sum to 1 if matching LULC classes using fixed proportions")

})

test_that("inputChecks returns error if harmonisation radius is not an integer", {
  
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
  match_LC_classes <- matrix(data = c(0, 1, 1, 0, 0, 0, 0,
                                      1, 0, 1, 0, 0, 1, 0,
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
  
  harmonisation_radius <- 0.5
  
  expect_error(inputChecks(ref_map_file_name = ref_map_file_name,
                           LC_deltas_file_list = LC_deltas_file_list,
                           LC_deltas_type = LC_deltas_type,
                           ref_map_type = ref_map_type,
                           cell_size_unit = cell_size_unit,
                           match_LC_classes = match_LC_classes,
                           kernel_radius = kernel_radius,
                           harmonisation_radius = harmonisation_radius,
                           simulation_type = simulation_type,
                           discrete_output_map = discrete_output_map,
                           random_seed = random_seed,
                           output_file_prefix = output_file_prefix,
                           output_dir_path = output_dir_path,
                           fuzzy_multiplier = fuzzy_multiplier),
               "Harmonisation radius must be an integer")
  
})

test_that("inputChecks returns error if harmonisation radius is not between 1 and 5", {
  
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
  match_LC_classes <- matrix(data = c(0, 1, 1, 0, 0, 0, 0,
                                      1, 0, 1, 0, 0, 1, 0,
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
  
  harmonisation_radius <- -1
  
  expect_error(inputChecks(ref_map_file_name = ref_map_file_name,
                           LC_deltas_file_list = LC_deltas_file_list,
                           LC_deltas_type = LC_deltas_type,
                           ref_map_type = ref_map_type,
                           cell_size_unit = cell_size_unit,
                           match_LC_classes = match_LC_classes,
                           kernel_radius = kernel_radius,
                           harmonisation_radius = harmonisation_radius,
                           simulation_type = simulation_type,
                           discrete_output_map = discrete_output_map,
                           random_seed = random_seed,
                           output_file_prefix = output_file_prefix,
                           output_dir_path = output_dir_path,
                           fuzzy_multiplier = fuzzy_multiplier),
               "Harmonisation radius must be between 1 and 5")
  
  harmonisation_radius <- 8
  
  expect_error(inputChecks(ref_map_file_name = ref_map_file_name,
                           LC_deltas_file_list = LC_deltas_file_list,
                           LC_deltas_type = LC_deltas_type,
                           ref_map_type = ref_map_type,
                           cell_size_unit = cell_size_unit,
                           match_LC_classes = match_LC_classes,
                           kernel_radius = kernel_radius,
                           harmonisation_radius = harmonisation_radius,
                           simulation_type = simulation_type,
                           discrete_output_map = discrete_output_map,
                           random_seed = random_seed,
                           output_file_prefix = output_file_prefix,
                           output_dir_path = output_dir_path,
                           fuzzy_multiplier = fuzzy_multiplier),
               "Harmonisation radius must be between 1 and 5")
  
})

test_that("checkMatchLCClassesRow returns error if any values are greater than 1", {
  
  match_LC_classes_row <- c(2.5, 1, 0, 0.55, 0.6)
  match_LC_classes_row2 <- c(2, 2, 0, 0, 0, 0, 0)
  match_LC_classes_row3 <- c(0, 0, 0, 0, 50, 0, 0)
  match_LC_classes_row4 <- c(0, 0, 1.0000001, 0, 0, 0)
  
  expect_error(checkMatchLCClassesRows(match_LC_classes_row), 
               "All numbers in match_LC_classes matrix must be between 0 and 1")
  expect_error(checkMatchLCClassesRows(match_LC_classes_row2), 
               "All numbers in match_LC_classes matrix must be between 0 and 1")
  expect_error(checkMatchLCClassesRows(match_LC_classes_row3), 
               "All numbers in match_LC_classes matrix must be between 0 and 1")
  expect_error(checkMatchLCClassesRows(match_LC_classes_row4), 
               "All numbers in match_LC_classes matrix must be between 0 and 1")
  
})

test_that("checkMatchLCClassesRow returns error if any values are less than 0", {
  
  match_LC_classes_row <- c(0, 1, 0, 0.55, -0.2)
  match_LC_classes_row2 <- c(0, 0, -0.0000000001)
  
  expect_error(checkMatchLCClassesRows(match_LC_classes_row), 
               "All numbers in match_LC_classes matrix must be between 0 and 1")
  expect_error(checkMatchLCClassesRows(match_LC_classes_row2), 
               "All numbers in match_LC_classes matrix must be between 0 and 1")
  
})

test_that("checkMatchLCClassesRow returns error if a row is not all zeros and sums to less than 1", {
  
  match_LC_classes_row <- c(0, 0.1, 0, 0.5, 0, 0.2)
  match_LC_classes_row2 <- c(0.0000001, 0, 0, 0, 0)
  match_LC_classes_row3 <- c(0, 0, 0, 0.999999989)
  
  expect_error(checkMatchLCClassesRows(match_LC_classes_row), 
               "Rows in match_LC_classes must sum to 1 if matching LULC classes using fixed proportions")
  expect_error(checkMatchLCClassesRows(match_LC_classes_row2), 
               "Rows in match_LC_classes must sum to 1 if matching LULC classes using fixed proportions")
  expect_error(checkMatchLCClassesRows(match_LC_classes_row3), 
               "Rows in match_LC_classes must sum to 1 if matching LULC classes using fixed proportions")
})

test_that("checkMatchLCClassesRow does not return error if row contains all zeros", {
  
  match_LC_classes_row <- c(0, 0, 0, 0, 0, 0, 0)
  
  expect_silent(checkMatchLCClassesRows(match_LC_classes_row))
})

test_that("checkMatchLCClassesRow does not return error if row contains multiple ones", {
  
  match_LC_classes_row <- c(1, 1, 1, 1, 1, 1)
  match_LC_classes_row2 <- c(1, 1, 0, 0, 0, 0, 0)
  match_LC_classes_row3 <- c(1.00000001, 0, 0, 0, 0, 1)
  
  expect_silent(checkMatchLCClassesRows(match_LC_classes_row))
  expect_silent(checkMatchLCClassesRows(match_LC_classes_row2))
  expect_silent(checkMatchLCClassesRows(match_LC_classes_row3))
})

test_that("checkMatchLClassesRow does not return error if row contains a single 1", {
  
  match_LC_classes_row <- c(1, 0, 0, 0, 0, 0, 0)
  
  expect_silent(checkMatchLCClassesRows(match_LC_classes_row))
})

test_that("checkMatchLCClassesRow does not return error if change is split between two classes", {
  
  match_LC_classes_row <- c(0.5, 0.5, 0, 0, 0, 0, 0)
  match_LC_classes_row2 <- c(0.45, 0, 0, 0.55, 0, 0)
  match_LC_classes_row3 <- c(0.4532897528, 0, 0, 0, 0, 0.5467102472)
  
  expect_silent(checkMatchLCClassesRows(match_LC_classes_row))
  expect_silent(checkMatchLCClassesRows(match_LC_classes_row2))
  expect_silent(checkMatchLCClassesRows(match_LC_classes_row3))
})

test_that("checkMatchLCClassesRow does not return error if change is split between more than two classes", {
  
  match_LC_classes_row <- c(0.333333333, 0.333333333, 0.333333333, 0, 0, 0, 0)
  match_LC_classes_row2 <- c(0.45, 0, 0, 0.2555, 0.1445, 0.15)
  match_LC_classes_row3 <- c(0.15666, 0.00333, 0.4566, 0.22, 0.005, 0.1, 0.05841)
  
  expect_silent(checkMatchLCClassesRows(match_LC_classes_row))
  expect_silent(checkMatchLCClassesRows(match_LC_classes_row2))
  expect_silent(checkMatchLCClassesRows(match_LC_classes_row3))
})

test_that("checkMatchLCClassesRow returns error if row contains values both equal to and less than 1", {
  
  match_LC_classes_row <- c(0, 0, 0.5, 0, 1, 0, 0.2)
  match_LC_classes_row2 <- c(0, 0, 0.5, 0, 1, 0.3, 0.2)
  
  expect_error(checkMatchLCClassesRows(match_LC_classes_row), 
               "Rows in match_LC_classes must sum to 1 if matching LULC classes using fixed proportions")
  expect_error(checkMatchLCClassesRows(match_LC_classes_row2), 
               "Rows in match_LC_classes must sum to 1 if matching LULC classes using fixed proportions")
})
