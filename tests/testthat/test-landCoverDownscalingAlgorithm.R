
#' @importFrom withr defer

# Create a shared temp directory once for the whole file
shared_temp_dir <- withr::local_tempdir()

# Ensure cleanup when the file finishes (scope = globalenv() so it runs after all tests)
withr::defer(unlink(shared_temp_dir,
                    recursive = TRUE,
                    force = TRUE),
             envir = globalenv())

test_that("assignRefMapCells assigns grid cells to nearest neighbours" , {

  # Set up ref map data
  ref_map_crds <- data.frame(x = c(0.5, 0.5, 1.5, 1.5),
                             y = c(0.5, 1.5, 0.5, 1.5),
                             coarse_ID = c(1, 1, 2, 2))
  ref_map <- terra::rast(ref_map_crds,
                         type = "xyz")

  # Set up LC deltas data
  LC_deltas_crds <- matrix(data = c(0.5, 1,
                                    1.5, 1),
                           nrow = 2,
                           ncol = 2,
                           byrow = TRUE)
  colnames(LC_deltas_crds) <- c("x", "y")

  LC_deltas_cell_numbers <- 1:2

  # Get expected output
  expected_polygons <- terra::as.polygons(ref_map)
  names(expected_polygons) <- "coarse_ID"

  # Get actual output
  actual_polygons <- assignRefMapCells(ref_map = ref_map,
                                       LC_deltas_coords = LC_deltas_crds,
                                       LC_deltas_cell_numbers = LC_deltas_cell_numbers)

  expect_true(terra::all.equal(actual_polygons,
                               expected_polygons))

})

test_that("assignRefMap returns polygons with same CRS as reference map", {

  # Set up ref map data
  ref_map_crds <- data.frame(x = c(0.5, 0.5, 1.5, 1.5),
                             y = c(0.5, 1.5, 0.5, 1.5),
                             coarse_ID = c(1, 1, 2, 2))
  ref_map <- terra::rast(ref_map_crds,
                         type = "xyz",
                         crs = "EPSG:4326")

  # Set up LC deltas data
  LC_deltas_crds <- matrix(data = c(0.5, 1,
                                    1.5, 1),
                           nrow = 2,
                           ncol = 2,
                           byrow = TRUE)
  colnames(LC_deltas_crds) <- c("x", "y")

  LC_deltas_cell_numbers <- 1:2

  # Get actual output
  actual_polygons <- assignRefMapCells(ref_map = ref_map,
                                       LC_deltas_coords = LC_deltas_crds,
                                       LC_deltas_cell_numbers = LC_deltas_cell_numbers)

  expect_equal(terra::crs(actual_polygons),
               terra::crs(ref_map))

})

test_that("setRefMapLevels returns sorted data frame with sequential integer values if reference map contains areas", {

  ref_map <- terra::rast(xmin = 0,
                         xmax = 3,
                         ymin = 0,
                         ymax = 3,
                         res = 1,
                         nlyrs = 3,
                         names = c("pas", "pri", "urb"))
  terra::values(ref_map[["pas"]]) <- c(4, 16, 13, 11, 13, 13, 14, 9, 15)
  terra::values(ref_map[["pri"]]) <- 20 - c(4, 16, 13, 11, 13, 13, 14, 9, 15)
  terra::values(ref_map[["urb"]]) <- 5

  expected_levels <- data.frame(id = 1:3,
                                Land_cover = names(ref_map))

  expect_equal(setRefMapLevels(ref_map,
                               ref_map_type = "areas"),
               expected_levels)
})

test_that("setRefMapLevels returns data frame with values from map if reference map is an unclassified discrete map", {

  ref_map <- terra::rast(xmin = 0,
                         xmax = 3,
                         ymin = 0,
                         ymax = 3,
                         res = 1,
                         nlyrs = 1)
  terra::values(ref_map) <- c(11, 22, 33, 11, 33, 33, 22, 33, 22)

  expected_levels <- data.frame(id = c(11, 22, 33),
                                Land_cover= c("LC11", "LC22", "LC33"))

  expect_equal(setRefMapLevels(ref_map = ref_map,
                               ref_map_type = "discrete"),
               expected_levels)
})

test_that("setRefMapLevels returns ordered data frame if reference map is a classified discrete map", {

  ref_map <- terra::rast(xmin = 0,
                         xmax = 3,
                         ymin = 0,
                         ymax = 3,
                         res = 1,
                         nlyrs = 1)
  terra::values(ref_map) <- c(11, 22, 33, 11, 33, 33, 22, 33, 22)

  ref_map_levels <- data.frame(id = c(11, 22, 33),
                               Land_cover = c("Urb", "Crop", "Pas"))
  levels(ref_map) <- ref_map_levels

  expected_levels <- data.frame(id = c(11, 22, 33),
                                Land_cover = c("Urb", "Crop", "Pas"))

  expect_equal(setRefMapLevels(ref_map = ref_map,
                               ref_map_type = "discrete"),
               expected_levels)

  ## Check that correct levels are returned when input levels are not sequentially ordered
  ref_map2 <- ref_map
  ref_map2_levels <- data.frame(id = c(22, 11, 33),
                                Land_cover = c("Crop", "Urb", "Pas"))
  levels(ref_map2) <- ref_map2_levels

  expect_equal(setRefMapLevels(ref_map = ref_map2,
                               ref_map_type = "discrete"),
               expected_levels)

})

test_that("convertDiscreteRefMapsToAreas returns map when a classified discrete reference map is input", {

  #### Check that names match when order of values is sequential
  ## Set up discrete reference map that is classified
  ref_map_names <- c("Urb", "Crop", "Past")
  ref_map <- terra::rast(xmin = 0,
                         xmax = 3,
                         ymin = 0,
                         ymax = 3,
                         res = 1,
                         nlyrs = 1)
  terra::values(ref_map) <- c(11, 22, 33, 11, 33, 33, 22, 33, 22)

  ref_map_levels <- data.frame(id = c(11, 22, 33),
                               Land_cover = ref_map_names)
  levels(ref_map) <- ref_map_levels

  ## Set up actual reference map
  actual_ref_map <- convertDiscreteRefMapToAreas(ref_map = ref_map,
                                                 cell_size_unit = "km")

  ## Set up expected reference map
  expected_ref_map <- terra::segregate(ref_map) * terra::cellSize(ref_map,
                                                                  unit = "km")
  names(expected_ref_map) <- ref_map_names
  expected_ref_map <- terra::as.data.frame(expected_ref_map,
                                           xy = TRUE)

  ## Check that layer names and maps match
  expect_equal(names(actual_ref_map),
               ref_map_names)
  expect_equal(as.data.frame(actual_ref_map,
                             xy = TRUE),
               expected_ref_map)

  #### Check that names match when order of values is not sequential
  ref_map_names2 <- c("Crop", "Urb", "Past")
  ref_map2 <- ref_map

  ref_map_levels2 <- data.frame(id = c(22, 11, 33),
                               Land_cover = ref_map_names2)
  levels(ref_map2) <- ref_map_levels2

  ## Set up actual reference map
  actual_ref_map2 <- convertDiscreteRefMapToAreas(ref_map = ref_map2,
                                                  cell_size_unit = "km")

  ## Check that layer names match
  expect_equal(names(actual_ref_map2),
               ref_map_names)
  expect_equal(as.data.frame(actual_ref_map2,
                             xy = TRUE),
               expected_ref_map)

})

test_that("convertDiscreteRefMapsToAreas returns correct map when an unclassified discrete reference map is input", {

  ## Set up discrete reference map that is not classified
  ref_map <- terra::rast(xmin = 0,
                         xmax = 3,
                         ymin = 0,
                         ymax = 3,
                         res = 1,
                         nlyrs = 1)
  terra::values(ref_map) <- c(11, 22, 33, 11, 33, 33, 22, 33, 22)

  ## Set up actual reference map
  actual_ref_map <- convertDiscreteRefMapToAreas(ref_map = ref_map,
                                                 cell_size_unit = "km")

  ## Check that layer names match
  expect_equal(names(actual_ref_map),
               c("LC11", "LC22", "LC33"))

})

test_that("createDiscreteRefMap returns correct map when reference map type is areas", {

  downscaled_map <- terra::rast(xmin = 0,
                                xmax = 3,
                                ymin = 0,
                                ymax = 3,
                                res = 1,
                                nlyrs = 2,
                                names = c("pas", "pri"))
  terra::values(downscaled_map[["pas"]]) <- c(4, 16, 13, 11, 13, 13, 14, 9, 15)
  terra::values(downscaled_map[["pri"]]) <- 20 - c(4, 16, 13, 11, 13, 13, 14, 9, 15)

  ref_map_levels <- data.frame(id = c(1, 2),
                               Land_cover = c("pas",
                                              "pri"))

  expected_map <- terra::rast(xmin = 0,
                              xmax = 3,
                              ymin = 0,
                              ymax = 3,
                              res = 1,
                              nlyrs = 1)
  terra::values(expected_map) <- c(2, 1, 1,
                                   1, 1, 1,
                                   1, 2, 1)
  levels(expected_map) <- ref_map_levels

  actual_map <- createDiscreteMap(downscaled_map = downscaled_map,
                                  ref_map_levels = ref_map_levels,
                                  ref_map_type = "areas")

  expect_equal(as.data.frame(actual_map),
               as.data.frame(expected_map))
})

test_that("createDiscreteRefMap returns correct map when reference map type is discrete and unclassified", {

  downscaled_map <- terra::rast(xmin = 0,
                                xmax = 3,
                                ymin = 0,
                                ymax = 3,
                                res = 1,
                                nlyrs = 2,
                                names = c("pas", "pri"))
  terra::values(downscaled_map[["pas"]]) <- c(4, 16, 13, 11, 13, 13, 14, 9, 15)
  terra::values(downscaled_map[["pri"]]) <- 20 - c(4, 16, 13, 11, 13, 13, 14, 9, 15)

  ref_map_levels <- data.frame(id = c(17, 22),
                               Land_cover = c("LC17",
                                              "LC22"))

  expected_map <- terra::rast(xmin = 0,
                              xmax = 3,
                              ymin = 0,
                              ymax = 3,
                              res = 1,
                              nlyrs = 1)
  terra::values(expected_map) <- c(22, 17, 17,
                                   17, 17, 17,
                                   17, 22, 17)
  levels(expected_map) <- ref_map_levels

  actual_map <- createDiscreteMap(downscaled_map = downscaled_map,
                                  ref_map_levels = ref_map_levels,
                                  ref_map_type = "discrete")

  expect_equal(as.data.frame(actual_map),
               as.data.frame(expected_map))
})

test_that("createDiscreteRefMap returns correct map when reference map type is discrete and classified", {

  downscaled_map <- terra::rast(xmin = 0,
                                xmax = 3,
                                ymin = 0,
                                ymax = 3,
                                res = 1,
                                nlyrs = 2,
                                names = c("pas", "pri"))
  terra::values(downscaled_map[["pas"]]) <- c(4, 16, 13, 11, 13, 13, 14, 9, 15)
  terra::values(downscaled_map[["pri"]]) <- 20 - c(4, 16, 13, 11, 13, 13, 14, 9, 15)

  ref_map_levels <- data.frame(id = c(16, 22),
                               Land_cover = c("pas",
                                              "pri"))

  expected_map <- terra::rast(xmin = 0,
                              xmax = 3,
                              ymin = 0,
                              ymax = 3,
                              res = 1,
                              nlyrs = 1)
  terra::values(expected_map) <- c(22, 16, 16,
                                   16, 16, 16,
                                   16, 22, 16)
  levels(expected_map) <- ref_map_levels

  actual_map <- createDiscreteMap(downscaled_map = downscaled_map,
                                  ref_map_levels = ref_map_levels,
                                  ref_map_type = "discrete")

  expect_equal(as.data.frame(actual_map),
               as.data.frame(expected_map))
})

test_that("downscaleLC works with area-based reference map and deterministic method", {
  
  ## This is old system test 1
  
  ## Set up for downscaling
  ref_map_file_name <- test_path("testdata", 
                                 "downscaleLC_reference_map.tif")
  LC_delta_files <- list(test_path("testdata", 
                                   "downscaleLC_LUC_1.tif"),
                         test_path("testdata", 
                                   "downscaleLC_LUC_2.tif"))
                               
  PLUM_LC_classes <- c("managedForest",
                       "unmanagedForest",
                       "otherNatural",
                       "cropland",
                       "pasture",
                       "barren",
                       "urban")
  reference_LC_classes <- c("pri",
                            "sec",
                            "crp",
                            "pas",
                            "urb")
  final_LC_classes <- matrix(data = c(0, 1, 0.5, 0, 0, 0, 0,
                                      1, 0, 0.5, 0, 0, 1, 0,
                                      0, 0, 0, 1, 0, 0, 0,
                                      0, 0, 0, 0, 1, 0, 0,
                                      0, 0, 0, 0, 0, 0, 1),
                             nrow = 7,
                             ncol = 5,
                             dimnames = list(PLUM_LC_classes,
                                             reference_LC_classes))
  kernel_radius <- 1
  random_seed <- 402
  output_file_prefix <- "test1"
  
  ## Run downscaling
  downscaleLC(ref_map_file_name = ref_map_file_name,
              LC_deltas_file_list = LC_delta_files,
              match_LC_classes = final_LC_classes,
              kernel_radius = kernel_radius,
              simulation_type = "deterministic",
              discrete_output_map = TRUE,
              random_seed = random_seed,
              output_file_prefix = output_file_prefix,
              output_dir_path = shared_temp_dir)
  
  
  ## Load results
  ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                 "/",
                                 output_file_prefix,
                                 "_Time1.tif"))
  ds_map_2 <- terra::rast(paste0(shared_temp_dir,
                                 "/",
                                 output_file_prefix,
                                 "_Time2.tif"))
  discrete_ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time1.tif"))
  discrete_ds_map_2 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time2.tif"))
  
  ## Load expected results
  exp_ds_map_1 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test1_Time1.tif"))
  exp_ds_map_2 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test1_Time2.tif"))
  exp_discrete_ds_map_1 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test1_Discrete_Time1.tif"))
  exp_discrete_ds_map_2 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test1_Discrete_Time2.tif"))
  
  ## Tests
  expect_equal(as.data.frame(ds_map_1), 
               as.data.frame(exp_ds_map_1))
  expect_equal(as.data.frame(ds_map_2), 
               as.data.frame(exp_ds_map_2))
  expect_equal(as.data.frame(discrete_ds_map_1), 
               as.data.frame(exp_discrete_ds_map_1))
  expect_equal(as.data.frame(discrete_ds_map_2), 
               as.data.frame(exp_discrete_ds_map_2))
  
})

test_that("downscaleLC works with discrete reference map and deterministic method", {
  
  ## This is old system test 2
  
  ## Set up for downscaling
  ref_map_file_name <- test_path("testdata", 
                                 "downscaleLC_discrete_reference_map.tif")
  LC_delta_files <- list(test_path("testdata", 
                                   "downscaleLC_LUC_1.tif"),
                         test_path("testdata", 
                                   "downscaleLC_LUC_2.tif"))
  
  PLUM_LC_classes <- c("managedForest",
                       "unmanagedForest",
                       "otherNatural",
                       "cropland",
                       "pasture",
                       "barren",
                       "urban")
  reference_LC_classes <- c("LC11",
                            "LC12",
                            "LC13",
                            "LC14",
                            "LC15")
  final_LC_classes <- matrix(data = c(0, 1, 0.5, 0, 0, 0, 0,
                                      1, 0, 0.5, 0, 0, 1, 0,
                                      0, 0, 0, 1, 0, 0, 0,
                                      0, 0, 0, 0, 1, 0, 0,
                                      0, 0, 0, 0, 0, 0, 1),
                             nrow = 7,
                             ncol = 5,
                             dimnames = list(PLUM_LC_classes,
                                             reference_LC_classes))
  kernel_radius <- 1
  random_seed <- 503
  output_file_prefix <- "test2"
  
  ## Run downscaling
  downscaleLC(ref_map_file_name = ref_map_file_name,
              LC_deltas_file_list = LC_delta_files,
              ref_map_type = "discrete",
              cell_size_unit = "m",
              match_LC_classes = final_LC_classes,
              kernel_radius = kernel_radius,
              simulation_type = "deterministic",
              discrete_output_map = TRUE,
              random_seed = random_seed,
              output_file_prefix = output_file_prefix,
              output_dir_path = shared_temp_dir)
  
  
  ## Load results
  ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                 "/",
                                 output_file_prefix,
                                 "_Time1.tif"))
  ds_map_2 <- terra::rast(paste0(shared_temp_dir,
                                 "/",
                                 output_file_prefix,
                                 "_Time2.tif"))
  discrete_ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time1.tif"))
  discrete_ds_map_2 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time2.tif"))
  
  ## Load expected results
  exp_ds_map_1 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test2_Time1.tif"))
  exp_ds_map_2 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test2_Time2.tif"))
  exp_discrete_ds_map_1 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test2_Discrete_Time1.tif"))
  exp_discrete_ds_map_2 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test2_Discrete_Time2.tif"))
  
  ## Tests
  expect_equal(as.data.frame(ds_map_1), 
               as.data.frame(exp_ds_map_1))
  expect_equal(as.data.frame(ds_map_2), 
               as.data.frame(exp_ds_map_2))
  expect_equal(as.data.frame(discrete_ds_map_1), 
               as.data.frame(exp_discrete_ds_map_1))
  expect_equal(as.data.frame(discrete_ds_map_2), 
               as.data.frame(exp_discrete_ds_map_2))
  
})

test_that("downscaleLC works deterministic method and coarse cell areas provided", {
  
  ## This is old system test 3
  
  ## Set up for downscaling
  ref_map_file_name <- test_path("testdata", 
                                 "downscaleLC_reference_map.tif")
  LC_delta_files <- list(test_path("testdata", 
                                   "downscaleLC_LUC_with_area_1.tif"),
                         test_path("testdata", 
                                   "downscaleLC_LUC_with_area_2.tif"))
  
  PLUM_LC_classes <- c("managedForest",
                       "unmanagedForest",
                       "otherNatural",
                       "cropland",
                       "pasture",
                       "barren",
                       "urban")
  reference_LC_classes <- c("pri",
                            "sec",
                            "crp",
                            "pas",
                            "urb")
  final_LC_classes <- matrix(data = c(0, 1, 0.5, 0, 0, 0, 0,
                                      1, 0, 0.5, 0, 0, 1, 0,
                                      0, 0, 0, 1, 0, 0, 0,
                                      0, 0, 0, 0, 1, 0, 0,
                                      0, 0, 0, 0, 0, 0, 1),
                             nrow = 7,
                             ncol = 5,
                             dimnames = list(PLUM_LC_classes,
                                             reference_LC_classes))
  kernel_radius <- 1
  random_seed <- 402
  output_file_prefix <- "test3"
  
  ## Run downscaling
  downscaleLC(ref_map_file_name = ref_map_file_name,
              LC_deltas_file_list = LC_delta_files,
              match_LC_classes = final_LC_classes,
              kernel_radius = kernel_radius,
              simulation_type = "deterministic",
              discrete_output_map = TRUE,
              random_seed = random_seed,
              output_file_prefix = output_file_prefix,
              output_dir_path = shared_temp_dir)
  
  
  ## Load results
  ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                 "/",
                                 output_file_prefix,
                                 "_Time1.tif"))
  ds_map_2 <- terra::rast(paste0(shared_temp_dir,
                                 "/",
                                 output_file_prefix,
                                 "_Time2.tif"))
  discrete_ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time1.tif"))
  discrete_ds_map_2 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time2.tif"))
  
  ## Load expected results
  exp_ds_map_1 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test3_Time1.tif"))
  exp_ds_map_2 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test3_Time2.tif"))
  exp_discrete_ds_map_1 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test3_Discrete_Time1.tif"))
  exp_discrete_ds_map_2 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test3_Discrete_Time2.tif"))
  
  ## Tests
  expect_equal(as.data.frame(ds_map_1), 
               as.data.frame(exp_ds_map_1))
  expect_equal(as.data.frame(ds_map_2), 
               as.data.frame(exp_ds_map_2))
  expect_equal(as.data.frame(discrete_ds_map_1), 
               as.data.frame(exp_discrete_ds_map_1))
  expect_equal(as.data.frame(discrete_ds_map_2), 
               as.data.frame(exp_discrete_ds_map_2))
  
})

test_that("downscaleLC works with discrete reference map, deterministic method and coarse cell areas provided", {
  
  ## This is old system test 4
  
  ## Set up for downscaling
  ref_map_file_name <- test_path("testdata", 
                                 "downscaleLC_discrete_reference_map.tif")
  LC_delta_files <- list(test_path("testdata", 
                                   "downscaleLC_LUC_with_area_1.tif"),
                         test_path("testdata", 
                                   "downscaleLC_LUC_with_area_2.tif"))
  
  PLUM_LC_classes <- c("managedForest",
                       "unmanagedForest",
                       "otherNatural",
                       "cropland",
                       "pasture",
                       "barren",
                       "urban")
  reference_LC_classes <- c("LC11",
                            "LC12",
                            "LC13",
                            "LC14",
                            "LC15")
  final_LC_classes <- matrix(data = c(0, 1, 0.5, 0, 0, 0, 0,
                                      1, 0, 0.5, 0, 0, 1, 0,
                                      0, 0, 0, 1, 0, 0, 0,
                                      0, 0, 0, 0, 1, 0, 0,
                                      0, 0, 0, 0, 0, 0, 1),
                             nrow = 7,
                             ncol = 5,
                             dimnames = list(PLUM_LC_classes,
                                             reference_LC_classes))
  kernel_radius <- 1
  random_seed <- 503
  output_file_prefix <- "test4"
  
  ## Run downscaling
  downscaleLC(ref_map_file_name = ref_map_file_name,
              LC_deltas_file_list = LC_delta_files,
              ref_map_type = "discrete",
              cell_size_unit = "m",
              match_LC_classes = final_LC_classes,
              kernel_radius = kernel_radius,
              simulation_type = "deterministic",
              discrete_output_map = TRUE,
              random_seed = random_seed,
              output_file_prefix = output_file_prefix,
              output_dir_path = shared_temp_dir)
  
  
  ## Load results
  ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                 "/",
                                 output_file_prefix,
                                 "_Time1.tif"))
  ds_map_2 <- terra::rast(paste0(shared_temp_dir,
                                 "/",
                                 output_file_prefix,
                                 "_Time2.tif"))
  discrete_ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time1.tif"))
  discrete_ds_map_2 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time2.tif")) 
  
  ## Load expected results
  exp_ds_map_1 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test4_Time1.tif"))
  exp_ds_map_2 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test4_Time2.tif"))
  exp_discrete_ds_map_1 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test4_Discrete_Time1.tif"))
  exp_discrete_ds_map_2 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test4_Discrete_Time2.tif"))
  
  ## Tests
  expect_equal(as.data.frame(ds_map_1), 
               as.data.frame(exp_ds_map_1))
  expect_equal(as.data.frame(ds_map_2), 
               as.data.frame(exp_ds_map_2))
  expect_equal(as.data.frame(discrete_ds_map_1), 
               as.data.frame(exp_discrete_ds_map_1))
  expect_equal(as.data.frame(discrete_ds_map_2), 
               as.data.frame(exp_discrete_ds_map_2))
  
})

test_that("downscaleLC works with HILDA+ data and deterministic method", {

  ## This is old system test 5
  ## The HILDA+ data is from Winkler, Karina; Fuchs, Richard; Rounsevell,
  ## Mark D A; Herold, Martin (2020): HILDA+ Global Land Use Change between
  ## 1960 and 2019 [dataset]. PANGAEA, https://doi.org/10.1594/PANGAEA.921846
  ## The HILDA+ data was cropped to five cells in the south-east of England and
  ## the years 2009 to 2019 were subsetted from this. Land use change was
  ## calculated from the HILDA+ data at 0.5 degree resolution for the purposes
  ## of downscaling land use change.

  ## Set up for downscaling
  first_timestep <- as.character(seq(2009, 2018, 1))
  second_timestep <- as.character(seq(2010, 2019, 1))
  all_timesteps <- as.character(seq(2009, 2019, 1))

  ref_map_file_name <- test_path("testdata",
                                       "HILDA_ref_map_SE_England_2009.tif")

  LUC_file_list <- list(test_path("testdata",
                                  "HILDA_LUC_SE_England_2009-2010.tif"),
                        test_path("testdata",
                                  "HILDA_LUC_SE_England_2010-2011.tif"))

  hilda_UK_LC_classes <- c(11,
                           22,
                           33,
                           40,
                           41,
                           44,
                           45,
                           55,
                           66,
                           77)
  hilda_UK_LC_class_names <- paste0("LC",
                                    as.character(hilda_UK_LC_classes))
  final_LC_types <- matrix(data = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                    0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                                    0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                                    0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
                                    0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                                    0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                                    0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                                    0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                                    0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                                    0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
                           nrow = 10,
                           ncol = 10,
                           byrow = TRUE,
                           dimnames = list(hilda_UK_LC_class_names,
                                           hilda_UK_LC_class_names))
  kernel_radius <- 1
  random_seed <- 1653316227
  output_file_prefix <- "test5"

  ## Run downscaling with warning checks
  ## Warnings are expected from this function as there is unallocated LULC change
  ## when downscaling the HILDA+ land use change data. Multiple warnings are 
  ## expected, so they are suppressed here to allow unit testing of the function
  ## outputs.
  suppressWarnings(downscaleLC(ref_map_file_name = ref_map_file_name,
                               LC_deltas_file_list = LUC_file_list,
                               ref_map_type = "discrete",
                               cell_size_unit = "km",
                               match_LC_classes = final_LC_types,
                               kernel_radius = kernel_radius,
                               discrete_output_map = TRUE,
                               random_seed = random_seed,
                               output_file_prefix = output_file_prefix,
                               output_dir_path = shared_temp_dir))

  ## Load results
  ds_map_1 <- terra::rast(paste0(shared_temp_dir,
                                 "/",
                                 output_file_prefix,
                                 "_Time1.tif"))
  ds_map_2 <- terra::rast(paste0(shared_temp_dir,
                                 "/",
                                 output_file_prefix,
                                 "_Time2.tif"))
  discrete_ds_map_1 <- terra::rast(paste0(shared_temp_dir,
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time1.tif"))
  discrete_ds_map_2 <- terra::rast(paste0(shared_temp_dir,
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time2.tif"))

  ## Load expected results
  exp_ds_map_1 <- terra::rast(test_path("testdata",
                                        "downscaleLC_test5_Time1.tif"))
  exp_ds_map_2 <- terra::rast(test_path("testdata",
                                        "downscaleLC_test5_Time2.tif"))
  exp_discrete_ds_map_1 <- terra::rast(test_path("testdata",
                                                 "downscaleLC_test5_Discrete_Time1.tif"))
  exp_discrete_ds_map_2 <- terra::rast(test_path("testdata",
                                                 "downscaleLC_test5_Discrete_Time2.tif"))

  ## Tests
  expect_equal(as.data.frame(ds_map_1),
               as.data.frame(exp_ds_map_1))
  expect_equal(as.data.frame(ds_map_2),
               as.data.frame(exp_ds_map_2))
  expect_equal(as.data.frame(discrete_ds_map_1),
               as.data.frame(exp_discrete_ds_map_1))
  expect_equal(as.data.frame(discrete_ds_map_2),
               as.data.frame(exp_discrete_ds_map_2))

})

test_that("downscaleLC works with area-based reference map and stochastic method", {
  
  ## This is old system test 6
  
  ## Set up for downscaling
  ref_map_file_name <- test_path("testdata", 
                                 "downscaleLC_reference_map.tif")
  LC_delta_files <- list(test_path("testdata", 
                                   "downscaleLC_LUC_1.tif"),
                         test_path("testdata", 
                                   "downscaleLC_LUC_2.tif"))
  
  PLUM_LC_classes <- c("managedForest",
                       "unmanagedForest",
                       "otherNatural",
                       "cropland",
                       "pasture",
                       "barren",
                       "urban")
  reference_LC_classes <- c("pri",
                            "sec",
                            "crp",
                            "pas",
                            "urb")
  final_LC_classes <- matrix(data = c(0, 1, 0.5, 0, 0, 0, 0,
                                      1, 0, 0.5, 0, 0, 1, 0,
                                      0, 0, 0, 1, 0, 0, 0,
                                      0, 0, 0, 0, 1, 0, 0,
                                      0, 0, 0, 0, 0, 0, 1),
                             nrow = 7,
                             ncol = 5,
                             dimnames = list(PLUM_LC_classes,
                                             reference_LC_classes))
  kernel_radius <- 1
  random_seed <- 402
  output_file_prefix <- "test6"
  
  ## Run downscaling
  downscaleLC(ref_map_file_name = ref_map_file_name,
              LC_deltas_file_list = LC_delta_files,
              match_LC_classes = final_LC_classes,
              kernel_radius = kernel_radius,
              simulation_type = "fuzzy",
              discrete_output_map = TRUE,
              random_seed = random_seed,
              output_file_prefix = output_file_prefix,
              output_dir_path = shared_temp_dir)
  
  
  ## Load results
  ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                 "/",
                                 output_file_prefix,
                                 "_Time1.tif"))
  ds_map_2 <- terra::rast(paste0(shared_temp_dir,
                                 "/",
                                 output_file_prefix,
                                 "_Time2.tif"))
  discrete_ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time1.tif"))
  discrete_ds_map_2 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time2.tif")) 
  
  ## Load expected results
  exp_ds_map_1 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test6_Time1.tif"))
  exp_ds_map_2 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test6_Time2.tif"))
  exp_discrete_ds_map_1 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test6_Discrete_Time1.tif"))
  exp_discrete_ds_map_2 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test6_Discrete_Time2.tif"))
  
  ## Tests
  expect_equal(as.data.frame(ds_map_1), 
               as.data.frame(exp_ds_map_1))
  expect_equal(as.data.frame(ds_map_2), 
               as.data.frame(exp_ds_map_2))
  expect_equal(as.data.frame(discrete_ds_map_1), 
               as.data.frame(exp_discrete_ds_map_1))
  expect_equal(as.data.frame(discrete_ds_map_2), 
               as.data.frame(exp_discrete_ds_map_2))
  
})

test_that("downscaleLC works with discrete reference map and stochastic method", {
  
  ## This is old system test 7
  
  ## Set up for downscaling
  ref_map_file_name <- test_path("testdata", 
                                 "downscaleLC_discrete_reference_map.tif")
  LC_delta_files <- list(test_path("testdata", 
                                   "downscaleLC_LUC_1.tif"),
                         test_path("testdata", 
                                   "downscaleLC_LUC_2.tif"))
  
  PLUM_LC_classes <- c("managedForest",
                       "unmanagedForest",
                       "otherNatural",
                       "cropland",
                       "pasture",
                       "barren",
                       "urban")
  reference_LC_classes <- c("LC11",
                            "LC12",
                            "LC13",
                            "LC14",
                            "LC15")
  final_LC_classes <- matrix(data = c(0, 1, 0.5, 0, 0, 0, 0,
                                      1, 0, 0.5, 0, 0, 1, 0,
                                      0, 0, 0, 1, 0, 0, 0,
                                      0, 0, 0, 0, 1, 0, 0,
                                      0, 0, 0, 0, 0, 0, 1),
                             nrow = 7,
                             ncol = 5,
                             dimnames = list(PLUM_LC_classes,
                                             reference_LC_classes))
  kernel_radius <- 1
  random_seed <- 503
  output_file_prefix <- "test7"
  
  ## Run downscaling
  downscaleLC(ref_map_file_name = ref_map_file_name,
              LC_deltas_file_list = LC_delta_files,
              ref_map_type = "discrete",
              cell_size_unit = "m",
              match_LC_classes = final_LC_classes,
              kernel_radius = kernel_radius,
              simulation_type = "fuzzy",
              discrete_output_map = TRUE,
              random_seed = random_seed,
              output_file_prefix = output_file_prefix,
              output_dir_path = shared_temp_dir)
  
  
  ## Load results
  ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                 "/",
                                 output_file_prefix,
                                 "_Time1.tif"))
  ds_map_2 <- terra::rast(paste0(shared_temp_dir,
                                 "/",
                                 output_file_prefix,
                                 "_Time2.tif"))
  discrete_ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time1.tif"))
  discrete_ds_map_2 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time2.tif")) 
  
  ## Load expected results
  exp_ds_map_1 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test7_Time1.tif"))
  exp_ds_map_2 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test7_Time2.tif"))
  exp_discrete_ds_map_1 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test7_Discrete_Time1.tif"))
  exp_discrete_ds_map_2 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test7_Discrete_Time2.tif"))
  
  ## Tests
  expect_equal(as.data.frame(ds_map_1), 
               as.data.frame(exp_ds_map_1))
  expect_equal(as.data.frame(ds_map_2), 
               as.data.frame(exp_ds_map_2))
  expect_equal(as.data.frame(discrete_ds_map_1), 
               as.data.frame(exp_discrete_ds_map_1))
  expect_equal(as.data.frame(discrete_ds_map_2), 
               as.data.frame(exp_discrete_ds_map_2))
  
})

test_that("downscaleLC works with HILDA+ data and stochastic method", {
  
  ## This is old system test 8
  ## The HILDA+ data is from Winkler, Karina; Fuchs, Richard; Rounsevell,
  ## Mark D A; Herold, Martin (2020): HILDA+ Global Land Use Change between
  ## 1960 and 2019 [dataset]. PANGAEA, https://doi.org/10.1594/PANGAEA.921846
  ## The HILDA+ data was cropped to five cells in the south-east of England and
  ## the years 2009 to 2019 were subsetted from this. Land use change was
  ## calculated from the HILDA+ data at 0.5 degree resolution for the purposes
  ## of downscaling land use change.
  
  ## Set up for downscaling
  first_timestep <- as.character(seq(2009, 2018, 1))
  second_timestep <- as.character(seq(2010, 2019, 1))
  all_timesteps <- as.character(seq(2009, 2019, 1))
  
  ref_map_file_name <- test_path("testdata",
                                 "HILDA_ref_map_SE_England_2009.tif")
  
  LUC_file_list <- list(test_path("testdata",
                                  "HILDA_LUC_SE_England_2009-2010.tif"),
                        test_path("testdata",
                                  "HILDA_LUC_SE_England_2010-2011.tif"))
  
  hilda_UK_LC_classes <- c(11,
                           22,
                           33,
                           40,
                           41,
                           44,
                           45,
                           55,
                           66,
                           77)
  hilda_UK_LC_class_names <- paste0("LC",
                                    as.character(hilda_UK_LC_classes))
  final_LC_types <- matrix(data = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                    0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                                    0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                                    0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
                                    0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                                    0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                                    0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                                    0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                                    0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                                    0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
                           nrow = 10,
                           ncol = 10,
                           byrow = TRUE,
                           dimnames = list(hilda_UK_LC_class_names,
                                           hilda_UK_LC_class_names))
  kernel_radius <- 1
  random_seed <- 1653316227
  output_file_prefix <- "test8"
  
  ## Run downscaling with warning checks
  ## Warnings are expected from this function as there is unallocated LULC change
  ## when downscaling the HILDA+ land use change data. Multiple warnings are 
  ## expected, so they are suppressed here to allow unit testing of the function
  ## outputs.
  suppressWarnings(downscaleLC(ref_map_file_name = ref_map_file_name,
                               LC_deltas_file_list = LUC_file_list,
                               ref_map_type = "discrete",
                               cell_size_unit = "km",
                               match_LC_classes = final_LC_types,
                               kernel_radius = kernel_radius,
                               simulation_type = "fuzzy",
                               fuzzy_multiplier = 1,
                               discrete_output_map = TRUE,
                               random_seed = random_seed,
                               output_file_prefix = output_file_prefix,
                               output_dir_path = shared_temp_dir))
  
  ## Load results
  ds_map_1 <- terra::rast(paste0(shared_temp_dir,
                                 "/",
                                 output_file_prefix,
                                 "_Time1.tif"))
  ds_map_2 <- terra::rast(paste0(shared_temp_dir,
                                 "/",
                                 output_file_prefix,
                                 "_Time2.tif"))
  discrete_ds_map_1 <- terra::rast(paste0(shared_temp_dir,
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time1.tif"))
  discrete_ds_map_2 <- terra::rast(paste0(shared_temp_dir,
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time2.tif"))
  
  ## Load expected results
  exp_ds_map_1 <- terra::rast(test_path("testdata",
                                        "downscaleLC_test8_Time1.tif"))
  exp_ds_map_2 <- terra::rast(test_path("testdata",
                                        "downscaleLC_test8_Time2.tif"))
  exp_discrete_ds_map_1 <- terra::rast(test_path("testdata",
                                                 "downscaleLC_test8_Discrete_Time1.tif"))
  exp_discrete_ds_map_2 <- terra::rast(test_path("testdata",
                                                 "downscaleLC_test8_Discrete_Time2.tif"))
  
  ## Tests
  expect_equal(as.data.frame(ds_map_1),
               as.data.frame(exp_ds_map_1))
  expect_equal(as.data.frame(ds_map_2),
               as.data.frame(exp_ds_map_2))
  expect_equal(as.data.frame(discrete_ds_map_1),
               as.data.frame(exp_discrete_ds_map_1))
  expect_equal(as.data.frame(discrete_ds_map_2),
               as.data.frame(exp_discrete_ds_map_2))
  
})

test_that("downscaleLC works with area-based reference map and null method", {
  
  ## This is old system test 9
  
  ## Set up for downscaling
  ref_map_file_name <- test_path("testdata", 
                                 "downscaleLC_reference_map.tif")
  LC_delta_files <- list(test_path("testdata", 
                                   "downscaleLC_LUC_1.tif"),
                         test_path("testdata", 
                                   "downscaleLC_LUC_2.tif"))
  
  PLUM_LC_classes <- c("managedForest",
                       "unmanagedForest",
                       "otherNatural",
                       "cropland",
                       "pasture",
                       "barren",
                       "urban")
  reference_LC_classes <- c("pri",
                            "sec",
                            "crp",
                            "pas",
                            "urb")
  final_LC_classes <- matrix(data = c(0, 1, 0.5, 0, 0, 0, 0,
                                      1, 0, 0.5, 0, 0, 1, 0,
                                      0, 0, 0, 1, 0, 0, 0,
                                      0, 0, 0, 0, 1, 0, 0,
                                      0, 0, 0, 0, 0, 0, 1),
                             nrow = 7,
                             ncol = 5,
                             dimnames = list(PLUM_LC_classes,
                                             reference_LC_classes))
  kernel_radius <- 1
  random_seed <- 402
  output_file_prefix <- "test9"
  
  ## Run downscaling
  downscaleLC(ref_map_file_name = ref_map_file_name,
              LC_deltas_file_list = LC_delta_files,
              match_LC_classes = final_LC_classes,
              kernel_radius = kernel_radius,
              simulation_type = "null_model",
              discrete_output_map = TRUE,
              random_seed = random_seed,
              output_file_prefix = output_file_prefix,
              output_dir_path = shared_temp_dir)
  
  
  ## Load results
  ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                 "/",
                                 output_file_prefix,
                                 "_Time1.tif"))
  ds_map_2 <- terra::rast(paste0(shared_temp_dir,
                                 "/",
                                 output_file_prefix,
                                 "_Time2.tif"))
  discrete_ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time1.tif"))
  discrete_ds_map_2 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time2.tif")) 
  
  ## Load expected results
  exp_ds_map_1 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test9_Time1.tif"))
  exp_ds_map_2 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test9_Time2.tif"))
  exp_discrete_ds_map_1 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test9_Discrete_Time1.tif"))
  exp_discrete_ds_map_2 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test9_Discrete_Time2.tif"))
  
  ## Tests
  expect_equal(as.data.frame(ds_map_1), 
               as.data.frame(exp_ds_map_1))
  expect_equal(as.data.frame(ds_map_2), 
               as.data.frame(exp_ds_map_2))
  expect_equal(as.data.frame(discrete_ds_map_1), 
               as.data.frame(exp_discrete_ds_map_1))
  expect_equal(as.data.frame(discrete_ds_map_2), 
               as.data.frame(exp_discrete_ds_map_2))
  
})

test_that("downscaleLC works with discrete reference map and null method", {
  
  ## This is old system test 10
  
  ## Set up for downscaling
  ref_map_file_name <- test_path("testdata", 
                                 "downscaleLC_discrete_reference_map.tif")
  LC_delta_files <- list(test_path("testdata", 
                                   "downscaleLC_LUC_1.tif"),
                         test_path("testdata", 
                                   "downscaleLC_LUC_2.tif"))
  
  PLUM_LC_classes <- c("managedForest",
                       "unmanagedForest",
                       "otherNatural",
                       "cropland",
                       "pasture",
                       "barren",
                       "urban")
  reference_LC_classes <- c("LC11",
                            "LC12",
                            "LC13",
                            "LC14",
                            "LC15")
  final_LC_classes <- matrix(data = c(0, 1, 0.5, 0, 0, 0, 0,
                                      1, 0, 0.5, 0, 0, 1, 0,
                                      0, 0, 0, 1, 0, 0, 0,
                                      0, 0, 0, 0, 1, 0, 0,
                                      0, 0, 0, 0, 0, 0, 1),
                             nrow = 7,
                             ncol = 5,
                             dimnames = list(PLUM_LC_classes,
                                             reference_LC_classes))
  kernel_radius <- 1
  random_seed <- 503
  output_file_prefix <- "test10"
  
  ## Run downscaling
  downscaleLC(ref_map_file_name = ref_map_file_name,
              LC_deltas_file_list = LC_delta_files,
              ref_map_type = "discrete",
              cell_size_unit = "m",
              match_LC_classes = final_LC_classes,
              kernel_radius = kernel_radius,
              simulation_type = "null_model",
              discrete_output_map = TRUE,
              random_seed = random_seed,
              output_file_prefix = output_file_prefix,
              output_dir_path = shared_temp_dir)
  
  
  ## Load results
  ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                 "/",
                                 output_file_prefix,
                                 "_Time1.tif"))
  ds_map_2 <- terra::rast(paste0(shared_temp_dir,
                                 "/",
                                 output_file_prefix,
                                 "_Time2.tif"))
  discrete_ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time1.tif"))
  discrete_ds_map_2 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time2.tif")) 
  
  ## Load expected results
  exp_ds_map_1 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test10_Time1.tif"))
  exp_ds_map_2 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test10_Time2.tif"))
  exp_discrete_ds_map_1 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test10_Discrete_Time1.tif"))
  exp_discrete_ds_map_2 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test10_Discrete_Time2.tif"))
  
  ## Tests
  expect_equal(as.data.frame(ds_map_1), 
               as.data.frame(exp_ds_map_1))
  expect_equal(as.data.frame(ds_map_2), 
               as.data.frame(exp_ds_map_2))
  expect_equal(as.data.frame(discrete_ds_map_1), 
               as.data.frame(exp_discrete_ds_map_1))
  expect_equal(as.data.frame(discrete_ds_map_2), 
               as.data.frame(exp_discrete_ds_map_2))
  
})

test_that("downscaleLC works with HILDA+ data and null method", {
  
  ## This is old system test 11
  ## The HILDA+ data is from Winkler, Karina; Fuchs, Richard; Rounsevell,
  ## Mark D A; Herold, Martin (2020): HILDA+ Global Land Use Change between
  ## 1960 and 2019 [dataset]. PANGAEA, https://doi.org/10.1594/PANGAEA.921846
  ## The HILDA+ data was cropped to five cells in the south-east of England and
  ## the years 2009 to 2019 were subsetted from this. Land use change was
  ## calculated from the HILDA+ data at 0.5 degree resolution for the purposes
  ## of downscaling land use change.
  
  ## Set up for downscaling
  first_timestep <- as.character(seq(2009, 2018, 1))
  second_timestep <- as.character(seq(2010, 2019, 1))
  all_timesteps <- as.character(seq(2009, 2019, 1))
  
  ref_map_file_name <- test_path("testdata",
                                 "HILDA_ref_map_SE_England_2009.tif")
  
  LUC_file_list <- list(test_path("testdata",
                                  "HILDA_LUC_SE_England_2009-2010.tif"),
                        test_path("testdata",
                                  "HILDA_LUC_SE_England_2010-2011.tif"))
  
  hilda_UK_LC_classes <- c(11,
                           22,
                           33,
                           40,
                           41,
                           44,
                           45,
                           55,
                           66,
                           77)
  hilda_UK_LC_class_names <- paste0("LC",
                                    as.character(hilda_UK_LC_classes))
  final_LC_types <- matrix(data = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                    0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                                    0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                                    0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
                                    0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                                    0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                                    0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                                    0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                                    0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                                    0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
                           nrow = 10,
                           ncol = 10,
                           byrow = TRUE,
                           dimnames = list(hilda_UK_LC_class_names,
                                           hilda_UK_LC_class_names))
  kernel_radius <- 1
  random_seed <- 1653316227
  output_file_prefix <- "test11"
  
  ## Run downscaling with warning checks
  ## Warnings are expected from this function as there is unallocated LULC change
  ## when downscaling the HILDA+ land use change data. Multiple warnings are 
  ## expected, so they are suppressed here to allow unit testing of the function
  ## outputs.
  suppressWarnings(downscaleLC(ref_map_file_name = ref_map_file_name,
                               LC_deltas_file_list = LUC_file_list,
                               ref_map_type = "discrete",
                               cell_size_unit = "km",
                               match_LC_classes = final_LC_types,
                               kernel_radius = kernel_radius,
                               simulation_type = "null_model",
                               discrete_output_map = TRUE,
                               random_seed = random_seed,
                               output_file_prefix = output_file_prefix,
                               output_dir_path = shared_temp_dir))
  
  ## Load results
  ds_map_1 <- terra::rast(paste0(shared_temp_dir,
                                 "/",
                                 output_file_prefix,
                                 "_Time1.tif"))
  ds_map_2 <- terra::rast(paste0(shared_temp_dir,
                                 "/",
                                 output_file_prefix,
                                 "_Time2.tif"))
  discrete_ds_map_1 <- terra::rast(paste0(shared_temp_dir,
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time1.tif"))
  discrete_ds_map_2 <- terra::rast(paste0(shared_temp_dir,
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time2.tif"))
  
  ## Load expected results
  exp_ds_map_1 <- terra::rast(test_path("testdata",
                                        "downscaleLC_test11_Time1.tif"))
  exp_ds_map_2 <- terra::rast(test_path("testdata",
                                        "downscaleLC_test11_Time2.tif"))
  exp_discrete_ds_map_1 <- terra::rast(test_path("testdata",
                                                 "downscaleLC_test11_Discrete_Time1.tif"))
  exp_discrete_ds_map_2 <- terra::rast(test_path("testdata",
                                                 "downscaleLC_test11_Discrete_Time2.tif"))
  
  ## Tests
  expect_equal(as.data.frame(ds_map_1),
               as.data.frame(exp_ds_map_1))
  expect_equal(as.data.frame(ds_map_2),
               as.data.frame(exp_ds_map_2))
  expect_equal(as.data.frame(discrete_ds_map_1),
               as.data.frame(exp_discrete_ds_map_1))
  expect_equal(as.data.frame(discrete_ds_map_2),
               as.data.frame(exp_discrete_ds_map_2))
  
})

test_that("downscaleLC works with area-based reference map and fractional LUC map", {
  
  ## This is old system test 12
  
  ## Set up for downscaling
  ref_map_file_name <- test_path("testdata", 
                                 "downscaleLC_reference_map.tif")
  LC_delta_files <- list(test_path("testdata", 
                                   "downscaleLC_LUC_fractional_1.tif"),
                         test_path("testdata", 
                                   "downscaleLC_LUC_fractional_2.tif"))
  
  PLUM_LC_classes <- c("managedForest",
                       "unmanagedForest",
                       "otherNatural",
                       "cropland",
                       "pasture",
                       "barren",
                       "urban")
  reference_LC_classes <- c("pri",
                            "sec",
                            "crp",
                            "pas",
                            "urb")
  final_LC_classes <- matrix(data = c(0, 1, 0.5, 0, 0, 0, 0,
                                      1, 0, 0.5, 0, 0, 1, 0,
                                      0, 0, 0, 1, 0, 0, 0,
                                      0, 0, 0, 0, 1, 0, 0,
                                      0, 0, 0, 0, 0, 0, 1),
                             nrow = 7,
                             ncol = 5,
                             dimnames = list(PLUM_LC_classes,
                                             reference_LC_classes))
  kernel_radius <- 1
  random_seed <- 402
  output_file_prefix <- "test12"
  
  ## Run downscaling
  downscaleLC(ref_map_file_name = ref_map_file_name,
              LC_deltas_file_list = LC_delta_files,
              LC_deltas_type = "proportions",
              match_LC_classes = final_LC_classes,
              kernel_radius = kernel_radius,
              simulation_type = "deterministic",
              discrete_output_map = TRUE,
              random_seed = random_seed,
              output_file_prefix = output_file_prefix,
              output_dir_path = shared_temp_dir)
  
  ## Load results
  ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                 "/",
                                 output_file_prefix,
                                 "_Time1.tif"))
  ds_map_2 <- terra::rast(paste0(shared_temp_dir,
                                 "/",
                                 output_file_prefix,
                                 "_Time2.tif"))
  discrete_ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time1.tif"))
  discrete_ds_map_2 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time2.tif")) 
  
  ## Load expected results
  exp_ds_map_1 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test12_Time1.tif"))
  exp_ds_map_2 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test12_Time2.tif"))
  exp_discrete_ds_map_1 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test12_Discrete_Time1.tif"))
  exp_discrete_ds_map_2 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test12_Discrete_Time2.tif"))
  
  ## Tests
  expect_equal(as.data.frame(ds_map_1), 
               as.data.frame(exp_ds_map_1))
  expect_equal(as.data.frame(ds_map_2), 
               as.data.frame(exp_ds_map_2))
  expect_equal(as.data.frame(discrete_ds_map_1), 
               as.data.frame(exp_discrete_ds_map_1))
  expect_equal(as.data.frame(discrete_ds_map_2), 
               as.data.frame(exp_discrete_ds_map_2))
  
})

test_that("downscaleLC works with area-based reference map, fractional LUC map and reference map cells not assigned", {
  
  ## This is old system test 13
  
  ## Set up for downscaling
  ref_map_file_name <- test_path("testdata", 
                                 "downscaleLC_reference_map.tif")
  LC_delta_files <- list(test_path("testdata", 
                                   "downscaleLC_LUC_fractional_1.tif"),
                         test_path("testdata", 
                                   "downscaleLC_LUC_fractional_2.tif"))
  
  PLUM_LC_classes <- c("managedForest",
                       "unmanagedForest",
                       "otherNatural",
                       "cropland",
                       "pasture",
                       "barren",
                       "urban")
  reference_LC_classes <- c("pri",
                            "sec",
                            "crp",
                            "pas",
                            "urb")
  final_LC_classes <- matrix(data = c(0, 1, 0.5, 0, 0, 0, 0,
                                      1, 0, 0.5, 0, 0, 1, 0,
                                      0, 0, 0, 1, 0, 0, 0,
                                      0, 0, 0, 0, 1, 0, 0,
                                      0, 0, 0, 0, 0, 0, 1),
                             nrow = 7,
                             ncol = 5,
                             dimnames = list(PLUM_LC_classes,
                                             reference_LC_classes))
  kernel_radius <- 1
  random_seed <- 402
  output_file_prefix <- "test13"
  
  ## Run downscaling
  downscaleLC(ref_map_file_name = ref_map_file_name,
              LC_deltas_file_list = LC_delta_files,
              LC_deltas_type = "proportions",
              assign_ref_cells = FALSE,
              match_LC_classes = final_LC_classes,
              kernel_radius = kernel_radius,
              simulation_type = "deterministic",
              discrete_output_map = TRUE,
              random_seed = random_seed,
              output_file_prefix = output_file_prefix,
              output_dir_path = shared_temp_dir)
  
  ## Load results
  ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                 "/",
                                 output_file_prefix,
                                 "_Time1.tif"))
  ds_map_2 <- terra::rast(paste0(shared_temp_dir,
                                 "/",
                                 output_file_prefix,
                                 "_Time2.tif"))
  discrete_ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time1.tif"))
  discrete_ds_map_2 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time2.tif")) 
  
  ## Load expected results
  exp_ds_map_1 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test13_Time1.tif"))
  exp_ds_map_2 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test13_Time2.tif"))
  exp_discrete_ds_map_1 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test13_Discrete_Time1.tif"))
  exp_discrete_ds_map_2 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test13_Discrete_Time2.tif"))
  
  ## Tests
  expect_equal(as.data.frame(ds_map_1), 
               as.data.frame(exp_ds_map_1))
  expect_equal(as.data.frame(ds_map_2), 
               as.data.frame(exp_ds_map_2))
  expect_equal(as.data.frame(discrete_ds_map_1), 
               as.data.frame(exp_discrete_ds_map_1))
  expect_equal(as.data.frame(discrete_ds_map_2), 
               as.data.frame(exp_discrete_ds_map_2))
  
})

test_that("downscaleLC works with dynamic matching of LULC change and reference map classes at coarse grid cell level", {
  
  ## This is old system test 14
  
  ## Set up for downscaling
  ref_map_file_name <- test_path("testdata", 
                                 "downscaleLC_reference_map.tif")
  LC_delta_files <- list(test_path("testdata", 
                                   "downscaleLC_LUC_1.tif"),
                         test_path("testdata", 
                                   "downscaleLC_LUC_2.tif"))
  
  PLUM_LC_classes <- c("managedForest",
                       "unmanagedForest",
                       "otherNatural",
                       "cropland",
                       "pasture",
                       "barren",
                       "urban")
  reference_LC_classes <- c("pri",
                            "sec",
                            "crp",
                            "pas",
                            "urb")
  final_LC_classes <- matrix(data = c(0, 1, 1, 0, 0, 0, 0,
                                      1, 0, 1, 0, 0, 1, 0,
                                      0, 0, 0, 1, 0, 0, 0,
                                      0, 0, 0, 0, 1, 0, 0,
                                      0, 0, 0, 0, 0, 0, 1),
                             nrow = 7,
                             ncol = 5,
                             dimnames = list(PLUM_LC_classes,
                                             reference_LC_classes))
  kernel_radius <- 1
  random_seed <- 402
  output_file_prefix <- "test14"
  
  ## Run downscaling
  downscaleLC(ref_map_file_name = ref_map_file_name,
              LC_deltas_file_list = LC_delta_files,
              LC_deltas_type = "areas",
              assign_ref_cells = TRUE,
              match_LC_classes = final_LC_classes,
              kernel_radius = kernel_radius,
              simulation_type = "deterministic",
              discrete_output_map = TRUE,
              random_seed = random_seed,
              output_file_prefix = output_file_prefix,
              output_dir_path = shared_temp_dir)
  
  ## Load results
  ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                 "/",
                                 output_file_prefix,
                                 "_Time1.tif"))
  ds_map_2 <- terra::rast(paste0(shared_temp_dir,
                                 "/",
                                 output_file_prefix,
                                 "_Time2.tif"))
  discrete_ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time1.tif"))
  discrete_ds_map_2 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time2.tif")) 
  
  ## Load expected results
  exp_ds_map_1 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test14_Time1.tif"))
  exp_ds_map_2 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test14_Time2.tif"))
  exp_discrete_ds_map_1 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test14_Discrete_Time1.tif"))
  exp_discrete_ds_map_2 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test14_Discrete_Time2.tif"))
  
  ## Tests
  expect_equal(as.data.frame(ds_map_1), 
               as.data.frame(exp_ds_map_1))
  expect_equal(as.data.frame(ds_map_2), 
               as.data.frame(exp_ds_map_2))
  expect_equal(as.data.frame(discrete_ds_map_1), 
               as.data.frame(exp_discrete_ds_map_1))
  expect_equal(as.data.frame(discrete_ds_map_2), 
               as.data.frame(exp_discrete_ds_map_2))
  
})

test_that("downscaleLC works with classified discrete reference map and deterministic method", {
  
  ## This is old system test 15
  
  ## Set up for downscaling
  ref_map_file_name <- test_path("testdata", 
                                 "downscaleLC_classified_reference_map.tif")
  LC_delta_files <- list(test_path("testdata", 
                                   "downscaleLC_LUC_1.tif"),
                         test_path("testdata", 
                                   "downscaleLC_LUC_2.tif"))
  
  PLUM_LC_classes <- c("managedForest",
                       "unmanagedForest",
                       "otherNatural",
                       "cropland",
                       "pasture",
                       "barren",
                       "urban")
  reference_LC_classes <- c("Urban",
                            "Cropland",
                            "Pasture",
                            "Forest",
                            "Grasslands")
  final_LC_classes <- matrix(data = c(0, 1, 0.5, 0, 0, 0, 0,
                                      1, 0, 0.5, 0, 0, 1, 0,
                                      0, 0, 0, 1, 0, 0, 0,
                                      0, 0, 0, 0, 1, 0, 0,
                                      0, 0, 0, 0, 0, 0, 1),
                             nrow = 7,
                             ncol = 5,
                             dimnames = list(PLUM_LC_classes,
                                             reference_LC_classes))
  kernel_radius <- 1
  random_seed <- 503
  output_file_prefix <- "test15"
  
  ## Run downscaling
  downscaleLC(ref_map_file_name = ref_map_file_name,
              LC_deltas_file_list = LC_delta_files,
              ref_map_type = "discrete",
              cell_size_unit = "m",
              match_LC_classes = final_LC_classes,
              kernel_radius = kernel_radius,
              simulation_type = "deterministic",
              discrete_output_map = TRUE,
              random_seed = random_seed,
              output_file_prefix = output_file_prefix,
              output_dir_path = shared_temp_dir)
  
  ## Load results
  ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                 "/",
                                 output_file_prefix,
                                 "_Time1.tif"))
  ds_map_2 <- terra::rast(paste0(shared_temp_dir,
                                 "/",
                                 output_file_prefix,
                                 "_Time2.tif"))
  discrete_ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time1.tif"))
  discrete_ds_map_2 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time2.tif")) 
  
  ## Load expected results
  exp_ds_map_1 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test15_Time1.tif"))
  exp_ds_map_2 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test15_Time2.tif"))
  exp_discrete_ds_map_1 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test15_Discrete_Time1.tif"))
  exp_discrete_ds_map_2 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test15_Discrete_Time2.tif"))
  
  ## Tests
  expect_equal(as.data.frame(ds_map_1), 
               as.data.frame(exp_ds_map_1))
  expect_equal(as.data.frame(ds_map_2), 
               as.data.frame(exp_ds_map_2))
  expect_equal(as.data.frame(discrete_ds_map_1), 
               as.data.frame(exp_discrete_ds_map_1))
  expect_equal(as.data.frame(discrete_ds_map_2), 
               as.data.frame(exp_discrete_ds_map_2))
  
})

test_that("downscaleLC works with increased harmonisation radius", {
  
  ## This is old system test 16
  
  ## Set up for downscaling
  ref_map_file_name <- test_path("testdata", 
                                 "downscaleLC_reference_map.tif")
  LC_delta_files <- list(test_path("testdata", 
                                   "downscaleLC_LUC_1.tif"),
                         test_path("testdata", 
                                   "downscaleLC_LUC_2.tif"))
  
  PLUM_LC_classes <- c("managedForest",
                       "unmanagedForest",
                       "otherNatural",
                       "cropland",
                       "pasture",
                       "barren",
                       "urban")
  reference_LC_classes <- c("pri",
                            "sec",
                            "crp",
                            "pas",
                            "urb")
  final_LC_classes <- matrix(data = c(0, 1, 0.5, 0, 0, 0, 0,
                                      1, 0, 0.5, 0, 0, 1, 0,
                                      0, 0, 0, 1, 0, 0, 0,
                                      0, 0, 0, 0, 1, 0, 0,
                                      0, 0, 0, 0, 0, 0, 1),
                             nrow = 7,
                             ncol = 5,
                             dimnames = list(PLUM_LC_classes,
                                             reference_LC_classes))
  kernel_radius <- 1
  random_seed <- 402
  output_file_prefix <- "test16"
  
  ## Run downscaling
  downscaleLC(ref_map_file_name = ref_map_file_name,
              LC_deltas_file_list = LC_delta_files,
              LC_deltas_type = "areas",
              assign_ref_cells = TRUE,
              match_LC_classes = final_LC_classes,
              kernel_radius = kernel_radius,
              harmonisation_radius = 3, 
              simulation_type = "deterministic",
              discrete_output_map = TRUE,
              random_seed = random_seed,
              output_file_prefix = output_file_prefix,
              output_dir_path = shared_temp_dir)
  
  ## Load results
  ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                 "/",
                                 output_file_prefix,
                                 "_Time1.tif"))
  ds_map_2 <- terra::rast(paste0(shared_temp_dir,
                                 "/",
                                 output_file_prefix,
                                 "_Time2.tif"))
  discrete_ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time1.tif"))
  discrete_ds_map_2 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time2.tif")) 
  
  ## Load expected results
  exp_ds_map_1 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test16_Time1.tif"))
  exp_ds_map_2 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test16_Time2.tif"))
  exp_discrete_ds_map_1 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test16_Discrete_Time1.tif"))
  exp_discrete_ds_map_2 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test16_Discrete_Time2.tif"))
  
  ## Tests
  expect_equal(as.data.frame(ds_map_1), 
               as.data.frame(exp_ds_map_1))
  expect_equal(as.data.frame(ds_map_2), 
               as.data.frame(exp_ds_map_2))
  expect_equal(as.data.frame(discrete_ds_map_1), 
               as.data.frame(exp_discrete_ds_map_1))
  expect_equal(as.data.frame(discrete_ds_map_2), 
               as.data.frame(exp_discrete_ds_map_2))
  
})

test_that("downscaleLC works with discrete reference map, deterministic method and LUC maps named using numbers", {
  
  ## Set up for downscaling
  ref_map_file_name <- test_path("testdata", 
                                 "downscaleLC_discrete_reference_map.tif")
  LC_delta_files <- list(test_path("testdata", 
                                   "downscaleLC_LUC_numeric_names_1.tif"),
                         test_path("testdata", 
                                   "downscaleLC_LUC_numeric_names_2.tif"))
  
  PLUM_LC_classes <- as.character(c(10, 
                                    20, 
                                    30, 
                                    40, 
                                    50, 
                                    60, 
                                    70))
  reference_LC_classes <- c("LC11",
                            "LC12",
                            "LC13",
                            "LC14",
                            "LC15")
  final_LC_classes <- matrix(data = c(0, 1, 0.5, 0, 0, 0, 0,
                                      1, 0, 0.5, 0, 0, 1, 0,
                                      0, 0, 0, 1, 0, 0, 0,
                                      0, 0, 0, 0, 1, 0, 0,
                                      0, 0, 0, 0, 0, 0, 1),
                             nrow = 7,
                             ncol = 5,
                             dimnames = list(PLUM_LC_classes,
                                             reference_LC_classes))
  kernel_radius <- 1
  random_seed <- 503
  output_file_prefix <- "test17"
  
  ## Run downscaling
  downscaleLC(ref_map_file_name = ref_map_file_name,
              LC_deltas_file_list = LC_delta_files,
              ref_map_type = "discrete",
              cell_size_unit = "m",
              match_LC_classes = final_LC_classes,
              kernel_radius = kernel_radius,
              simulation_type = "deterministic",
              discrete_output_map = TRUE,
              random_seed = random_seed,
              output_file_prefix = output_file_prefix,
              output_dir_path = shared_temp_dir)
  
  
  ## Load results
  ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                 "/",
                                 output_file_prefix,
                                 "_Time1.tif"))
  ds_map_2 <- terra::rast(paste0(shared_temp_dir,
                                 "/",
                                 output_file_prefix,
                                 "_Time2.tif"))
  discrete_ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time1.tif"))
  discrete_ds_map_2 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time2.tif"))
  
  ## Load expected results
  exp_ds_map_1 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test17_Time1.tif"))
  exp_ds_map_2 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test17_Time2.tif"))
  exp_discrete_ds_map_1 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test17_Discrete_Time1.tif"))
  exp_discrete_ds_map_2 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test17_Discrete_Time2.tif"))
  
  ## Tests
  expect_equal(as.data.frame(ds_map_1), 
               as.data.frame(exp_ds_map_1))
  expect_equal(as.data.frame(ds_map_2), 
               as.data.frame(exp_ds_map_2))
  expect_equal(as.data.frame(discrete_ds_map_1), 
               as.data.frame(exp_discrete_ds_map_1))
  expect_equal(as.data.frame(discrete_ds_map_2), 
               as.data.frame(exp_discrete_ds_map_2))
  
})

test_that("downscaleLC works with classified discrete reference map with non-alphanumeric in LULC names and deterministic method", {
  
  ## Set up for downscaling
  ref_map_file_name <- test_path("testdata", 
                                 "downscaleLC_classified_ref_map_spaces.tif")
  LC_delta_files <- list(test_path("testdata", 
                                   "downscaleLC_LUC_1.tif"),
                         test_path("testdata", 
                                   "downscaleLC_LUC_2.tif"))
  
  PLUM_LC_classes <- c("managedForest",
                       "unmanagedForest",
                       "otherNatural",
                       "cropland",
                       "pasture",
                       "barren",
                       "urban")
  reference_LC_classes <- c("Urban areas", 
                            "Crop land", 
                            "Pasture/rangeland", 
                            "Forest and woodlands", 
                            "Grasslands and sparse")
  final_LC_classes <- matrix(data = c(0, 1, 0.5, 0, 0, 0, 0,
                                      1, 0, 0.5, 0, 0, 1, 0,
                                      0, 0, 0, 1, 0, 0, 0,
                                      0, 0, 0, 0, 1, 0, 0,
                                      0, 0, 0, 0, 0, 0, 1),
                             nrow = 7,
                             ncol = 5,
                             dimnames = list(PLUM_LC_classes,
                                             reference_LC_classes))
  kernel_radius <- 1
  random_seed <- 503
  output_file_prefix <- "test18"
  
  ## Run downscaling
  downscaleLC(ref_map_file_name = ref_map_file_name,
              LC_deltas_file_list = LC_delta_files,
              ref_map_type = "discrete",
              cell_size_unit = "m",
              match_LC_classes = final_LC_classes,
              kernel_radius = kernel_radius,
              simulation_type = "deterministic",
              discrete_output_map = TRUE,
              random_seed = random_seed,
              output_file_prefix = output_file_prefix,
              output_dir_path = shared_temp_dir)
  
  ## Load results
  ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                 "/",
                                 output_file_prefix,
                                 "_Time1.tif"))
  ds_map_2 <- terra::rast(paste0(shared_temp_dir,
                                 "/",
                                 output_file_prefix,
                                 "_Time2.tif"))
  discrete_ds_map_1 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time1.tif"))
  discrete_ds_map_2 <- terra::rast(paste0(shared_temp_dir, 
                                          "/",
                                          output_file_prefix,
                                          "_Discrete_Time2.tif")) 
  
  ## Load expected results
  exp_ds_map_1 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test18_Time1.tif"))
  exp_ds_map_2 <- terra::rast(test_path("testdata", 
                                        "downscaleLC_test18_Time2.tif"))
  exp_discrete_ds_map_1 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test18_Discrete_Time1.tif"))
  exp_discrete_ds_map_2 <- terra::rast(test_path("testdata", 
                                                 "downscaleLC_test18_Discrete_Time2.tif"))
  
  ## Tests
  expect_equal(as.data.frame(ds_map_1), 
               as.data.frame(exp_ds_map_1))
  expect_equal(as.data.frame(ds_map_2), 
               as.data.frame(exp_ds_map_2))
  expect_equal(as.data.frame(discrete_ds_map_1), 
               as.data.frame(exp_discrete_ds_map_1))
  expect_equal(as.data.frame(discrete_ds_map_2), 
               as.data.frame(exp_discrete_ds_map_2))
  
})
