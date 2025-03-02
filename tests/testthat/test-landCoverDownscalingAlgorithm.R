
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
