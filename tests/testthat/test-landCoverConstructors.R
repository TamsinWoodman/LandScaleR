
test_that("getNeighbourCells returns correct neighbour cells", {
  
  test_raster <- terra::rast(nrow = 10, 
                             ncol = 10, 
                             ymin = 0, 
                             ymax = 10,
                             xmin = 0, 
                             xmax = 10,
                             vals = 1:100)
  test_raster <- c(test_raster, 
                   terra::rast(test_raster, 
                               vals = 101:200))
  names(test_raster) <- c("t1", 
                          "t2")
  
  actual_neighbours_1 <- getNeighbourCells(LC_deltas = test_raster, 
                                           cell_number = 50,
                                           harmonisation_radius = 2)
  expected_neighbours_1 <- c(39, 40, NaN, 49, NaN, 59, 60, NaN, 28, 29, 30, NaN,
                             NaN, 38, NaN, 48, NaN, 58, NaN, 68, 69, 70, NaN, 
                             NaN)

  actual_neighbours_2 <- getNeighbourCells(LC_deltas = test_raster, 
                                           cell_number = 45, 
                                           harmonisation_radius = 5)
  expected_neighbours_2 <- c(34, 35, 36, 44, 46, 54, 55, 56, 23, 24, 25, 26, 27,
                             33, 37, 43, 47, 53, 57, 63, 64, 65, 66, 67, 12, 13,
                             14, 15, 16, 17, 18, 22, 28, 32, 38, 42, 48, 52, 58,
                             62, 68, 72, 73, 74, 75, 76, 77, 78, 1, 2, 3, 4, 5,
                             6, 7, 8, 9, 11, 19, 21, 29, 31, 39, 41, 49, 51, 59,
                             61, 69, 71, 79, 81, 82, 83, 84, 85, 86, 87, 88, 89,
                             NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN,
                             NaN, NaN, 10, NaN, 20, NaN, 30, NaN, 40, NaN, 50,
                             NaN, 60, NaN, 70, NaN, 80, NaN, 90, NaN, 91, 92, 
                             93, 94, 95, 96, 97, 98, 99, 100)
  
  actual_neighbours_3 <- getNeighbourCells(LC_deltas = test_raster, 
                                           cell_number = 22, 
                                           harmonisation_radius = 1)
  expected_neighbours_3 <- c(11, 12, 13, 21, 23, 31, 32, 33)
    
  expect_equal(actual_neighbours_1,
               expected_neighbours_1)
  expect_equal(actual_neighbours_2,
               expected_neighbours_2)
  expect_equal(actual_neighbours_3,
               expected_neighbours_3)
})

test_that("getNeighbourCells returns correct neighbour cells across the date line", {
  
  test_raster <- terra::rast(nrow = 10, 
                             ncol = 10, 
                             vals = 1:100)
  test_raster <- c(test_raster, 
                   terra::rast(test_raster, 
                               vals = 101:200))
  names(test_raster) <- c("t1", 
                          "t2")
  
  actual_neighbours_1 <- getNeighbourCells(LC_deltas = test_raster, 
                                           cell_number = 50,
                                           harmonisation_radius = 2)
  expected_neighbours_1 <- c(39, 40, 31, 49, 41, 59, 60, 51, 28, 29, 30, 21,
                             22, 38, 32, 48, 42, 58, 52, 68, 69, 70, 61, 62)
  
  expect_equal(actual_neighbours_1,
               expected_neighbours_1)
})
