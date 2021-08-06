
test_that("checkLandCoverAreasInOneCell returns invisible NULL", {
  test_grid_cell <- data.frame(x = 1, y = 1, LC1 = 5, LC2 = 5, LC3 = 5)
  grid_cell_area <- 15
  LC_types <- c("LC1", "LC2", "LC3")

  expect_equal(checkLandCoverAreasInOneCell(test_grid_cell,
                                            grid_cell_area,
                                            LC_types),
               invisible(NULL))
})

test_that("checkLandCoverAreasInOneCell returns invisible NULL if area is 0.1% different", {
  test_grid_cell <- data.frame(x = 1, y = 1, LC1 = 5, LC2 = 5, LC3 = 5)
  grid_cell_area <- 15.015
  LC_types <- c("LC1", "LC2", "LC3")

  expect_equal(checkLandCoverAreasInOneCell(test_grid_cell,
                                            grid_cell_area,
                                            LC_types),
               invisible(NULL))
})

test_that("checkLandCoverAreasInOneCell gives warning if area is >0.1% different", {
  test_grid_cell <- data.frame(x = 1, y = 1, LC1 = 5, LC2 = 5, LC3 = 5)
  grid_cell_area <- 16
  LC_types <- c("LC1", "LC2", "LC3")

  expect_warning(checkLandCoverAreasInOneCell(test_grid_cell,
                                              grid_cell_area,
                                              LC_types),
                 " is >0.1% different to expected land cover area")
})
