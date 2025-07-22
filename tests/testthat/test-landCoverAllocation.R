
test_that("getCellMatchLCClasses makes no changes if sum of all rows equals 1", {
  
  test_coarse_cell <- createCoarseCellForTests()
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
  
  # Case 1 - one coarse class split between two ref classes
  expected_df1 <- matrix(data = c(0, 1, 0.5, 0, 0, 0, 0,
                                  1, 0, 0.5, 0, 0, 1, 0,
                                  0, 0, 0, 1, 0, 0, 0,
                                  0, 0, 0, 0, 1, 0, 0,
                                  0, 0, 0, 0, 0, 0, 1),
                         nrow = 7,
                         ncol = 5,
                         dimnames = list(coarse_LC_classes,
                                         ref_LC_classes))
  actual_df1 <- getCellMatchLCClasses(match_LC_classes = expected_df1, 
                                      coarse_cell = test_coarse_cell)
  
  # Case 2 - every coarse class matched to one ref class
  expected_df2 <- matrix(data = c(0, 1, 0, 0, 0, 0, 0,
                                  1, 0, 1, 0, 0, 1, 0,
                                  0, 0, 0, 1, 0, 0, 0,
                                  0, 0, 0, 0, 1, 0, 0,
                                  0, 0, 0, 0, 0, 0, 1),
                         nrow = 7,
                         ncol = 5,
                         dimnames = list(coarse_LC_classes,
                                         ref_LC_classes))
  actual_df2 <- getCellMatchLCClasses(match_LC_classes = expected_df2, 
                                      coarse_cell = test_coarse_cell)
  
  expect_equal(actual_df1, 
               expected_df1)
  expect_equal(actual_df2,
               expected_df2)
})

test_that("getCellMatchLCClasses gets cell-specific ratios if any rows sum to > 1", {
  
  test_coarse_cell <- createCoarseCellForTests()
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
  
  # Case 1 - split proportionally between two ref classes
  match_LC_classes1 <- matrix(data = c(0, 1, 1, 0, 0, 0, 0,
                                       1, 0, 1, 0, 0, 1, 0,
                                       0, 0, 0, 1, 0, 0, 0,
                                       0, 0, 0, 0, 1, 0, 0,
                                       0, 0, 0, 0, 0, 0, 1),
                              nrow = 7,
                              ncol = 5,
                              dimnames = list(coarse_LC_classes, 
                                              ref_LC_classes))
  expected_df1 <- matrix(data = c(0, 1, 0.46153846, 0, 0, 0, 0,
                                  1, 0, 0.53846154, 0, 0, 1, 0,
                                  0, 0, 0, 1, 0, 0, 0,
                                  0, 0, 0, 0, 1, 0, 0,
                                  0, 0, 0, 0, 0, 0, 1),
                         nrow = 7,
                         ncol = 5,
                         dimnames = list(coarse_LC_classes, 
                                         ref_LC_classes))
  actual_df1 <- getCellMatchLCClasses(match_LC_classes = match_LC_classes1, 
                                      coarse_cell = test_coarse_cell)
  
  # Case 2 - split one coarse class with set proportions and one with grid 
  # cell-specific proportions
  match_LC_classes2 <- matrix(data = c(0, 1, 0.5, 0, 0, 0, 0,
                                       1, 0, 0.5, 0, 0, 1, 0,
                                       0, 0, 0, 1, 0, 0, 0,
                                       0, 0, 0, 0, 1, 0, 0,
                                       0, 0, 0, 0, 0, 1, 1),
                              nrow = 7,
                              ncol = 5,
                              dimnames = list(coarse_LC_classes, 
                                              ref_LC_classes))
  expected_df2 <- matrix(data = c(0, 1, 0.5, 0, 0, 0, 0,
                                  1, 0, 0.5, 0, 0, 0.59574468, 0,
                                  0, 0, 0, 1, 0, 0, 0,
                                  0, 0, 0, 0, 1, 0, 0,
                                  0, 0, 0, 0, 0, 0.40425532, 1),
                         nrow = 7,
                         ncol = 5,
                         dimnames = list(coarse_LC_classes, 
                                         ref_LC_classes))
  actual_df2 <- getCellMatchLCClasses(match_LC_classes = match_LC_classes2, 
                                      coarse_cell = test_coarse_cell)
  
  # Case 3 - split one coarse class between all ref classes
  match_LC_classes3 <- matrix(data = c(0, 1, 1, 0, 0, 0, 0,
                                       1, 0, 1, 0, 0, 1, 0,
                                       0, 0, 1, 1, 0, 0, 0,
                                       0, 0, 1, 0, 1, 0, 0,
                                       0, 0, 1, 0, 0, 1, 1),
                              nrow = 7,
                              ncol = 5,
                              dimnames = list(coarse_LC_classes, 
                                              ref_LC_classes))
  expected_df3 <- matrix(data = c(0, 1, 0.24, 0, 0, 0, 0,
                                  1, 0, 0.28, 0, 0, 0.59574468, 0,
                                  0, 0, 0.12, 1, 0, 0, 0,
                                  0, 0, 0.17, 0, 1, 0, 0,
                                  0, 0, 0.19, 0, 0, 0.40425532, 1),
                         nrow = 7,
                         ncol = 5,
                         dimnames = list(coarse_LC_classes, 
                                         ref_LC_classes))
  actual_df3 <- getCellMatchLCClasses(match_LC_classes = match_LC_classes3, 
                                      coarse_cell = test_coarse_cell)
  
  # Case 4 - split proportionally between three ref classes when one is zero in 
  # the ref map
  test_coarse_cell4 <- test_coarse_cell
  test_coarse_cell4@ref_cells[["pas"]] <- test_coarse_cell4@ref_cells[["pas"]] + 
    test_coarse_cell4@ref_cells[["urb"]]
  terra::values(test_coarse_cell4@ref_cells[["urb"]]) <- 0
  test_coarse_cell4@agg_ref_cells <- unlist(terra::global(test_coarse_cell4@ref_cells,
                                                          fun = "sum",
                                                          na.rm = TRUE))
  names(test_coarse_cell4@agg_ref_cells) <- names(test_coarse_cell4@ref_cells)
  
  match_LC_classes4 <- matrix(data = c(0, 1, 1, 0, 0, 0, 0,
                                       1, 0, 1, 0, 0, 1, 0,
                                       0, 0, 0, 1, 0, 0, 0,
                                       0, 0, 0, 0, 1, 0, 0,
                                       0, 0, 1, 0, 0, 0, 1),
                              nrow = 7,
                              ncol = 5,
                              dimnames = list(coarse_LC_classes, 
                                              ref_LC_classes))
  expected_df4 <- matrix(data = c(0, 1, 0.46153846, 0, 0, 0, 0,
                                  1, 0, 0.53846154, 0, 0, 1, 0,
                                  0, 0, 0, 1, 0, 0, 0,
                                  0, 0, 0, 0, 1, 0, 0,
                                  0, 0, 0, 0, 0, 0, 1),
                         nrow = 7,
                         ncol = 5,
                         dimnames = list(coarse_LC_classes, 
                                         ref_LC_classes))
  actual_df4 <- getCellMatchLCClasses(match_LC_classes = match_LC_classes4, 
                                      coarse_cell = test_coarse_cell4)
  
  expect_equal(actual_df1, 
               expected_df1)
  expect_equal(actual_df2, 
               expected_df2)
  expect_equal(actual_df3, 
               expected_df3)
  expect_equal(actual_df4, 
               expected_df4)
})

test_that("getCellMatchLCClasses splits LULC equally if any rows sum to >1 and all classes are zero in the ref map", {
  
  test_coarse_cell <- createCoarseCellForTests()
  test_coarse_cell@ref_cells[["crp"]] <- test_coarse_cell@ref_cells[["crp"]] + 
    test_coarse_cell@ref_cells[["pri"]] + 
    test_coarse_cell@ref_cells[["sec"]]
  terra::values(test_coarse_cell@ref_cells[["pri"]]) <- 0
  terra::values(test_coarse_cell@ref_cells[["sec"]]) <- 0
  test_coarse_cell@agg_ref_cells <- unlist(terra::global(test_coarse_cell@ref_cells,
                                                         fun = "sum",
                                                         na.rm = TRUE))
  names(test_coarse_cell@agg_ref_cells) <- names(test_coarse_cell@ref_cells)
  
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
  
  # Case 1 - split between two ref classes which are both zero in the ref map
  match_LC_classes1 <- matrix(data = c(0, 1, 1, 0, 0, 0, 0,
                                       1, 0, 1, 0, 0, 1, 0,
                                       0, 0, 0, 1, 0, 0, 0,
                                       0, 0, 0, 0, 1, 0, 0,
                                       0, 0, 0, 0, 0, 0, 1),
                              nrow = 7,
                              ncol = 5,
                              dimnames = list(coarse_LC_classes, 
                                              ref_LC_classes))
  expected_df1 <- matrix(data = c(0, 1, 0.5, 0, 0, 0, 0,
                                  1, 0, 0.5, 0, 0, 1, 0,
                                  0, 0, 0, 1, 0, 0, 0,
                                  0, 0, 0, 0, 1, 0, 0,
                                  0, 0, 0, 0, 0, 0, 1),
                         nrow = 7,
                         ncol = 5,
                         dimnames = list(coarse_LC_classes, 
                                         ref_LC_classes))
  actual_df1 <- getCellMatchLCClasses(match_LC_classes = match_LC_classes1, 
                                      coarse_cell = test_coarse_cell)
  
  # Case 2 - split between 3 ref classes which are all zero in the ref map
  test_coarse_cell2 <- test_coarse_cell
  test_coarse_cell2@ref_cells[["pas"]] <- test_coarse_cell2@ref_cells[["pas"]] + 
    test_coarse_cell2@ref_cells[["urb"]]
  terra::values(test_coarse_cell2@ref_cells[["urb"]]) <- 0
  test_coarse_cell2@agg_ref_cells <- unlist(terra::global(test_coarse_cell2@ref_cells,
                                                          fun = "sum",
                                                          na.rm = TRUE))
  names(test_coarse_cell2@agg_ref_cells) <- names(test_coarse_cell2@ref_cells)
  
  match_LC_classes2 <- matrix(data = c(0, 1, 1, 0, 0, 0, 0,
                                       1, 0, 1, 0, 0, 1, 0,
                                       0, 0, 0, 1, 0, 0, 0,
                                       0, 0, 0, 0, 1, 0, 0,
                                       0, 0, 1, 0, 0, 0, 1),
                              nrow = 7,
                              ncol = 5,
                              dimnames = list(coarse_LC_classes, 
                                              ref_LC_classes))
  expected_df2 <- matrix(data = c(0, 1, 0.33333333, 0, 0, 0, 0,
                                  1, 0, 0.33333333, 0, 0, 1, 0,
                                  0, 0, 0, 1, 0, 0, 0,
                                  0, 0, 0, 0, 1, 0, 0,
                                  0, 0, 0.33333333, 0, 0, 0, 1),
                         nrow = 7,
                         ncol = 5,
                         dimnames = list(coarse_LC_classes, 
                                         ref_LC_classes))
  actual_df2 <- getCellMatchLCClasses(match_LC_classes = match_LC_classes2, 
                                      coarse_cell = test_coarse_cell2)
  
  expect_equal(actual_df1, 
               expected_df1)
  expect_equal(actual_df2, 
               expected_df2)
})

test_that("sortKernelDensities sorts a data frame by kernel_density column", {

  test_df <- data.frame(x = c(1, 2, 3, 4),
                        y = c(2, 3, 2, 2),
                        kernel_density = c(2, 1, 3, 4))

  expected_df <- data.frame(x = c(4, 3, 1, 2),
                            y = c(2, 2, 2, 3),
                            kernel_density = c(4, 3, 2, 1))
  row.names(expected_df) <- NULL
  actual_df <- sortKernelDensities(test_df)
  row.names(actual_df) <- NULL

  expect_equal(actual_df,
               expected_df)
})


test_that("getLCTransitions returns data frame with expected LC conversions", {

  test_LC_deltas_names <- c("cropland",
                            "pasture",
                            "forest",
                            "grassland")
  test_LC_deltas <- c(80, 20, -60, -40)
  names(test_LC_deltas) <- test_LC_deltas_names

  expected_df <- data.frame(LC_to_name = rep(c("cropland", "pasture"),
                                             each = 2),
                            LC_from_name = rep(c("forest", "grassland"),
                                               times = 2),
                            LC_conversion = c(-48, -32, -12, -8),
                            row.names = NULL)

  actual_df <- getLCTransitions(test_LC_deltas)

  expect_equal(actual_df,
               expected_df)
})

test_that("getLCTransitions returns NULL if there is no land cover change in a coarse cell", {

  test_LC_deltas_names <- c("cropland",
                            "pasture",
                            "forest",
                            "grassland")
  test_LC_deltas <- c(0, 0, 0, 0)

  actual_output <- getLCTransitions(test_LC_deltas)

  expect_null(actual_output)
})

test_that("updateRefCells updates expected reference map cells", {

  cell_numbers <- c(4, 5, 2, 9)
  cells_for_allocation <- data.frame(cell_number = cell_numbers,
                                     pas = c(5, 3, 0, 1),
                                     kernel_density = c(1.5, 1.2, 0.3, 0),
                                     actual_conversion = c(4, 3, 0, 0))
  ref_cells <- terra::rast(xmin = 0,
                    xmax = 3,
                    ymin = 0,
                    ymax = 3,
                    res = 1,
                    nlyrs = 2,
                    names = c("pas", "pri"))
  terra::values(ref_cells[["pas"]]) <- c(12, 0, 3, 5, 3, 3, 2, 7, 1)
  terra::values(ref_cells[["pri"]]) <- c(4, 16, 13, 11, 13, 13, 14, 9, 15)

  LC_to_name <- "pri"
  LC_from_name <- "pas"

  expected_ref_cells <- ref_cells
  terra::values(expected_ref_cells[["pas"]]) <- c(12, 0, 3, 1, 0, 3, 2, 7, 1)
  terra::values(expected_ref_cells[["pri"]]) <- c(4, 16, 13, 15, 16, 13, 14, 9, 15)

  actual_ref_cells <- updateRefCells(cells_for_allocation = cells_for_allocation,
                                     updated_ref_cells = ref_cells,
                                     LC_to_name = LC_to_name,
                                     LC_from_name = LC_from_name)

  expect_equal(terra::values(actual_ref_cells),
               terra::values(expected_ref_cells))
})

test_that("getCellsForAllocation returns expected cell numbers", {

  ref_cells <- terra::rast(xmin = 0,
                           xmax = 3,
                           ymin = 0,
                           ymax = 3,
                           res = 1,
                           nlyrs = 2,
                           names = c("pas", "pri"))
  terra::values(ref_cells[["pas"]]) <- c(12, 0, 3, 5, 3, 3, 2, 7, 1)
  terra::values(ref_cells[["pri"]]) <- c(4, 16, 13, 11, 13, 13, 14, 9, 15)

  LC_from_name <- "pas"

  expected_cells_for_allocation <- c(1, 3, 4, 5, 6, 7, 8, 9)

  actual_cells_for_allocation <- getCellsForAllocation(ref_map = ref_cells,
                                                       LC_from_name = LC_from_name)

  expect_equal(actual_cells_for_allocation,
               expected_cells_for_allocation)
})

test_that("getCellsForAllocation returns zero length integer vector if no cells contain the LC to be converted", {

  ref_cells <- terra::rast(xmin = 0,
                           xmax = 3,
                           ymin = 0,
                           ymax = 3,
                           res = 1,
                           nlyrs = 2,
                           names = c("pas", "pri"))
  terra::values(ref_cells[["pas"]]) <- rep(0, 9)
  terra::values(ref_cells[["pri"]]) <- c(4, 16, 13, 11, 13, 13, 14, 9, 15)

  LC_from_name <- "pas"

  expected_cells_for_allocation <- integer(0)

  actual_cells_for_allocation <- getCellsForAllocation(ref_map = ref_cells,
                                                       LC_from_name = LC_from_name)

  expect_equal(actual_cells_for_allocation,
               expected_cells_for_allocation)
})

test_that("getAllocationDF returns expected data frame", {

  ref_cells <- terra::rast(xmin = 0,
                           xmax = 3,
                           ymin = 0,
                           ymax = 3,
                           res = 1,
                           nlyrs = 2,
                           names = c("pas", "pri"))
  terra::values(ref_cells[["pas"]]) <- c(12, 0, 0, 0, 3, 3, 2, 7, 1)
  terra::values(ref_cells[["pri"]]) <- c(4, 16, 13, 11, 13, 13, 14, 9, 15)

  kernel_densities <- ref_cells
  terra::values(kernel_densities[["pas"]]) <- c(0.1, 1, 0.3, 0.5, 1.1, 1.2, 0.9, 0.1, 0.1)
  terra::values(kernel_densities[["pri"]]) <- c(0.4, 1.3, 1.2, 0.7, 0.5, 0.2, 0.3, 0.9, 1.5)

  LC_from_name <- "pas"
  LC_to_name <- "pri"
  cell_numbers_for_allocation <- c(1, 5, 6, 7, 8, 9)

  expected_allocation_df <- data.frame(cell_number = cell_numbers_for_allocation,
                                       dec_LC_area = c(12, 3, 3, 2, 7, 1),
                                       kernel_density = c(0.4, 0.5, 0.2, 0.3, 0.9, 1.5),
                                       actual_conversion = 0)

  actual_allocation_df <- getAllocationDF(cell_numbers_for_allocation = cell_numbers_for_allocation,
                                          updated_ref_cells = ref_cells,
                                          LC_from_name = LC_from_name,
                                          LC_to_name = LC_to_name,
                                          kernel_densities = kernel_densities)

  expect_equal(actual_allocation_df,
               expected_allocation_df)
})
