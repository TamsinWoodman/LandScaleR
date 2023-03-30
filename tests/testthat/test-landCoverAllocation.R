
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

