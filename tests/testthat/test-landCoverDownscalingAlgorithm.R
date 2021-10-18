
test_that("addCellIDs returns a data frame with sorted x, y and coarse_ID columns", {
  test_LC_df <- data.frame(x = c(2, 5, 4, 3, 2, 3, 1, 1),
                           y = c(8, 4, 3, 6, 6, 3, 2, 1),
                           LC1 = c(100, 40, 30, 50, 35, 10, 0, 12))

  expected_LC_df <- data.frame(x = c(1, 1, 2, 2, 3, 3, 4, 5),
                               y = c(1, 2, 6, 8, 3, 6, 3, 4),
                               LC1 = c(12, 0, 35, 100, 10, 50, 30, 40),
                               coarse_ID = seq(1:8))

  actual_LC_df <- addCellIDs(test_LC_df,
                             "coarse_ID")

  # remove row names as not expecting them to be identical
  row.names(expected_LC_df) <- NULL
  row.names(actual_LC_df) <- NULL

  # Check if actual output is same as expected output
  expect_identical(actual_LC_df,
                   expected_LC_df)
})

test_that("assignRefMapCells assigns grid cells to nearest neighbours" , {
  coarse_scale_df <- data.frame(x = c(1, 1),
                                y = c(1, 3))
  fine_scale_df <- data.frame(x = c(0.5, 0.5, 3.5),
                              y = c(0.5, 2.5, 1.5))

  expected_df <- cbind(fine_scale_df,
                       coarse_ID = as.integer(c(1, 2, 1)))

  actual_df <- assignRefMapCells(fine_scale_df,
                                 coarse_scale_df)

  expect_identical(actual_df,
                   expected_df)
})

test_that("convertDiscreteLCToLCAreasOneCell returns vector of land cover areas", {
  LC_class <- "pri"
  ref_map_LC_types <- c("pri",
                        "sec",
                        "urb")
  ref_map_cell_area <- 25

  expected_vector <- c(25, 0, 0)
  names(expected_vector) <- ref_map_LC_types

  actual_vector <- convertDiscreteLCToLCAreasOneCell(LC_class = LC_class,
                                                     ref_map_LC_types = ref_map_LC_types,
                                                     ref_map_cell_area = ref_map_cell_area)

  expect_identical(actual_vector,
                   expected_vector)
})

test_that("convertDiscreteLCToLCAreasOneCell throws error if LC class is not a reference map LC class", {
  LC_class <- "crp"
  ref_map_LC_types <- c("pri",
                        "sec",
                        "urb")
  ref_map_cell_area <- 25

  expect_error(convertDiscreteLCToLCAreasOneCell(LC_class = LC_class,
                                                 ref_map_LC_types = ref_map_LC_types,
                                                 ref_map_cell_area = ref_map_cell_area),
               " is not a land cover class in the ref_map_LC_types vector")
})

test_that("convertDiscreteLCToLCAreas returns data frame", {
  ref_map_raw <- data.frame(x = c(1, 2),
                            y = c(1, 2),
                            Land_class = c("pri", "urb"))
  ref_map_LC_types = c("pri",
                       "urb")
  ref_map_cell_area = 2
  LC_column_name = "Land_class"

  expected_df <- data.frame(x = c(1, 2),
                            y = c(1, 2),
                            pri = c(2, 0),
                            urb = c(0, 2))

  actual_df <- convertDiscreteLCToLCAreas(ref_map = ref_map_raw,
                                          ref_map_LC_types = ref_map_LC_types,
                                          ref_map_cell_area = ref_map_cell_area,
                                          LC_column_name = LC_column_name)

  expect_identical(actual_df,
                   expected_df)
})

test_that("convertDiscreteLCToLCAreas returns data frame with extra land cover class columns", {
  ref_map_raw <- data.frame(x = c(1, 2),
                            y = c(1, 2),
                            Land_class = c("pri", "urb"))
  ref_map_LC_types = c("pri",
                       "urb",
                       "sec",
                       "pas")
  ref_map_cell_area = 2
  LC_column_name = "Land_class"

  expected_df <- data.frame(x = c(1, 2),
                            y = c(1, 2),
                            pri = c(2, 0),
                            urb = c(0, 2),
                            sec = c(0, 0),
                            pas = c(0, 0))

  actual_df <- convertDiscreteLCToLCAreas(ref_map = ref_map_raw,
                                          ref_map_LC_types = ref_map_LC_types,
                                          ref_map_cell_area = ref_map_cell_area,
                                          LC_column_name = LC_column_name)

  expect_identical(actual_df,
                   expected_df)
})

test_that("convertDiscreteLCToLCAreas throws error if LC_column_name does not exist", {
  ref_map_raw <- data.frame(x = c(1, 2),
                            y = c(1, 2),
                            Land_class = c("pri", "urb"))
  ref_map_LC_types = c("pri",
                       "urb")
  ref_map_cell_area = 2
  LC_column_name = "Land_cover_class"

  expect_error(convertDiscreteLCToLCAreas(ref_map = ref_map_raw,
                                          ref_map_LC_types = ref_map_LC_types,
                                          ref_map_cell_area = ref_map_cell_area,
                                          LC_column_name = LC_column_name),
               "was not found as a column name in the reference map")
})
