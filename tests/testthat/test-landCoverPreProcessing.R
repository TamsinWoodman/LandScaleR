
test_that("calculateLCDeltaInputChecks stops if land cover maps are not data frames", {

  LC_map_1 <- data.frame(x = c(0, 1),
                         y = c(0, 2),
                         pri = c(0.5, 0.5),
                         sec = c(1, 1))
  LC_map_2 <- matrix()
  LC_classes <- c("pri",
                  "sec")

  expect_error(calculateLCDeltasInputChecks(LC_map_1 = LC_map_1,
                                            LC_map_2 = LC_map_2,
                                            LC_classes = LC_classes,
                                            x_col = "x",
                                            y_col = "y"),
               "Land cover map 2 is not a data frame.")
  expect_error(calculateLCDeltasInputChecks(LC_map_1 = LC_map_2,
                                            LC_map_2 = LC_map_2,
                                            LC_classes = LC_classes,
                                            x_col = "x",
                                            y_col = "y"),
               "Land cover map 1 is not a data frame.")
})

test_that("calculateLCDeltaInputChecks stops if land cover maps have different numbers of rows", {

  LC_map_1 <- data.frame(x = c(0, 1),
                         y = c(0, 2),
                         pri = c(0.5, 0.5),
                         sec = c(1, 1))
  LC_map_2 <- data.frame(x = c(0),
                         y = c(0),
                         pri = c(0.5),
                         sec = c(1))
  LC_classes <- c("pri",
                  "sec")

  expect_error(calculateLCDeltasInputChecks(LC_map_1 = LC_map_1,
                                            LC_map_2 = LC_map_2,
                                            LC_classes = LC_classes,
                                            x_col = "x",
                                            y_col = "y"),
               "Land cover maps contain different numbers of rows.")
})

test_that("calculateLCDeltaInputChecks stops if land cover classes are not a vector", {

  LC_map_1 <- data.frame(x = c(0, 1),
                         y = c(0, 2),
                         pri = c(0.5, 0.5),
                         sec = c(1, 1))
  LC_map_2 <- data.frame(x = c(0, 1),
                         y = c(0, 2),
                         pri = c(0.5, 0.5),
                         sec = c(1, 1))
  LC_classes <- matrix(c("pri", "sec"))

  expect_error(calculateLCDeltasInputChecks(LC_map_1 = LC_map_1,
                                            LC_map_2 = LC_map_2,
                                            LC_classes = LC_classes,
                                            x_col = "x",
                                            y_col = "y"),
               "Land cover classes are not in the form of a vector.")
})

test_that("calculateLCDeltaInputChecks stops if land cover classes are not data frame columns", {

  LC_map_1 <- data.frame(x = c(0, 1),
                         y = c(0, 2),
                         pri = c(0.5, 0.5),
                         urb = c(1, 1))
  LC_map_2 <- data.frame(x = c(0, 1),
                         y = c(0, 2),
                         pri = c(0.5, 0.5),
                         sec = c(1, 1))
  LC_classes <- c("pri", "sec")

  expect_error(calculateLCDeltasInputChecks(LC_map_1 = LC_map_1,
                                            LC_map_2 = LC_map_2,
                                            LC_classes = LC_classes,
                                            x_col = "x",
                                            y_col = "y"),
               "Land cover class columns are missing from land cover map 1.")
  expect_error(calculateLCDeltasInputChecks(LC_map_1 = LC_map_2,
                                            LC_map_2 = LC_map_1,
                                            LC_classes = LC_classes,
                                            x_col = "x",
                                            y_col = "y"),
               "Land cover class columns are missing from land cover map 2.")
})

test_that("calculateLCDeltaInputChecks stops if there are extra land cover classes in the data frames", {

  LC_map_1 <- data.frame(x = c(0, 1),
                         y = c(0, 2),
                         pri = c(0.5, 0.5),
                         sec = c(2, 3),
                         urb = c(1, 1))
  LC_map_2 <- data.frame(x = c(0, 1),
                         y = c(0, 2),
                         pri = c(0.5, 0.5),
                         sec = c(1, 1))
  LC_classes <- c("pri", "sec")

  expect_warning(calculateLCDeltasInputChecks(LC_map_1 = LC_map_1,
                                              LC_map_2 = LC_map_2,
                                              LC_classes = LC_classes,
                                              x_col = "x",
                                              y_col = "y"),
                 "There are one or more extra land cover classes in land cover map 1. These will not be included in output land cover change.")
  expect_warning(calculateLCDeltasInputChecks(LC_map_1 = LC_map_2,
                                              LC_map_2 = LC_map_1,
                                              LC_classes = LC_classes,
                                              x_col = "x",
                                              y_col = "y"),
               "There are one or more extra land cover classes in land cover map 2. These will not be included in output land cover change.")
})

test_that("calculateLCDeltaInputChecks stops if the x-coordinate column name is missing from at least one data frame", {

  LC_map_1 <- data.frame(y = c(0, 2),
                         pri = c(0.5, 0.5),
                         sec = c(2, 3))
  LC_map_2 <- data.frame(x = c(0, 1),
                         y = c(0, 2),
                         pri = c(0.5, 0.5),
                         sec = c(1, 1))
  LC_classes <- c("pri", "sec")

  expect_error(calculateLCDeltasInputChecks(LC_map_1 = LC_map_1,
                                            LC_map_2 = LC_map_2,
                                            LC_classes = LC_classes,
                                            x_col = "x",
                                            y_col = "y"),
               "X-coordinate column name is missing from land cover map 1.")
  expect_error(calculateLCDeltasInputChecks(LC_map_1 = LC_map_2,
                                              LC_map_2 = LC_map_1,
                                              LC_classes = LC_classes,
                                              x_col = "x",
                                              y_col = "y"),
                 "X-coordinate column name is missing from land cover map 2.")
})

test_that("calculateLCDeltaInputChecks stops if the y-coordinate column name is missing from at least one data frame", {

  LC_map_1 <- data.frame(x = c(0, 1),
                         pri = c(0.5, 0.5),
                         sec = c(2, 3))
  LC_map_2 <- data.frame(x = c(0, 1),
                         y = c(0, 2),
                         pri = c(0.5, 0.5),
                         sec = c(1, 1))
  LC_classes <- c("pri", "sec")

  expect_error(calculateLCDeltasInputChecks(LC_map_1 = LC_map_1,
                                            LC_map_2 = LC_map_2,
                                            LC_classes = LC_classes,
                                            x_col = "x",
                                            y_col = "y"),
               "Y-coordinate column name is missing from land cover map 1.")
  expect_error(calculateLCDeltasInputChecks(LC_map_1 = LC_map_2,
                                            LC_map_2 = LC_map_1,
                                            LC_classes = LC_classes,
                                            x_col = "x",
                                            y_col = "y"),
               "Y-coordinate column name is missing from land cover map 2.")
})

test_that("calculateLCDeltas returns difference between land cover columns from two data frames", {
  LC_map_1 <- data.frame(x = c(0, 1),
                         y = c(0, 2),
                         pri = c(0.5, 0.5),
                         sec = c(2, 3))
  LC_map_2 <- data.frame(x = c(0, 1),
                         y = c(0, 2),
                         pri = c(0.5, 0.5),
                         sec = c(1, 1))
  LC_classes <- c("pri", "sec")

  expected_LC_deltas <- data.frame(x = c(0, 1),
                                   y = c(0, 2),
                                   pri = c(0, 0),
                                   sec = c(-1, -2))
  actual_LC_deltas <- calculateLCDeltas(LC_map_1 = LC_map_1,
                                        LC_map_2 = LC_map_2,
                                        LC_classes = LC_classes)

  expect_identical(actual_LC_deltas,
                   expected_LC_deltas)
})

test_that("calculateLCDeltas stops if the coordinates in two land cover maps do not match after sorting", {
  LC_map_1 <- data.frame(x = c(0, 1.5),
                         y = c(0, 2),
                         pri = c(0.5, 0.5),
                         sec = c(2, 3))
  LC_map_2 <- data.frame(x = c(0, 1),
                         y = c(0, 2),
                         pri = c(0.5, 0.5),
                         sec = c(1, 1))
  LC_map_3 <- data.frame(x = c(0, 1),
                         y = c(0, 10),
                         pri = c(0.5, 0.5),
                         sec = c(1, 1))
  LC_classes <- c("pri", "sec")

  expect_error(calculateLCDeltas(LC_map_1 = LC_map_1,
                                 LC_map_2 = LC_map_2,
                                 LC_classes = LC_classes),
               "Coordinates differ between the two timesteps")
  expect_error(calculateLCDeltas(LC_map_1 = LC_map_1,
                                 LC_map_2 = LC_map_3,
                                 LC_classes = LC_classes),
               "Coordinates differ between the two timesteps")
})

test_that("calculateLCDeltas throws warning and ignores extra land cover columns in data frames", {
  LC_map_1 <- data.frame(x = c(0, 1),
                         y = c(0, 2),
                         pri = c(0.5, 0.5),
                         sec = c(2, 3),
                         urb = c(1, 4))
  LC_map_2 <- data.frame(x = c(0, 1),
                         y = c(0, 2),
                         pri = c(0.5, 0.5),
                         sec = c(1, 1))
  LC_classes <- c("pri", "sec")

  expected_LC_deltas <- data.frame(x = c(0, 1),
                                   y = c(0, 2),
                                   pri = c(0, 0),
                                   sec = c(-1, -2))

  expect_warning(actual_LC_deltas <- calculateLCDeltas(LC_map_1 = LC_map_1,
                                        LC_map_2 = LC_map_2,
                                        LC_classes = LC_classes))
  expect_identical(actual_LC_deltas,
                   expected_LC_deltas)
})
