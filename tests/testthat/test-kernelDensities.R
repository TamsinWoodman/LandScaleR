
test_that("getDistMatrix returns matrix of cell distances", {

  expected_matrix <- matrix(data = c(2, 1, 2,
                                     1, NA, 1,
                                     2, 1, 2),
                            nrow = 3,
                            ncol = 3,
                            byrow = TRUE)

  expect_equal(getDistMatrix(1),
               expected_matrix)

  ## Could add different cases, e.g. values of 2, 3, 4
  ## Could also check that getDistMatrix returns NA if a value of 0 is used as the input

})

test_that("kernelDensitiesEquation returns expected kernel density", {

  neighbour_values <- c(10, 0.5, 0, 7, 5, 0, 2, 8, 10)

  expected_kd <- 3.3125
  actual_kd <- kernelDensitiesEquation(neighbour_values,
                                       getDistMatrix(1))

  expect_equal(actual_kd,
               expected_kd)
})

test_that("kernelDensitiesEquation returns NaN if neighbouring cells are all NA", {

  neighbour_values <- c(NA, NA, NA, NA, 2, NA, NA, NA, NA)

  expected_kd <- NaN
  actual_kd <- kernelDensitiesEquation(neighbour_values,
                                       getDistMatrix(1))

  expect_equal(actual_kd,
               expected_kd)
})
