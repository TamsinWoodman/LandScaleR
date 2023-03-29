
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
