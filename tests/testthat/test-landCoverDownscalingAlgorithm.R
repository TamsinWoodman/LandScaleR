
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
