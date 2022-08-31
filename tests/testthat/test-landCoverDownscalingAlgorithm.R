
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
