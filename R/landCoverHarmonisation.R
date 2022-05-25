# Harmonisation functions
# During harmonisation, the algorithm looks for nearby cells in which to put any
# land cover change that was not allocated to a specific coarse-scale cell

harmoniseUnallocatedLCDeltas <- function(LC_allocation_params) {

  print(paste0("Starting harmonisation..."))
  harmonisation_start_time <- Sys.time()

  # Extract variables that are needed to look for unallocated land cover
  LC_deltas <- slot(LC_allocation_params,
                    "LC_deltas")
  ref_map <- slot(LC_allocation_params,
                  "ref_map")

  harmonised_LC_deltas_df <- slot(LC_deltas,
                                  "LC_map")
  LC_deltas_cell_area <- slot(LC_deltas,
                              "cell_area")
  initial_ref_map_df <- slot(ref_map,
                             "LC_map")
  LC_classes <- slot(ref_map,
                     "LC_classes")
  ref_map_cell_area <- slot(ref_map,
                            "cell_area")

  # Get a data frame of unallocated land cover deltas
  unallocated_LC_deltas_df <- LCAllocationChecks(LC_deltas_df = harmonised_LC_deltas_df,
                                                 LC_deltas_cell_area = LC_deltas_cell_area,
                                                 LC_classes = LC_classes,
                                                 ref_map_df = initial_ref_map_df,
                                                 ref_map_cell_area = ref_map_cell_area)

  # Run harmonisation if required
  if (!is.null(unallocated_LC_deltas_df)) {

    # Extract variables from object
    kernel_radius <- slot(LC_allocation_params,
                          "kernel_radius")
    random_seed <- slot(LC_allocation_params,
                        "random_seed")

    LC_deltas_cell_resolution <- slot(LC_deltas,
                                      "cell_resolution")
    harmonised_ref_map_df <- initial_ref_map_df
    harmonised_agg_ref_map_df <- slot(ref_map,
                                      "agg_LC_map")
    ref_map_cell_resolution <- slot(ref_map,
                                    "cell_resolution")

    for (i in 1:nrow(unallocated_LC_deltas_df)) {

      # It doesn't make sense to look for nearest neighbours with FNN package.
      # If you specify 20 nearest neighbours and have a linear strip of land,
      # unallocated LC deltas could be placed up to 10 degrees away if each grid
      # cell is 0.5 degrees.
      # Therefore, it makes more sense to look for grid cells that are one cell
      # away, followed by two, and so on.
      # Check three rings of neighbour cells max here - this gives max. 48
      # possible cells in which to put land-use
      # The FNN package is then used to search for nearest neighbours out of the
      # 48 selected cells.
      # Land cover is allocated to the nearest cell first, then the second
      # nearest, and so on until all max. 48 have been searched.
      number_rings <- 2

      kernel_xy_dist <- calculateXYKernelDistances(ref_map_cell_resolution = LC_deltas_cell_resolution,
                                                   kernel_radius = number_rings)

      neighbour_cells <- findNeighbourCells(kernel_xy_dist = kernel_xy_dist,
                                            grid_cell = unlist(unallocated_LC_deltas_df[i, ]),
                                            ref_map_df = harmonised_agg_ref_map_df)

      neighbour_cells_order <- FNN::get.knnx(neighbour_cells[ , c("x", "y")],
                                             unallocated_LC_deltas_df[i, c("x", "y")],
                                             nrow(neighbour_cells),
                                             algorithm = "kd_tree")
      neighbour_cells_index <- neighbour_cells_order$nn.index

      for (j in 1:length(neighbour_cells_index)) {

        order_index <- neighbour_cells_index[j]

        # Calculate transition matrix here
        LC_transitions <- getLCTransitions(LC_deltas_cell = unlist(unallocated_LC_deltas_df[i, ]),
                                           LC_classes = LC_classes)

        # Break the for loop if LC_transitions is NA
        # This could happen if the sum of all LC deltas at the start of the
        # algorithm did not equal 0
        if (is.null(LC_transitions)) {
          break
        }

        neighbour_cell <- neighbour_cells[order_index, ]
        neighbour_coarse_ID <- neighbour_cell[ , "coarse_ID"]

        # Get reference map cells that are assigned to this coarse-scale cell
        assigned_ref_map_cells <- harmonised_ref_map_df[harmonised_ref_map_df$coarse_ID == neighbour_coarse_ID, ]

        harmonised_LC_maps <- allocateLCTransitions(LC_transitions = LC_transitions,
                                                    agg_ref_map_cell = neighbour_cell,
                                                    initial_ref_map_df = initial_ref_map_df,
                                                    assigned_ref_map_cells = assigned_ref_map_cells,
                                                    coarse_ID = neighbour_coarse_ID,
                                                    kernel_xy_dist = kernel_xy_dist,
                                                    random_seed = random_seed,
                                                    LC_deltas_cell = unlist(unallocated_LC_deltas_df[i, ]))

        # Update the harmonised reference map
        for (j in 1:nrow(harmonised_LC_maps[["ref_map_cells"]])) {

          harmonised_ref_map_cell <- harmonised_LC_maps[["ref_map_cells"]][j, ]
          ref_ID <- harmonised_ref_map_cell[ , "ref_ID"]

          harmonised_ref_map_df[harmonised_ref_map_df$ref_ID == ref_ID, ] <- harmonised_ref_map_cell
        }

        # Update the harmonised aggregate reference map and the LC deltas
        harmonised_agg_ref_map_df[harmonised_agg_ref_map_df$coarse_ID == neighbour_coarse_ID, ] <- harmonised_LC_maps[["agg_ref_map_cell"]]
        unallocated_LC_deltas_df[i, ] <- t(harmonised_LC_maps[["LC_deltas_cell"]])

        # Break if all land cover change has been allocated after downscaling in this cell
        if (all(round(unallocated_LC_deltas_df[i, LC_classes], 8) == 0)) {
          break
        }

      }
    }

    # Check if there is any unallocated land cover change after harmonisation
    unallocated_LC_deltas_post_harmonisation <- LCAllocationChecks(LC_deltas_df = as.data.frame(unallocated_LC_deltas_df),
                                                                   LC_deltas_cell_area = LC_deltas_cell_area,
                                                                   LC_classes = LC_classes,
                                                                   ref_map_df = harmonised_ref_map_df,
                                                                   ref_map_cell_area = ref_map_cell_area)

    if (!is.null(unallocated_LC_deltas_post_harmonisation)) {
      warning("There is unallocated land cover change in the following cells after harmonisation: ")
      print(unallocated_LC_deltas_post_harmonisation)
    }

    # Time check
    harmonisation_end_time <- Sys.time()
    timeCheckMessage(harmonisation_start_time,
                     harmonisation_end_time,
                     "Completed harmonisation in ")

    slot(ref_map,
         "LC_map") <- harmonised_ref_map_df
    slot(ref_map,
         "agg_LC_map") <- harmonised_agg_ref_map_df

  } else {

    print("No harmonisation required")

  }

  return(ref_map)
}
