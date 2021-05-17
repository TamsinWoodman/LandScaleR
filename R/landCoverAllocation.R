#' Intensification of land-uses
#'
intensifyLandUses <- function(fine_scale_df_with_IDs,
                              kernel_density_df,
                              land_use_deltas,
                              transition_priorities) {

  # for each coarse-scale grid cell
    # loop through transition priorities
      # intensify land-use if delta > 0 for a land-use type
  for (i in 1:nrow(land_use_deltas)) {
    coarse_ID <- land_use_deltas$coarse_ID

    # Loop through the land-uses
    for (j in 1:nrow(transition_priorities)) {
      land_use_one <- row.names(transition_priorities)[j]
      land_use_one_delta <- land_use_deltas[i, land_use_one]

      # Intensify land-use if it increases in the cell
      if (land_use_one_delta > 0) {
        print(paste(i, land_use_one))

        # Get sorted list of priorities
        sorted_priority_land_uses <- getSortedTransitionPriorities(transition_priorities,
                                                                   j)

        # Loop through the other land-uses in priority order
        for (k in 1:length(sorted_priority_land_uses)) {
          land_use_two <- sorted_priority_land_uses[k]

          if (land_use_deltas[i, land_use_two] < 0) {
            print(paste0(i, " negative ", land_use_two))

            # Get cells to intensify land-use one in
            cells_for_intensification <- fine_scale_df_with_IDs[which(fine_scale_df_with_IDs[ , land_use_one] > 0 &
                                                                        fine_scale_df_with_IDs[ , land_use_two] > 0 &
                                                                        fine_scale_df_with_IDs[ , "coarse_ID"] == i), ]

            # Get kernel densities for cells to intensify land-use one in
            kernel_densities_for_intensification <- kernel_density_df[which(kernel_density_df[ , "fine_ID"] %in% cells_for_intensification[ , "fine_ID"]), ]

            # Calculate tentative conversion values
            sum_of_kernel_densities <- sum(kernel_densities_for_intensification[ , land_use_one])
            tentative_conversion <- land_use_one_delta * (kernel_densities_for_intensification[ , land_use_one] / sum_of_kernel_densities)
            print(tentative_conversion)

            # Calculate the actual conversion values
            actual_conversion <- pmin(cells_for_intensification[ , land_use_two],
                                      tentative_conversion)
            print(actual_conversion)

            # Calculate total conversion value
            total_conversion <- sum(actual_conversion)


            print(total_conversion)
            print(land_use_one_delta)
            ### Need to add a break in the for loop if the total_conversion value
            ### has met the land_use_one_delta
          }
        }

        # Get fine-scale data frame with corresponding cells


      }

    }
  }
}

#' Get a sorted list of transition priorities for land-use allocation
getSortedTransitionPriorities <- function(transition_priorities,
                                          row_number) {

  all_sorted_priorities <- sort(transition_priorities[row_number, ])
  sorted_priorities <- all_sorted_priorities[-which(all_sorted_priorities == 0)]
  sorted_priority_land_uses <- names(sorted_priorities)

  return(sorted_priority_land_uses)
}

#' Intensify one land-use
#'
intensifyOneLandUse <- function() {

  # Get data frame with cells for intensification and corresponding kernel densities

  # Calculate tentative conversion for each cell

  # Calculate actual conversion for each cell

  # Return new areas and remaining

}
