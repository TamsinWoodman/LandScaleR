#' Allocation of land-uses
#'
runLCAllocation <- function(ref_map_with_IDs,
                            kernel_density_df,
                            LC_deltas,
                            transition_priorities,
                            expansion_factor) {

  # Calculate intensification LC deltas
  # Should be LC_deltas multiplied by 1 - expansion_factor

  # Run first round of intensification
  intensified_LCs <- allocateLCs(ref_map_with_IDs,
                                 kernel_density_df,
                                 LC_deltas = LC_deltas_intensify,
                                 transition_priorities,
                                 allocationType = "intensify")

  # Calculate expansion LC deltas
  # Should be LC_deltas multiplied by expansion_factor

  # Run expansion
  expanded_LCs <- allocateLCs(intensified_LCs@ref_map_with_IDs,
                              kernel_density_df,
                              LC_deltas = LC_deltas_expand,
                              transition_priorities,
                              allocationType = "expand")

  # LC deltas for scond round of intensification are the remaining LC delta
  # that were not allocated in either the intensification or expansion round

  # Run second round of intensification
  final_LCs <- allocateLCs(ref_map_with_IDs,
                           kernel_density_df,
                           LC_deltas = expanded_LCs@LC_deltas,
                           transition_priorities,
                           allocationType = "intensify")

  return(final_LCs)
}

#' Intensification of land-uses
#'
allocateLCs <- function(ref_map_with_IDs,
                        kernel_density_df,
                        LC_deltas,
                        transition_priorities,
                        allocationType = "intensify") {

  updated_ref_map_with_IDs <- ref_map_with_IDs

  # for each coarse-scale grid cell
    # loop through transition priorities
      # intensify land-use if delta > 0 for a land-use type
  for (i in 1:nrow(LC_deltas)) {
    coarse_ID <- LC_deltas$coarse_ID[i]

    # Loop through the land-uses
    for (j in 1:nrow(transition_priorities)) {
      LC_inc_name <- row.names(transition_priorities)[j]
      LC_inc_delta <- LC_deltas[i, LC_inc_name]

      # Intensify land-use if it increases in the cell
      if (LC_inc_delta > 0) {
        print(paste(i, LC_inc_name, LC_inc_delta))

        # Get sorted list of priorities
        sorted_priority_LCs <- getSortedTransitionPriorities(transition_priorities,
                                                             j)

        # Loop through the other land-uses in priority order
        for (k in 1:length(sorted_priority_LCs)) {
          LC_dec_name <- sorted_priority_LCs[k]
          LC_dec_delta <- LC_deltas[i, LC_dec_name]

          if (LC_dec_delta < 0) {

            print(paste0(i, " negative ", LC_dec_name, LC_dec_delta))

            # Get cells to intensify land-use one in
            ### Getting the cells in which to expand/intensify LCs will be a
            ### switch statement --> then the rest of the function is the same
            ### irrespective of whether you're intensifying or expanding LCs
            cells_for_allocation <- getCellsToIntensifyLC(updated_ref_map_with_IDs,
                                                         LC_inc_name,
                                                         LC_dec_name,
                                                         coarse_ID)

            # Get LC increasing to LC decreasing conversions
            LC_conversion_df <- getLCIncreasingToLCDecreasingConversions(kernel_density_df,
                                                                         LC_inc_name,
                                                                         cells_for_allocation[ , "ref_ID"],
                                                                         LC_inc_delta,
                                                                         LC_dec_delta,
                                                                         cells_for_allocation[ , LC_dec_name])

            # Calculate total conversion value
            total_conversion <- sum(LC_conversion_df$actual_conversion)
            print(total_conversion)

            ### Need something in here to update the LC areas of the reference
            ### map cells

            ### Update the LC_deltas so that you don't take away LC from LC
            ### types that have already lost their delta value
            # Update LC_inc_delta in this for loop and in data frame
            LC_inc_delta <- LC_inc_delta - total_conversion
            LC_deltas[i, LC_inc_name] <- LC_inc_delta

            # Update LC_dec_delta in data frame
            LC_deltas[i, LC_dec_name] <- LC_dec_delta + total_conversion

            # I don't think we need to break the for loop now because LC_inc_delta
            # is being reset within this loop
            # Break the for loop if the total_conversion value is equal to
            # LC_inc_delta
            # if (total_conversion == LC_inc_delta) {
            #   break
            # }
          }
        }

      }

    }
  }
  return(LC_deltas)
}

#' Get a sorted list of transition priorities for land-use allocation
getSortedTransitionPriorities <- function(transition_priorities,
                                          row_number) {

  all_sorted_priorities <- sort(transition_priorities[row_number, ])
  sorted_priorities <- all_sorted_priorities[-which(all_sorted_priorities == 0)]
  sorted_priority_LCs <- names(sorted_priorities)

  return(sorted_priority_LCs)
}

#' Get cells in which to intensify or expand one LC into another LC
getCellsToIntensifyLC <- function(updated_ref_map_with_IDs,
                                  LC_inc_name,
                                  LC_dec_name,
                                  coarse_ID) {

  cells_to_intensify <- updated_ref_map_with_IDs[which(updated_ref_map_with_IDs[ , LC_inc_name] > 0 &
                                                         updated_ref_map_with_IDs[ , LC_dec_name] > 0 &
                                                         updated_ref_map_with_IDs[ , "coarse_ID"] == coarse_ID), ]

  return(cells_to_intensify)
}

#' Get conversions of the increasing LC into the decreasing LC
getLCIncreasingToLCDecreasingConversions <- function(kernel_density_df,
                                                     LC_inc_name,
                                                     cells_for_allocation_ref_IDs,
                                                     LC_inc_delta,
                                                     LC_dec_delta,
                                                     LC_dec_values) {

  # Get kernel densities for cells to allocate LC conversion to
  LC_conversion_df <- getKernelDensitiesForAllocation(kernel_density_df,
                                                      LC_inc_name,
                                                      cells_for_allocation_ref_IDs)

  # Calculate tentative conversion values
  # Max amount of LC change used for tentative conversion is the min
  # out of LC_inc_delta and LC_inc_delta
  LC_delta_for_tc <- getLCDeltaForTentativeConversion(LC_inc_delta,
                                                      LC_dec_delta)

  # Sum kernel densities
  sum_of_kernel_densities <- sum(LC_conversion_df[ , LC_inc_name])

  # Calculate tentative conversion values
  LC_conversion_df$tentative_conversion <- calculateTentativeConversionValues(LC_delta_for_tc,
                                                                              LC_conversion_df[ , LC_inc_name],
                                                                              sum_of_kernel_densities)
  #print(tentative_conversion)

  # Calculate the actual conversion values
  LC_conversion_df$actual_conversion <- calculateActualConversion(LC_dec_values,
                                                                  LC_conversion_df$tentative_conversion)

  return(LC_conversion_df)
}

#' Get kernel densities for the cells to which to allocate land-use one
#'
getKernelDensitiesForAllocation <- function(kernel_density_df,
                                            LC_inc_name,
                                            ref_IDs) {

  kernel_densities_for_allocation <- kernel_density_df[which(kernel_density_df[ , "ref_ID"] %in% ref_IDs), c(LC_inc_name, "ref_ID")]

  return(kernel_densities_for_allocation)
}

#' Get LC delta for tentative LC conversions, which is the minimum of delta of
#' LC one and delta of LC two
getLCDeltaForTentativeConversion <- function(LC_inc_delta,
                                             LC_dec_delta) {

  LC_delta_for_tc <- min(abs(LC_inc_delta), abs(LC_dec_delta))

  return(LC_delta_for_tc)
}

#' Calculate tentative conversion values
calculateTentativeConversionValues <- function(LC_delta_for_tc,
                                               kernel_densities,
                                               sum_of_kernel_densities) {

  tentative_conversions <- LC_delta_for_tc * (kernel_densities / sum_of_kernel_densities)

  return(tentative_conversions)
}

#' Calculate actual conversion value for each cell, which is the minimum of LC
#' two and the tentative conversion value for each cell
calculateActualConversion <- function(LC_dec_values,
                                      tentative_conversion) {

  actual_conversion <- pmin(LC_dec_values,
                            tentative_conversion)

  return(actual_conversion)
}

#' Intensify one land-use
#'
intensifyOneLandUse <- function() {

  # Get data frame with cells for intensification and corresponding kernel densities

  # Calculate tentative conversion for each cell

  # Calculate actual conversion for each cell

  # Return new areas and remaining

}
