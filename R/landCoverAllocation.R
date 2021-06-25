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

  # LC deltas for second round of intensification are the remaining LC delta
  # that were not allocated in either the intensification or expansion round

  # Run second round of intensification
  final_LCs <- allocateLCs(expanded_LCs@ref_map_with_IDs,
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
  updated_LC_deltas <- LC_deltas

  # Loop through the rows in the updated_LC_deltas data frame
  for (i in 1:nrow(updated_LC_deltas)) {
    coarse_ID <- updated_LC_deltas$coarse_ID[i]

    # Loop through the land-uses
    for (j in 1:nrow(transition_priorities)) {
      LC_inc_name <- row.names(transition_priorities)[j]
      LC_inc_delta <- updated_LC_deltas[i, LC_inc_name]

      # Allocate the land cover if it increases in the cell
      if (LC_inc_delta > 0) {
        print(paste(i, LC_inc_name, LC_inc_delta))

        # Get sorted list of priorities
        sorted_priority_LCs <- getSortedTransitionPriorities(transition_priorities,
                                                             j)

        # Loop through the other land covers in priority order
        for (k in 1:length(sorted_priority_LCs)) {
          LC_dec_name <- sorted_priority_LCs[k]
          LC_dec_delta <- updated_LC_deltas[i, LC_dec_name]

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

            # Update the reference map with new land cover areas
            updated_ref_map_with_IDs <- updateRefMapWithLCConversions(updated_ref_map_with_IDs,
                                                                      LC_conversion_df,
                                                                      LC_dec_name,
                                                                      LC_inc_name)

            # Calculate total conversion value
            total_conversion <- sum(LC_conversion_df$actual_conversion)
            print(total_conversion)

            # Update LC_inc_delta in this for loop and in data frame
            LC_inc_delta <- updateLCIncDelta(LC_inc_delta,
                                             total_conversion)
            updated_LC_deltas[i, LC_inc_name] <- LC_inc_delta

            # Update LC_dec_delta in data frame
            updated_LC_deltas[i, LC_dec_name] <- updateLCDecDelta(LC_dec_delta,
                                                                  total_conversion)
          }
        }

      }

    }
  }

  allocated_LCs <- new("LCDataClass",
                       ref_map_df = updated_ref_map_with_IDs,
                       LC_deltas = updated_LC_deltas,
                       LC_types = row.names(transition_priorities))

  return(allocated_LCs)
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

#' Update reference map with land cover conversion values
updateRefMapWithLCConversions <- function(updated_ref_map_with_IDs,
                                          LC_conversion_df,
                                          LC_dec_name,
                                          LC_inc_name) {

  newly_updated_ref_map_with_IDs <- updated_ref_map_with_IDs

  for (i in 1:nrow(LC_conversion_df)) {
    ref_ID <- LC_conversion_df[i, "ref_ID"]
    actual_conversion <- LC_conversion_df[i, "actual_conversion"]

    # Update LCs in this cell
    newly_updated_ref_map_with_IDs <- updateOneRefMapCellWithLCConversions(newly_updated_ref_map_with_IDs,
                                                                           ref_ID,
                                                                           LC_inc_name,
                                                                           LC_dec_name,
                                                                           actual_conversion)
  }

  return(newly_updated_ref_map_with_IDs)
}

#' Update one reference map cells with land cover conversion balues
updateOneRefMapCellWithLCConversions <- function(updated_ref_map_with_IDs,
                                                 ref_ID,
                                                 LC_inc_name,
                                                 LC_dec_name,
                                                 actual_conversion) {

  newly_updated_ref_map_with_IDs <- updated_ref_map_with_IDs

  # Update area of the increasing LC
  current_LC_inc_area <-  newly_updated_ref_map_with_IDs[newly_updated_ref_map_with_IDs$ref_ID == ref_ID, LC_inc_name]
  new_LC_inc_area <- current_LC_inc_area + actual_conversion
  newly_updated_ref_map_with_IDs[newly_updated_ref_map_with_IDs$ref_ID == ref_ID, LC_inc_name] <- new_LC_inc_area

  # Update area of the decreasing LC
  current_LC_dec_area <-  newly_updated_ref_map_with_IDs[newly_updated_ref_map_with_IDs$ref_ID == ref_ID, LC_dec_name]
  new_LC_dec_area <- current_LC_dec_area - actual_conversion
  newly_updated_ref_map_with_IDs[newly_updated_ref_map_with_IDs$ref_ID == ref_ID, LC_dec_name] <- new_LC_dec_area

  return(newly_updated_ref_map_with_IDs)
}

# Update LC increasing delta value
updateLCIncDelta <- function(LC_inc_delta,
                             total_conversion) {

  LC_inc_delta_updated <- LC_inc_delta - total_conversion

  return(LC_inc_delta_updated)
}

# Update LC decreasing delta value
updateLCDecDelta <- function(LC_dec_delta,
                             total_conversion) {

  LC_dec_delta_updated <- LC_dec_delta + total_conversion

  return(LC_dec_delta_updated)
}
