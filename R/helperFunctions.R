# Time check messages
timeCheckMessage <- function(start_time,
                             end_time,
                             message) {

  time_taken <- difftime(end_time,
                         start_time,
                         units = "auto")
  time_taken <- format(time_taken)

  print(paste0(message,
               time_taken))
}

# Randomly sorts a data frame
randomiseDataFrame <- function(input_df) {

  df_rows <- sample(nrow(input_df))

  random_input_df <- input_df[df_rows, ]

  return(random_input_df)
}
