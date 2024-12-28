if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}
pacman::p_load_current_gh("sportsdataverse/cfbfastR", dependencies = TRUE, update = TRUE)

usethis::edit_r_environ()   #Insert individual API Key

library(cfbfastR)

all_data <- list()

# Loop over years 2019 to 2023
for (year in 2019:2023) {
  
  # Loop over weeks 1 to 15
  for (week in 1:15) {
    
    # Collect data from the api into a dataframe
    df <- cfbd_pbp_data(
      year = year,
      season_type = "both",
      week = week,
      epa_wpa = TRUE,
    )
    
    # Store the data in the list using a unique key for year-week
    all_data[[paste(year, week, sep = "_")]] <- df
  }
}

# Convert data back to dataframe
final_data <- do.call(rbind, all_data)

write.csv(final_data, "cfb_playbyplay.csv", row.names = FALSE)
