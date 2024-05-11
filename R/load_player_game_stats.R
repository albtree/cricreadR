## Adjust below code so it reads in from github repository though

load_player_game_stats <- function(comp_code = NULL,
                                   team = NULL,
                                   season = NULL,
                                   cricinfo_id = NULL,
                                   ...) {
  suppressPackageStartupMessages(library(glue))
  # Check if at least one argument is provided
  if (is.null(comp_code)) {
    stop("At least the argument (competition_code) must be provided. Please run the function cricscrapR::competition_codes() for valid competition codes")
  }

  details_comp <- read.csv("data/comp_codes.csv") |>
    filter(competition_code == comp_code) |>
    slice_head(n = 1)
  data <- readRDS(glue("data/{details_comp$competition_code}_{details_comp$gender}_player_game_stats.rds"))#

  # Create a logical condition based on provided arguments
  condition <- TRUE
  if (!is.null(cricinfo_id)) {
    condition <- condition & (data$cricinfo_id == cricinfo_id)
  }
  if (!is.null(team)) {
    condition <- condition & (data$team == team)
  }
  if (!is.null(season)) {
    condition <- condition & (data$season == season)
  }


  # Apply the filter condition
  filtered_data <- data |>
    filter(condition)

  return(filtered_data)
}


