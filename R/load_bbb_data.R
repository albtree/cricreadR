## Adjust below code so it reads in from github repository though

load_bbb_data <- function(comp_code = NULL,
                                   season = NULL,
                                   ...) {
  suppressPackageStartupMessages(library(glue))
  # Check if at least one argument is provided
  if (is.null(comp_code)) {
    stop("At least the argument (competition_code) must be provided. Please run the function cricscrapR::competition_codes() for valid competition_codes")
  }

  details <- read.csv("https://github.com/albtree/cricreadR/raw/main/data/comp_codes.csv") |>
    filter(competition_code == comp_code) |>
    slice_head(n = 1)
  data <- readRDS(url(glue("https://github.com/albtree/cricreadR/raw/main/data/{details$competition_code}_{details$gender}_ball_by_ball.rds")))#

  # Create a logical condition based on provided arguments
  condition <- TRUE
  if (!is.null(season)) {
    condition <- condition & (data$season == season)
  }


  # Apply the filter condition
  filtered_data <- data |>
    filter(condition)

  return(filtered_data)
}


