
competition_codes <- function(){

suppressPackageStartupMessages(library(tidyverse))

competition_codes_df <- read.csv("https://github.com/albtree/cricreadR/raw/main/data/comp_codes.csv") |>
  arrange(competition_code, -season)
  return(competition_codes_df)
}


