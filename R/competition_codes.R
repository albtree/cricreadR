
competition_codes <- function(){
<<<<<<< Updated upstream
suppressPackageStartupMessages(library(tidyverse))
competition_codes_df <- read.csv("data/comp_codes.csv") |>
=======
  suppressPackageStartupMessages(library(tidyverse))

competition_codes_df <- read.csv("https://github.com/albtree/cricreadR/raw/main/data/comp_codes.csv") |>
>>>>>>> Stashed changes
  arrange(competition_code, -season)
  return(competition_codes_df)
}


