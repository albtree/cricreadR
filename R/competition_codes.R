
competition_codes <- function(){

competition_codes_df <- read.csv("data/comp_codes.csv") |>
  arrange(competition_code, -season)
  return(competition_codes_df)
}


