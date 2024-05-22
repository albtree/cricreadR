
load_team_logos <- function(){

suppressPackageStartupMessages(library(tidyverse))

team_logos <- read.csv("https://raw.githubusercontent.com/albtree/cricket-headshots/main/team_info.csv")
  return(team_logos)
}


