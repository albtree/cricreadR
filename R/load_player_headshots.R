
load_player_headshots <- function(){

player_headshots <- readRDS(url("https://github.com/albtree/cricket-headshots/raw/main/headshot_url_data_cleaned.rda"))
  return(player_headshots)
}


