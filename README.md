# cricreadR
<img src="https://github.com/albtree/cricreadR/assets/88771954/74ab4059-3327-4180-8a39-e60ca18fecd3" width="250" height="250">

**under development**

A package of functions to read in Women's and Men's cricket ball by ball data, and player statistics. If competitions are unavailable, functions are able to scrape ball by ball data from ESPN cricinfo, apply Impact and Win Probability models to the data, and summarise by game or competition.

To install package from github use following code:
```
# install.packages("devtools")
devtools::install_github("albtree/cricreadR")
```

# Usage
There are two main styles of functions in `cricreadR` - those for loading data directly from the repository - either in ball by ball format, or player statistics summarised by either game in a competition, or by the full competition. If a competition goes across two calendar years e.g. 2023/2024, then the earlier year is used as the official season.
```
load_bbb_data(comp_code = "WBBL", season = 2023) # Returns Women's Big Bash League ball by ball data for the 2023 season.
load_player_competition_stats(comp_code = "IPL", season = 2023, team = "Sunrisers Hyderabad") # Returns player level competition level statistics for the Sunrisers Hyderabad team for the 2023 Indian Premier League season
load_player_game_stats(comp_code = "WSS", cricinfo_id = 803971) # Returns player level statistics for all games by the player represented by cricinfo_id 803971 (Amelia Kerr) in the Women's Super Smash League
competition_codes() # Returns a dataframe of all the competitions and seasons available in the data repository
```
