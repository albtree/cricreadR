# cricreadR
<img src="https://github.com/albtree/cricreadR/assets/88771954/9d320f4f-79ca-4045-89d7-38abb083a931" width="250" height="250">

***August 15th 2025 Update: After a 10 month hiatus due to an ESPNCricInfo API permission change, cricreadR has re-commenced ingesting matches. Data should now be updating daily. Due to new costs involved, only select competitions will be accessible:
- Men's & Women's Big Bash League
- Men's & Womne's Indian Premier League
- Major League Cricket
- Men's & Women's Vitality Blast (Maybe :))
- Men's & Women's The Hundred
- Men's & Women's T20 World Cups

Furthermore, the ```cric_t20_series_scrape()``` function will be deprecated as due to API changes users will no longer be able to use the package to scrape their own series. At a later date, the ```cric_apply_models()``` function will also be deprecated as all readable data will have models pre-applied to it.
***

A package of functions to read in Women's and Men's T20 and The Hundred cricket ball by ball data, and player statistics. If competitions are unavailable, functions are able to scrape ball by ball data from ESPN cricinfo, apply Impact and Win Probability models to the data, and summarise by game or competition. At the current point in time this package does not support One Day (50 over) matches or Test matches.

To install package from github use following code:
```
# install.packages("pak")
pak::pak("albtree/cricreadR")
```

# Usage
There are two main styles of functions in `cricreadR` - the  first are for loading data directly from the repository - either in ball by ball format, or player statistics summarised by either games in a competition, or by the full competition. If a competition goes across two calendar years e.g. 2023/2024, then the earlier year is used as the official season. 
```
load_bbb_data(comp_code = "WBBL", season = 2023) 
# Returns Women's Big Bash League ball by ball data for the 2023 season.
load_player_competition_stats(comp_code = "IPL", season = 2023, team = "Sunrisers Hyderabad") 
# Returns player level competition level statistics for the Sunrisers Hyderabad team for the 2023 Indian Premier League season
load_player_game_stats(comp_code = "WSS", cricinfo_id = 803971) 
# Returns player level statistics for all games by the player represented by cricinfo_id 803971 (Amelia Kerr) in the Women's Super Smash League
competition_codes() 
# Returns a dataframe of all the competitions and seasons available in the data repository
load_team_logos()
# Returns a datafarme of team logos and colour codes
load_player_headshots()
# Returns a dataframe of player headshot URLs
```

The second set of functions allow users to scrape the URL of the Fixture and Results page of ESPN Cricinfo for ball by ball data, apply Impact, Win Probability and Expected Run models, and summarise by game or competition. In order to adhere to ethical data scraping, users are encouraged to check first in `competition_codes()` to see if the competition they're requesting is available, before resorting to using `cric_t20_series_scrape`.
```
cric_t20_series_scrape("https://www.espncricinfo.com/series/major-league-cricket-2023-1357742/match-schedule-fixtures-and-results")
# Scrapes ball by ball data for the included link. Takes ~20 seconds/match.
cric_apply_models()
# Applies models to freshly scraped ball by ball data
```
Once the `cric_apply_models()` has been applied to a scraped ball by ball data the user is then able to apply the appropriate summarising function to summarise either by games, or competition, for T20 competitions, or for The Hundred.
```
cric_t20_summarise_comp()
cric_t20_summarise_games()
cric_100_summarise_comp()
cric_100_summarise_games()
```

