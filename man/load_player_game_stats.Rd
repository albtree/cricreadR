\name{load_player_game_stats}
\alias{load_player_game_stats}
\title{Load Player stats summarised by Competition & Season}
\usage{
load_player_game_stats(comp_code = x, team = y, season = z, cricinfo_id = xxxx)
}
\description{
Returns a dataframe that has individual player statistics summarised by game within a competition.
Prior to using this function users can use cricscrapR::competition_codes() to return a list of available competition codes and seasons.
}
\examples{
load_player_game_stats(comp_code = "WBBL", season = 2023) |> arrange(-total_impact)
# A tibble: 1,198 × 44
   player  cricinfo_id team  season competition match_id custom_match_id total_impact total_impact_per_ball
   <chr>         <dbl> <chr> <chr>  <chr>          <dbl> <chr>                  <dbl>                 <dbl>
 1 Grace …      381268 Bris… 2023   Women's Bi…  1387176 NA                      80.0                 1.08
 2 Lizell…      483484 Hoba… 2023   Women's Bi…  1387222 NA                      55.3                 0.987
 3 Sophie…      231740 Pert… 2023   Women's Bi…  1387174 NA                      47.4                 0.817
 4 Beth M…      381258 Pert… 2023   Women's Bi…  1387207 NA                      47.4                 0.752
 5 Sophie…      231740 Pert… 2023   Women's Bi…  1387202 NA                      45.4                 0.547
 6 Beth M…      381258 Pert… 2023   Women's Bi…  1387212 NA                      43.1                 0.719
 7 Lizell…      483484 Hoba… 2023   Women's Bi…  1387188 NA                      41.1                 0.761
 8 Katie …      678831 Adel… 2023   Women's Bi…  1387175 NA                      40.9                 0.786
 9 Chamar…      318853 Sydn… 2023   Women's Bi…  1387182 NA                      38.0                 0.469
10 Chamar…      318853 Sydn… 2023   Women's Bi…  1387194 NA                      35.0                 0.637
# ℹ 1,188 more rows
# ℹ 35 more variables: total_bat_impact <dbl>, bat_impact_per_ball <dbl>, total_bowl_impact <dbl>,
#   bowl_impact_per_ball <dbl>, runs_for <dbl>, balls_faced <dbl>, boundaries <dbl>, dots_against <dbl>,
#   sixes <dbl>, non_boundary_balls_faced <dbl>, non_boundary_runs_gained <dbl>, wickets_lost <dbl>,
#   wickets_taken <dbl>, runs_against <dbl>, balls_bowled <dbl>, boundaries_against <dbl>,
#   dots_bowled <dbl>, bowl_economy <dbl>, bowl_average <dbl>, strike_rate <dbl>, average <dbl>,
#   balls_total <dbl>, boundary_perc_against <dbl>, boundary_percentage <dbl>, six_percentage <dbl>, …
# ℹ Use `print(n = ...)` to see more rows
}
