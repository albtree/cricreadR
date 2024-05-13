\name{load_bbb_data}
\alias{load_bbb_data}
\title{Load Ball by Ball by Competition & Season}
\usage{
load_bbb_data(comp_code = x, season = z)
}
\description{
Returns a dataframe that has ball by ball data by Competition and Season.
Prior to using this function users can use cricscrapR::competition_codes() to return a list of available competition codes and seasons.
}
\examples{
load_bbb_data(competition_code = "WSS", season = 2023)
# A tibble: 6,554 × 69
   match_id season type_clean competition_code competition  gender venue date       timestamp inning_number
      <int> <chr>  <chr>      <chr>            <chr>        <chr>  <chr> <date>     <lgl>             <dbl>
 1  1409506 2023   T20        WSS              Women's Sup… female Eden… 2023-12-19 NA                    1
 2  1409506 2023   T20        WSS              Women's Sup… female Eden… 2023-12-19 NA                    1
 3  1409506 2023   T20        WSS              Women's Sup… female Eden… 2023-12-19 NA                    1
 4  1409506 2023   T20        WSS              Women's Sup… female Eden… 2023-12-19 NA                    1
 5  1409506 2023   T20        WSS              Women's Sup… female Eden… 2023-12-19 NA                    1
 6  1409506 2023   T20        WSS              Women's Sup… female Eden… 2023-12-19 NA                    1
 7  1409506 2023   T20        WSS              Women's Sup… female Eden… 2023-12-19 NA                    1
 8  1409506 2023   T20        WSS              Women's Sup… female Eden… 2023-12-19 NA                    1
 9  1409506 2023   T20        WSS              Women's Sup… female Eden… 2023-12-19 NA                    1
10  1409506 2023   T20        WSS              Women's Sup… female Eden… 2023-12-19 NA                    1
# ℹ 6,544 more rows
# ℹ 59 more variables: id <int>, overs_unique <dbl>, overs_actual <dbl>, over_number <int>,
#   ball_number <int>, balls_remaining <int>, total_inning_runs <int>, run_rate <dbl>, target <int>,
#   total_inning_wickets <int>, bowl_team <chr>, bowl_team_id <int>, bowl_team_abbr <chr>,
#   bowler_cricinfo_id <int>, bowler_player_id <int>, bowler <chr>, bowler_wickets_cumulative <int>,
#   title <chr>, bat_team <chr>, bat_team_id <int>, bat_team_abbr <chr>, batter_cricinfo_id <int>,
#   batter_player_id <int>, batter <chr>, batter_runs_cumulative <int>, total_runs <int>, …
}
