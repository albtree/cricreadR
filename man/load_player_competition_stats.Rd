\name{load_player_competition_stats}
\alias{load_player_competition_stats}
\title{Load Player stats summarised by Competition & Season}
\usage{
load_player_competition_stats(comp_code = x, team = y, season = z, cricinfo_id = xxxx)
}
\description{
Returns a dataframe that has individual player statistics summarised by competition and season.
Prior to using this function users can use cricscrapR::competition_codes() to return a list of available competition codes and seasons.
}
\examples{
load_player_competition_stats(comp_code = "WCPL", team = "Barbados Royals Women", season = 2022) |> head()
# A tibble: 6 × 42
  player          cricinfo_id team   season competition total_impact total_impact_per_ball total_bat_impact
  <chr>                 <dbl> <chr>  <chr>  <chr>              <dbl>                 <dbl>            <dbl>
1 Aaliyah Alleyne      661441 Barba… 2022   Women's Ca…       -19.4                 -0.324          -19.4
2 Afy Fletcher         355351 Barba… 2022   Women's Ca…         9.69                 0.14           -11.2
3 Britney Cooper       424629 Barba… 2022   Women's Ca…       -14.1                 -1.18           -14.1
4 Chinelle Henry       474190 Barba… 2022   Women's Ca…       -23.7                 -0.226          -28.2
5 Chloe Tryon          453370 Barba… 2022   Women's Ca…         7.19                 0.104            2.19
6 Fatima Sana         1144886 Barba… 2022   Women's Ca…         3.09                 0.07             0.388
# ℹ 34 more variables: bat_impact_per_ball <dbl>, total_bowl_impact <dbl>, bowl_impact_per_ball <dbl>,
#   runs_for <dbl>, balls_faced <dbl>, boundaries <dbl>, dots_against <dbl>, sixes <dbl>,
#   non_boundary_balls_faced <dbl>, non_boundary_runs_gained <dbl>, wickets_lost <dbl>,
#   wickets_taken <dbl>, runs_against <dbl>, balls_bowled <dbl>, boundaries_against <dbl>,
#   dots_bowled <dbl>, bowl_economy <dbl>, bowl_average <dbl>, strike_rate <dbl>, average <dbl>,
#   balls_total <dbl>, boundary_perc_against <dbl>, boundary_percentage <dbl>, six_percentage <dbl>,
#   dots_bowled_percentage <dbl>, dots_batted_percentage <dbl>, non_boundary_strike_rate <dbl>, …
}
