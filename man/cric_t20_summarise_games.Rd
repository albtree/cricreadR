\name{cric_t20_game_summary}
\alias{cric_t20_game_summary}
\title{Summarise Ball by Ball Data into Game totals for Twenty20 Competitions}
\usage{
cric_t20_game_summary(x)
}
\description{
Summarises ball by ball data that has already had cric_female_t20_models() or cric_male_t20_models() applied to it into a summary by player for each game within that dataset.
Impact is the predicted effect a player has had on their (batting), or their opponent's (if bowling) final team score.
Win Probability Added is the predicted Win Probability that each player added.
}
\examples{
df ## Dataframe that has previously been run through cric_female_t20_models() or cric_male_t20_models()
cric_t20_game_summary(df)
head(df)
# A tibble: 6 × 42
  player        cricinfo_id team  competition match_id runs_for wickets_lost balls_faced
  <chr>               <dbl> <chr> <chr>          <dbl>    <dbl>        <dbl>       <dbl>
1 Abdul Samad       1175485 Sunr… Indian Pre…  1422121       15            1          11
2 Abhishek Sha…     1070183 Sunr… Indian Pre…  1422121       32            1          19
3 Abhishek Sha…     1070183 Sunr… Indian Pre…  1422126       63            1          23
4 Abishek Porel     1277545 Delh… Indian Pre…  1422120       32            1          10
5 Abishek Porel     1277545 Delh… Indian Pre…  1422127        9            1          10
6 Aiden Markram      600498 Sunr… Indian Pre…  1422121       18            1          13
# ℹ 34 more variables: boundaries <dbl>, dots_against <dbl>, sixes <dbl>,
#   total_bat_impact <dbl>, non_boundary_balls_faced <dbl>,
#   non_boundary_runs_gained <dbl>, wickets_taken <dbl>, runs_against <dbl>,
#   balls_bowled <dbl>, boundaries_against <dbl>, dots_bowled <dbl>,
#   total_bowl_impact <dbl>, bowl_impact_per_ball <dbl>, bat_impact_per_ball <dbl>,
#   bowl_economy <dbl>, bowl_average <dbl>, strike_rate <dbl>, average <dbl>,
#   total_impact <dbl>, balls_total <dbl>, total_impact_per_ball <dbl>, …
}
