\name{cric_t20_comp_summary}
\alias{cric_t20_comp_summary}
\title{Summarise Ball by Ball Data into Twenty20 Competition totals}
\usage{
cric_t20_comp_summary(x)
}
\description{
Summarises ball by ball data that has already had cric_female_t20_models() or cric_male_t20_models() applied to it into a summary by player for that Twenty20 Competition.
Impact is the predicted effect a player has had on their (batting), or their opponent's (if bowling) final team score.
Win Probability Added is the predicted Win Probability that each player added.
}
\examples{
df ## Dataframe that has previously been run through cric_female_t20_models() or cric_male_t20_models()
cric_t20_comp_summary(df)
head(df)
# A tibble: 6 × 42
  player         cricinfo_id team  competition season runs_for wickets_lost balls_faced
  <chr>                <dbl> <chr> <chr>        <dbl>    <dbl>        <dbl>       <dbl>
1 Abdul Samad        1175485 Sunr… Indian Pre…   2024       15            1          11
2 Abhishek Shar…     1070183 Sunr… Indian Pre…   2024       95            2          42
3 Abishek Porel      1277545 Delh… Indian Pre…   2024       41            2          20
4 Aiden Markram       600498 Sunr… Indian Pre…   2024       60            1          46
5 Ajinkya Rahane      277916 Chen… Indian Pre…   2024       39            2          32
6 Alzarri Joseph      670031 Roya… Indian Pre…   2024        0            0           0
# ℹ 34 more variables: boundaries <dbl>, dots_against <dbl>, sixes <dbl>,
#   total_bat_impact <dbl>, non_boundary_balls_faced <dbl>,
#   non_boundary_runs_gained <dbl>, wickets_taken <dbl>, runs_against <dbl>,
#   balls_bowled <dbl>, boundaries_against <dbl>, dots_bowled <dbl>,
#   total_bowl_impact <dbl>, bowl_impact_per_ball <dbl>, bat_impact_per_ball <dbl>,
#   bowl_economy <dbl>, bowl_average <dbl>, strike_rate <dbl>, average <dbl>,
#   total_impact <dbl>, balls_total <dbl>, total_impact_per_ball <dbl>, …
}
