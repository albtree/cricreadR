\name{cric_apply_models}
\alias{cric_apply_models}
\title{Applies an Impact Model, Win Probability Model, and Expected Run models to scraped ball by ball data}
\usage{
cric_apply_models(x)
}
\description{
Applies Ball level Expected Run model, a ball level Expected Innings Total model, and a Win Probability Model to scraped ball by ball data for Twenty20 Matches.
Ball level Expected Run models & Win Probability Models are not applied to ball by ball data for The Hundred competition.
Impact is the predicted effect a player has had on their (batting), or their opponent's (if bowling) final team score.
Win Probability is the predicted probability a team will win at the conclusion of that ball.
Can apply to a previously scraped ball by ball dataframe
}
\examples{
df ## Previously scraped ball by ball dataframe
cric_apply_models(df)
df  |> dplyr::select(match_id, custom_match_id, inningNumber, overNumber, ballNumber, wp, xrun, exp_innings) |> head()
# A tibble: 6 × 8
  match_id custom_match_id                      inningNumber overNumber ballNumber    wp  xrun exp_innings
     <int> <chr>                                       <dbl>      <int>      <int> <dbl> <dbl>       <dbl>
1  1380589 Women's Caribbean Premier League 20…            1          1          1 0.532 0.651        142.
2  1380589 Women's Caribbean Premier League 20…            1          1          2 0.532 0.651        141.
3  1380589 Women's Caribbean Premier League 20…            1          1          3 0.532 0.651        140.
4  1380589 Women's Caribbean Premier League 20…            1          1          4 0.428 0.651        140.
5  1380589 Women's Caribbean Premier League 20…            1          1          5 0.420 0.638        139.
6  1380589 Women's Caribbean Premier League 20…            1          1          6 0.455 0.638        139.
}
