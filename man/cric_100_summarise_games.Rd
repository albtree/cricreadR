\name{cric_100_summarise_games}
\alias{cric_100_summarise_games}
\title{Summarise Ball by Ball Data into Game totals for TheHundred Competitions}
\usage{
cric_100_summarise_games(x)
}
\description{
Summarises ball by ball data that has already had cric_apply_models() applied to it into a summary by player for each game within that Hundred dataset.
Impact is the predicted effect a player has had on their (batting), or their opponent's (if bowling) final team score.
Win Probability Added is the predicted Win Probability that each player added.
}
\examples{
df ## Dataframe that has previously been run through cric_female_100_models() or cric_male_100_models()
cric_100_summarise_games(df)
head(df)
}
