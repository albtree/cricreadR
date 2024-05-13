\name{cric_t20_series_scrape}
\alias{cric_t20_series_scrape}
\title{Cricket Twenty20 Ball by Ball Scraper - For a whole series/Competition}
\usage{
cric_t20_bbb(x)
}
\description{
Scrapes all games on a ESPN Cricinfo Twenty20 Fixture page and wrangles data into a ball by ball data frame.
Does not scrape games with the status "ABANDONED" or "NO RESULT".
Each game takes ~15-20 seconds to scrape
}
\examples{
df <- cric_t20_series_scrape("https://www.espncricinfo.com/series/major-league-cricket-2023-1357742/match-schedule-fixtures-and-results")
head(df)
# A tibble: 6 × 60
  match_id season type_clean competition_code competition   gender venue date       timestamp inningNumber
     <int> <chr>  <chr>      <chr>            <chr>         <chr>  <chr> <date>     <chr>            <dbl>
1  1381361 2023   T20        MLC              Major League… male   Gran… 2023-07-13 2023-07-…            1
2  1381361 2023   T20        MLC              Major League… male   Gran… 2023-07-13 2023-07-…            1
3  1381361 2023   T20        MLC              Major League… male   Gran… 2023-07-13 2023-07-…            1
4  1381361 2023   T20        MLC              Major League… male   Gran… 2023-07-13 2023-07-…            1
5  1381361 2023   T20        MLC              Major League… male   Gran… 2023-07-13 2023-07-…            1
6  1381361 2023   T20        MLC              Major League… male   Gran… 2023-07-13 2023-07-…            1
# ℹ 50 more variables: id <int>, oversUnique <dbl>, oversActual <dbl>, overNumber <int>,
#   ballNumber <int>, balls_remaining <dbl>, totalInningRuns <int>, run_rate <dbl>, target <int>,
#   totalInningWickets <int>, bowl_team <chr>, bowl_team_id <int>, bowl_team_abbr <chr>,
#   bowler_cricinfo_id <int>, bowlerPlayerId <int>, bowler <chr>, bowler_wickets_cumulative <int>,
#   title <chr>, bat_team <chr>, bat_team_id <int>, bat_team_abbr <chr>, batter_cricinfo_id <int>,
#   batterPlayerId <int>, batter <chr>, batter_runs_cumulative <int>, totalRuns <int>,
#   runs_off_bat <int>, isFour <lgl>, isSix <lgl>, pitchLine <chr>, pitchLength <chr>, shotType <chr>, …
}

