library(cricscrapR)
library(glue)
library(readr)
library(tictoc)

#Unsure which season the script got to before breaking
fixture_vector <- c('https://www.espncricinfo.com/series/super-smash-2023-24-1409472/match-schedule-fixtures-and-results',
                    'https://www.espncricinfo.com/series/super-smash-2022-23-1341190/match-schedule-fixtures-and-results',
                    'https://www.espncricinfo.com/series/super-smash-2021-22-1289602/match-schedule-fixtures-and-results',
                    'https://www.espncricinfo.com/series/super-smash-2020-21-1233984/match-schedule-fixtures-and-results')
fixture_vector <- c('https://www.espncricinfo.com/series/super-smash-2019-20-1197604/match-schedule-fixtures-and-results',
                    'https://www.espncricinfo.com/series/super-smash-2018-19-1160494/match-schedule-fixtures-and-results',
                    'https://www.espncricinfo.com/series/super-smash-2017-18-1118532/match-schedule-fixtures-and-results',
                    'https://www.espncricinfo.com/series/super-smash-2016-17-1053263/match-schedule-fixtures-and-results')

tictoc::tic()
Sys.time()

df_bbb <- data.frame()
df_bbb <- map_dfr(fixture_vector, cric_t20_series_scrape)
#df_bbb <- cricscrapR::cric_t20_series_scrape('https://www.espncricinfo.com/series/icc-women-s-world-twenty20-2012-13-533235/match-schedule-fixtures-and-results')
df <- data.frame()
df <-cricscrapR::cric_apply_models(df_bbb)
df_games <- data.frame()
df_games <- cricscrapR::cric_t20_game_summary(df)
df_comp <- data.frame()
df_comp <- cricscrapR::cric_t20_comp_summary(df)

details <- data.frame()
details <- df |>
  slice_head(n=1) |>
  dplyr::select(competition_code, season, gender) |>
  mutate(season = as.integer(season))

#prev_bbb <- read_rds(glue("data/{details$competition_code}_{details$gender}_ball_by_ball.rds"))#
#df <- bind_rows(prev_bbb, df) |>#
#  distinct(match_id, season, id, overs_unique, .keep_all = TRUE)#

write_csv(df, glue("data/{details$competition_code}_{details$gender}_ball_by_ball.csv"))
saveRDS(df, glue("data/{details$competition_code}_{details$gender}_ball_by_ball.rds"))

#prev_games <- read_rds(glue("data/{details$competition_code}_{details$gender}_player_game_stats.rds"))#
#df_games <- bind_rows(prev_games, df_games) |>#
#  distinct(cricinfo_id, season, match_id, .keep_all = TRUE) #
write_csv(df_games, glue("data/{details$competition_code}_{details$gender}_player_game_stats.csv"))
saveRDS(df_games, glue("data/{details$competition_code}_{details$gender}_player_game_stats.rds"))

#prev_comp <- read_rds(glue("data/{details$competition_code}_{details$gender}_player_comp_stats.rds"))#
#df_comp <- bind_rows(prev_comp, df_comp) |>#
#  distinct(cricinfo_id, season, competition, .keep_all = TRUE)#
write_csv(df_comp, glue("data/{details$competition_code}_{details$gender}_player_comp_stats.csv"))
saveRDS(df_comp, glue("data/{details$competition_code}_{details$gender}_player_comp_stats.rds"))


prev_comp_codes <- read_csv("data/comp_codes.csv") |>
  mutate(season = as.integer(season))
comp_code_df <- data.frame()
comp_code_df <- bind_rows(prev_comp_codes,
                          details) |>
  distinct(competition_code, season, .keep_all = TRUE)
write_csv(comp_code_df, "data/comp_codes.csv")

prev_match_ids <- read_csv("data/prev_match_ids.csv")
unique_match_ids <- unique(df$match_id) |>
  as.data.frame()
colnames(unique_match_ids)[1] = "match_id"
unique_match_ids_df <- bind_rows(prev_match_ids, unique_match_ids) |>
  distinct(match_id, .keep_all = TRUE)
write_csv(unique_match_ids_df, "data/prev_match_ids.csv")

tictoc::toc()
