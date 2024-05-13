
cric_t20_series_scrape <- function(x){
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(library(rvest))
  suppressPackageStartupMessages(library(jsonlite))
  suppressPackageStartupMessages(library(httr))
  suppressPackageStartupMessages(library(glue))

  fixture_url <- x
  fixture_match <- read_html(fixture_url, handle = curl::new_handle("useragent" = "Mozilla/5.0")) |>
    html_nodes('script') |>
    html_text()

  fixture_match <- purrr::pluck(fixture_match, -1)
  fixture_match_ids_df <- fromJSON(fixture_match, simplifyDataFrame = TRUE)
  fixture_match_ids <- pluck(fixture_match_ids_df, "props", "appPageProps", "data", "content", "matches",.default = NA) |>
    as.data.frame() |>
    dplyr::select(objectId, status)
  colnames(fixture_match_ids)[1] = "match_id"
  fixture_match_ids_series <- pluck(fixture_match_ids_df, "query") |> as.data.frame()
  fixture_match_ids <- bind_cols(fixture_match_ids, fixture_match_ids_series) |>
    filter(status == "RESULT")

  comp_code_df <- pluck(fixture_match_ids_df, "props", "appPageProps", "data", "series", "trophy", "shortName", .default = NA) |>
    as.data.frame()
  colnames(comp_code_df)[1] = "competition_code"
  comp_code_df$series_name <- pluck(fixture_match_ids_df, "props", "appPageProps", "data", "series", "name", .default = NA)
  comp_code_df <- comp_code_df |>
    mutate(comp_code_temp = str_extract_all(series_name, "[:upper:]"),
           comp_code_temp = sapply(comp_code_temp, paste0, collapse = ''),
           competition_code = coalesce(comp_code_temp, competition_code)) |>
    dplyr::select(-comp_code_temp)

  length_fixture <- length(fixture_match_ids$match_id)
  duration <- length_fixture*20
  print(glue('{length_fixture} matches to scrape at an average of 15-20 seconds means predicted run time = {duration} seconds. Have you checked if this competition ({comp_code_df$competition_code}) is available via the cric_readr function?'))

  seriesId <- fixture_match_ids |>
    slice_head(n =1) |>
    dplyr::select(seriesId) |>
    as.vector()

  bbb_data <- data.frame()
  match_data <- data.frame()
  innings <- data.frame()
  player_list_team_1_all <- data.frame()
  player_list_team_2_all <- data.frame()
  df <- data.frame()
  df_teams <- data.frame()
  df_custom_match_id <- data.frame()

  for(match_id in fixture_match_ids$match_id){
    for(inning_number in 1:2){
      for(from_over_number in 1:20){
        headers = c(
          `User-Agent` = 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:99.0) Gecko/20100101 Firefox/99.0')
        params = list('lang' = 'en',
                      'seriesId' = as.character(seriesId),
                      'matchId' = as.character(match_id),
                      'inningNumber' = as.character(inning_number),
                      'commentType' = 'ALL',
                      'sortDirection' = 'DESC',
                      'fromInningOver' = from_over_number)
        res <- httr::GET(url = 'https://hs-consumer-api.espncricinfo.com/v1/pages/match/comments', httr::add_headers(.headers=headers), query = params)

        x_url <- httr::content(res, as = "text")

        tryCatch({
          match_data_url <- str_c("https://www.espncricinfo.com/ci/engine/match/", match_id,".json")
          match_data_json <- read_html(match_data_url, handle = curl::new_handle("useragent" = "Mozilla/5.0")) |>
            html_text() |>
            parse_json(simplifyDataFrame = TRUE)

          match_data_df <-
            pluck(match_data_json, "series", .default = NA) |>
            as.data.frame() |>
            dplyr::select(trophy_abbreviation, series_name, season) |>
            rename(competition = series_name,
                   competition_code = trophy_abbreviation)
          match_data_df$venue <- pluck(match_data_json, "match", "ground_name", .default = NA)
          match_data_df$date <- pluck(match_data_json, "match", "start_date_raw", .default = NA)
          match_data_df$type <- pluck(match_data_json, "match", "general_class_card", .default = NA)
          match_data_df$match_id <- match_id
          match_data_df <- match_data_df |>
            mutate(type_clean = case_when(
              str_detect(competition_code, "100") ~ "The Hundred",
              str_detect(type, "20") ~ "T20"),
              balls_remaining_initial = case_when(type_clean == "The Hundred" ~ 100,
                                                  type_clean == "T20" ~ 120),
              balls_default_over = case_when(type_clean == "The Hundred" ~ 5,
                                             type_clean == "T20" ~ 6),
              date = as.Date(date)) |>
            dplyr::select(-type)

          innings_df <- pluck(match_data_json, "innings", .default = NA) |>
            as.data.frame() |>
            dplyr::select(innings_number, target) |>
            mutate(innings_number = as.integer(innings_number))
          innings_df$match_id <- match_id

          df_custom_match_id_df <- pluck(match_data_json, "match", .default = NA) |>
            as.data.frame() |>
            dplyr::select(team1_abbreviation, team2_abbreviation, start_date_raw) |>
            rename(custom_team1_abbr = team1_abbreviation,
                   custom_team2_abbr = team2_abbreviation)
          df_custom_match_id_df$match_id <- match_id


          player_list_team_1 <- pluck(match_data_json, "team", "player", 1) |>
            as.data.frame() |>
            dplyr::select(player_id, object_id, known_as)
          player_list_team_2 <- pluck(match_data_json, "team", "player", 2) |>
            as.data.frame() |>
            dplyr::select(player_id, object_id, known_as)

          df_teams_innings <- pluck(match_data_json, "match", .default = NA) |>
            as.data.frame() |>
            dplyr::select(team1_id, team1_name, team1_abbreviation)

          df_team_name_1 <-  pluck(match_data_json, "match", .default = NA) |>
            as.data.frame() |>
            dplyr::select(team1_id, team1_name, team1_abbreviation, batting_first_team_id) |>
            rename(team_id = team1_id,
                   team_name = team1_name,
                   team_abbreviation = team1_abbreviation) |>
            mutate(team_id = as.integer(team_id)) |>
            mutate(bat_innings = case_when(team_id == batting_first_team_id ~ 1,
                                           team_id != batting_first_team_id ~ 2),
                   bowl_innings = case_when(bat_innings == 1 ~ 2,
                                            bat_innings == 2 ~ 1,
                                            TRUE ~ bat_innings))
          df_team_name_1$match_id <- match_id


          df_team_name_2 <-  pluck(match_data_json, "match", .default = NA) |>
            as.data.frame() |>
            dplyr::select(team2_id, team2_name, team2_abbreviation, batting_first_team_id) |>
            rename(team_id = team2_id,
                   team_name = team2_name,
                   team_abbreviation = team2_abbreviation) |>
            mutate(team_id = as.integer(team_id),
                   bat_innings = case_when(team_id == batting_first_team_id ~ 1,
                                           team_id != batting_first_team_id ~ 2),
                   bowl_innings = case_when(bat_innings == 1 ~ 2,
                                            bat_innings == 2 ~ 1,
                                            TRUE ~ bat_innings))
          df_team_name_2$match_id <- match_id


          df_teams_df <- bind_rows(df_team_name_1,
                                     df_team_name_2)

          df_df <- x_url |>
            jsonlite::parse_json(simplifyDataFrame = TRUE) |>
            purrr::pluck("comments") |>
            unnest(predictions) |>
            dplyr::select(any_of(c("id", "inningNumber", "ballsActual", "ballsUnique",
                                   "oversUnique", "oversActual", "overNumber",
                                   "ballNumber", "bowlerPlayerId", "totalRuns", "batsmanRuns", "batsmanPlayerId",
                                   "isFour", "isSix", "isWicket", "dismissalType",
                                   "byes", "legbyes", "wides", "noballs", "penalties",
                                   "wagonX", "wagonY", "wagonZone",
                                   "pitchLine", "pitchLength",
                                   "shotType", "shotControl",
                                   "timestamp", "outPlayerId",
                                   "totalInningRuns", "totalInningWickets",
                                   "title", "dismissalText", "score", "winProbability")))
          df_df$match_id <- match_id

          player_list_team_1_all <- bind_rows(player_list_team_1_all, player_list_team_1)
          player_list_team_2_all <- bind_rows(player_list_team_2_all, player_list_team_2)
          df_teams <- bind_rows(df_teams, df_teams_df) |>
            distinct(match_id, team_id, bat_innings, bowl_innings, .keep_all = TRUE)

          innings <- bind_rows(innings, innings_df)
          match_data <- bind_rows(match_data, match_data_df)
          df_custom_match_id <- bind_rows(df_custom_match_id, df_custom_match_id_df)

          df <- bind_rows(df,df_df)
        }, error = function(msg){
          print("A scraped innings may not have completed all 20 overs")
        }
        )
      }}
  }



  match_data <- match_data |>
    distinct(competition_code, competition,
             season, venue, date,
             match_id, type_clean, balls_remaining_initial,
             balls_default_over) |>
    mutate(season = str_sub(season, 1, 4),
           comp_code_temp = str_extract_all(competition, "[:upper:]"),
           comp_code_temp = sapply(comp_code_temp, paste0, collapse = ''),
           competition_code = coalesce(comp_code_temp, competition_code))  |>
    dplyr::select(-comp_code_temp)

  innings <- innings |>
    distinct(match_id, innings_number, target) |>
    dplyr::select(match_id, innings_number, target) |>
    mutate(innings_number = as.integer(innings_number))


  df_custom_match_id <- df_custom_match_id |>
    mutate(custom_match_id = str_c(custom_team1_abbr, "_", custom_team2_abbr, "_", start_date_raw)) |>
    dplyr::select(match_id, custom_match_id)

  player_list <- bind_rows(player_list_team_1_all, player_list_team_2_all) |>
    distinct(player_id, object_id, known_as, .keep_all = TRUE) |>
    rename(cricinfo_id = object_id,
           player = known_as,
           player_id = player_id) |>
    mutate(player_id = as.integer(player_id))

  df <- df |>
    unpack(cols = c(dismissalText), names_sep = "_") |>
    left_join(player_list, by = c('bowlerPlayerId' = 'player_id'), na_matches = "never") |>
    rename(bowler = player, bowler_cricinfo_id = cricinfo_id) |>
    left_join(player_list, by = c('batsmanPlayerId' = 'player_id'), na_matches = "never") |>
    rename(batter = player, batter_cricinfo_id = cricinfo_id,
           batterPlayerId = batsmanPlayerId) |>
    left_join(player_list, by = c('outPlayerId' = 'player_id'), na_matches = "never") |>
    rename(out_player = player, out_player_cricinfo_id = cricinfo_id) |>
    left_join(df_teams, by = c('match_id' = 'match_id', 'inningNumber' = 'bat_innings')) |>
    rename(bat_team_id = team_id,
           bat_team = team_name,
           bat_team_abbr = team_abbreviation) |>
    dplyr::select(-bowl_innings) |>
    left_join(df_teams, by = c('match_id' = 'match_id', 'inningNumber' = 'bowl_innings')) |>
    rename(bowl_team_id = team_id,
           bowl_team = team_name,
           bowl_team_abbr = team_abbreviation) |>
    dplyr::select(-bat_innings) |>
    left_join(innings, by = c('match_id' = 'match_id', 'inningNumber' = 'innings_number')) |>
    left_join(match_data, by = "match_id") |>
    left_join(df_custom_match_id, by = "match_id") |>
    mutate(custom_match_id = str_c(competition_code, "_", custom_match_id))

  bbb_data <- df |>
    distinct(match_id, id, .keep_all = TRUE) |>
    arrange(id)


  bbb_data <- bbb_data |>
    mutate(overs_minus_1 = overNumber-1,
           expected_balls_passed = (overs_minus_1 * balls_default_over) + ballNumber,
           balls_remaining = balls_remaining_initial-expected_balls_passed,
           extras = noballs + byes + legbyes + wides) |>
    separate_wider_delim(oversActual, delim = ".", names = c('over', 'ball_number'), cols_remove = FALSE) |>
    mutate(ball_number = as.integer(ball_number),
           over = as.integer(over),
           ball_div_by_six = ball_number/6,
           run_rate = totalInningRuns/(over+ball_div_by_six),
           run_rate = round(run_rate, digits = 2),
           gender = case_when(str_detect(competition, "omen") ~ "female",
                              TRUE ~ "male")) |>
    rename(runs_off_bat = batsmanRuns) |>
    group_by(match_id, batter_cricinfo_id) |>
    arrange(id) |>
    mutate(batter_runs_cumulative = cumsum(runs_off_bat)) |>
    ungroup() |>
    group_by(match_id, bowler_cricinfo_id) |>
    arrange(id) |>
    mutate(bowler_wickets_cumulative = cumsum(isWicket)) |>
    ungroup() |>
    mutate(season = as.integer(season)) |>
    dplyr::select(match_id, custom_match_id, season, type_clean, competition_code, competition, gender, venue,
                  date, timestamp, inningNumber, id, oversUnique,
                  oversActual, overNumber, ballNumber, balls_remaining,
                  totalInningRuns, run_rate, target, totalInningWickets,
                  bowl_team, bowl_team_id, bowl_team_abbr,
                  bowler_cricinfo_id, bowlerPlayerId, bowler, bowler_wickets_cumulative,
                  title, bat_team, bat_team_id, bat_team_abbr,
                  batter_cricinfo_id, batterPlayerId, batter, batter_runs_cumulative,
                  totalRuns, runs_off_bat, isFour, isSix,
                  pitchLine, pitchLength, shotType, shotControl,
                  wagonX, wagonY, wagonZone,
                  byes, legbyes, wides, noballs, penalties, extras,
                  out_player_cricinfo_id, outPlayerId, out_player,
                  dismissalType, dismissalText_short,
                  dismissalText_long, dismissalText_commentary,
                  everything(), -expected_balls_passed,
                  -overs_minus_1,
                  -balls_default_over,
                  -balls_remaining_initial,
                  -ballsActual, -ballsUnique,
                  -over, -ball_number, -ball_div_by_six, -batting_first_team_id.x,
                  -batting_first_team_id.y) |>
    rename(inning_number = inningNumber,
           overs_unique = oversUnique,
           overs_actual = oversActual,
           over_number = overNumber,
           ball_number = ballNumber,
           total_inning_runs = totalInningRuns,
           total_inning_wickets = totalInningWickets,
           bowler_player_id = bowlerPlayerId,
           batter_player_id = batterPlayerId,
           total_runs = totalRuns,
           is_four = isFour,
           is_six = isSix,
           is_wicket = isWicket,
           pitch_line = pitchLine,
           pitch_length = pitchLength,
           shot_type = shotType,
           shot_control = shotControl,
           wagon_x = wagonX,
           wagon_y = wagonY,
           wagon_zone = wagonZone,
           out_player_id = outPlayerId,
           dismissal_type = dismissalType,
           dismissal_text_shot = dismissalText_short,
           dismissal_text_long = dismissalText_long,
           dismissal_text_commentary = dismissalText_commentary)

}
