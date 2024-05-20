
cric_apply_models <- function(x){
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(library(xgboost))

top_row <- x |>
  slice_head(n = 1)
  bbb <- x |>
    arrange(id) |>
    group_by(match_id, inning_number) |>
    mutate(delivery_no = row_number(),
           max_delivery_no = max(delivery_no)) |>
    ungroup() |>
    mutate(runs_gained = runs_off_bat+extras,
           balls_remaining = max_delivery_no - delivery_no,
           run_chase = target-total_inning_runs,
           boundary = if_else(runs_off_bat >= 4, 1, 0),
           dot = if_else(runs_gained == 0, 1, 0),
           six = if_else(runs_off_bat >= 6, 1, 0)) %>%
    group_by(match_id, inning_number) %>%
    arrange(delivery_no) %>%
    mutate(wickets_prior_to_ball = lag(total_inning_wickets),
           runs_prior_to_ball = lag(total_inning_runs),
           balls_prior_to_ball = lag(balls_remaining),
           run_rate_prior_to_ball = lag(run_rate),
           run_chase_prior_to_ball = lag(run_chase)) %>%
    ungroup() %>%
    mutate(wickets_prior_to_ball = replace_na(wickets_prior_to_ball, 0),
           runs_prior_to_ball = replace_na(runs_prior_to_ball, 0),
           run_rate_prior_to_ball = replace_na(run_rate_prior_to_ball, 0),
           run_chase_prior_to_ball = coalesce(run_chase_prior_to_ball, run_chase)) |>
    group_by(match_id, inning_number) |>
    fill(balls_prior_to_ball, .direction = "up") |>
    ungroup() |>
    arrange(match_id, inning_number, delivery_no)|>
    rename(wickets_lost_yet = total_inning_wickets,
           runs_scored_yet = total_inning_runs) |>
    mutate(phase = case_when(over_number <= 6 ~ "powerplay",
                             over_number >6 & over_number <= 16 ~ "middle",
                             over_number >= 15 ~ "death"),
           wides_or_noballs = noballs+wides,
           is_real_ball = case_when(wides_or_noballs < 1 ~ 1,
                                    TRUE ~ 0))

innings1 <- bbb %>%
    filter(inning_number == 1)

innings2 <- bbb %>%
    filter(inning_number == 2)

testing1 <- bbb %>%
    filter(inning_number == 1) %>%
    dplyr::select(balls_remaining, runs_scored_yet, wickets_lost_yet) %>%
    as.matrix()

testing1_xrun <- bbb %>%
    filter(inning_number == 1) %>%
    dplyr::select(wickets_prior_to_ball, runs_prior_to_ball, balls_prior_to_ball) %>%
    as.matrix()

testing2 <- bbb %>%
    filter(inning_number == 2) %>%
    dplyr::select(balls_remaining, runs_scored_yet, wickets_lost_yet, run_chase) %>%
    as.matrix()

testing2_xrun <- bbb %>%
    filter(inning_number == 2) %>%
    dplyr::select(wickets_prior_to_ball, runs_prior_to_ball, balls_prior_to_ball, run_chase_prior_to_ball) %>%
    as.matrix()


if (top_row$gender == "male" & top_row$type_clean == "T20")
{
  xrun_1 <- cricreadR:::male_1_t20_xrun
  xrun_2 <- cricreadR:::male_2_t20_xrun
  wpa_1 <- cricreadR:::male_1_t20_wpa
  wpa_2 <- cricreadR:::male_2_t20_wpa
  impact_1 <- cricreadR:::male_1_t20_impact
  impact_2 <- cricreadR:::male_2_t20_impact
}

  if (top_row$gender == "male" & top_row$type_clean == "The Hundred")
  {
    xrun_1 <- cricreadR:::male_1_t20_xrun
    xrun_2 <- cricreadR:::male_2_t20_xrun
    wpa_1 <- cricreadR:::male_1_t20_wpa
    wpa_2 <- cricreadR:::male_2_t20_wpa
    impact_1 <- cricreadR:::male_1_100_impact
    impact_2 <- cricreadR:::male_2_100_impact
  }

  if (top_row$gender == "female" & top_row$type_clean == "T20")
  {
    xrun_1 <- cricreadR:::female_1_t20_xrun
    xrun_2 <- cricreadR:::female_2_t20_xrun
    wpa_1 <- cricreadR:::female_1_t20_wpa
    wpa_2 <- cricreadR:::female_2_t20_wpa
    impact_1 <- cricreadR:::female_1_t20_impact
    impact_2 <- cricreadR:::female_2_t20_impact
  }

  if (top_row$gender == "female" & top_row$type_clean == "The Hundred")
  {
    xrun_1 <- cricreadR:::female_1_t20_xrun
    xrun_2 <- cricreadR:::female_2_t20_xrun
    wpa_1 <- cricreadR:::female_1_t20_wpa
    wpa_2 <- cricreadR:::female_2_t20_wpa
    impact_1 <- cricreadR:::female_1_100_impact
    impact_2 <- cricreadR:::female_2_100_impact
  }



  test_1 <- predict(wpa_1, testing1) %>% as.data.frame() %>% mutate_if(is.numeric, round, 4)
  innings1$wp <- test_1$.
  test_1_xrun <- predict(xrun_1, testing1_xrun) %>% as.data.frame() %>% mutate_if(is.numeric, round, 4)
  innings1$xrun <- test_1_xrun$.
  innings1$exp_innings <- predict(impact_1, innings1)


  test_2 <- predict(wpa_2, testing2) %>% as.data.frame() %>% mutate_if(is.numeric, round, 4)
  innings2$wp <- test_2$.
  test_2_xrun <- predict(xrun_2, testing2_xrun) %>% as.data.frame() %>% mutate_if(is.numeric, round, 4)
  innings2$xrun <- test_2_xrun$.
  innings2$exp_innings <- predict(impact_2, innings2)


  bbb_all <- bind_rows(innings1, innings2)


  if (top_row$type_clean == "The Hundred")
  {
    bbb_all <-  bbb_all |>
      dplyr::select(everything(), -wp, -xrun)
  }

  wpa_df <- bbb_all |>
    arrange(match_id, inning_number, delivery_no) |>
    rename(total_inning_wickets = wickets_lost_yet,
           total_inning_runs = runs_scored_yet) |>
    dplyr::select(-runs_gained, -run_chase, -wickets_prior_to_ball, -runs_prior_to_ball,
                  -balls_prior_to_ball, -run_chase_prior_to_ball, -run_rate_prior_to_ball, -max_delivery_no, -wides_or_noballs)


}

