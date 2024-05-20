
cric_100_summarise_comp <- function(x){
 suppressPackageStartupMessages(library(tidyverse))

  bbb_df <- x %>%
  group_by(match_id, inning_number) %>%
  mutate(impact_prev = lag(exp_innings)) %>%
  ungroup() %>%
  mutate(bat_impact = exp_innings-impact_prev,
         bat_impact = coalesce(bat_impact, total_runs)) %>%
  mutate(bowl_impact = -bat_impact)

batters_nbsr <- bbb_df %>%
  filter(boundary == 0) %>%
  group_by(batter, batter_cricinfo_id, bat_team, competition, season) %>%
  summarise(non_boundary_balls_faced = n(),
            non_boundary_runs_gained = sum(runs_off_bat, na.rm = TRUE)) %>%
  ungroup()

batters_wickets <- bbb_df |>
  group_by(out_player, out_player_cricinfo_id, bat_team, competition, season) |>
  summarise(wickets_lost = sum(is_wicket, na.rm = TRUE)) |>
  ungroup()

batters_df <- bbb_df %>%
  group_by(batter, batter_cricinfo_id, bat_team, competition, season) %>%
  summarise(runs_for = sum(runs_off_bat, na.rm = TRUE),
            #wickets_lost = sum(wicket, na.rm = TRUE),
            balls_faced= sum(is_real_ball),
            boundaries = sum(boundary),
            dots_against = sum(dot),
            sixes = sum(six),
            total_bat_impact = sum(bat_impact, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(batters_nbsr, by = c('batter', 'batter_cricinfo_id', 'bat_team', 'competition', 'season'), na_matches = "never") |>
  left_join(batters_wickets, by = c('batter' = 'out_player', 'batter_cricinfo_id' = 'out_player_cricinfo_id', 'bat_team', 'competition', 'season'), na_matches = "never")


bowlers_df <- bbb_df %>%
  group_by(bowler, bowler_cricinfo_id, bowl_team, competition, season) %>%
  summarise(wickets_taken = sum(is_wicket, na.rm = TRUE),
            runs_against = sum(total_runs, na.rm = TRUE),
            balls_bowled= n(),
            boundaries_against = sum(boundary),
            dots_bowled = sum(dot),
            total_bowl_impact = sum(bowl_impact, na.rm = TRUE)) %>%
  ungroup()
bat_only <- batters_df %>%
  rename(player = batter,
         cricinfo_id = batter_cricinfo_id,
         team = bat_team)

bowl_only <- bowlers_df %>%
  rename(player = bowler,
         cricinfo_id = bowler_cricinfo_id,
         team = bowl_team)

logos_and_colours <- read.csv("https://raw.githubusercontent.com/albtree/cricket-headshots/main/team_info.csv")
competitions <- read.csv("https://raw.githubusercontent.com/albtree/cricket-headshots/main/cricket_competitions.csv")
headshots <- read.csv("https://raw.githubusercontent.com/albtree/cricket-headshots/main/headshot_url_data_cleaned.csv")|>
  mutate(cricinfo_id = as.integer(cricinfo_id))


df_both <- bat_only %>%
  full_join(bowl_only, by = c('player' = 'player', 'cricinfo_id' = 'cricinfo_id', 'team' = 'team',
                                  'competition' = 'competition', 'season' = 'season'), na_matches = "never")%>%
  mutate(total_bat_impact = replace_na(total_bat_impact, 0),
         total_bowl_impact = replace_na(total_bowl_impact,0),
         balls_bowled = replace_na(balls_bowled, 0),
         balls_faced = replace_na(balls_faced, 0)) %>%
  mutate_if(is.integer, as.numeric) %>%
  #group_by(player, cricinfo_id, season) %>%
  #mutate(team = unique(team[!is.na(team)])) %>%
  #replace(., is.na(.),0)%>%
  #ungroup()%>%
  group_by(player, cricinfo_id, team, competition, season) %>%
  summarise(across(c(1:15), sum)) %>% #was 1:17 when XRA included, rows 1:15 without XRA
  ungroup() %>%
  mutate(bowl_impact_per_ball = (total_bowl_impact/balls_bowled),
         bowl_impact_per_ball = replace_na(bowl_impact_per_ball,0),
         bat_impact_per_ball = (total_bat_impact/balls_faced),
         bat_impact_per_ball = replace_na(bat_impact_per_ball,0),
         bowl_economy = runs_against/balls_bowled,
         bowl_average = runs_against/wickets_taken,
         strike_rate = (runs_for/balls_faced),
         average = runs_for/wickets_lost,
         total_impact = total_bat_impact+total_bowl_impact,
         balls_total = balls_bowled+balls_faced,
         total_impact_per_ball = total_impact/balls_total,
         boundary_perc_against = boundaries_against/balls_bowled,
         boundary_percentage = boundaries/balls_faced,
         six_percentage = sixes/balls_faced,
         dots_bowled_percentage = dots_bowled/balls_bowled,
         dots_batted_percentage = dots_against/balls_faced,
         non_boundary_strike_rate = (non_boundary_runs_gained/non_boundary_balls_faced)) |>
  mutate_if(is.numeric, round, 3)%>%
  left_join(logos_and_colours, by = c('team' = 'team',
                                      'competition' = 'league'), na_matches = "never") |>
  left_join(competitions, by = c('competition'), na_matches = "never") |>
  left_join(headshots, by = c('cricinfo_id' = 'cricinfo_id'), na_matches = "never") |>
  dplyr::select(player, cricinfo_id,
                team, season, competition, total_impact, total_impact_per_ball, total_bat_impact,
                bat_impact_per_ball, total_bowl_impact, bowl_impact_per_ball, everything())
}

