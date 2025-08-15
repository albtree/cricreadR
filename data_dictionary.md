# Data Dictionary - ball by ball data

`match_id`: ESPN Cricinfo match id

`custom_match_id`: cricreadR match id

`season`: Year in which the competition took place. If the competition crossed over two years eg 2023/2024 then the season will be labelled as the earlier year 

`type_clean`: Twenty20 or The Hundred

`competition_code`: Competition code eg IPL for Indian Premier League

`competition`: Competition name

`gender`: Female or Male

`venue`: Venue

`date`: Date of match

`timestamp`: Timestamp of play in UTC time

`inning_number`: Inning Number: 1-2

`id`: Unique play ID

`overs_unique`: True over and ball of the innings taking into account no balls and wides in that over e.g. 4.07 = 4 overs and 7 balls have passed.

`overs_actual`: Actual over and ball of the innings according to the official scorecard. Balls are capped at 6 per over. If a no ball or wide then that ball is replayed e.g. No ball on 3.1 will mean the next ball is also labelled 3.1

`over_number`: Current over of the innings

`ball_number`: Current ball of the over taking into account no balls and wides in that over e.g. Ball number 7 in that over indicates the 7th ball bowled in the over.

`balls_remaining`: Known balls remaining at that point in time. Does not account for future wides or no balls.

`total_inning_runs`: Cumulative of inning runs up to that ball

`run_rate`: Run rate up to that ball

`target`: Target total to win if in Innings 2

`total_inning_wickets`: Cumulative number of wickets so far in innings up to that ball

`bowl_team`: Bowling team

`bowl_team_id`: Bowling team ID

`bowl_team_abbr`: Bowling team abbreviation 

`bowler_cricinfo_id`: Bowler ESPN Cricinfo ID

`bowler_player_id`: Bowler Player ID

`bowler`: Bowler name

`bowling_hand`: Typical bowler hand - right, left

`bowling_pacespin`: Whether the bowler is a pace or spin bowler. Two options: `pace bowler` or `spin bowler`

`bowling_style_long`: Detailed description of bowling style

`bowler_wickets_cumulative`: Cumulative wickets of bowler up to that point of the innings

`title`: Label of which bowler to which batter

`bat_team`: Batting team

`bat_team_id`: Batting team ID

`bat_team_abbr`: Batting team abbreviation

`batter_cricinfo_id`: Batter ESPN Cricinfo ID

`batter_player_id`: Batter Player ID

`batter`: Batter name

`batting_style`: Batting style - `rhb` or `lhb` indicating 'Right hand batter' or 'Left hand batter'

`batter_runs_cumulative`: Cumulative runs of batter up to that point of the innings

`total_runs`: Total runs from that ball - includes runs off the bat and extra runs via sundries

`runs_off_bat`: Runs off the bat

`is_four`: A four was hit

`is_six`: A six was hit

`pitch_line`: Pitch line of bowl

`pitch_length`: Pitch lengh of bowl

`shot_type`: Shot type by batter

`shot_control`: Defined by ESPN Cricinfo as "Did the ball go where the batter intended it to go?" 1 = in control (true shot). 2 = not in control (false shot)

`wagon_x`: Incomplete field. (definition to be confirmed). X co-ordinates of the where the ball crosses the boundary or is fielded. If `0` then field has not been entered.

`wagon_y`: Incomplete field. Y co-ordinates of the where the ball crosses the boundary or is fielded. If `0` then field has not been entered.

`wagon_zone`: Incomplete field. `1-8` octant zone of where the ball is hit to. Starting at `1` in the top right octant progressing clockwise to `8` in the top left octant.

`byes`: Runs from byes on the ball.

`legbyes`: Runs from leg byes on the ball.

`wides`: Runs from wides on the ball.

`noballs`: Runs from no balls on the ball.

`penalties`: Runs from penalties on the ball.

`extras`: Total amount of runs from extras (`byes`, `legbyes`, `wides`, `noballs`, `penalties`) on the ball

`out_player_cricinfo_id`: Dismissed player ESPN Cricinfo ID 

`out_player_id`: Dismissed Player ID

`out_player`: Dismissed Player name

`dismissal_type`: Type of dismissal in integer form

`dismissal_text_shot`: Type of dismissal short desciption

`dismissal_text_long`: Type of dismissal long description

`dismissal_text_commentary`: Commentary of dismissal

`is_wicket`: A wicket occurred

`score`: ESPNCricInfo’s expected innings total

`winProbability`: ESPNCricInfo’s win probability model prediction

`delivery_no`: Delivery number of the Inning

`boundary`: A boundary was hit

`dot`: A dot ball was bowled

`six`: A six was hit

`phase`: Phase of the Inning: `powerplay`, `middle`, or `death` overs

`wp`: cricreadR’s win probability model prediction

`xrun`: cricreadR’s expected runs for that ball

`exp_innings`: cricreadR’s expected innings total

`is_real_ball`: Not a wide or no ball
