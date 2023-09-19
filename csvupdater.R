pbp <- load_participation(2021:2023, include_pbp = TRUE)

pbpj <- pbp |> 
  select(season, week, posteam, defteam, offense_formation, offense_personnel,
         defenders_in_box, defense_personnel, number_of_pass_rushers, shotgun, half_seconds_remaining,
         yardline_100, pass_oe, ep, epa, down, ydstogo, yards_gained, receiver_player_id, receiving_yards, rushing_yards,
         rusher_player_id, rush_attempt, rush_touchdown, qb_scramble, complete_pass, yards_after_catch, rush, pass,
         air_yards, success, qb_kneel, pass_touchdown, wp, run_location, run_gap, sack, pass_location, cpoe, offense_players,
         defense_players)

write.csv(pbpj, "shinypbp.csv")

pbp4th <- nfl4th::load_4th_pbp(2021:2023)
pbp4th <- pbp4th |> 
  filter(!is.na(go_boost))

pbp4th <- pbp4th |> 
  select(season, posteam, week, go_boost, go, vegas_wp)
write.csv(pbp4th, "pbp4th.csv")

current <- fourth %>%
  filter(go_boost > 1.5, !is.na(go_boost), !is.na(go)) %>%
  filter(vegas_wp > .05) %>%
  group_by(posteam, season) %>%
  summarize(go = mean(go)/100) |> 
  ungroup() %>% 
  pivot_wider(names_from = season, values_from = go) |> 
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))





