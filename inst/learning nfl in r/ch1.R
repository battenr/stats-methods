library(nflreadr)


# 1.3 Unique Offensive Line Combinations ----

participation <- nflreadr::load_participation(2022, include_pbp = TRUE)
rosters <- nflreadr::load_rosters(2022) %>%
  select(full_name, gsis_id, depth_chart_position)

oline_participation <- participation %>%
  filter(play_type %in% c("pass", "run")) %>%
  group_by(nflverse_game_id, possession_team, fixed_drive) %>%
  filter(fixed_drive == 1 | fixed_drive == 2) %>%
  filter(row_number() == 1) %>%
  select(nflverse_game_id, play_id, possession_team, 
         offense_personnel, offense_players) %>%
  dplyr::mutate(gsis_id = stringr::str_split(offense_players, ";")) %>%
  tidyr::unnest(c(gsis_id)) %>%
  left_join(rosters, by = c("gsis_id" = "gsis_id"))

oline_participation <- oline_participation %>%
  filter(depth_chart_position %in% c("T", "G", "C")) %>%
  group_by(nflverse_game_id, possession_team) %>%
  mutate(starting_line = toString(full_name)) %>%
  select(nflverse_game_id, possession_team, 
         offense_personnel, starting_line) %>%
  distinct()