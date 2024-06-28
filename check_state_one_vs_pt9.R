library(ggplot2)
library(dplyr)

load(here::here("data", "R_data_files", "all_potw_permits.Rda"))

source(here::here("theme_tina.R"))

top_bin <- 10 ## should be a round number

design_flow_counts_one_decimal_state <- icis_permits2 %>%
  arrange(EXTERNAL_PERMIT_NMBR, VERSION_NMBR) %>%
  group_by(EXTERNAL_PERMIT_NMBR) %>%             ## keep only most recent permit
  slice(1) %>%                               ##
  ungroup() %>%
  mutate(state = substr(EXTERNAL_PERMIT_NMBR, 1, 2)) |>
  filter(design_flow_round_one_decimal %in% c(.9, 1)) |>
  group_by(state, design_flow_round_one_decimal) %>%
  count() |>
  ungroup() |>
  pivot_wider(names_from = design_flow_round_one_decimal, values_from = n,
              names_prefix = "flow_") |>
  mutate(flow_total = flow_0.9 + flow_1,
         flow_diff = flow_0.9 - flow_1,
         flow_diff_pct = flow_diff/flow_total) |>
  arrange(desc(flow_diff))
