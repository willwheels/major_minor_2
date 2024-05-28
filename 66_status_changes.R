library(tidytable)
library(ggplot2)

load(here::here("data", "R_data_files", "all_potw_permits.Rda"))

icis_permits_changes <- icis_permits2 |>
  mutate(MAJOR_MINOR_STATUS_FLAG = if_else(MAJOR_MINOR_STATUS_FLAG == "", "N", "MAJOR_MINOR_STATUS_FLAG"),
         VERSION_NMBR = if_else(VERSION_NMBR == 0, 10, VERSION_NMBR)) |>
  arrange(EXTERNAL_PERMIT_NMBR, VERSION_NMBR) |>
  group_by(EXTERNAL_PERMIT_NMBR) |>
  mutate(status_change = !MAJOR_MINOR_STATUS_FLAG == lag(MAJOR_MINOR_STATUS_FLAG)) |>
  summarise(num_versions = n(), num_status_changes = sum(status_change, na.rm = TRUE)) |>
  ungroup()

summary(icis_permits_changes)

ggplot(icis_permits_changes, aes(x = num_versions)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 12, by = 2)) +
  theme_minimal()

icis_permits_changes |>
  filter(num_versions %in% c(2, 3, 4, 5)) |>
  select(EXTERNAL_PERMIT_NMBR, num_versions, num_status_changes) |>
  ggplot(aes(x = num_status_changes)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 12, by = 2)) +
  theme_minimal() +
  facet_wrap(~ num_versions)

icis_permits_changes |>
  group_by(num_versions) |>
  summarise(mean_changes = mean(num_status_changes))
            