
library(ggplot2)
library(dplyr)

load(here::here("data", "R_data_files", "all_potw_permits.Rda"))
load(here::here("data", "R_data_files", "all_potw_permits_most_recent.Rda"))


ggplot(icis_permits2 %>% filter(design_flow_round <= 5, design_flow_round > 0), 
       aes(x = design_flow_round)) +
  geom_histogram(binwidth = .1) + 
  annotate("rect", xmin = .7, xmax = 1.2, ymin = 250, ymax = 450, alpha = .2, color = "red") +
  annotate("text", x = 1.5, y = 1500, label = "1 MGD") +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(title = "Histogram of POTW Flows") +
  xlab("Design Flow (rounded to single decimal)") + ylab("Count") +
  theme_minimal()

ggsave("design_flow_hist.png", path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")



ggplot(icis_permits2 %>% filter(design_flow_round_smaller >= .8, design_flow_round_small <= 1.2), 
       aes(x = design_flow_round_small)) +
  geom_histogram(binwidth = .01) +
  
  theme_minimal()

ggplot(icis_permits2 %>% filter(design_flow_round_small >= .8, design_flow_round_small <= 1.2), 
       aes(x = design_flow_round_small)) +
  geom_histogram(binwidth = .001) + 
  theme_minimal()

design_flow_counts <- icis_permits2 %>%
  group_by(design_flow_round_small) %>%
  count()

ggplot(icis_permits2 %>% filter(flow_over_design < 1, flow_over_design > -1), 
       aes(x = flow_over_design)) +
  geom_histogram(binwidth = .05) + 
  theme_minimal()


ggplot(icis_permits2 %>% filter(flow_over_design < 1, flow_over_design > -1, 
                                design_flow_round >=.9, design_flow_round <= 1.1), 
       aes(x = flow_over_design)) +
  geom_histogram(binwidth = .05) + 
  theme_minimal() +
  facet_wrap(~design_flow_round)

ggplot(icis_permits2 %>% filter(flow_over_design < 5, flow_over_design > -5, TOTAL_DESIGN_FLOW_NMBR <= 2),
       aes(x = TOTAL_DESIGN_FLOW_NMBR, y = flow_over_design)) +
  geom_point() + 
  theme_minimal()


## CORRECT TO ACTUAL SNC CODES

qncr <- fread(here::here("data", "NPDES_QNCR_HISTORY.csv"))


qncr2 <- qncr %>%
  filter(YEARQTR > 20180) %>%
  mutate(year = as.numeric(substr(as.character(YEARQTR), 1, 4)),
         quarter = as.numeric(substr(as.character(YEARQTR), 5, 5)),
         snc_cat_1 = (HLRNC %in% c("D", "E", "S", "T", "X")*1)) %>%
  group_by(NPDES_ID) %>%
  summarise(num_snc = sum(snc_cat_1), num_e90 = sum(NUME90Q))

summary(qncr2)


npdes_inspections <- fread(here::here("data", "NPDES_INSPECTIONS.csv"),
                           select = c("NPDES_ID", "ACTIVITY_ID",
                                      "COMP_MONITOR_TYPE_CODE", "COMP_MONITOR_TYPE_DESC", 
                                      "ACTUAL_BEGIN_DATE", "ACTUAL_END_DATE"))


## I could use begin date to try to fill in end date if the latter is missing, which it rarely is

npdes_inspections <- npdes_inspections %>%
  mutate(end_date = lubridate::mdy(ACTUAL_END_DATE)) %>%
  filter(end_date >= lubridate::mdy("01-01-2018")) %>%
  distinct() %>%  ## eliminate multiple entries on the same day
  group_by(NPDES_ID) %>%
  summarise(num_inspections = n(), num_CEI = sum(COMP_MONITOR_TYPE_CODE == "CEI"))

summary(npdes_inspections)

icis_permits2 <- icis_permits2 %>%
  left_join(qncr2, by = c("EXTERNAL_PERMIT_NMBR" = "NPDES_ID")) %>%
  left_join(npdes_inspections, by = c("EXTERNAL_PERMIT_NMBR" = "NPDES_ID"))


icis_permits3 <- icis_permits2 %>%
  filter(TOTAL_DESIGN_FLOW_NMBR <= 2) %>%
  group_by(design_flow_round) %>%
  summarise(mean_snc = mean(num_snc, na.rm = TRUE), 
            mean_e90 = mean(num_e90, na.rm = TRUE),
            mean_num_limits = mean(num_limits, na.rm = TRUE),
            mean_inspections = mean(num_inspections, na.rm = TRUE),
            mean_CEI = mean(num_CEI, na.rm = TRUE),
            num_in_bin = n()) %>%
  mutate(e90_rate = mean_e90/mean_num_limits) %>%
  arrange(desc(design_flow_round)) %>%
  ungroup()


ggplot(icis_permits3, aes(x = design_flow_round, y = mean_snc)) +
  geom_line() +
  labs(title = "Mean Number of Quarters in SNC Since 2018 by Design Flow") +
  ylab("Mean Quarters in SNC") + xlab("Design Flow (rounded to single decimal)") +
  theme_minimal()

ggsave("snc_by_design_flow.png", path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")

ggplot(icis_permits3, aes(x = design_flow_round, y = mean_snc, size = num_in_bin)) +
  geom_point() +
  labs(title = "Mean Number of Quarters in SNC Since 2018 by Design Flow") +
  ylab("Mean Quarters in SNC") + xlab("Design Flow (rounded to single decimal)") +
  theme_minimal()

ggsave("snc_by_design_flow_dots.png", path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")

ggplot(icis_permits3, aes(x = design_flow_round, y = mean_e90)) +
  geom_line() +
  labs(title = "Mean Number of E90 Violations Since 2018 by Design Flow") +
  ylab("Mean Number E90s") + xlab("Design Flow (rounded to single decimal)") +
  theme_minimal()

ggsave("e90_by_design_flow.png",  path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")

ggplot(icis_permits3, aes(x = design_flow_round, y = mean_e90, size = num_in_bin)) +
  geom_point() +
  labs(title = "Mean Number of E90 Violations Since 2018 by Design Flow") +
  ylab("Mean Number E90s") + xlab("Design Flow (rounded to single decimal)") +
  theme_minimal()

ggsave("e90_by_design_flow_dots.png",  path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")


ggplot(icis_permits3, aes(x = design_flow_round, y = mean_inspections)) +
  geom_line() +
  labs(title = "Mean Number of Inspections Since 2018 by Design Flow") +
  ylab("Mean Number Inspectionss") + xlab("Design Flow (rounded to single decimal)") +
  theme_minimal()

ggsave("inspections_by_design_flow.png", path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")

ggplot(icis_permits3, aes(x = design_flow_round, y = mean_inspections, size = num_in_bin)) +
  geom_point() +
  annotate("text", x = 1.2, y = 5.75, label = "1 MGD") +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(title = "Mean Number of Inspections Since 2018 by Design Flow") +
  ylab("Mean Number Inspectionss") + xlab("Design Flow (rounded to single decimal)") +
  theme_minimal()

ggsave("inspections_by_design_flow_dots.png",  path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")

ggplot(icis_permits3, aes(x = design_flow_round, y = mean_CEI, size = num_in_bin)) +
  geom_point() +
  labs(title = "Mean Number of Compliance (CEI) Inspections Since 2018 by Design Flow") +
  ylab("Mean Compliance Inspectionss") + xlab("Design Flow (rounded to single decimal)") +
  theme_minimal()

ggsave("cei_inspections_by_design_flow_dots.png", path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")

ggplot(icis_permits3, aes(x = design_flow_round, y = mean_num_limits, size = num_in_bin)) +
  geom_point() +
  labs(title = "Mean Number of Limits Since 2018 by Design Flow") +
  ylab("Mean Number Limits") + xlab("Design Flow (rounded to single decimal)") +
  theme_minimal()

ggsave("number_limits_by_design_flow_dots.png",  path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")

ggplot(icis_permits3, aes(x = design_flow_round, y = e90_rate, size = num_in_bin)) +
  geom_point() +
  annotate("text", x = 1.2, y = 0.5, label = "1 MGD") +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(title = "Rate of Effluent Violations Since 2018 by Design Flow") +
  ylab("Rate of Effluent Violations") + xlab("Design Flow (rounded to single decimal)") +
  theme_minimal()

ggsave("rate_e90s_by_design_flow_dots.png",  path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")


## Checking to see if results are similar

ggplot(icis_permits_most_recent_summarized, aes(x = design_flow_round, y = mean_num_limits_per_year, size = num_in_bin)) +
  geom_point() +
  labs(title = "Mean Number of Limits in Most Recent Permit by Design Flow") +
  ylab("Mean Number Limits") + xlab("Design Flow (rounded to single decimal)") +
  theme_minimal()

ggplot(icis_permits_most_recent_summarized, aes(x = design_flow_round, y = mean_e90, size = num_in_bin)) +
  geom_point() +
  labs(title = "Mean Number of Effluent Violations Since 2018 by Design Flow") +
  ylab("Mean Number Limits") + xlab("Design Flow (rounded to single decimal)") +
  theme_minimal()


### this is better calculated through the effluent data

ggplot(icis_permits_most_recent_summarized, aes(x = design_flow_round, y = mean_e90_ratio, size = num_in_bin)) +
  geom_point() +
  labs(title = "Mean Number Effluent Violation Ratio by Design Flow") +
  ylab("Mean Number Limits") + xlab("Design Flow (rounded to single decimal)") +
  theme_minimal()


