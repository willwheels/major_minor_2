# Figures for AERE

library(ggplot2)
library(dplyr)

load(here::here("data", "R_data_files", "all_potw_permits.Rda"))
load(here::here("data", "R_data_files", "all_potw_permits_most_recent.Rda"))


source(here::here("theme_tina.R"))


## Design flow histogram
ggplot(icis_permits2 %>% filter(design_flow_round_one_decimal <= 5, design_flow_round_one_decimal > 0), 
       aes(x = design_flow_round_one_decimal)) +
  geom_histogram(binwidth = .1, color = "white") + 
  annotate("rect", xmin = .7, xmax = 1.2, ymin = 250, ymax = 450, alpha = .2, color = "red") +
  annotate("text", x = 1.5, y = 6700, label = "1 MGD") +
  geom_vline(xintercept = 1, linetype = 2) +
  #labs(title = "Histogram of POTW Flows") +
  xlab("Design Flow (MGD, 0.1 width bins)") + ylab("Count") +
  theme_tina

ggsave("design_flow_hist.png", path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")



icis_permits3 <- icis_permits2 %>%
  rename_with(tolower) %>%
  group_by(external_permit_nmbr) %>%             ## keep only most recent permit
  slice(1) %>%                                   ##
  ungroup() %>%
  mutate(e90_ratio = num_e90/num_limits_per_year) %>%
  filter(total_design_flow_nmbr <= 10) %>%
  group_by(design_flow_round_one_decimal) %>%
  summarise(sd_snc = sd(num_snc, na.rm = TRUE),
            sd_e90 = sd(num_e90, na.rm = TRUE),
            sd_inspections = sd(num_inspections, na.rm = TRUE),
            sd_cei = sd(num_cei, na.rm = TRUE),
            sd_e90_ratio = sd(e90_ratio, na.rm = TRUE),
            mean_snc = mean(num_snc, na.rm = TRUE), 
            mean_e90 = mean(num_e90, na.rm = TRUE),
            mean_num_limits_per_year = mean(num_limits_per_year, na.rm = TRUE),
            mean_inspections = mean(num_inspections, na.rm = TRUE),
            mean_cei = mean(num_cei, na.rm = TRUE),
            mean_e90_ratio = mean(e90_ratio, na.rm = TRUE),
            mean_e90_rate = sum(num_e90, na.rm = TRUE)/n(),
            num_in_bin = n()) %>%
  arrange(desc(design_flow_round_one_decimal)) %>%
  ungroup()

icis_permits3 <- icis_permits3 %>%
  filter(design_flow_round_one_decimal <= 2)

ggplot(icis_permits3, aes(x = design_flow_round_one_decimal, y = mean_snc, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) + 
  geom_errorbar(aes(ymin = mean_snc - 1.96*sd_snc/sqrt(num_in_bin), 
                    ymax = mean_snc + 1.96*sd_snc/sqrt(num_in_bin)),
                    width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(icis_permits3$mean_snc) + 0.5, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean Quarters in SNC") + xlab("Design Flow (rounded to single decimal)") +
  theme_tina

ggsave("snc_by_design_flow_dots.png", path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")



## Mean e90
ggplot(icis_permits3, aes(x = design_flow_round_one_decimal, y = mean_e90, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) + 
  geom_errorbar(aes(ymin = mean_e90 - 1.96*sd_e90/sqrt(num_in_bin), 
                    ymax = mean_e90 + 1.96*sd_e90/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(icis_permits3$mean_e90) + 0.5, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean No. Effluent Violations") + xlab("Design Flow (rounded to single decimal)") +
  theme_tina

ggsave("e90_by_design_flow_dots.png", path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")



## Mean e90 rate
ggplot(icis_permits3, aes(x = design_flow_round_one_decimal, y = mean_e90_rate, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) + 
  annotate("text", x = 1.05, y = max(icis_permits3$mean_e90_rate) + 0.05, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean Effluent Violation Rate") + xlab("Design Flow") +
  theme_tina

ggsave("e90_rate_by_design_flow_dots.png", path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")
