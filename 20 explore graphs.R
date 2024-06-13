
library(ggplot2)
library(dplyr)

load(here::here("data", "R_data_files", "all_potw_permits.Rda"))
load(here::here("data", "R_data_files", "all_potw_permits_most_recent.Rda"))

source(here::here("theme_tina.R"))

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


icis_permits2 %>%
  arrange(EXTERNAL_PERMIT_NMBR, VERSION_NMBR) %>%
  group_by(EXTERNAL_PERMIT_NMBR) %>%             ## keep only most recent permit
  slice(1) %>%                               ##
  ungroup() %>%
  filter(design_flow_round_one_decimal <= 5, design_flow_round_one_decimal > 0) %>%
  ggplot(aes(x = design_flow_round_one_decimal)) +
  geom_histogram(binwidth = .1) +
  theme_minimal()

ggplot(icis_permits2 %>% arrange(EXTERNAL_PERMIT_NMBR, VERSION_NMBR) %>%
         group_by(EXTERNAL_PERMIT_NMBR) %>%             ## keep only most recent permit
         slice(1) %>%                               ##
         ungroup() %>%
         filter(design_flow_round_one_decimal <= 5, design_flow_round_one_decimal >= 0.1),
       aes(x = design_flow_round_one_decimal)) +
  geom_histogram(binwidth = .1, color = "white") + 
  annotate("rect", xmin = .7, xmax = 1.2, ymin = 250, ymax = 450, alpha = .2, color = "red") +
  annotate("text", x = 1.5, y = 2000, label = "1 MGD") +
  geom_vline(xintercept = 1, linetype = 2) +
  #labs(title = "Histogram of POTW Flows") +
  xlab("Design Flow (MGD, 0.1 width bins)") + ylab("Count") +
  theme_tina

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
  group_by(design_flow_round_one_decimal) %>%
  count()

ggplot(design_flow_counts %>% filter(design_flow_round_one_decimal <= 5, design_flow_round_one_decimal > 0), 
       aes(x = design_flow_round_one_decimal, y = n)) +
  geom_col() + 
  annotate("rect", xmin = .7, xmax = 1.2, ymin = 250, ymax = 450, alpha = .2, color = "red") +
  annotate("text", x = 1.3, y = 6700, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  #labs(title = "Histogram of POTW Flows") +
  xlab("Design Flow (MGD, 0.1 width bins)") + ylab("Count") +
  theme_tina

ggsave("design_flow_col.png", path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")


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

icis_permits_most_recent_summarized <- icis_permits2 %>%
  group_by(EXTERNAL_PERMIT_NMBR) %>%             ## keep only most recent permit
  slice(1) %>%                                   ##
  ungroup() %>%
  mutate(e90_ratio = num_e90/num_limits_per_year) %>%
  filter(TOTAL_DESIGN_FLOW_NMBR <= 10) %>%
  group_by(design_flow_round_one_decimal) %>%
  summarise(mean_snc = mean(num_snc, na.rm = TRUE), 
            sd_snc = sd(num_snc, na.rm = TRUE),
            mean_e90 = mean(num_e90, na.rm = TRUE),
            mean_num_limits_per_year = mean(num_limits_per_year, na.rm = TRUE),
            mean_inspections = mean(num_inspections, na.rm = TRUE),
            sd_inspections = sd(num_inspections, na.rm = TRUE),
            mean_CMS = mean(num_CMS, na.rm = TRUE),
            sd_CMS = sd(num_CMS, na.rm = TRUE),
            mean_e90_ratio = mean(e90_ratio, na.rm = TRUE),
            num_in_bin = n()) %>%
  arrange(desc(design_flow_round_one_decimal)) %>%
  ungroup()



icis_permits3 <- icis_permits_most_recent_summarized %>%
  filter(design_flow_round_one_decimal <= 2)



ggplot(icis_permits3, aes(x = design_flow_round_one_decimal, y = mean_snc, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +  
  geom_errorbar(aes(ymin = mean_snc - 1.96*sd_snc/sqrt(num_in_bin), 
                    ymax = mean_snc + 1.96*sd_snc/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, 
           y = max(subset(icis_permits3, design_flow_round_one_decimal <=2)$mean_snc)*1.1, 
           label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean Quarters in SNC") + xlab("Design Flow") +
  theme_tina

ggsave("snc_by_design_flow_dots.png", path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")

ggplot(icis_permits3, aes(x = design_flow_round, y = mean_e90)) +
  geom_line() +
  labs(title = "Mean Number of E90 Violations Since 2018 by Design Flow") +
  ylab("Mean Number E90s") + xlab("Design Flow (rounded to single decimal)") +
  theme_minimal()


ggplot(icis_permits3, aes(x = design_flow_round, y = mean_e90, size = num_in_bin)) +
  geom_point() +
  labs(title = "Mean Number of E90 Violations Since 2018 by Design Flow") +
  ylab("Mean Number E90s") + xlab("Design Flow (rounded to single decimal)") +
  theme_tina

ggsave("e90_by_design_flow_dots.png",  path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")


ggplot(icis_permits3, aes(x = design_flow_round, y = mean_inspections)) +
  geom_line() +
  labs(title = "Mean Number of Inspections Since 2018 by Design Flow") +
  ylab("Mean Number Inspectionss") + xlab("Design Flow (rounded to single decimal)") +
  theme_minimal()


ggplot(icis_permits3, aes(x = design_flow_round_one_decimal, y = mean_inspections, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +  
  geom_errorbar(aes(ymin = mean_inspections - 1.96*sd_inspections/sqrt(num_in_bin), 
                    ymax = mean_inspections + 1.96*sd_inspections/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, 
           y = max(subset(icis_permits3, design_flow_round_one_decimal <=2)$mean_inspections) + 2, 
           label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean Number of Inspections") + xlab("Design Flow") +
  theme_tina

ggsave("inspections_by_design_flow_dots.png",  path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")

ggplot(icis_permits3, aes(x = design_flow_round_one_decimal, y = mean_CMS, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +  
  geom_errorbar(aes(ymin = mean_CMS - 1.96*sd_CMS/sqrt(num_in_bin), 
                    ymax = mean_CMS + 1.96*sd_CMS/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, 
           y = max(subset(icis_permits3, design_flow_round_one_decimal <=2)$mean_inspections) + 2, 
           label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean Number of Inspections") + xlab("Design Flow") +
  theme_tina

ggsave("cms_inspections_by_design_flow_dots.png", path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")

ggplot(icis_permits3, aes(x = design_flow_round, y = mean_num_limits_per_year, size = num_in_bin)) +
  geom_point() +
  labs(title = "Mean Number of Limits/YEar Since 2018 by Design Flow") +
  ylab("Mean Number Limits") + xlab("Design Flow (rounded to single decimal)") +
  theme_minimal()

ggsave("number_limits_by_design_flow_dots.png",  path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")

ggplot(icis_permits3, aes(x = design_flow_round, y = mean_e90_ratio, size = num_in_bin)) +
  geom_point() +
  annotate("text", x = 1.2, y = 0.11, label = "1 MGD") +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(title = "Rate of Effluent Violations Since 2018 by Design Flow") +
  ylab("Rate of Effluent Violations") + xlab("Design Flow (rounded to single decimal)") +
  theme_minimal()

ggsave("rate_e90s_by_design_flow_dots.png",  path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")



