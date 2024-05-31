## This script creates figures for flow and conventional pollutant quantity and concentration
## Here, we use discharge ratio (DMR/Limit)


library(readr)
library(dplyr)
library(sqldf)
library(magrittr)
library(tidylog) #wrapper for tidy that gives information on each output
library(tidytable)
library(udpipe)
library(ggplot2)

## Get info on permit components to determine POTWs
perm_components <- fread(here::here("data", "csv_files", "NPDES_PERM_COMPONENTS.csv"))
colnames(perm_components) <- tolower(colnames(perm_components))

perm_components <- perm_components %>%
  select(-component_type_code) %>%
  filter(component_type_desc == "POTW")

perm_components_pretreat <- fread(here::here("data", "csv_files", "NPDES_PERM_COMPONENTS.csv")) %>%
  rename_with(tolower) %>%
  select(-component_type_code) %>%
  filter(component_type_desc == "Pretreatment") %>%
  mutate(pretreat_flag = 1) %>%
  select(-component_type_desc) 

# Flow plots ---
load(here::here("data", "R_data_files", "flow_dmrs.Rda"))
load(here::here("data", "R_data_files", "all_potw_permits.Rda"))
colnames(icis_permits2) <- tolower(colnames(icis_permits2))

icis_permits2 <- icis_permits2 %>%
  select(external_permit_nmbr, version_nmbr, component_type_desc,
         design_flow_round_one_decimal, design_flow_round_two_decimals,
         design_flow_round_three_decimals)

flow_icis <- flow_icis %>%
  left_join(icis_permits2, by=c("external_permit_nmbr", "version_nmbr")) %>%
  filter(!(permit_status_code %in% c("NON", "TRM"))) %>%
  filter(facility_type_indicator == "POTW" | component_type_desc == "POTW") %>%
  left_join(perm_components_pretreat, by = "external_permit_nmbr") %>%
  mutate(pretreat_flag = if_else(is.na(pretreat_flag), 0, 1)) %>%
  arrange(external_permit_nmbr, version_nmbr, monitoring_period_end_date2) %>%
  filter(!is.na(total_design_flow_nmbr)) %>%
  mutate(flow_over_design = flow_correct / total_design_flow_nmbr,
         limit_value_standard_units = as.numeric(limit_value_standard_units))


flow_icis2 <- flow_icis %>%
  filter(flow_over_design <= 100 & flow_correct <= 100) %>%
  filter(total_design_flow_nmbr <= 2 & limit_value_nmbr > 0 & !is.na(limit_value_nmbr)) %>%
  mutate(flow_discharge_ratio = flow_correct/limit_value_standard_units) %>%
  filter(!is.na(flow_discharge_ratio)) %>%
  group_by(design_flow_round_one_decimal) %>%
  summarise(mean_flow_discharge_ratio = mean(flow_discharge_ratio, na.rm = TRUE),
            sd_flow_discharge_ratio = sd(flow_discharge_ratio, na.rm = TRUE),
            num_in_bin = n()) %>%
  arrange(desc(design_flow_round_one_decimal)) %>%
  ungroup()


a <- ggplot(subset(flow_icis2, mean_flow_discharge_ratio != Inf), aes(x = design_flow_round_one_decimal, y = mean_flow_discharge_ratio, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_flow_discharge_ratio - 1.96*sd_flow_discharge_ratio/sqrt(num_in_bin), 
                    ymax = mean_flow_discharge_ratio + 1.96*sd_flow_discharge_ratio/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(subset(flow_icis2, mean_flow_discharge_ratio != Inf)$mean_flow_discharge_ratio, na.rm = TRUE) + 0.1, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Flow/Flow Limit") + xlab("Design Flow (MGD)") +
  theme_minimal()

a <- a + theme(legend.position="bottom")
a <- a + theme(text=element_text(size=10))
a <- a + theme(axis.text=element_text(size=20))
a <- a + theme(axis.title.x = element_text(size=30, margin=margin(20,0,0,0)))
a <- a + theme(axis.title.y = element_text(size=30, margin=margin(0,20,0,0)))
a <- a + theme(legend.key.size = unit(3, 'mm'), #change legend key size
               legend.title = element_text(size=20), #change legend title font size
               legend.text = element_text(size=20), #change legend text font size
               legend.title.align = 0.5,
               legend.margin=margin(10,0,0,0)) 
a <- a + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
a
ggsave(here::here("results", "flow_discharge_ratio_by_design_flow.png"), dpi = 300, plot=a, h = 8.5, w = 11, units = "in", bg = "white")


# Mean concentration plots ----

load(here::here("data", "R_data_files", "mean_conc.Rda"))
load(here::here("data", "R_data_files", "max_conc.Rda"))
load(here::here("data", "R_data_files", "mean_quant.Rda"))
load(here::here("data", "R_data_files", "max_quant.Rda"))



prep_mean_conc <- function(type, pollutant){
  
  assign("data", type)
  
  data <- data %>%
    filter(pollutant_type == pollutant) %>%
    filter(total_design_flow_nmbr <= 2 & limit_value_nmbr > 0 & !is.na(limit_value_nmbr)) %>%
    mutate(limit_value_standard_units = as.numeric(limit_value_standard_units)) %>%
    mutate(discharge_ratio = mean_conc/limit_value_standard_units, na.rm = TRUE) %>%
    group_by(design_flow_round) %>%
    filter(!is.na(discharge_ratio)) %>%
    summarise(mean_discharge_ratio = mean(discharge_ratio, na.rm = TRUE),
              sd_discharge_ratio = sd(discharge_ratio, na.rm = TRUE),
              num_in_bin = n(),
              pollutant_type = first(pollutant_type)) %>%
    ungroup() %>%
    
    return(data)
  
}



prep_mean_quant <- function(type, pollutant){
  
  assign("data", type)
  
  data <- data %>%
    filter(pollutant_type == pollutant) %>%
    filter(total_design_flow_nmbr <= 2 & limit_value_nmbr > 0 & !is.na(limit_value_nmbr)) %>%
    mutate(limit_value_standard_units = as.numeric(limit_value_standard_units)) %>%
    mutate(discharge_ratio = mean_quant/limit_value_standard_units, na.rm = TRUE) %>%
    filter(!is.na(discharge_ratio)) %>%
    group_by(design_flow_round) %>%
    summarise(mean_discharge_ratio = mean(discharge_ratio, na.rm = TRUE),
              sd_discharge_ratio = sd(discharge_ratio, na.rm = TRUE),
              num_in_bin = n(),
              pollutant_type = first(pollutant_type)) %>%
    ungroup() %>%
    
    return(data)
  
}    





dir_create_if_not_exist <- function(dir_name, ...) {
  
  
  if (!dir.exists(here::here(dir_name, ...))) {dir.create(here::here(dir_name, ...))}
}

dir_create_if_not_exist("results")



# BOD plots ----

BOD_c2 <- prep_mean_conc(type = c2, pollutant = "BOD")
BOD_q1 <- prep_mean_quant(type = q1, pollutant = "BOD")


## BOD Conc. ----
plot <- ggplot(subset(BOD_c2, mean_discharge_ratio != Inf), aes(x = design_flow_round, y = mean_discharge_ratio, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_discharge_ratio - 1.96*sd_discharge_ratio/sqrt(num_in_bin), 
                    ymax = mean_discharge_ratio + 1.96*sd_discharge_ratio/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(subset(BOD_c2, mean_discharge_ratio != Inf)$mean_discharge_ratio)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean BOD Discharge Ratio (Conc.)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=24, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=24, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "BOD_discharge_ratio_conc_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")


## BOD quant ----
plot <- ggplot(subset(BOD_q1, mean_discharge_ratio != Inf), aes(x = design_flow_round, y = mean_discharge_ratio, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_discharge_ratio - 1.96*sd_discharge_ratio/sqrt(num_in_bin), 
                    ymax = mean_discharge_ratio + 1.96*sd_discharge_ratio/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(subset(BOD_c2, mean_discharge_ratio != Inf)$mean_discharge_ratio)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean BOD Discharge Ratio (Quant.)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=24, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=24, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "BOD_discharge_ratio_quant_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")


# DO limits not available -- only 72 of 7014 DO observations have a limit value 

# TSS plots ----

TSS_c2 <- prep_mean_conc(type = c2, pollutant = "TSS")
TSS_q1 <- prep_mean_quant(type = q1, pollutant = "TSS")

## TSS Conc. ----
plot <- ggplot(subset(TSS_c2, mean_discharge_ratio != Inf), aes(x = design_flow_round, y = mean_discharge_ratio, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_discharge_ratio - 1.96*sd_discharge_ratio/sqrt(num_in_bin), 
                    ymax = mean_discharge_ratio + 1.96*sd_discharge_ratio/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(subset(BOD_c2, mean_discharge_ratio != Inf)$mean_discharge_ratio)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean TSS Discharge Ratio (Conc.)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=24, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=24, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "TSS_discharge_ratio_conc_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")


## TSS quant ----
plot <- ggplot(subset(TSS_q1, mean_discharge_ratio != Inf), aes(x = design_flow_round, y = mean_discharge_ratio, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_discharge_ratio - 1.96*sd_discharge_ratio/sqrt(num_in_bin), 
                    ymax = mean_discharge_ratio + 1.96*sd_discharge_ratio/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(subset(BOD_c2, mean_discharge_ratio != Inf)$mean_discharge_ratio)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean TSS Discharge Ratio (Quant.)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=24, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=24, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "TSS_discharge_ratio_quant_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")


# TN plots ----

TN_c2 <- prep_mean_conc(type = c2, pollutant = "TN")
TN_q1 <- prep_mean_quant(type = q1, pollutant = "TN")

## TN Conc. ----
plot <- ggplot(subset(TN_c2, mean_discharge_ratio != Inf), aes(x = design_flow_round, y = mean_discharge_ratio, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_discharge_ratio - 1.96*sd_discharge_ratio/sqrt(num_in_bin), 
                    ymax = mean_discharge_ratio + 1.96*sd_discharge_ratio/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(subset(TN_c2, mean_discharge_ratio != Inf)$mean_discharge_ratio)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean TN Discharge Ratio (Conc.)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=24, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=24, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "TN_discharge_ratio_conc_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")


## TN quant ----
plot <- ggplot(subset(TN_q1, mean_discharge_ratio != Inf), aes(x = design_flow_round, y = mean_discharge_ratio, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_discharge_ratio - 1.96*sd_discharge_ratio/sqrt(num_in_bin), 
                    ymax = mean_discharge_ratio + 1.96*sd_discharge_ratio/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(subset(TN_c2, mean_discharge_ratio != Inf)$mean_discharge_ratio)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean TN Discharge Ratio (Quant.)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=24, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=24, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "TN_discharge_ratio_quant_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")


# TP plots ----

TP_c2 <- prep_mean_conc(type = c2, pollutant = "TP")
TP_q1 <- prep_mean_quant(type = q1, pollutant = "TP")

## TP Conc. ----
plot <- ggplot(subset(TP_c2, mean_discharge_ratio != Inf), aes(x = design_flow_round, y = mean_discharge_ratio, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_discharge_ratio - 1.96*sd_discharge_ratio/sqrt(num_in_bin), 
                    ymax = mean_discharge_ratio + 1.96*sd_discharge_ratio/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(subset(TP_c2, mean_discharge_ratio != Inf)$mean_discharge_ratio)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean TP Discharge Ratio (Conc.)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=24, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=24, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "TP_discharge_ratio_conc_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")


## TP quant ----
plot <- ggplot(subset(TP_q1, mean_discharge_ratio != Inf), aes(x = design_flow_round, y = mean_discharge_ratio, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_discharge_ratio - 1.96*sd_discharge_ratio/sqrt(num_in_bin), 
                    ymax = mean_discharge_ratio + 1.96*sd_discharge_ratio/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(subset(BOD_c2, mean_discharge_ratio != Inf)$mean_discharge_ratio)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean TP Discharge Ratio (Quant.)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=24, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=24, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "TP_discharge_ratio_quant_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")


