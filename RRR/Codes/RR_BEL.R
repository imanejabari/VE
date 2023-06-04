# Clear Working Directory ####
rm(list=ls())

# Libraries ####
library(ggplot2)
library(readxl)
library(scales)
library(dplyr)

# Data ####
RiskRed = read_xlsx("Data/CI_BEL.xlsx")
RiskRed$Date = as.Date(RiskRed$Date)

## CI plot ####
ggplot(RiskRed, aes(x = Date)) +
  geom_line(aes(y = CI_NV, color = "Unvaccinated")) +
  geom_line(aes(y = CI_V, color = "Vaccinated")) +
  geom_line(aes(y = `CI_V+`, color = "1 Booster")) +
  geom_line(aes(y = `CI_V++`, color = "2 Booster")) +
  facet_grid(vars(Age), scales = 'free') + 
  xlab("Date") +
  ylab("Cumulative Incidence") + labs(color = "Vaccination Status")

# Relative risk reduction in each group ####
RiskRed$RR_NV = 1 - RiskRed$CI_V / RiskRed$CI_NV         # Vacc
RiskRed$RR_NVV = 1 - RiskRed$`CI_V+` / RiskRed$CI_NV     # 1 booster
RiskRed$RR_NVVV = 1 - RiskRed$`CI_V++` / RiskRed$CI_NV   # 2 booster

## Plots of RRR by age groups BEL ####

## RRR between unvaccinated and vaccinated (Nov-2021 to Aug-22)
ggplot(data = RiskRed %>%
         filter(RR_NV != "NA") %>%
         filter(RR_NV > -2),
       aes(x = Date,
           y = RR_NV, colour = factor(Age))) +
  geom_point(show.legend = FALSE) + 
  geom_line(show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent) + 
  facet_grid(vars(Age), scales = 'free') + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") + 
  xlab("Date") + ylab("Relative Risk Reduction")

## RRR between vaccinated and vaccinated with booster
ggplot(data = RiskRed %>%
         filter(RR_NVV != "NA") %>%
         filter(RR_NVV > -3),
       aes(x = Date,
           y = RR_NVV, colour = factor(Age))) +
  geom_point(show.legend = FALSE) + 
  geom_line(show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent) + 
  facet_grid(vars(Age), scales = 'free') + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") + 
  xlab("Date") + ylab("Relative Risk Reduction")


## RRR between vaccinated and vaccinated with 2 boosters
ggplot(data = RiskRed %>%
         filter(RR_NVVV != "NA") %>%
         filter(RR_NVVV > -3),
       aes(x = Date,
           y = RR_NVVV, colour = factor(Age))) +
  geom_point(show.legend = FALSE) + 
  geom_line(show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent) + 
  facet_grid(vars(Age), scales = 'free') + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") + 
  xlab("Date") + ylab("Relative Risk Reduction")
