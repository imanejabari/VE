# Clear Working Directory ####
rm(list=ls())

# Libraries ####
library(readxl)
library(ggplot2)
library(scales)
library(dplyr)

# Cases ####

# Data about cases :
FR <- read_xlsx("Data/Data_FR.xlsx")
FR$Week = format(as.Date(FR$date), "%yW%U")
FR <- rename(FR, Age = age)

Age <- c("0-19", "20-39", "40-59", "60-79", "80+")
FR$Age <- factor(FR$Age, levels = unique(FR$Age),
                 labels = Age)

FR = FR %>% group_by(Week, Age) %>%
  summarize(
    Unvacc = sum(as.numeric(`nb_PCR+`)[`Vacc stat` == "NV"]),
    Primo = sum(as.numeric(`nb_PCR+`)[`Vacc stat` == "Primo+" & `Vacc stat` == "Primo-"]),
    Primo_Eff = sum(as.numeric(`nb_PCR+`)[`Vacc stat` == "Primo+"]),
    Vacc_Done = sum(as.numeric(`nb_PCR+`)[`Vacc stat` == "Complet"]),
    Vacc_Boost = sum(as.numeric(`nb_PCR+`)[`Vacc stat` == "1 booster"]),
    Vacc_Boost2 = sum(as.numeric(`nb_PCR+`)[`Vacc stat` == "2 booster"]),
  )


# Vaccination ####

# Data about vaccination :

VaccFR <- read_xlsx("Data/Vaccin_FR.xlsx")
VaccFR$Week = format(as.Date(VaccFR$date), "%yW%U")

# Population in France by age group
population = data_frame(VaccFR$classe_age, VaccFR$population_insee)
population = subset(population, !duplicated(VaccFR$classe_age))

age_group = unique(VaccFR$classe_age)

# Group data by week and age group to have the cumulative number of vaccinated (1 or 2 doses or booster)
VaccFR <- VaccFR %>%
  group_by(Week, classe_age)%>%
  summarize(Cumu_1 = sum(effectif_cumu_1_inj),
            Cumu_Done = sum(effectif_cumu_termine),
            Cumu_Boost = sum(Effectif_cumu_rappel))

# New vaccine data frame for France : we need to estimate the corresponding number of vaccinated for the age groups of the FR data
Vacc = data_frame(Week = VaccFR$Week, Age = VaccFR$classe_age)
Vacc$Age <- factor(Vacc$Age, levels = age_group[1:length(Age)],
                   labels = Age)
Vacc = subset(Vacc, Age != 'NA')

# Assuming that the age distribution within each age group is uniformly distributed
# We create a data frame with the weights corresponding to each age group
Weights = data_frame(Age = age_group)
Weights$weight1 <- as.integer(Weights$Age %in% c("00-11", "12-17")) + ifelse((Weights$Age == "18-24"), (19-18+1)/(24-18+1), 0) #0-19
Weights$weight2 <- as.integer(Weights$Age == "25-39") + ifelse((Weights$Age == "18-24"), (24-20+1)/(24-18+1), 0) #20-39
Weights$weight3 <- as.integer(Weights$Age == "40-54") + ifelse((Weights$Age == "55-64"), (59-55+1)/(64-55+1), 0) #40-59
Weights$weight4 <- as.integer(Weights$Age == "65-74") + ifelse((Weights$Age == "55-64"), (64-60+1)/(64-55+1), 0) + ifelse((Weights$Age == "75 et +"), (79-75+1)/(120-75+1), 0) #60-79
Weights$weight5 <- ifelse((Weights$Age == "75 et +"), (120-80+1)/(120-75+1), 0) #80+ we assume that the maximum age in France is 120 years old

# We extract the matrix of weights and take its transpose
weights <- t(as.matrix(Weights[, 2:ncol(Weights)]))

# Weighted population 
pop = data.frame(Age = Age)
pop$population = weights %*% population$`VaccFR$population_insee`

# We add this information to the new Vacc data frame for each age group
Vacc$Population = rep(pop$population, length.out = nrow(Vacc))

# We create a list with 3 lists to contain the elements of the weighted cumulative vaccinated (1 or 2 doses or booster)
CUMU <- list(Cumu_1 = list(),
             Cumu_Done = list(),
             Cumu_Boost = list())
factors = c("Cumu_1","Cumu_Done","Cumu_Boost")

# Computation of matrix-vector product between the weights (5 x 8 matrix) and the 8 elements vectors that correspond to the data of every age group for one week
# We do that for the 3 columns of VaccFR corresponding to the cumulative vaccinated with 1, 2 or a booster dose
for (factor in factors) {
  for (i in seq(1, length(VaccFR[[factor]]), by = length(age_group))) {
    tmp  = VaccFR[[factor]] # We select the current vector from VaccFR data frame
    tmp = tmp[i:(i+length(age_group)-1)] # We select the current group of 8 elements from the vector
    res = weights %*% tmp # We multiply the weights matrix by the current group of 8 elements
    CUMU[[factor]][[i]]  = res # We store the result in the list
  }
  # We add the computed values to the Vacc data frame and replace all NA values by 0
  Vacc[[factor]] <- do.call(rbind, CUMU[[factor]])
  Vacc[[factor]] <- ifelse(is.na(Vacc[[factor]]), 0, Vacc[[factor]])
}

# We compute the population at risk for the unvaccinated, the primo-vaccinated, the fully-vaccinated and the vaccinated with booster
Vacc$PaR_Unvacc = Vacc$Population - Vacc$Cumu_1
Vacc$PaR_Primo = Vacc$Cumu_1 - Vacc$Cumu_Done
Vacc$PaR_Vacc_Done = Vacc$Cumu_Done - Vacc$Cumu_Boost
Vacc$PaR_Boost = Vacc$Cumu_Boost
Vacc$PaR_Unvacc = ifelse((Vacc$PaR_Unvacc >= 0), Vacc$PaR_Unvacc, 0)


# Computation of RRR ####

# Puting all data together (FR and Vacc)
RRR <- merge(FR, Vacc, by = c("Age", "Week"))

# We calculate the cumulative incidence in each group
RRR$CI_Primo = RRR$Primo_Eff/RRR$PaR_Primo
RRR$CI_Vacc_Done = RRR$Vacc_Done/RRR$PaR_Vacc_Done
RRR$CI_Boost = RRR$Vacc_Boost/RRR$PaR_Boost
RRR$CI_Unvacc = RRR$Unvacc/RRR$PaR_Unvacc
RRR$CI_Unvacc[RRR$CI_Unvacc == Inf] <- NA # In the 80+ group, we remove inf values

# Then, we can calculate the relative risk reduction in each group
RRR$RR_Primo = 1 - RRR$CI_Primo/RRR$CI_Unvacc
RRR$RR_Done = 1 - RRR$CI_Vacc_Done/RRR$CI_Unvacc
RRR$RR_Boost = 1 - RRR$CI_Boost/RRR$CI_Unvacc

# Plot CI
ggplot(RRR, aes(x = Week)) +
  geom_line(aes(y = CI_Unvacc*10^5, color = "Unvaccinated", group = 1)) +
  geom_line(aes(y = CI_Vacc_Done*10^5, color = "Vaccinated", group = 1)) +
  geom_line(aes(y = CI_Boost*10^5, color = "1 Booster", group = 1)) +
  facet_grid(vars(Age), scales = 'free') + 
  scale_x_discrete(breaks = c("21W27", "21W37", "21W47", "22W05", "22W15","22W25", "22W35", "22W45", "23W03")) +
  xlab("Date") +
  ylab("Cumulative Incidence") + labs(color = "Vaccination Status")

# Plots RRR
ggplot(data = RRR,                        
       aes(x = Week,
           y = RR_Done, colour = factor(Age))) +
  geom_point(show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent) + 
  facet_grid(vars(Age), scales = 'free') + 
  scale_x_discrete(breaks = c("21W27", "21W37", "21W47", "22W05", "22W15","22W25", "22W35", "22W45", "23W03")) +
  xlab("Date") + ylab("Relative Risk Reduction")

ggplot(data = RRR,                        
       aes(x = Week,
           y = RR_Boost, colour = factor(Age))) +
  geom_point(show.legend = FALSE) + 
  scale_y_continuous(labels = scales::percent) + 
  facet_grid(vars(Age), scales = 'free') + 
  scale_x_discrete(breaks = c("21W27", "21W37", "21W47", "22W05", "22W15","22W25", "22W35", "22W45", "23W03")) +
  xlab("Date") + ylab("Relative Risk Reduction")
