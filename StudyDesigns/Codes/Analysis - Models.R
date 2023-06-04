###########################################################################
# Main Analysis
###########################################################################

# Preamble ----------------------------------------------------------------
# Clear Working Directory ####
rm(list=ls())

# Source R codes ####
# Library
source("Codes/Library.R")

# Load data
source("Codes/Data.R")

# Model for BEL --------------------

# BEL data
ds_BEL<- data_BEL %>% 
  mutate(N = sum(nb),
         .by = c("Week", "Age", "Vacc.stat")) %>% 
  filter(Week %in% unique(data_FR$Week)) %>%
  filter(N > 30) %>% 
  mutate(Percent = nb / N,
         Group = Vacc.stat,
         Country = "BEL")

# Plot of the number of tests
ds_BEL %>% 
  summarise(n = n(),
            nb = mean(nb), .by = c("Week", "Age", "Vacc.stat", "TEST_RESULT")) %>% 
  ggplot(aes(x = Week, y = nb, color = TEST_RESULT, group = TEST_RESULT)) +
  facet_grid(Age ~ Vacc.stat, scales = "free_y") +
  ylab("Number of tests") +
  scale_x_discrete(breaks = c("21W27", "21W47", "22W15", "22W35", "23W03")) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1))

# Case-control method ####
mod_BEL <- lapply(1:length(unique(ds_BEL$Week)), FUN = function(jj){
  
  # Set week
  week_jj <- unique(ds_BEL$Week)[jj]
  print(week_jj)
  
  # Subset on week
  ds_week <- ds_BEL %>% 
    filter(Week == week_jj) %>% 
    droplevels()
  
  # Get age group present within week with more than 1 group
  age_gr <- ds_week %>% 
    select(Age, Group) %>% 
    distinct() %>% 
    count(Age) %>% 
    filter(n > 1) %>% 
    pull(Age)
  
  # Get results
  res_age <- lapply(1:length(age_gr), FUN = function(ee){
    
    # Set age
    age_ee <- age_gr[ee]
    print(ee)
    # Subset data
    sub_ds <- ds_week %>% 
      filter(Age == age_ee) %>% 
      droplevels()
    
    # Simple logistic regression
    glmer_fit <- sub_ds %>% 
      glm(data = ., TEST_RESULT ~ Group,
          family = quasibinomial(link = "logit"), weights = nb)
    
    # Emmeans allows you to get mean estimates per group from the model
    logit_emmeans <- emmeans(glmer_fit, c("Group"), type = "response") %>%
      data.frame() %>%
      mutate(Week  = unique(sub_ds$Week),
             Age   = unique(sub_ds$Age))
    
    # To get the pairwise comparisons with 95% confidence intervals
    logit_contrast <- emmeans(glmer_fit, pairwise ~ Group,
                              type = "response")$contrast %>% 
      summary(infer = T) %>%
      data.frame() %>%
      mutate(Week  = unique(sub_ds$Week),
             Age   = unique(sub_ds$Age))
    
    # Return all the results
    list(
      logit_emmeans    = logit_emmeans,
      logit_contrast   = logit_contrast)
  }) # end lapply ee
  
  # Return results split per model rather than age
  list(
    logit_emmeans    = map(res_age, "logit_emmeans") %>% bind_rows(),
    logit_contrast   = map(res_age, "logit_contrast") %>% bind_rows())
})

# Get results per model rather week ####
# Logistic regression
logit_emmeans_BEL   <- map(mod_BEL, "logit_emmeans") %>% bind_rows() %>% 
  relocate(c("Week", "Age"), .before = 1)
logit_contrast_BEL   <- map(mod_BEL, "logit_contrast") %>% bind_rows() %>%  
  relocate(c("Week", "Age"), .before = 1)

## Plot results per model for BEL Case-Control####
# Probas
ggplot(data = logit_emmeans_BEL,
       aes(x = Week, y = prob, color = Group, group = Group)) +
  geom_line(linewidth = 1) +
  facet_grid(Age ~ ., scales = "free_y") +
  ylab("Probability") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(breaks = c("21W27", "21W37", "21W47", "22W05", "22W15","22W25", "22W35", "22W45", "23W03"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1))

# Vaccine effectiveness by age group (for each vaccination status)
ggplot(data = logit_contrast_BEL %>%
         filter(contrast == "Complet / NV") %>%
         filter(odds.ratio < 5),
       aes(x = Week, y = 1-odds.ratio, color = Age, group = 1)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  facet_grid(Age ~., scales = "free") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Vaccine Effectiveness") +
  scale_x_discrete(breaks = c("21W27", "21W47", "22W15", "22W35", "23W03")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1))

ggplot(data = logit_contrast_BEL %>%
         filter(contrast == "1 booster / NV") %>%
         filter(odds.ratio < 5),
       aes(x = Week, y = 1-odds.ratio, color = Age, group = 1)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  facet_grid(Age ~., scales = "free") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Vaccine Effectiveness") +
  scale_x_discrete(breaks = c("21W27", "21W47", "22W15", "22W35", "23W03")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1))

ggplot(data = logit_contrast_BEL %>%
         filter(contrast == "2 booster / NV") %>%
         filter(odds.ratio < 5),
       aes(x = Week, y = 1-odds.ratio, color = Age, group = 1)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  facet_grid(Age ~., scales = "free") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Vaccine Effectiveness") +
  scale_x_discrete(breaks = c("21W27", "21W47", "22W15", "22W35", "23W03")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1))



# Model for FR --------------------

# Descriptive Analysis for FR Data ----------------------------------------------------
# Longitudinal count ####
ds <- data_FR %>% 
  filter(Vacc.stat %in% c("Complet", "Primo", "NV", "1 booster", "2 booster")) %>% 
  left_join(data_FR %>% 
              summarise(n = n(),
                        N = sum(nb),
                        N_sympt = sum(nb_sympt),
                        .by = c("Date", "Age", "Group"))) %>% 
  filter(N != 0) %>% 
  mutate(Percent = nb / N,
         Percent_sympt = nb_sympt / N_sympt)

# Plot of proportion of positive tests estimated on the data (Case control)
# Per week
ds %>% 
  summarise(
    nb = sum(nb),
    .by = c("Week", "Age", "Group", "TEST_RESULT")) %>% 
  mutate(
    N = sum(nb),
    .by = c("Week", "Age", "Group")) %>% 
  filter(TEST_RESULT == "1" & N > 30) %>% 
  mutate(Percent = nb / N) %>% 
  ggplot(aes(x = Week, y = Percent, color = Group, group = Group)) +
  facet_grid(Age ~ ., scales = "free_y") +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(breaks = c("21W27", "21W37", "21W47", "22W05", "22W15","22W25", "22W35", "22W45", "23W03"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1))

# Plot of the number of tests
# Week
ds %>% 
  summarise(n = n(),
            nb = mean(nb), .by = c("Week", "Age", "Vacc.stat", "TEST_RESULT")) %>% 
  ggplot(aes(x = Week, y = nb, color = TEST_RESULT, group = TEST_RESULT)) +
  facet_grid(Age ~ Vacc.stat, scales = "free_y") +
  ylab("Number of tests") +
  scale_x_discrete(breaks = c("21W27", "21W47", "22W15", "22W35", "23W03"))+
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1))

# Day
ds %>% 
  ggplot(aes(x = Date, y = nb, color = TEST_RESULT, group = TEST_RESULT)) +
  facet_grid(Age ~ Vacc.stat, scales = "free_y") +
  ylab("Number of tests") +
  geom_point(size = 0.1) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1))


# Model FR by vaccination status --------------------

# FR data
ds_FR <- data_FR %>% 
  filter(Vacc.stat %in% c("Complet", "NV", "1 booster", "2 booster")) %>% 
  summarise(
    nb = sum(nb),
    nb_sympt = sum(nb_sympt),
    .by = c("Week", "Age", "Vacc.stat", "TEST_RESULT")) %>% 
  mutate(#n = n(),
    N = sum(nb),
    N_sympt = sum(nb_sympt),
    .by = c("Week", "Age", "Vacc.stat")) %>% 
  filter(Week %in% unique(data_FR$Week)) %>%
  filter(N != 0) %>% 
  mutate(Percent = nb / N,
         Percent_sympt = nb_sympt / N_sympt,
         Group = Vacc.stat,
         Country = "FR")

# Case-control method ####
mod_FR_cc <- lapply(1:length(unique(ds_FR$Week)), FUN = function(jj){
  
  # Set week
  week_jj <- unique(ds_FR$Week)[jj]
  print(week_jj)
  
  # Subset on week
  ds_week <- ds_FR %>% 
    filter(Week == week_jj) %>% 
    filter(N > 30) %>% 
    droplevels()
  
  # Get age group present within week with more than 1 group
  age_gr <- ds_week %>% 
    select(Age, Group) %>% 
    distinct() %>% 
    count(Age) %>% 
    filter(n > 1) %>% 
    pull(Age)
  
  # Get results
  res_age <- lapply(1:length(age_gr), FUN = function(ee){
    
    # Set age
    age_ee <- age_gr[ee]
    print(ee)
    # Subset data
    sub_ds <- ds_week %>% 
      filter(Age == age_ee) %>% 
      droplevels()
    
    # Check that here are more than 1 dates to perform mix model
    # Otherwise perform simple model
    if(length(unique(sub_ds$Date)) > 1){
      
      # Logistic regression
      # Random Logistic regression with day to day variability taken into account
      glmer_fit <- sub_ds %>% 
        glmer(data = .,
              TEST_RESULT ~ Group + (1 | Date),
              family = binomial(link = "logit"),
              weights = nb)
      
      # Emmeans allows you to get mean estimates per group from the model
      logit_emmeans <- emmeans(glmer_fit, c("Group"), type = "response") %>%
        data.frame() %>%
        mutate(Week  = unique(sub_ds$Week),
               Age   = unique(sub_ds$Age))
      
      # To get the pairwise comparisons with 95% confidence intervals
      logit_contrast <- emmeans(glmer_fit, pairwise ~ Group,
                                type = "response")$contrast %>% 
        summary(infer = T) %>%
        data.frame() %>%
        mutate(Week  = unique(sub_ds$Week),
               Age   = unique(sub_ds$Age))
      
    } else {
      
      # Simple logistic regression
      glmer_fit <- sub_ds %>% 
        glm(data = ., TEST_RESULT ~ Group,
            family = quasibinomial(link = "logit"), weights = nb)
      
      # Emmeans allows you to get mean estimates per group from the model
      logit_emmeans <- emmeans(glmer_fit, c("Group"), type = "response") %>%
        data.frame() %>%
        mutate(Week  = unique(sub_ds$Week),
               Age   = unique(sub_ds$Age))
      
      # To get the pairwise comparisons with 95% confidence intervals
      logit_contrast <- emmeans(glmer_fit, pairwise ~ Group,
                                type = "response")$contrast %>% 
        summary(infer = T) %>%
        data.frame() %>%
        mutate(Week  = unique(sub_ds$Week),
               Age   = unique(sub_ds$Age))
      
    } # end else date
    
    # Return all the results
    list(
      logit_emmeans    = logit_emmeans,
      logit_contrast   = logit_contrast)
    
  }) # end lapply ee
  
  # Return results split per model rather than age
  list(
    logit_emmeans    = map(res_age, "logit_emmeans") %>% bind_rows(),
    logit_contrast   = map(res_age, "logit_contrast") %>% bind_rows())
})

# Get results per model rather week ####
# Logistic regression
logit_emmeans_FR_cc    <- map(mod_FR_cc, "logit_emmeans") %>% bind_rows() %>% 
  relocate(c("Week", "Age"), .before = 1)
logit_contrast_FR_cc   <- map(mod_FR_cc, "logit_contrast") %>% bind_rows() %>%  
  relocate(c("Week", "Age"), .before = 1)

## Plot results for FR Case-Control ####
# Probas
ggplot(data = logit_emmeans_FR_cc,
       aes(x = Week, y = prob, color = Group, group = Group)) +
  geom_line(linewidth = 1) +
  facet_grid(Age ~ ., scales = "free_y") +
  ylab("Probability") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(breaks = c("21W27", "21W37", "21W47", "22W05", "22W15","22W25", "22W35", "22W45", "23W03"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1))

# Vaccine effectiveness by age group (for each vaccination status)
ggplot(data = logit_contrast_FR_cc %>%
         filter(contrast == "Complet / NV") %>%
         filter(odds.ratio < 5),
       aes(x = Week, y = 1-odds.ratio, color = Age, group = 1)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  facet_grid(Age ~., scales = "free") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Vaccine Effectiveness") +
  scale_x_discrete(breaks = c("21W27", "21W47", "22W15", "22W35", "23W03")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1))

ggplot(data = logit_contrast_FR_cc%>%
         filter(contrast == "1 booster / NV") %>%
         filter(odds.ratio < 5),
       aes(x = Week, y = 1-odds.ratio, color = Age, group = 1)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  facet_grid(Age ~., scales = "free") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Vaccine Effectiveness") +
  scale_x_discrete(breaks = c("21W27", "21W47", "22W15", "22W35", "23W03")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1))

ggplot(data = logit_contrast_FR_cc%>%
         filter(contrast == "2 booster / NV") %>%
         filter(odds.ratio < 5),
       aes(x = Week, y = 1-odds.ratio, color = Age, group = 1)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  facet_grid(Age ~., scales = "free") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Vaccine Effectiveness") +
  scale_x_discrete(breaks = c("21W27", "21W47", "22W15", "22W35", "23W03")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1))


# TND #####
mod_FR_TND <- lapply(1:length(unique(ds_FR$Week)), FUN = function(jj){
  
  # Set week
  week_jj <- unique(ds_FR$Week)[jj]
  print(week_jj)
  
  # Subset on week
  ds_week <- ds_FR %>% 
    filter(Week == week_jj) %>% 
    filter(N_sympt > 30) %>% 
    droplevels()
  
  # Get age group
  age_gr <- ds_week %>% 
    select(Age, Group) %>% 
    distinct() %>% 
    count(Age) %>% 
    filter(n > 1) %>% 
    pull(Age)
  
  # Get results
  res_age <- lapply(1:length(age_gr), FUN = function(ee){
    
    # Set age
    age_ee <- age_gr[ee]
    print(ee)
    # Subset data
    sub_ds <- ds_week %>% 
      filter(Age == age_ee) %>% 
      droplevels()
    
    # Check that here are more than 1 dates to perform mix model
    # Otherwise perform simple model
    if(length(unique(sub_ds$Date)) > 1){
      
      # Logistic regression
      # Random Logistic regression with day to day variability taken into account
      glmer_fit <- sub_ds %>% 
        glmer(data = .,
              TEST_RESULT ~ Group + (1 | Date),
              family = binomial(link = "logit"),
              weights = nb_sympt) %>% 
        emmeans(c("Group"), type = "response")
      
      # Emmeans allows you to get mean estimates per group from the model
      logit_emmeans <- emmeans(glmer_fit, c("Group"), 
                               type = "response", offset = 0) %>%
        data.frame() %>%
        mutate(Week  = unique(sub_ds$Week),
               Age   = unique(sub_ds$Age))
      
      # To get the pairwise comparisons with 95% confidence intervals
      logit_contrast <- emmeans(glmer_fit, pairwise ~ Group,
                                type = "response")$contrast %>% 
        summary(infer = T) %>%
        data.frame() %>%
        mutate(Week  = unique(sub_ds$Week),
               Age   = unique(sub_ds$Age))
      
    } else {
      
      # Simple logistic regression
      glmer_fit <- sub_ds %>% 
        glm(data = ., TEST_RESULT ~ Group,
            family = quasibinomial(link = "logit"), weights = nb_sympt)
      
      # Emmeans allows you to get mean estimates per group from the model
      logit_emmeans <- emmeans(glmer_fit, c("Group"), type = "response") %>%
        data.frame() %>%
        mutate(Week  = unique(sub_ds$Week),
               Age   = unique(sub_ds$Age))
      
      # To get the pairwise comparisons with 95% confidence intervals
      logit_contrast <- emmeans(glmer_fit, pairwise ~ Group,
                                type = "response")$contrast %>% 
        summary(infer = T) %>%
        data.frame() %>%
        mutate(Week  = unique(sub_ds$Week),
               Age   = unique(sub_ds$Age))
      
    } # end else date
    
    
    # Return all the results
    list(
      logit_emmeans    = logit_emmeans,
      logit_contrast   = logit_contrast)
    
  }) # end lapply ee
  
  # Return results split per model rather than age
  list(
    logit_emmeans    = map(res_age, "logit_emmeans") %>% bind_rows(),
    logit_contrast   = map(res_age, "logit_contrast") %>% bind_rows())
  
})

# Get results per model rather week ####
# Logistic regression
logit_emmeans_FR_TND    <- map(mod_FR_TND, "logit_emmeans") %>% bind_rows() %>% 
  relocate(c("Week", "Age"), .before = 1)
logit_contrast_FR_TND   <- map(mod_FR_TND, "logit_contrast") %>% bind_rows() %>%  
  relocate(c("Week", "Age"), .before = 1)

## Plot results for FR Test-Negative ####
# Probas
ggplot(data = logit_emmeans_FR_TND,
       aes(x = Week, y = prob, color = Group, group = Group)) +
  geom_line(linewidth = 1) +
  facet_grid(Age ~ ., scales = "free_y") +
  ylab("Probability") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(breaks = c("21W27", "21W37", "21W47", "22W05", "22W15","22W25", "22W35", "22W45", "23W03"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1))

# Vaccine effectiveness by age group (for each vaccination status)
ggplot(data = logit_contrast_FR_TND %>%
         filter(contrast == "Complet / NV") %>%
         filter(odds.ratio < 5),
       aes(x = Week, y = 1-odds.ratio, color = Age, group = 1)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  facet_grid(Age ~., scales = "free") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Vaccine Effectiveness") +
  scale_x_discrete(breaks = c("21W27", "21W47", "22W15", "22W35", "23W03")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1))

ggplot(data = logit_contrast_FR_TND%>%
         filter(contrast == "1 booster / NV") %>%
         filter(odds.ratio < 5),
       aes(x = Week, y = 1-odds.ratio, color = Age, group = 1)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  facet_grid(Age ~., scales = "free") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Vaccine Effectiveness") +
  scale_x_discrete(breaks = c("21W27", "21W47", "22W15", "22W35", "23W03")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1))

ggplot(data = logit_contrast_FR_TND%>%
         filter(contrast == "2 booster / NV") %>%
         filter(odds.ratio < 5),
       aes(x = Week, y = 1-odds.ratio, color = Age, group = 1)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  facet_grid(Age ~., scales = "free") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Vaccine Effectiveness") +
  scale_x_discrete(breaks = c("21W27", "21W47", "22W15", "22W35", "23W03")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1))


# Comparison FR & BEL -----------------------------------------------------

# We only keep common weeks between FR and BEL
ds_simplified_FR <- data_FR %>% 
  filter(Vacc.stat %in% c("Complet", "NV", "1 booster", "2 booster")) %>% 
  summarise(
    nb = sum(nb),
    .by = c("Week", "Age", "Vacc.stat", "TEST_RESULT")) %>% 
  mutate(#n = n(),
    N = sum(nb),
    .by = c("Week", "Age", "Vacc.stat")) %>% 
  filter(Week %in% unique(data_BEL$Week)) %>%
  filter(N > 30) %>% 
  mutate(Percent = nb / N,
         Group = Vacc.stat,
         Country = "FR")

# Bind dataset and remove week with only one vaccinated group
ds_simplified <- ds_BEL %>% 
  bind_rows(ds_simplified_FR) %>% 
  mutate(Week = factor(Week, levels = unique(Week)))


# Comparison case-control
mod_comp <- lapply(1:length(unique(ds_simplified$Week)), FUN = function(jj){
  
  # Set week
  week_jj <- unique(ds_simplified$Week)[jj]
  print(week_jj)
  
  # Subset on week
  ds_week <- ds_simplified %>% 
    filter(Week == week_jj) %>% 
    droplevels()
  
  # Get age group present within week with more than 1 group
  country_gr <- ds_week %>% 
    select(Country, Group) %>% 
    distinct() %>% 
    count(Country) %>% 
    filter(n > 1) %>% 
    pull(Country)
  
  # Get results
  res_country <- lapply(1:length(country_gr), FUN = function(ee){
    
    # Set age
    country_ee <- country_gr[ee]
    print(ee)
    # Subset data
    sub_ds <- ds_week %>% 
      filter(Country == country_ee) %>% 
      droplevels()
    
    # Simple logistic regression
    glm_fit <- sub_ds %>% 
      glm(data = ., TEST_RESULT ~ Group + Age, # Remove the 'Age' covariate to not consider it in the confounding
          family = quasibinomial(link = "logit"), weights = nb)
    
    # Emmeans allows you to get mean estimates per group from the model
    logit_emmeans <- emmeans(glm_fit, c("Group"), type = "response") %>%
      data.frame() %>%
      mutate(Week      = unique(sub_ds$Week),
             Country   = unique(sub_ds$Country))
    
    # To get the pairwise comparisons with 95% confidence intervals
    logit_contrast <- emmeans(glm_fit, pairwise ~ Group,
                              type = "response")$contrast %>% 
      summary(infer = T) %>%
      data.frame() %>%
      mutate(Week      = unique(sub_ds$Week),
             Country   = unique(sub_ds$Country))
    
    # Return all the results
    list(
      logit_emmeans    = logit_emmeans,
      logit_contrast   = logit_contrast)
    
  }) # end lapply ee
  
  # Return results split per model rather than age
  list(
    logit_emmeans    = map(res_country, "logit_emmeans") %>% bind_rows(),
    logit_contrast   = map(res_country, "logit_contrast") %>% bind_rows())
  
})

# Get results per model rather week ####
# Logistic regression
logit_emmeans_comp   <- map(mod_comp, "logit_emmeans") %>% bind_rows() %>% 
  relocate(c("Week", "Country"), .before = 1)
logit_contrast_comp   <- map(mod_comp, "logit_contrast") %>% bind_rows() %>%  
  relocate(c("Week", "Country"), .before = 1)

# Plot results per country ####
# Probas to be positive by vaccination status
ggplot(data = logit_emmeans_comp%>% 
         filter(prob < 0.99), 
       aes(x = Week, y = prob, color = Group, group = Group)) +
  geom_line(linewidth = 1) +
  facet_grid(Country ~ .) +
  scale_y_continuous(n.breaks = 10) +
  ylab("Probability") +
  scale_x_discrete(breaks = c("21W27", "21W37", "21W47", "22W05", "22W15","22W25", "22W35", "22W45", "23W03"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1))

# VE
ggplot(data = logit_contrast_comp%>% 
         filter(grepl("NV", contrast))%>%
         filter(odds.ratio < 5), 
       aes(x = Week, y = 1 - odds.ratio, color = contrast, group = 1)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  # geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  facet_grid(Country ~ contrast, scales = "free") +
  ylab("Vaccine Effectiveness") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(breaks = c("21W27", "21W47", "22W15", "22W35", "23W03")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1))

# VE with countries on the same graph
ggplot(data = logit_contrast_comp %>% 
         filter(grepl("NV", contrast))%>%
         filter(odds.ratio < 5),
       aes(x = Week, y = 1 - odds.ratio, color = Country, group = Country)) +
  geom_point() +
  geom_line() +
  # geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  facet_grid(.~ contrast, scales = "free") +
  ylab("Vaccine Effectiveness") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(breaks = c("21W27", "21W47", "22W15", "22W35", "23W03")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1))


# Estimations with Week as a confounding ####

# Get age group present within week with more than 1 group
age_gr <- ds_simplified %>% 
  select(Age, Group) %>% 
  distinct() %>% 
  count(Age) %>% 
  filter(n > 1) %>% 
  pull(Age)

# Get results
mod_by_age <- lapply(1:length(age_gr), FUN = function(jj){
  
  # Set age
  age_ee <- age_gr[jj]
  print(jj)
  
  # Subset data on age
  ds_age <- ds_simplified %>% 
    filter(Age == age_ee) %>% 
    droplevels()
  
  # Get age group present within week with more than 1 group
  country_gr <- ds_age %>% 
    select(Country, Group) %>% 
    distinct() %>% 
    count(Country) %>% 
    filter(n > 1) %>% 
    pull(Country)
  
  # Get results
  res_country <- lapply(1:length(country_gr), FUN = function(ee){
    
    # Set age
    country_ee <- country_gr[ee]
    print(ee)
    
    # Subset data
    sub_ds <- ds_age %>% 
      filter(Country == country_ee) %>% 
      droplevels()
    
    # Simple logistic regression
    glm_fit <- sub_ds %>% 
      glm(data = ., TEST_RESULT ~ Group + Week,
          family = quasibinomial(link = "logit"), weights = nb)
    
    # Emmeans allows you to get mean estimates per group from the model
    logit_emmeans <- emmeans(glm_fit, c("Group"), type = "response") %>%
      data.frame() %>%
      mutate(Age       = unique(sub_ds$Age),
             Country   = unique(sub_ds$Country))
    
    # To get the pairwise comparisons with 95% confidence intervals
    logit_contrast <- emmeans(glm_fit, pairwise ~ Group,
                              type = "response")$contrast %>% 
      summary(infer = T) %>%
      data.frame() %>%
      mutate(Age       = unique(sub_ds$Age),
             Country   = unique(sub_ds$Country))
    
    
    # Return all the results
    list(
      logit_emmeans    = logit_emmeans,
      logit_contrast   = logit_contrast)
    
  }) # end lapply ee
  
  # Return results split per model rather than age
  list(
    logit_emmeans    = map(res_country, "logit_emmeans") %>% bind_rows(),
    logit_contrast   = map(res_country, "logit_contrast") %>% bind_rows())
})

# Get results per model rather week ####
# Logistic regression
logit_contrast_by_age   <- map(mod_by_age, "logit_contrast") %>% bind_rows() %>%  
  relocate(c("Age", "Country"), .before = 1)

VE_by_age = logit_contrast_by_age%>%filter(grepl("NV", contrast))

ggplot(subset(VE_by_age, Country == 'FR'), aes(x = Age, y = 1-odds.ratio, fill = contrast)) +
  geom_col(position = "dodge") +
  labs(x = "Age Groups", y = "Vaccine Effectiveness") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Vaccination Status")

ggplot(subset(VE_by_age, Country == 'BEL'), aes(x = Age, y = 1-odds.ratio, fill = contrast)) +
  geom_col(position = "dodge") +
  labs(x = "Age Groups", y = "Vaccine Effectiveness") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Vaccination Status")

# Age adapted comparison ####
mod_comp_noage <- lapply(1:length(unique(ds_simplified$Week)), FUN = function(jj){
  
  # Set week
  week_jj <- unique(ds_simplified$Week)[jj]
  print(week_jj)
  
  # Subset on week
  ds_week <- ds_simplified %>% 
    filter(Week == week_jj) %>% 
    droplevels()
  
  # Get age group present within week with more than 1 group
  country_gr <- ds_week %>% 
    select(Country, Group) %>% 
    distinct() %>% 
    count(Country) %>% 
    filter(n > 1) %>% 
    pull(Country)
  
  # Get results
  res_country <- lapply(1:length(country_gr), FUN = function(ee){
    
    # Set age
    country_ee <- country_gr[ee]
    print(ee)
    # Subset data
    sub_ds <- ds_week %>% 
      filter(Country == country_ee) %>% 
      droplevels()
    
    # Simple logistic regression
    glm_fit <- sub_ds %>% 
      glm(data = ., TEST_RESULT ~ Group, # Remove the 'Age' covariate to not consider it in the confounding
          family = quasibinomial(link = "logit"), weights = nb)
    
    # Emmeans allows you to get mean estimates per group from the model
    logit_emmeans <- emmeans(glm_fit, c("Group"), type = "response") %>%
      data.frame() %>%
      mutate(Week      = unique(sub_ds$Week),
             Country   = unique(sub_ds$Country))
    
    # To get the pairwise comparisons with 95% confidence intervals
    logit_contrast <- emmeans(glm_fit, pairwise ~ Group,
                              type = "response")$contrast %>% 
      summary(infer = T) %>%
      data.frame() %>%
      mutate(Week      = unique(sub_ds$Week),
             Country   = unique(sub_ds$Country))
    
    # Return all the results
    list(
      logit_emmeans    = logit_emmeans,
      logit_contrast   = logit_contrast)
    
  }) # end lapply ee
  
  # Return results split per model rather than age
  list(
    logit_emmeans    = map(res_country, "logit_emmeans") %>% bind_rows(),
    logit_contrast   = map(res_country, "logit_contrast") %>% bind_rows())
  
})

# Get results per model rather week ####
# Logistic regression
logit_emmeans_comp_noage   <- map(mod_comp_noage, "logit_emmeans") %>% bind_rows() %>% 
  relocate(c("Week", "Country"), .before = 1)
logit_contrast_comp_noage   <- map(mod_comp_noage, "logit_contrast") %>% bind_rows() %>%  
  relocate(c("Week", "Country"), .before = 1)

merged_data_age <- merge(logit_contrast_comp, logit_contrast_comp_noage, by = c("Week", "Country", "contrast"))
ggplot(data = merged_data_age %>% 
         filter(grepl("NV", contrast))%>%
         filter(odds.ratio.x < 5) %>%
         filter(odds.ratio.y < 5),
       aes(x = Week, group = 1)) +
  geom_line(aes(y = 1 - odds.ratio.x, color = "Yes")) +
  geom_line(aes(y = 1 - odds.ratio.y, color = "No")) +
  facet_grid(Country ~ contrast, scales = "free") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Vaccine Effectiveness") +
  scale_x_discrete(breaks = c("21W27", "21W47", "22W15", "22W35", "23W03")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1)) +
  scale_color_manual(values = c("Yes" = "#5086FF", "No" = "#22B436")) +
  guides(color = guide_legend(title = "Age adapted ?", 
                              title.position = "top",
                              keywidth = unit(1, "cm"),
                              keyheight = unit(0.5, "cm")))
