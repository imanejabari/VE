###########################################################################
# 14 days Analysis
###########################################################################

# Preamble ----------------------------------------------------------------
# Clear Working Directory ####
rm(list=ls())

# Source R codes ####
# Library
source("Codes/Library.R")

# Load data
source("Codes/Data.R")


# Primo-vaccination comparison between only more than 14 days vaccinated VS all vaccinated  --------------------

# Primo-vaccinated for more than 14 days
ds_primo_eff <- data_FR %>% 
  filter(Group %in% c("Primo 14j", "NV")) %>% 
  summarise(
    nb = sum(nb),
    .by = c("Date", "Vacc.stat", "TEST_RESULT","Age", "Week")) %>% 
  mutate(#n = n(),
    N = sum(nb),
    .by = c("Date", "Vacc.stat","Age")) %>% 
  filter(N != 0) %>% 
  mutate(Percent = nb / N,
         Group = Vacc.stat)

# Model #####
mod_primo_eff <- lapply(1:length(unique(ds_primo_eff$Week)), FUN = function(jj){
  
  # Set week
  week_jj <- unique(ds_primo_eff$Week)[jj]
  print(week_jj)
  
  # Subset on week
  ds_week <- ds_primo_eff %>% 
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
      logit_emmeans   = logit_emmeans,
      logit_contrast  = logit_contrast)
    
  }) # end lapply ee
  
  # Return results split per model rather than age
  list(
    logit_emmeans    = map(res_age, "logit_emmeans") %>% bind_rows(),
    logit_contrast   = map(res_age, "logit_contrast") %>% bind_rows())
})

# Get results per model rather week ####
# Logistic regression
logit_emmeans_primo_eff    <- map(mod_primo_eff, "logit_emmeans") %>% bind_rows() %>% 
  relocate(c("Week", "Age"), .before = 1)
logit_contrast_primo_eff   <- map(mod_primo_eff, "logit_contrast") %>% bind_rows() %>%  
  relocate(c("Week", "Age"), .before = 1)


# All primo-vaccinated
ds_primo_unef <- data_FR %>% 
  filter(Vacc.stat %in% c("Primo", "NV")) %>% 
  summarise(
    nb = sum(nb),
    .by = c("Date", "Vacc.stat", "TEST_RESULT","Age", "Week")) %>% 
  mutate(#n = n(),
    N = sum(nb),
    .by = c("Date", "Vacc.stat","Age")) %>% 
  filter(N != 0) %>% 
  mutate(Percent = nb / N,
         Group = Vacc.stat)

# Model ####
mod_primo_unef <- lapply(1:length(unique(ds_primo_unef$Week)), FUN = function(jj){
  
  # Set week
  week_jj <- unique(ds_primo_unef$Week)[jj]
  print(week_jj)
  
  # Subset on week
  ds_week <- ds_primo_unef %>% 
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
      logit_emmeans   = logit_emmeans,
      logit_contrast  = logit_contrast)
    
  }) # end lapply ee
  
  # Return results split per model rather than age
  list(
    logit_emmeans    = map(res_age, "logit_emmeans") %>% bind_rows(),
    logit_contrast   = map(res_age, "logit_contrast") %>% bind_rows())
})

# Get results per model rather week ####
# Logistic regression
logit_emmeans_primo_unef    <- map(mod_primo_unef, "logit_emmeans") %>% bind_rows() %>% 
  relocate(c("Week", "Age"), .before = 1)
logit_contrast_primo_unef   <- map(mod_primo_unef, "logit_contrast") %>% bind_rows() %>%  
  relocate(c("Week", "Age"), .before = 1)


# VE primo-vacc VS primo-vacc 14days
merged_data_FR_primo <- merge(logit_contrast_primo_eff, logit_contrast_primo_unef, by = c("Week", "Age", "contrast"))
ggplot(data = merged_data_FR_primo %>% 
         filter(odds.ratio.x < 5) %>%
         filter(odds.ratio.y < 5),
       aes(x = Week, group = 1)) +
  geom_line(aes(y = 1 - odds.ratio.x, color = "Primo-vaccinated")) +
  geom_line(aes(y = 1 - odds.ratio.y, color = "Primo vaccinated +14d")) +
  facet_grid(Age ~ contrast, scales = "free") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Vaccine Effectiveness") +
  scale_x_discrete(breaks = c("21W27", "21W47", "22W15", "22W35", "23W03")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1)) +
  scale_color_manual(values = c("Primo-vaccinated" = "#5086FF", "Primo vaccinated +14d" = "#22B436")) +
  guides(color = guide_legend(title = "Vaccination Status", 
                              title.position = "top",
                              keywidth = unit(1, "cm"),
                              keyheight = unit(0.5, "cm")))
