###########################################################################
# Data
###########################################################################

# Preamble ----------------------------------------------------------------
# Clear Working Directory ####
rm(list=ls())

# Source R codes ####
# Load data
source("Codes/Library.R")

# Data Import -------------------------------------------------------------
# Path ####
Data_path <- "Data/"

# Load original data base ####
raw_data_FR <- read_xlsx(paste0(Data_path, "Data_FR.xlsx"))
raw_data_BEL <- read_xlsx(paste0(Data_path, "Data_BEL.xlsx"))

# FR Data ---------------------------------------------------------
data_FR <- raw_data_FR %>% 
  mutate(Date       = as.Date(date, origin = "1899-12-30"),
         Vaccinated = ifelse(VACCINATED == "vaccinated", 1, 0),
         Week       = format(Date, "%yW%U"),
         # Correct manually one week as issue in the format conversion
         Week       = ifelse(Week == "22W00", "21W52", Week),
         TEST_RESULT = as.factor(TEST_RESULT),
         Group = ifelse(is.na(Time), Vacc.stat, 
                        paste(Vacc.stat, Time)) %>% 
           factor(levels = c("NV", "Primo 0-14j", "Primo 14j","Complet 3m-", 
                             "Complet 3-6m", "Complet 6m+",
                             "1 booster 3m-", "1 booster 3-6m", "1 booster 6m+", 
                             "2 booster 3m-", "2 booster 3-6m", 
                             "2 booster 6m+"))) %>% 
  rename(Age = age)

# BEL Data ---------------------------------------------------------
data_BEL <- raw_data_BEL %>% 
  mutate(Vaccinated = ifelse(VACCINATED == "vaccinated", 1, 0),
         TEST_RESULT = as.factor(TEST_RESULT),
         Week = gsub("_", "W", Week)) %>% 
  rename(Age = age)


# Remove all datasets except the one for Analysis ####
rm(list=setdiff(ls(), c("data_FR", "data_BEL","correct_encoding")))
