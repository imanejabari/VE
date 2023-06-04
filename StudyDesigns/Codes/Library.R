###########################################################################
# Libraries & Functions
###########################################################################

# Library -----------------------------------------------------------------
# Read data files
library(readxl)
library(foreign)

# Manage data files
library(tidyverse)
library(plyr)
library(dplyr)
#library(glue)
#library(matrixStats)
library(stringr)
library(reshape)
library(reshape2)
#library(forcats)
library(gtools)

# Plot
library(ggplot2)
#library(cowplot)
#library(RColorBrewer)
library(ggpubr)
#library(gridExtra)
#library(grid)

# Export
library(rtf)

# Analysis
library(MASS)
library(lme4) 
library(rstatix)
library(performance)
library(emmeans)
library(lmerTest)
#library(samplesizeCMH)

# Plot mediation
#require(glue)
#require(DiagrammeR)

# Set up tidylog color printing ####
library(crayon)
library(tidylog)
# crayon <- function(x) cat(green$italic(x), sep = "\n")
# options("tidylog.display" = list(crayon))
# 
# 
# # Functions ---------------------------------------------------------------
# # Source.utf8 ####
# source.utf8 <- function(f) {
#   l <- readLines(f, encoding="UTF-8")
#   eval(parse(text=l), envir=.GlobalEnv)
# }