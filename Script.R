# Kobayashi Maru

#Install and load packages.
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

# Import data.
raw_data <- read.csv("study2_n1.csv")
working <- raw_data

#' No treatment --> higher QoL? Explanation might be possibly less severe 
#' disease and also effect of chemotherapy.
str(working)
glimpse(working)
summary(working)
status(working)

# Remove irrelevant variables and recode factors.

# EDA.
