# R/exploration.R

#  Chargement des packages
library(tidyverse)
library(janitor)

# Chargement des données fusionnées
data <- read_csv("data/base_fusion.csv") %>%
  clean_names()
