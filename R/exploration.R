# R/exploration.R

#  Chargement des packages
library(tidyverse)
library(janitor)

# Chargement des données fusionnées
data <- read_csv("data/base_fusion.csv") %>%
  clean_names()

# Aperçu global
glimpse(data)

# Résumé statistique de toutes les colonnes numériques
summary(data)

# Nombre de valeurs manquantes par colonne
colSums(is.na(data))
