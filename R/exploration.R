# R/exploration.R

#  Chargement des packages
library(tidyverse)
library(janitor)

# Chargement des données fusionnées
data <- read_csv("../data/base_fusion.csv") %>%
  clean_names()

# Conversion des colonnes logiques (lgl) en numériques quand possible
data <- data %>%
  mutate(across(where(is.logical), ~ as.numeric(.)))

# Aperçu global
glimpse(data)

# Résumé statistique de toutes les colonnes numériques
summary(data)

# Nombre de valeurs manquantes par colonne
colSums(is.na(data))

# Distribution du taux d'abstention
ggplot(data, aes(x = percent_abs_ins)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution du taux d'abstention",
       x = "Taux d'abstention (%)",
       y = "Nombre de circonscriptions")

# Corrélation entre pauvreté et abstention
ggplot(data, aes(x = taux_de_pauvrete_au_seuil_de_60_percent_du_niveau_de_vie_median_en_percent,
                 y = percent_abs_ins)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Pauvreté vs Abstention",
       x = "Taux de pauvreté (%)",
       y = "Taux d’abstention (%)")

