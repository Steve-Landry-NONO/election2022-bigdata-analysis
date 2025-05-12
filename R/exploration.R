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

# Corrélation chômage vs abstention
ggplot(data, aes(x = part_de_la_population_active_au_chomage_en_percent,
                 y = percent_abs_ins)) +
  geom_point(alpha = 0.6, color = "darkorange") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Corrélation entre chômage et abstention",
    x = "Taux de chômage (%)",
    y = "Taux d’abstention (%)"
  )

# Corrélation niveau d'études (bac+3 et plus) vs abstention
ggplot(data, aes(x = part_de_la_population_active_detentrice_au_mieux_d_un_diplome_de_niveau_bac_3_ou_plus_en_percent,
                 y = percent_abs_ins)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue") +
  labs(
    title = "Corrélation entre niveau d'études (≥ bac+3) et abstention",
    x = "Part des diplômés bac+3 ou plus (%)",
    y = "Taux d’abstention (%)"
  )

# Corrélation niveau de vie médian vs abstention
ggplot(data, aes(x = niveau_de_vie_median_en_percent,
                 y = percent_abs_ins)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(
    title = "Corrélation entre niveau de vie médian et abstention",
    x = "Niveau de vie médian (en % du national)",
    y = "Taux d’abstention (%)"
  )
