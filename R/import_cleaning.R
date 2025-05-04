# R/import_cleaning.R

#  Chargement des packages nécessaires
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)     # pour lire les fichiers Excel
library(janitor)    # pour clean_names()

#  Chargement des données (.xlsx)
elections <- read_excel("data/resultats-par-niveau-cirlg-t1-france-entiere.xlsx")
insee <- read_excel("data/indic-stat-circonscriptions-legislatives-2022.xlsx", skip = 5) 

#  Nettoyage des noms de colonnes
elections <- clean_names(elections)
insee <- clean_names(insee)

# Sélection des colonnes à convertir (exclure les identifiants)
cols_to_numeric <- setdiff(names(insee), c(
  "code_de_la_circonscription_legislative_numero_de_departement_suivi_du_numero_de_circonscription",
  "nom_de_la_circonscription_legislative_nom_du_departement_suivi_du_numero_de_circonscription"
))

# Conversion sécurisée en numeric (suppression éventuelle des % ou virgules si besoin)
insee <- insee %>%
  mutate(across(all_of(cols_to_numeric), ~ as.numeric(gsub(",", ".", gsub("%", "", .)))))

# Aperçu des colonnes disponibles
glimpse(elections)
glimpse(insee)

#  Création d'une clé commune : code de circonscription
# Dans 'elections', on va créer une colonne 'code_circo' combinant le code département et le numéro de circonscription
elections <- elections %>%
  mutate(code_circo = paste0(code_du_departement, code_de_la_circonscription))

# Dans 'insee', on vérifie que la clé est bien 'codcirco' (ou un nom similaire)
#insee <- insee %>%
  #rename(code_circo = codcirco)  # adaptable si nécessaire après un coup d'œil à names(insee)
insee <- insee %>%
  rename(code_circo = code_de_la_circonscription_legislative_numero_de_departement_suivi_du_numero_de_circonscription)

# Fusion des deux bases
base_fusion <- left_join(elections, insee, by = "code_circo")

# Vérification rapide
glimpse(base_fusion)
summary(base_fusion)

#  Export facultatif de la base fusionnée pour réutilisation dans d’autres scripts
write_csv(base_fusion, "data/base_fusion.csv")
