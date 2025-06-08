# exploration_ciblee_robuste.R - Version corrigée et robuste

library(tidyverse)
library(ggcorrplot)

df <- read.csv("../data/base_fusion.csv")

# Fonction utilitaire
get_columns_with_keyword <- function(df, keyword) {
  colnames(df)[str_detect(colnames(df), regex(keyword, ignore_case = TRUE))]
}

# On convertit toutes les colonnes numériques possibles
df_cleaned <- df %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.)))) %>%
  select(where(~ sd(., na.rm = TRUE) > 0))  # garder seulement les colonnes avec variance

# --- Bloc Pauvreté ---
poverty_var <- "taux_de_pauvrete_au_seuil_de_60_percent_du_niveau_de_vie_median_en_percent"

if (poverty_var %in% colnames(df_cleaned)) {
  numeric_df <- df_cleaned %>% mutate(across(everything(), ~ replace_na(., 0)))
  cor_matrix <- cor(numeric_df, use = "complete.obs")
  if (!is.na(cor_matrix[poverty_var, poverty_var])) {
    poverty_cor <- cor_matrix[, poverty_var]
    top20_poverty <- sort(abs(poverty_cor), decreasing = TRUE)[2:21]
    png("../results/corr_poverty.png", width = 1000, height = 800)
    ggcorrplot(cor(numeric_df[, names(top20_poverty)], use = "complete.obs"),
               lab = TRUE, title = "Corrélations avec la pauvreté")
    dev.off()
  } else {
    message("️ Corrélation impossible avec la variable de pauvreté.")
  }
} else {
  message("️ La variable de pauvreté est absente.")
}

# --- Bloc Éducation ---
edu_vars <- get_columns_with_keyword(df, "diplome|niveau_d_etude|etude|scolarite")
edu_df <- df_cleaned %>% select(any_of(edu_vars)) %>% drop_na()

if (ncol(edu_df) >= 2) {
  png("../results/corr_education.png", width = 1000, height = 800)
  ggcorrplot(cor(edu_df, use = "complete.obs"), lab = TRUE, title = "Corrélations éducatives")
  dev.off()
} else {
  message("️ Pas assez de variables éducatives détectées.")
}

# --- Bloc Âge ---
age_vars <- get_columns_with_keyword(df, "age|centile")
age_df <- df_cleaned %>% select(any_of(age_vars)) %>% drop_na()

if (ncol(age_df) >= 2) {
  png("../results/corr_age.png", width = 1000, height = 800)
  ggcorrplot(cor(age_df, use = "complete.obs"), lab = TRUE, title = "Corrélations liées à l'âge")
  dev.off()
} else {
  message("️ Pas assez de variables liées à l'âge.")
}

# --- Bloc Logement ---
logement_vars <- get_columns_with_keyword(df, "logement|résidence|occupes|maisons|habit|ménage")
logement_df <- df_cleaned %>% select(any_of(logement_vars)) %>% drop_na()

if (ncol(logement_df) >= 2) {
  png("../results/corr_logement.png", width = 1000, height = 800)
  ggcorrplot(cor(logement_df, use = "complete.obs"), lab = TRUE, title = "Corrélations liées au logement")
  dev.off()
} else {
  message("️ Pas assez de variables liées au logement.")
}
