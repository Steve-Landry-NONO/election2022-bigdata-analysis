# Chargement des packages
library(ggplot2)
library(dplyr)
library(readr)
library(ggcorrplot)

# Lecture du fichier fusionné
df <- read_csv("../data/base_fusion.csv")

# Conversion explicite des colonnes ciblées
cols_to_force <- unique(c(
  "taux_de_pauvrete_au_seuil_de_60_percent_du_niveau_de_vie_median_en_percent",
  grep("diplome|niveau", names(df), value = TRUE),
  grep("age|centile", names(df), value = TRUE),
  grep("logement|residence|occupes|maisons|chauffes", names(df), value = TRUE)
))

df[cols_to_force] <- lapply(df[cols_to_force], function(x) suppressWarnings(as.numeric(as.character(x))))

# Extraction des colonnes numériques valides
df_numeric <- df %>%
  select(where(is.numeric)) %>%
  select(where(~ !all(is.na(.)) & sd(., na.rm = TRUE) > 0))

# Nettoyage des lignes incomplètes
df_numeric <- na.omit(df_numeric)

# --- BLOC 1 : Corrélation avec taux de pauvreté ---
poverty_var <- "taux_de_pauvrete_au_seuil_de_60_percent_du_niveau_de_vie_median_en_percent"
if (poverty_var %in% colnames(df_numeric)) {
  cor_with_poverty <- cor(df_numeric, use = "complete.obs")[, poverty_var]
  cor_data <- tibble::enframe(sort(cor_with_poverty, decreasing = TRUE), name = "variable", value = "correlation")
  
  p <- ggplot(cor_data, aes(x = reorder(variable, correlation), y = correlation)) +
    geom_bar(stat = "identity", fill = "darkred") +
    coord_flip() +
    labs(title = "Corrélations avec le taux de pauvreté", x = "", y = "Corrélation") +
    theme_minimal(base_size = 12)
  
  ggsave("../results/corr_poverty.png", p, width = 10, height = 8)
  print(" corr_poverty.png sauvegardé.")
} else {
  print(paste("️ Variable", poverty_var, "absente."))
}

# --- BLOC 2 : Corrélations sur les diplômes ---
education_vars <- grep("diplome|niveau", names(df_numeric), value = TRUE)
if (length(education_vars) > 1) {
  cor_matrix <- cor(df_numeric[, education_vars], use = "complete.obs")
  png("../results/education_correlations.png", width = 1000, height = 800)
  ggcorrplot(cor_matrix, type = "lower", lab = TRUE, lab_size = 2.5,
             colors = c("red", "white", "blue"), 
             title = "Corrélations entre les variables de niveau d’étude",
             ggtheme = theme_minimal())
  dev.off()
  print(" education_correlations.png sauvegardé.")
} else {
  print("️ Pas assez de variables de niveau d’étude détectées.")
}

# --- BLOC 3 : Corrélations sur l’âge ---
age_vars <- grep("age|centile", names(df_numeric), value = TRUE)
if (length(age_vars) > 1) {
  cor_matrix <- cor(df_numeric[, age_vars], use = "complete.obs")
  png("../results/age_correlations.png", width = 1000, height = 800)
  ggcorrplot(cor_matrix, type = "lower", lab = TRUE, lab_size = 2.5,
             colors = c("purple", "white", "green"), 
             title = "Corrélations entre les variables liées à l’âge",
             ggtheme = theme_minimal())
  dev.off()
  print("age_correlations.png sauvegardé.")
} else {
  print("️ Pas assez de variables d’âge détectées.")
}

# --- BLOC 4 : Corrélations sur le logement ---
logement_vars <- grep("logement|residence|occupes|maisons|chauffes", names(df_numeric), value = TRUE)
if (length(logement_vars) > 1) {
  cor_matrix <- cor(df_numeric[, logement_vars], use = "complete.obs")
  png("../results/logement_correlations.png", width = 1000, height = 800)
  ggcorrplot(cor_matrix, type = "lower", lab = TRUE, lab_size = 2.5,
             colors = c("darkgreen", "white", "darkblue"), 
             title = "Corrélations entre les variables de logement",
             ggtheme = theme_minimal())
  dev.off()
  print(" logement_correlations.png sauvegardé.")
} else {
  print("️ Pas assez de variables de logement détectées.")
}
