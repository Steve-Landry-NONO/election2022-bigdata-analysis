# R/exploration.R

#  Chargement des packages
library(tidyverse)
library(janitor)
#Pour visualisation
library(corrplot) 
library(ggcorrplot)
library(ggplot2) # pour les histogrammes des variables clés

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

# Sélection des colonnes numériques pertinentes
numeric_data <- select_if(data, is.numeric)

# Suppression des colonnes avec peu de variance (optionnel mais utile)
numeric_data <- numeric_data[, sapply(numeric_data, function(x) {
  s <- sd(x, na.rm = TRUE)
  !is.na(s) && s > 0
})]

# Sélection des 50 premières variables pour éviter la surcharge
selected_vars <- numeric_data[, 1:50]

# Calcul de la matrice de corrélation
cor_matrix_corrplot <- cor(selected_vars, use = "pairwise.complete.obs")

# Sauvegarde du graphique
png("../results/corrplot.png", width = 1200, height = 1000)
corrplot(
  cor_matrix_corrplot,
  method = "color",
  type = "lower",
  col = colorRampPalette(c("red", "white", "blue"))(200),
  addCoef.col = "black",
  tl.col = "black",
  tl.cex = 0.8,
  number.cex = 0.6,
  mar = c(0, 0, 2, 0),
  title = "Carte de chaleur des corrélations (corrplot)"
)
dev.off()

# Visualisation plus simple avec ggcorrplot
png("../results/ggcorrplot.png", width = 1600, height = 1200, res = 200)
ggcorrplot(cor_matrix, type = "lower", lab = TRUE, lab_size = 1.5,
           colors = c("blue", "white", "red"), title = "Matrice de corrélation")
dev.off()

# ggcorrplot pour les mêmes variables
corr_matrix <- cor(numeric_data, use = "complete.obs")
# Amélioration de la lisibilité du ggcorrplot
p2 <- ggcorrplot(
  cor_matrix,
  type = "lower",
  lab = TRUE,
  lab_size = 2.5,
  colors = c("red", "white", "blue"),
  title = "Matrice de Corrélation (ggcorrplot)",
  ggtheme = theme_minimal(base_size = 14)
) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("../results/ggcorrplot.png", plot = p2, width = 12, height = 10, dpi = 300)


# ────────────────────────────────────────────────
# Histogrammes de variables clés
# ────────────────────────────────────────────────

# Variables à tracer
vars_hist <- c("inscrits", "abstentions", "votants", "exprimes", 
               "blancs", "nuls", "voix", 
               "percent_abs_ins", "percent_vot_ins", 
               "percent_blancs_ins", "percent_nuls_ins",
               "percent_exp_ins", "percent_voix_ins")

# Générer les histogrammes et les sauvegarder
for (var in vars_hist) {
  p <- ggplot(data, aes_string(x = var)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    theme_minimal() +
    labs(title = paste("Distribution de", var),
         x = var, y = "Fréquence")
  
  ggsave(filename = paste0("../results/hist_", var, ".png"), plot = p, width = 8, height = 6)
}

# ────────────────────────────────────────────────
# Boxplots de variables clés par département
# ────────────────────────────────────────────────

# Vérifier que la variable département existe
if ("libelle_du_departement" %in% colnames(data)) {
  
  # Sélection de variables pertinentes pour les boxplots
  vars_box <- c("abstentions", "votants", "exprimes", 
                "percent_abs_ins", "percent_vot_ins", 
                "percent_exp_ins")
  
  for (var in vars_box) {
    p <- ggplot(data, aes_string(x = "libelle_du_departement", y = var)) +
      geom_boxplot(fill = "steelblue", color = "black", outlier.size = 1) +
      theme_minimal() +
      labs(title = paste("Boxplot de", var, "par département"),
           x = "Département", y = var) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 9, face = "bold"))
    # theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))

    
    ggsave(filename = paste0("../results/boxplot_", var, "_departement.png"), 
           plot = p, width = 10, height = 6)
  }
  
} else {
  message("Variable libelle_du_departement introuvable pour les boxplots.")
}

# Calcul des corrélations complètes
cor_matrix_full <- cor(numeric_data, use = "pairwise.complete.obs")

# Sélection des 20 variables avec la plus forte somme absolue de corrélation
cor_sums <- apply(abs(cor_matrix_full), 2, sum)
top20_vars <- names(sort(cor_sums, decreasing = TRUE))[1:20]

# Matrice réduite aux top 20
cor_matrix_top20 <- cor_matrix_full[top20_vars, top20_vars]

# Sauvegarde du plot
png("../results/corrplot_top20.png", width = 1000, height = 800)
corrplot(
  cor_matrix_top20,
  method = "color",
  type = "lower",
  col = colorRampPalette(c("red", "white", "blue"))(200),
  addCoef.col = "black",
  tl.col = "black",
  tl.cex = 0.9,
  number.cex = 0.75,
  mar = c(0, 0, 2, 0),
  title = "Corrélations des 20 variables numériques les plus liées"
)
dev.off()