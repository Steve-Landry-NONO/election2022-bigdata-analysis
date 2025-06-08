
# correlation_plots.R - Visualisation améliorée des corrélations

# Chargement des librairies
library(tidyverse)
library(ggcorrplot)
library(corrplot)
library(janitor)

# Chargement des données
data <- read_csv("../data/base_fusion.csv") %>% clean_names()

# Sélection des variables numériques
numeric_data <- data %>%
  select(where(is.numeric)) %>%
  select(where(~ sd(., na.rm = TRUE) > 0))

# Calcul de la matrice de corrélation
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

# --- 1. ggcorrplot amélioré sur une sélection de variables électorales ---
selected_vars <- c("inscrits", "abstentions", "percent_abs_ins",
                   "votants", "percent_vot_ins", "blancs", "percent_blancs_ins",
                   "nuls", "percent_nuls_ins", "exprimes", "percent_exp_ins",
                   "percent_voix_exp", "percent_voix_ins", "voix")

cor_selected <- cor_matrix[selected_vars, selected_vars]

ggcorrplot(cor_selected,
           method = "square",
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           colors = c("#B2182B", "white", "#2166AC"),
           title = "Corrélation entre variables électorales",
           tl.cex = 10,
           tl.col = "black",
           ggtheme = theme_minimal())

ggsave("../results/ggcorrplot_cleaned.png", width = 10, height = 8)

# --- 2. corrplot sur les 20 variables les plus corrélées ---
# Calcul des moyennes absolues des corrélations
mean_corr <- apply(abs(cor_matrix), 2, mean)
top_vars <- names(sort(mean_corr, decreasing = TRUE))[1:20]

cor_top <- cor_matrix[top_vars, top_vars]

# Graphique corrplot
png("../results/corrplot_top20.png", width = 1000, height = 800)
corrplot(cor_top, method = "color", type = "lower",
         col = colorRampPalette(c("blue", "white", "red"))(200),
         tl.col = "black", tl.cex = 0.8, number.cex = 0.6,
         title = "Corrélations des 20 variables numériques les plus liées")
dev.off()
