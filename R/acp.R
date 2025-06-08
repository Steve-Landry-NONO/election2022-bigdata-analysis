# acp.R 

# Chargement des packages nécessaires
library(FactoMineR)
library(factoextra)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(corrplot)
library(ggcorrplot)

# Création du dossier de résultats s'il n'existe pas
if (!dir.exists("../results")) dir.create("../results", recursive = TRUE)

# Chargement des données fusionnées
message("Chargement du fichier base_fusion.csv...")
base_fusion <- read_csv("../data/base_fusion.csv", show_col_types = FALSE)

# Conversion des colonnes logiques en numériques
base_fusion <- base_fusion %>%
  mutate(across(where(is.logical), as.numeric))

# Préparation des données numériques pour l'ACP
message("Préparation des données numériques pour l'ACP...")

# Filtrage plus tolérant : supprime colonnes avec +50% NA, remplace NA restants par la moyenne
seuil_na <- 0.5
base_numeric <- base_fusion %>%
  select(where(is.numeric)) %>%
  select(where(~ mean(is.na(.x)) < seuil_na)) %>%
  mutate(across(everything(), ~ replace_na(.x, mean(.x, na.rm = TRUE)))) %>%
  select(where(~ {
    v <- var(.x, na.rm = TRUE)
    !is.na(v) && v > 0
  }))

cat("Nombre de colonnes numériques après nettoyage :", ncol(base_numeric), "\n")
cat("Nombre de lignes :", nrow(base_numeric), "\n")

if (ncol(base_numeric) < 2 || nrow(base_numeric) < 2) {
  stop("Erreur : données insuffisantes après nettoyage pour réaliser une ACP.")
}

# Résumé statistique
message("Résumé statistique des données utilisées :")
print(summary(base_numeric))

# Histogrammes de variables (max 11)
hist_vars <- names(base_numeric)[1:min(11, ncol(base_numeric))]
for (var in hist_vars) {
  p <- ggplot(base_numeric, aes(x = .data[[var]])) +
    geom_histogram(fill = "steelblue", color = "white", bins = 30) +
    labs(title = paste("Histogramme de", var), x = var, y = "Fréquence")
  tryCatch({
    ggsave(filename = paste0("../results/hist_", var, ".png"), plot = p,
           width = 6, height = 4)
  }, error = function(e) {
    cat("Erreur lors de la sauvegarde de l'histogramme de", var, ":", e$message, "\n")
  })
}

# Boxplots par département (si la colonne est présente)
if ("code_du_departement" %in% names(base_fusion)) {
  box_vars <- names(base_numeric)[1:min(6, ncol(base_numeric))]
  for (var in box_vars) {
    p <- ggplot(base_fusion, aes(x = factor(code_du_departement), y = .data[[var]])) +
      geom_boxplot() +
      labs(title = paste("Distribution de", var, "par département"),
           x = "Département", y = var) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    tryCatch({
      ggsave(filename = paste0("../results/boxplot_", var, ".png"), plot = p,
             width = 7, height = 4)
    }, error = function(e) {
      cat("Erreur lors de la sauvegarde du boxplot de", var, ":", e$message, "\n")
    })
  }
}

# ACP
message("Exécution de l'ACP...")
res_acp <- PCA(base_numeric, scale.unit = TRUE, graph = FALSE)

# Sauvegarde des résultats ACP
saveRDS(res_acp, file = "../results/acp_resultats.rds")

# Graphiques ACP
png("../results/variance_expliquee.png", width = 800, height = 600)
print(fviz_eig(res_acp, addlabels = TRUE, ylim = c(0, 50)))
dev.off()

png("../results/cercle_variables.png", width = 800, height = 600)
print(fviz_pca_var(res_acp, col.var = "contrib",
                   gradient.cols = c("blue", "yellow", "red"), repel = TRUE))
dev.off()

png("../results/individus_pca.png", width = 800, height = 600)
print(fviz_pca_ind(res_acp, col.ind = "cos2",
                   gradient.cols = c("white", "blue", "red"), repel = TRUE))
dev.off()

png("../results/biplot_pca.png", width = 800, height = 600)
print(fviz_pca_biplot(res_acp, repel = TRUE,
                      col.var = "contrib", col.ind = "cos2"))
dev.off()

# Corrélation
cor_matrix <- cor(base_numeric, use = "pairwise.complete.obs")

png("../results/corrplot.png", width = 900, height = 800)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.6)
dev.off()

png("../results/ggcorrplot.png", width = 900, height = 800)
print(ggcorrplot(cor_matrix, hc.order = TRUE, type = "upper"))
dev.off()

# Top 20 corrélations
cor_pairs <- as.data.frame(as.table(cor_matrix))
cor_pairs <- cor_pairs[cor_pairs$Var1 != cor_pairs$Var2, ]
cor_pairs$abs_cor <- abs(cor_pairs$Freq)
top_vars <- unique(c(cor_pairs$Var1[order(-cor_pairs$abs_cor)][1:20],
                     cor_pairs$Var2[order(-cor_pairs$abs_cor)][1:20]))
top_cor <- cor_matrix[top_vars, top_vars]

png("../results/corrplot_top20.png", width = 900, height = 800)
corrplot(top_cor, method = "color", type = "upper", tl.cex = 0.7)
dev.off()
