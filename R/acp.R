# acp.R

# Chargement des packages nécessaires
library(FactoMineR)
library(factoextra)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(corrplot)     # pour les cartes de chaleur de corrélation
library(ggcorrplot)   # alternative ggplot2 pour les corrélations


#  Chargement des données fusionnées
message("Chargement du fichier base_fusion.csv...")
base_fusion <- read_csv("../data/base_fusion.csv", show_col_types = FALSE)

# Conversion des colonnes logiques en numériques lorsqu'elles
# correspondent à des indicateurs codés 0/1
base_fusion <- base_fusion %>%
  mutate(across(where(is.logical), as.numeric))

# Préparation des données
#  Sélection des variables numériques pertinentes pour l'ACP
# On retire les identifiants, noms, et colonnes textuelles
message("Préparation des données numériques pour l'ACP...")

base_numeric <- base_fusion %>%
  dplyr::select(where(is.numeric)) %>%
  tidyr::drop_na() %>%
  dplyr::select(where(~ var(.x, na.rm = TRUE) > 0))  # Retire les colonnes constantes

cat("Nombre de colonnes numériques après nettoyage :", ncol(base_numeric), "\n")
cat("Nombre de lignes après suppression des NA :", nrow(base_numeric), "\n")

# Vérification rapide et Résumé statistique
message("Résumé statistique des données utilisées :")
print(summary(base_numeric))

# -----------------------
# Visualisations préliminaires
# -----------------------

# Sélection de 11 variables numériques pour les histogrammes
hist_vars <- names(base_numeric)[1:min(11, ncol(base_numeric))]
for (var in hist_vars) {
  p <- ggplot(base_numeric, aes(x = .data[[var]])) +
    geom_histogram(fill = "steelblue", color = "white", bins = 30) +
    labs(title = paste("Histogramme de", var), x = var, y = "Fréquence")
  ggsave(filename = paste0("../results/hist_", var, ".png"), plot = p,
         width = 6, height = 4)
}

# Sélection de 6 variables numériques pour les boxplots par département
if ("code_du_departement" %in% names(base_fusion)) {
  box_vars <- names(base_numeric)[1:min(6, ncol(base_numeric))]
  for (var in box_vars) {
    p <- ggplot(base_fusion, aes(x = factor(code_du_departement), y = .data[[var]])) +
      geom_boxplot() +
      labs(title = paste("Distribution de", var, "par département"),
           x = "Département", y = var) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggsave(filename = paste0("../results/boxplot_", var, ".png"), plot = p,
           width = 7, height = 4)
  }
}


#  Réalisation de l'ACP
message("Exécution de l'ACP...")
res_acp <- PCA(base_numeric, scale.unit = TRUE, graph = FALSE)

#  Visualisation de la variance expliquée
fviz_eig(res_acp, addlabels = TRUE, ylim = c(0, 50)) +
  ggtitle("Pourcentage de variance expliquée par composante")

#  Graphique des individus
fviz_pca_ind(res_acp,
             geom.ind = "point",
             pointshape = 21,
             pointsize = 2,
             col.ind = "cos2", # Coloration selon la qualité de représentation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) +
  ggtitle("Projection des circonscriptions sur les axes principaux")

#  Graphique des variables
fviz_pca_var(res_acp,
             col.var = "contrib", # Coloration selon la contribution
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) +
  ggtitle("Corrélation des variables avec les axes")

#  Contributions aux axes (Top contributions)
cat("\n Top 10 variables contributives — Axe 1 :\n")
print(head(sort(res_acp$var$contrib[, 1], decreasing = TRUE), 10))

cat("\n Top 10 variables contributives — Axe 2 :\n")
print(head(sort(res_acp$var$contrib[, 2], decreasing = TRUE), 10))

#exports
message("Export des résultats d'ACP...")


eig_vals <- get_eigenvalue(res_acp)
write_csv(as.data.frame(eig_vals), "../results/acp_variance.csv")

# Coordonnées des individus
write_csv(as.data.frame(res_acp$ind$coord), "../results/acp_ind_coord.csv")

# Coordonnées des variables
write_csv(as.data.frame(res_acp$var$coord), "../results/acp_var_coord.csv")

message("ACP terminée et résultats exportés dans le dossier results/.")

# Graphique : Variance expliquée
png(filename = "../results/variance_expliquee.png", width = 800, height = 600)
barplot(res_acp$eig[, 2],
        names.arg = paste0("Dim", 1:nrow(res_acp$eig)),
        main = "Pourcentage de variance expliquée par dimension",
        col = "steelblue")
dev.off()

# Projections : individus
png(filename = "../results/individus_acp.png", width = 800, height = 600)
fviz_pca_ind(res_acp,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
dev.off()

# Projections : variables
png(filename = "../results/variables_acp.png", width = 800, height = 600)
fviz_pca_var(res_acp,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
dev.off()

# Contributions : Dim 1
png(filename = "../results/top_variables_dim1.png", width = 800, height = 600)
fviz_contrib(res_acp, choice = "var", axes = 1, top = 10)
dev.off()

# Contributions : Dim 2
png(filename = "../results/top_variables_dim2.png", width = 800, height = 600)
fviz_contrib(res_acp, choice = "var", axes = 2, top = 10)
dev.off()

saveRDS(res_acp, file = "../results/acp_resultats.rds")


# Variance expliquée
png("../results/variance_expliquee.png", width = 800, height = 600)
print(fviz_eig(res_acp, addlabels = TRUE, ylim = c(0, 50)))
dev.off()

# Cercle des corrélations (variables)
png("../results/cercle_variables.png", width = 800, height = 600)
print(fviz_pca_var(res_acp, col.var = "contrib", gradient.cols = c("blue", "yellow", "red"), repel = TRUE))
dev.off()

# Projection des individus
png("../results/individus_pca.png", width = 800, height = 600)
print(fviz_pca_ind(res_acp, col.ind = "cos2", gradient.cols = c("white", "blue", "red"), repel = TRUE))
dev.off()

# Biplot (individus + variables)
png("../results/biplot_pca.png", width = 800, height = 600)
print(fviz_pca_biplot(res_acp, repel = TRUE, col.var = "contrib", col.ind = "cos2"))
dev.off()

# -----------------------
# Analyses de corrélation
# -----------------------

cor_matrix <- cor(base_numeric, use = "pairwise.complete.obs")

# Carte de chaleur générale
png("../results/corrplot.png", width = 900, height = 800)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.6)
dev.off()

# Version ggcorrplot
png("../results/ggcorrplot.png", width = 900, height = 800)
print(ggcorrplot(cor_matrix, hc.order = TRUE, type = "upper"))
dev.off()

# Corrélations des 20 variables les plus liées entre elles
cor_pairs <- as.data.frame(as.table(cor_matrix))
cor_pairs <- cor_pairs[cor_pairs$Var1 != cor_pairs$Var2, ]
cor_pairs$abs_cor <- abs(cor_pairs$Freq)
top_vars <- unique(c(cor_pairs$Var1[order(-cor_pairs$abs_cor)][1:20],
                     cor_pairs$Var2[order(-cor_pairs$abs_cor)][1:20]))
top_cor <- cor_matrix[top_vars, top_vars]

png("../results/corrplot_top20.png", width = 900, height = 800)
corrplot(top_cor, method = "color", type = "upper", tl.cex = 0.7)
dev.off()

# -----------------------
# Notes et limites rencontrées
# -----------------------
# Certaines colonnes importées possédaient un type incorrect (logique)
# ou contenaient beaucoup de valeurs manquantes. Elles sont converties
# et filtrées ci-dessus avant calcul des corrélations et de l'ACP.
# Les noms de colonnes très longs issus des fichiers sources peuvent
# également complexifier la lisibilité des graphiques.



