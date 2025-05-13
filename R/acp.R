# acp.R

# Chargement des packages nécessaires
library(FactoMineR)
library(factoextra)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)


#  Chargement des données fusionnées
message("Chargement du fichier base_fusion.csv...")
base_fusion <- read_csv("../data/base_fusion.csv", show_col_types = FALSE)

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



