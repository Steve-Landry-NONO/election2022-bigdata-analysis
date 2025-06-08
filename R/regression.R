
# Chargement des librairies nécessaires
library(readr)
library(ggplot2)
library(broom)


# Chargement des données
df <- read_csv("../data/base_fusion.csv")

# Vérification de la cible
if (!"taux_de_pauvrete_au_seuil_de_60_percent_du_niveau_de_vie_median_en_percent" %in% names(df)) {
  stop(" La variable cible est absente du jeu de données.")
}

sum_na <- sum(is.na(df$taux_de_pauvrete_au_seuil_de_60_percent_du_niveau_de_vie_median_en_percent))
cat("Nombre de NA dans la variable cible :", sum_na, "\n")


# Filtrage pour conserver uniquement les lignes complètes
df <- df[complete.cases(df), ]

# ===============================
# 1. Régression sur le taux d’abstention
# ===============================
model_abstention <- lm(percent_abs_ins ~ age_moyen_de_la_population +
                       part_de_la_population_active_en_emploi_en_percent +
                       part_des_residences_principales_en_percent_des_logements +
                       taux_de_pauvrete_au_seuil_de_60_percent_du_niveau_de_vie_median_en_percent,
                       data = df)

# Résumé du modèle
summary_abstention <- summary(model_abstention)
capture.output(summary_abstention, file = "../results/regression_abstention.txt")

# Graphique des résidus
res_abst <- augment(model_abstention)
ggplot(res_abst, aes(.fitted, .resid)) +
  geom_point(color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Résidus vs Valeurs ajustées (Abstention)",
       x = "Valeurs ajustées", y = "Résidus") +
  theme_minimal()
ggsave("../results/residus_abstention.png")

# ===============================
# 2. Régression sur le score de Marine Le Pen
# ===============================
model_lepen <- lm(x53 ~ age_moyen_de_la_population +
                  part_de_la_population_active_en_emploi_en_percent +
                  taux_de_pauvrete_au_seuil_de_60_percent_du_niveau_de_vie_median_en_percent +
                  part_de_la_population_active_pas_ou_peu_diplome_en_percent,
                  data = df)

summary_lepen <- summary(model_lepen)
capture.output(summary_lepen, file = "../results/regression_lepen.txt")

res_lepen <- augment(model_lepen)
ggplot(res_lepen, aes(.fitted, .resid)) +
  geom_point(color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Résidus vs Valeurs ajustées (Score Le Pen)",
       x = "Valeurs ajustées", y = "Résidus") +
  theme_minimal()
ggsave("../results/residus_lepen.png")

cat(" Régressions terminées et fichiers sauvegardés dans le dossier results/\n")
