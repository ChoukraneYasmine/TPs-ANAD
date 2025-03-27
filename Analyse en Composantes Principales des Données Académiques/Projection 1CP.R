# Charger les bibliothèques nécessaires
library(readxl)      # Pour lire les fichiers Excel
library(FactoMineR)  # Pour effectuer l'ACP
library(factoextra)  # Pour visualiser les résultats de l'ACP
library(dplyr)       # Pour manipuler les données

# Étape 1 : Charger les données depuis des fichiers Excel
pv_deliberation <- read_excel("pv_deliberation1.xlsx")
pv_affectation <- read_excel("pv_affectation.xlsx")

# Étape 2 : Retirer les colonnes dont on a pa besion du PV de délibération
colonnes_a_retirer <- c("Moy_S1", "Decision_jury", "Crd_annuel", "Moy_annuelle", 
                        "Rang_annuel", "Moy_rachatS2", "Moy_S2", "Rang_S2", 
                        "Ne_S2", "Groupe_S2", "Moy_rachatS1", "Moy_S1", 
                        "Groupe_S1", "Ne_S1", "Rang_S1", "Crd_S1","Crd_S2","Moy_rachat","UEF1","UEF2","UET1","UED1","UEF4","UET2","UEM1","UEF3")
pv_deliberation <- pv_deliberation %>% select(-all_of(colonnes_a_retirer))
head(pv_deliberation)
# Étape 3 : Joindre les deux PV par le matricule de l'étudiant
merged_data <- merge(pv_deliberation, pv_affectation, by = "Matricule")
etudiant <- merged_data[merged_data$Matricule == "21/0061", ]
merged_data <- merged_data[merged_data$Matricule != "21/0061", ]
head(merged_data)
#
# Étape 4 : Supprimer la colonne "Matricule" car elle n'est pas utile pour l'ACP
merged_data <- merged_data %>% select(-Matricule)
head(merged_data)
# Étape 5 : Enlever la colonne "Affectation" car elle est qualitative
data_for_acp <- merged_data %>% select(-Affectation)
head(data_for_acp)
# Étape 6 : Effectuer l'ACP sur les données
acp_result <- PCA(data_for_acp, graph = TRUE)
# Visualiser les résultats initiaux
fviz_pca_ind(acp_result, label = "none", habillage =as.factor(merged_data$Affectation)) +
  ggtitle("ACP initiale") +
  theme_minimal()
# Visualiser la contribution des variables à l'Axe 1
fviz_contrib(acp_result, choice = "var", axes = 1, top = 16) +
  ggtitle("Contribution des variables à l'Axe 1") +
  theme_minimal()
# Visualiser la contribution des variables à l'Axe 2
fviz_contrib(acp_result, choice = "var", axes = 2, top = 16) +
  ggtitle("Contribution des variables à l'Axe 2") +
  theme_minimal()

# Étape 7 : Identifier et retirer les individus aberrants (outliers)
# Utilisation de la distance de Mahalanobis
distance_mahalanobis <- mahalanobis(data_for_acp, 
                                    center = colMeans(data_for_acp), 
                                    cov = cov(data_for_acp))

# Définir un seuil basé sur le chi-deux
threshold <- qchisq(0.975, df = ncol(data_for_acp))  # Seuil à 97.5%
outliers <- which(distance_mahalanobis > threshold)

# Supprimer les individus aberrants
cleaned_data <- data_for_acp[-outliers, ]
merged_data_cleaned <- merged_data[-outliers, ]
nrow(cleaned_data)
nrow(merged_data_cleaned)

# Étape 8 : Refaire l'ACP sur le dataset nettoyé
acp_result_cleaned <- PCA(cleaned_data, graph = TRUE)

# Visualiser les résultats après suppression des outliers
fviz_pca_ind(acp_result_cleaned, label = "none", habillage = as.factor(merged_data_cleaned$Affectation)) +
  ggtitle("ACP après suppression des outliers") +
  theme_minimal()

# Étape 9 : Se situer sur le nuage des individus
# Projeter l'étudiant retiré sur le nuage ACP
projection_etudiant <- predict(acp_result_cleaned, newdata = etudiant_data)  # 'etudiant_data' doit être utilisé ici
print(projection_etudiant)

x_coord <- projection_etudiant$coord[1, "Dim.1"]
y_coord <- projection_etudiant$coord[1, "Dim.2"]

# Créer un graphique ACP et ajouter la position de l'étudiant
fviz_pca_ind(acp_result_cleaned, label = "none") +
  geom_point(aes(x = x_coord, y = y_coord), 
             color = "blue", size = 3) +
  ggtitle("Position de l'étudiant retiré sur le nuage ACP") +
  theme_minimal()