# Charger les bibliothèques nécessaires
library(readxl)      # Pour lire les fichiers Excel
library(FactoMineR)  # Pour effectuer l'ACP
library(factoextra)  # Pour visualiser les résultats de l'ACP
library(dplyr)       # Pour manipuler les données

# Étape 1 : Charger les données depuis des fichiers Excel
pv_deliberation <- read_excel("pv_deliberation2.xlsx")

# Étape 2 :Préparer les données
colonnes_a_retirer <- c("Moy_S1", "Decision_jury", "Moy_annuelle", 
                        "Rang_annuel", "Moy_rachatS2", "Moy_S2", "Rang_S2", 
                        "Ne_S2", "Groupe_S2", "Moy_rachatS1", "Moy_S1", 
                        "Groupe_S1", "Ne_S1", "Rang_S1","Moy_rachat","UEF1.1.1","UEM1.1","UET1.1","UEF1.1.2","UEF1.2.2","UEM1.2","UET1.2","UEF1.2.1")
data <- pv_deliberation %>% select(-all_of(colonnes_a_retirer))
head(data)
etudiant <- data[data$Matricule == "21/0061", ]
data <- data[data$Matricule != "21/0061", ]
data <- data %>% select(-Matricule)
head(data)

# Étape 3 : Effectuer l'ACP sur les données
acp_result<- PCA(data, graph = TRUE)

# Visualiser la contribution des variables à l'Axe 1
fviz_contrib(acp_result, choice = "var", axes = 1, top = 16) +
  ggtitle("Contribution des variables à l'Axe 1") +
  theme_minimal()
# Visualiser la contribution des variables à l'Axe 2
fviz_contrib(acp_result, choice = "var", axes = 2, top = 16) +
  ggtitle("Contribution des variables à l'Axe 2") +
  theme_minimal()
# Étape 4 : Se situer sur le nuage des individus
# Projeter l'étudiant retiré sur le nuage ACP
projection_etudiant <- predict(acp_result, newdata = etudiant)  # 'etudiant_data' doit être utilisé ici
print(projection_etudiant)

x_coord <- projection_etudiant$coord[1, "Dim.1"]
y_coord <- projection_etudiant$coord[1, "Dim.2"]

# Créer un graphique ACP et ajouter la position de l'étudiant
fviz_pca_ind(acp_result, label = "none") +
  geom_point(aes(x = x_coord, y = y_coord), 
             color = "blue", size = 3) +
  ggtitle("Position de l'étudiant retiré sur le nuage ACP") +
  theme_minimal()