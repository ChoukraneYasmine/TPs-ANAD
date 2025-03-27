install.packages("caret")
# Charger les bibliothèques nécessaires
library(ggplot2)
library(scales)
library(FactoMineR)
library(factoextra)
library(caret)
# Création du dataset avec les réponses
family_data <- data.frame(
  Q1 = c("Moins de 2 personnes", "3-4 personnes", "5-6 personnes", "Plus de 6 personnes", 
         "3-4 personnes", "Plus de 6 personnes", "Moins de 2 personnes", "5-6 personnes", 
         "3-4 personnes", "Moins de 2 personnes", "Plus de 6 personnes", "3-4 personnes", 
         "Moins de 2 personnes", "5-6 personnes", "3-4 personnes", "Plus de 6 personnes", 
         "Moins de 2 personnes", "5-6 personnes", "3-4 personnes", "Moins de 2 personnes", 
         "3-4 personnes", "Plus de 6 personnes", "Moins de 2 personnes", "5-6 personnes", 
         "3-4 personnes", "Moins de 2 personnes", "5-6 personnes", "3-4 personnes", 
         "Moins de 2 personnes", "Plus de 6 personnes"),
  Q2 = c("Traditionnelle", "Moderne", "Mixte", "Autre", 
         "Moderne", "Mixte", "Traditionnelle", "Autre", 
         "Traditionnelle", "Mixte", "Autre", "Traditionnelle", 
         "Moderne", "Mixte", "Traditionnelle", "Moderne", 
         "Mixte", "Autre", "Traditionnelle", "Moderne", 
         "Mixte", "Autre", "Traditionnelle", "Moderne", 
         "Mixte", "Traditionnelle", "Moderne", "Autre", 
         "Mixte", "Traditionnelle"),
  Q3 = c("Arabe", "Tamazight", "Français", "Arabe", 
         "Tamazight", "Français", "Arabe", "Tamazight", 
         "Arabe", "Français", "Arabe", "Tamazight", 
         "Français", "Arabe", "Tamazight", "Arabe", 
         "Français", "Tamazight", "Arabe", "Français", 
         "Tamazight", "Arabe", "Français", "Tamazight", 
         "Arabe", "Tamazight", "Français", "Arabe", 
         "Tamazight", "Français"),
  Q4 = c("Très importante", "Peu importante", "Assez importante", "Très importante", 
         "Peu importante", "Pas importante", "Très importante", "Peu importante", 
         "Très importante", "Assez importante", "Peu importante", "Très importante", 
         "Peu importante", "Très importante", "Très importante", "Peu importante", 
         "Très importante", "Peu importante", "Assez importante", "Très importante", 
         "Très importante", "Peu importante", "Très importante", "Pas importante", 
         "Assez importante", "Très importante", "Très importante", "Peu importante", 
         "Très importante", "Pas importante"),
  Q5 = c("Les parents", "Les grands-parents", "Les parents", "Les grands-parents", 
         "Les parents", "Personne", "Les grands-parents", "Les aînés", 
         "Les parents", "Les parents", "Les grands-parents", "Les parents", 
         "Les grands-parents", "Les parents", "Les grands-parents", "Les parents", 
         "Les grands-parents", "Les aînés", "Les parents", "Les grands-parents", 
         "Les parents", "Les grands-parents", "Les parents", "Les aînés", 
         "Les parents", "Les grands-parents", "Les parents", "Les aînés", 
         "Les parents", "Personne"),
  Q6 = c("Toujours", "Rarement", "Souvent", "Toujours", 
         "Rarement", "Jamais", "Toujours", "Rarement", 
         "Toujours", "Souvent", "Rarement", "Toujours", 
         "Souvent", "Toujours", "Souvent", "Rarement", 
         "Toujours", "Rarement", "Souvent", "Toujours", 
         "Toujours", "Rarement", "Souvent", "Rarement", 
         "Souvent", "Toujours", "Toujours", "Rarement", 
         "Toujours", "Rarement"),
  Q7 = c("Une fois/semaine", "Rarement", "Tous les jours", "Lors d’occasions", 
         "Une fois/semaine", "Rarement", "Tous les jours", "Rarement", 
         "Tous les jours", "Une fois/semaine", "Rarement", "Tous les jours", 
         "Rarement", "Une fois/semaine", "Tous les jours", "Rarement", 
         "Lors d’occasions", "Rarement", "Une fois/semaine", "Tous les jours", 
         "Une fois/semaine", "Rarement", "Rarement", "Rarement", 
         "Tous les jours", "Lors d’occasions", "Une fois/semaine", "Rarement", 
         "Tous les jours", "Rarement"),
  Q8 = c("La transmission des valeurs", "Les réunions familiales", "Les rituels religieux", 
         "La préservation de l’histoire", "La transmission des valeurs", 
         "Les réunions familiales", "Les rituels religieux", 
         "Les réunions familiales", "Les rituels religieux", 
         "Les réunions familiales", "La transmission des valeurs", 
         "Les réunions familiales", "Les réunions familiales", 
         "Les rituels religieux", "Les réunions familiales", 
         "Les rituels religieux", "La transmission des valeurs", 
         "Les réunions familiales", "Les rituels religieux", 
         "Les réunions familiales", "Les réunions familiales", 
         "Les réunions familiales", "Les rituels religieux", 
         "Les réunions familiales", "Les rituels religieux", 
         "Les réunions familiales", "Les rituels religieux", 
         "Les réunions familiales", "Les réunions familiales", 
         "Les réunions familiales"),
  Q9 = c("Très harmonieuse", "Plutôt harmonieuse", "Très harmonieuse", 
         "Plutôt conflictuelle", "Très harmonieuse", "Très conflictuelle", 
         "Très harmonieuse", "Très harmonieuse", "Très harmonieuse", 
         "Plutôt harmonieuse", "Très harmonieuse", "Très harmonieuse", 
         "Plutôt conflictuelle", "Très harmonieuse", "Plutôt harmonieuse", 
         "Très harmonieuse", "Plutôt harmonieuse", "Très conflictuelle", 
         "Plutôt harmonieuse", "Très harmonieuse", "Très harmonieuse", 
         "Plutôt harmonieuse", "Très harmonieuse", "Très conflictuelle", 
         "Plutôt harmonieuse", "Très harmonieuse", "Très harmonieuse", 
         "Très conflictuelle", "Très harmonieuse", "Très conflictuelle"),
  Q10 = c("Oui, totalement", "Oui, mais exceptions", "Oui, totalement", 
          "Non, peu", "Oui, mais exceptions", "Non, pas du tout", 
          "Oui, totalement", "Oui, mais exceptions", "Oui, totalement", 
          "Oui, mais exceptions", "Non, peu", "Oui, totalement", 
          "Non, peu", "Oui, mais exceptions", "Oui, totalement", 
          "Non, peu", "Oui, totalement", "Non, pas du tout", 
          "Oui, mais exceptions", "Oui, totalement", "Oui, mais exceptions", 
          "Non, peu", "Oui, totalement", "Non, pas du tout", 
          "Oui, mais exceptions", "Oui, totalement", "Oui, mais exceptions", 
          "Non, pas du tout", "Oui, totalement", "Non, pas du tout")
)

# Afficher un aperçu des données
head(family_data)
# Afficher un résumé statistique des données
summary(family_data)

# 1. Nombre de questions et nombre de réponses
cat("Nombre de questions :", ncol(family_data), "\n")
cat("Nombre de réponses :", nrow(family_data), "\n\n")

# 2. Fréquences de chaque modalité pour chaque question
for (col in colnames(family_data)) {
  cat("\nFréquence des modalités pour la question :", col, "\n")
  freq_table <- table(family_data[[col]])
  print(freq_table)
}


# 3. Histogramme des fréquences pour chaque question
for (col in colnames(family_data)) {
col="Q8"
  ggplot(family_data, aes_string(x = col)) +
    geom_bar(aes(y = ..count../sum(..count..)), fill = "lightblue", color = "black") +
    scale_y_continuous(labels = percent_format()) +
    labs(title = paste("Histogramme des fréquences pour :", col),
         x = col,
         y = "Pourcentage") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> plot
  print(plot)
}

# 4. Mode de chaque question
calculate_mode <- function(x) {
  uniq_vals <- unique(x)
  uniq_vals[which.max(tabulate(match(x, uniq_vals)))]
}

cat("\nModes de chaque question :\n")
for (col in colnames(family_data)) {
  cat("Mode pour la question", col, ":", calculate_mode(family_data[[col]]), "\n")
}


# Créer un tableau disjonctif
disjoint_data <- dummyVars(~ ., data = family_data)
disjoint_data <- predict(disjoint_data, newdata = family_data)
disjoint_data <- as.data.frame(disjoint_data)

str(disjoint_data)
# Supprimer les colonnes et lignes vides
disjoint_data <- disjoint_data[, colSums(disjoint_data != 0) > 0]  # Supprime les colonnes vides
disjoint_data <- disjoint_data[rowSums(disjoint_data != 0) > 0, ]  # Supprime les lignes vides

head(disjoint_data)
# Vérifier les valeurs manquantes
sum(is.na(disjoint_data))

# Vérifier les dimensions du tableau
dim(disjoint_data)
disjoint_data <- na.omit(disjoint_data)
disjoint_data_factors <- data.frame(lapply(disjoint_data, factor))
library(FactoMineR)
disjoint_data_factors <- data.frame(lapply(disjoint_data, factor))
afcm_result <- MCA(disjoint_data_factors)
# Extraire les valeurs propres
eig <- afcm_result$eig

# Afficher le tableau des valeurs propres
print(eig)
# Créer le biplot individus-variables
plot(afcm_result, invisible = c("quali.sup", "quanti.sup"), main = "Biplot Individus - Variables")


# Extraire les contributions des individus
contrib_individus <- afcm_result$ind$contrib

# Extraire les contributions des variables
contrib_variables <- afcm_result$var$contrib

# Afficher les contributions des individus
print(contrib_individus)

# Afficher les contributions des variables
print(contrib_variables)

# Afficher les contributions triées pour chaque axe
for (i in 1:ncol(contrib_variables)) {
  cat("Contributions des variables pour l'axe ", i, ":\n", sep = "")
  
  # Trier les contributions de la colonne i (axe i) de la plus grande à la plus petite
  sorted_contrib <- sort(contrib_variables[, i], decreasing = TRUE)
  
  # Afficher les contributions triées
  print(sorted_contrib)
  cat("\n")
}

fviz_mca_ind(afcm_result, 
             geom = "point", 
             label = "none", 
             addEllipses = TRUE, 
             ellipse.level = 0.95, 
             title = "Graphique des individus")


# Graphique des variables
fviz_mca_var(afcm_result, 
             geom = "arrow", 
             col.var = "blue", 
             title = "Graphique des variables")
# Graphique des contributions des individus
fviz_contrib(afcm_result, 
             choice = "ind", 
             axes = 1, 
             top = 10, 
             title = "Contributions des individus à l'axe 1")

fviz_contrib(afcm_result, 
             choice = "ind", 
             axes = 2, 
             top = 10, 
             title = "Contributions des individus à l'axe 2")

# Graphique des modalités des variables
fviz_mca_var(afcm_result, 
             col.var = "darkred", 
             title = "Modalités des variables")

# Graphique des cosinus carrés des variables
fviz_cos2(afcm_result, 
          choice = "var", 
          axes = 1:2, 
          title = "Cosinus carrés des variables")

# Calculer le nombre de modalités par question (variable)
num_modalites <- sapply(family_data, function(x) length(unique(x)))  # Nombre de modalités par variable

# Calculer les fréquences des modalités pour chaque question
modalite_frequencies <- lapply(family_data, function(x) table(x))

# Calculer les contributions des modalités pour chaque variable
contributions <- lapply(modalite_frequencies, function(f) f / sum(f))

# Calculer l'influence ajustée pour chaque variable (question)
# Nous appliquons la normalisation en fonction du nombre de modalités
influence_adjusted <- mapply(function(contrib, num_modal) contrib / num_modal, contributions, num_modalites, SIMPLIFY = FALSE)

# Calculer l'influence totale pour chaque question en sommant les influences ajustées de ses modalités
total_influence <- sapply(influence_adjusted, function(contrib) sum(contrib))

# Trouver les questions les mieux représentées (les plus influentes)
# Tri des questions par influence totale décroissante
questions_representatives <- sort(total_influence, decreasing = TRUE)

# Afficher les questions les mieux représentées
questions_representatives

# Croiser les questions Q7 et Q9 pour créer un tableau de contingence
tableau_contingence <- table(family_data$Q7, family_data$Q9)

# Afficher le tableau de contingence
print(tableau_contingence)

# Effectuer l'Analyse des Correspondances (AFC) sur le tableau de contingence
afc_result <- CA(tableau_contingence)

# Visualiser le biplot des individus et des modalités
fviz_ca_biplot(afc_result)
# Visualiser la contribution des modalités sur le premier axe principal
fviz_contrib(afc_result, choice = "col", axes = 1)

# Visualiser la contribution des modalités sur le deuxième axe principal
fviz_contrib(afc_result, choice = "col", axes = 2)
# Visualiser l'inertie expliquée par chaque axe
fviz_eig(afc_result)




