# Analyse descriptive 
# Chargement des données (le jeu 'infert' est intégré à R)
data(infert)

# Inspection rapide avant de commencer 
str(infert)
summary(infert)
View(infert) # Pour visualiser les données

# On crée une fonction pour les statistiques descriptives
# Cette fonction calcule les statistiques pour une variable donnée
# Moyenne, Médiane, Écart-type sont calculés uniquement si la variable est numérique
# Mode et Fréquence sont calculés pour toutes les variables
summary_stats <- function(x) {
  var_type <- class(x)
  # Calcul du Mode (valeur la plus fréquente)
  mode_table <- table(x)
  mode_value <- names(sort(mode_table, decreasing = TRUE))[1]
  mode_freq <- max(mode_table)
  
  # Calcul des statistiques numériques 
  if (is.numeric(x) || is.integer(x)) {
    moy <- mean(x, na.rm=TRUE)
    med <- median(x, na.rm=TRUE)
    sd_val <- sd(x, na.rm=TRUE)
  } else {
    # NA pour les variables qualitatives (éducation)
    moy <- NA
    med <- NA
    sd_val <- NA
  }
  
  # Fréquence (nombre d'observations non manquantes)
  freq <- length(x) - sum(is.na(x))
  
  return(data.frame(
    Type = var_type,
    Moyenne = moy,
    Mediane = med,
    Ecart_type = sd_val,
    Mode = mode_value,
    Mode_freq = mode_freq,
    Frequence = freq
  ))
}

# Variables à inclure dans le tableau descriptif 
# On exclu stratum et pooled.stratum car variable pas intéressante pour une analyse descriptive

variables_a_analyser <- c("age", "parity", "induced", "case", "spontaneous", "education")

df_subset <- infert[, variables_a_analyser]

# Application de la fonction à chaque colonne pour créer le tableau de synthèse
resultats <- lapply(df_subset, summary_stats)
resultats_df <- do.call(rbind, resultats)
rownames(resultats_df) <- variables_a_analyser
View(resultats_df)

# 2. Croisement par groupe

# On installe dplyr si ce n'est pas déjà fait et on charge ensuite la librairie

library(dplyr)

# Facteur de groupe que l'on a choisi : 'case'  qui est le statut d'infertilité
# On compare les variables par Cas (1) = femme infertile vs Contrôles (0) = femme fertile

# Comparaison des Variables quantitatifs par groupe 'case'
resultats_quantitatifs_par_case <- infert |>
  group_by(case) |>
  summarise(
    N_groupe = n(),
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    parity_mean = mean(parity, na.rm = TRUE),
    parity_sd = sd(parity, na.rm = TRUE),
    spontaneous_mean = mean(spontaneous, na.rm = TRUE),
    spontaneous_sd = sd(spontaneous, na.rm = TRUE)
  )
# On peux aussi utiliser View(resultats_quantitatifs_par_case) si on le préfère
print(resultats_quantitatifs_par_case)

# Comparaison de la variable qualitative 'education' par groupe 'case'
# On utilise la fonction 'table' pour obtenir les fréquences croisées.

table_croisee <- table(infert$case, infert$education)

#On peux aussi utiliser View ici
print(table_croisee)

# Pour une meilleure interprétation on calcule les pourcentages par colonne (chaque groupe 'case' totalise les 100%)
# La fonction prop.table(table_croisee, margin = 1) nous calcule les proportions par ligne

proportions_education <- prop.table(table_croisee, margin = 1) * 100
print(proportions_education)

# 4. On va maintenant faire un boxplot pour montrer les résultats de l'étude utile pour notre diapo
boxplot(age ~ case, data = infert, 
        main = "Distribution de l'Âge par Statut d'Infertilité",
        xlab = "Statut d'infertilité (0=Contrôle/Fertile, 1=Cas/Infertile)",
        ylab = "Âge")

