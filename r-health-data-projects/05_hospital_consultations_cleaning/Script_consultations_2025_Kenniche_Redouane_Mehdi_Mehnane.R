# 1. On commence par charger la base de donnée
df_base <- read.csv("C:/Users/Redouane/Documents/M1 IDS/Technique données 1/Nettoyage et visualisation/consultations_2025.csv", sep=",", header = T,stringsAsFactors = FALSE, 
               na.strings = c("", "nan", "NaN", "NA"))
#  CHARGEMENT DES LIBRAIRIES 
library(dplyr)
library(tidyverse)
library(lubridate) # Pour le changement de la date
library(stringr)
library(ggplot2)
# 2. On nettoie ici la base de donnée dans cette partie
df <- df_base %>%
  
  # On régénère totalement la colonne pour éviter les doublons, on a ici 0-19 qui recommencent en boucle 
  mutate(id_patient = sprintf("PT%03d", row_number())) %>%
  
  # 2.1 AGE : Si négatif ou aberrant (>120), on met NA 
  mutate(age = ifelse(age < 0 | age > 120, NA, age)) %>%
  
  # 2.2 SEXE : Si ce n'est pas Homme/Femme, on met NA 
  mutate(sexe = toupper(sexe)) %>%
  mutate(sexe = case_when(
    sexe %in% c("HOMME", "M", "MALE") ~ "M",
    sexe %in% c("FEMME", "F", "FEMALE") ~ "F",
    TRUE ~ NA_character_ # <--- C'est ici qu'on force le NA
  )) %>%
  
  # 2.3 NOMS : On met juste en forme. Les NA restent des NA automatiquement.
  mutate(nom_patient = str_to_title(nom_patient)) %>%
  
  # 2.4 DATES
  mutate(date_consultation = dmy(date_consultation)) %>%
  # Ici on garde le remplacement par le mode (date fréquente) car une date NA plante les graphiques temporels
  mutate(date_consultation = replace_na(date_consultation, as.Date(names(which.max(table(date_consultation)))))) %>%
  
  # 2.5 POIDS & TAILLE
  mutate(
    poids = abs(poids), # 1. On enlève le signe négatif éventuel
    
    # 2. Règle du seuil : Si le poids est < 30 kg, c'est une erreur -> NA
    poids = ifelse(poids < 30, NA, poids), 
    
    # Même logique pour la taille (ex: en dessous de 50cm pour un adulte -> NA)
    taille = ifelse(taille < 50 | taille > 230, NA, taille) 
  ) %>%
  
  # 2.6 TENSION ARTERIELLE
  # 2.6 TENSION : Séparation et Correction Spécifique
  separate(tension_arterielle, into = c("tension_sys", "tension_dia"), sep = "[/-]", convert = TRUE, fill = "right") %>%
  
  mutate(
    # Ici on a décidé de compléter les valeurs de la tension diastolique manquante quand on a la valeur de la tension systolique, on suit la logique des autres tensions artérielles 
    # Si la systolique est 110 et qu'il n'y a pas de diastolique, on met 70 (Standard 110/70)
    tension_dia = ifelse(tension_sys == 110 & is.na(tension_dia), 70, tension_dia)
  ) %>%
  
  # 2.7 TEXTES : On a décidé ici de laisser les deux variables comme ça car faire du 0/1, oui/non pour ces variables ne semblement pas obligatoire car on a une seule données par variable
  # On aura pu remplacé les valeurs manquantes par non spécifié pour diagnostic ou Aucun pour traitement, mais on a pas jugé pertinent de le mettre ici car cela reviendrait à confirmer que le patient n'a pas de maladie/traitement or on a aucune information
  mutate(
    diagnostic = str_to_sentence(diagnostic),
    traitement = str_to_sentence(traitement)
  )
#On renomme maintenant les colonnes pour les mettre dans le bon format et finir le nettoyage

df_final <- df %>% rename(poids_kg = poids, taille_cm = taille,tension_sys_mmhg = tension_sys, tension_dia_mmhg=tension_dia)

#On peut vérifier  les modification avec summary(df_final) ou View(df_final) pour être sur

# 3. Analyse exploratoire 
age_moyen <- mean(df_final$age, na.rm = TRUE)
print(paste("Âge moyen des patients :", round(age_moyen, 1), "ans"))

# Calcul de la médiane de l'âge
age_median <- median(df_final$age, na.rm = TRUE)
print(paste("Âge médian des patients :", age_median, "ans"))

# Répartition par Sexe (pour voir si c'est équilibré)
print("Répartition par Sexe :")
print(table(df_final$sexe))

# On calcul l'IMC moyen (Indicateur de santé pertinent ici)
# Formule : Poids / (Taille en m)²
imc_moyen <- mean(df_final$poids_kg / ((df_final$taille_cm/100)^2), na.rm = TRUE)
print(paste("IMC Moyen de la population :", round(imc_moyen, 1)))

# Tensions moyennes (Systolique et Diastolique)
moy_sys <- mean(df_final$tension_sys, na.rm = TRUE)
moy_dia <- mean(df_final$tension_dia, na.rm = TRUE)
print(paste("Tension moyenne :", round(moy_sys, 0), "/", round(moy_dia, 0)))


# 4. Visualisations des données 
#Graphique 1 : Relation entre la taille et le poids selon le sexe

# On enlève les NA pour ce graphique
df_plot <- df_final %>% 
  filter(!is.na(poids_kg), !is.na(taille_cm), !is.na(sexe))
ggplot(df_plot, aes(x = taille_cm, y = poids_kg, color = sexe)) +
  
  # Points sobres
  geom_point(alpha = 0.7, size = 3) +
  
  # Tendances
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  
  # Couleurs sobres et cohérentes
  scale_color_manual(
    values = c("F" = "#e41a1c", "M" = "#377eb8"),
    labels = c("Femme", "Homme")
  ) +
  
  # Axes 
  scale_x_continuous(
    name = "Taille (cm)",
    limits = c(140, 200),
    breaks = seq(140, 200, 5)
  ) +
  scale_y_continuous(
    name = "Poids (kg)",
    limits = c(40, 120),
    breaks = seq(40, 120, 10)
  ) +
  
  # Titre propre
  labs(
    title = "Relation entre la taille et le poids selon le sexe",
    color = "Sexe"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

#Graphique 2 : Consultations mensuelles en 2025, L'histogramme est le choix pertinent car il permet de visualiser et comparer instantanément le volume d'activité pour chaque mois distinct, mettant ainsi en évidence le nombre de consultation réelle.
df_mois <- df_final %>%
  mutate(
    mois = format(date_consultation, "%B"),
    mois = str_to_title(mois),
    mois = factor(
      mois,
      levels = c(
        "Janvier", "Février", "Mars", "Avril",  #Ca permet de mettre les mois dans l'ordre
        "Mai", "Juin", "Juillet", "Août",
        "Septembre", "Octobre", "Novembre", "Décembre"
      )
    )
  ) %>%
  count(mois, name = "nb_consultations")

ggplot(df_mois, aes(x = mois, y = nb_consultations)) +
  geom_col(fill = "#1f77b4") +
  labs(
    title = "Consultations mensuelles en 2025",
    x = "Mois",
    y = "Nombre de consultations"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

# Puis on export si on le souhaite
#write.csv(df, "consultations_clean_2025.csv", row.names = FALSE)
