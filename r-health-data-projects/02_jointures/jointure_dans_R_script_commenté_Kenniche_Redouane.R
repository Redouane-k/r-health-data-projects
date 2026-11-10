#  Charger les packages nécessaires tout d'abord
library(survival)   # contient pbc et pbcseq
library(dplyr)      # va nous permettre de manipuler les données
library(tidyr)      # va nous permettre d'utiliser pivot_wider()

#Liste des variables que nous souhaitons extraire de pbcseq

variables_souhaitees <- c("bili", "albumin", "copper", "alk.phos", "ast","trig", "platelet", "protime")

# On garde seulement celles qui existent réellement dans notre tableau grâce à intersect 
variables_disponibles <- intersect(variables_souhaitees, names(pbcseq))

# Vérification pour n'avoir aucun problème plus tard
message("Variables disponibles pour pivot_wider :")
print(variables_disponibles)

# Passage de pbcseq au format large

# Le format large = une colonne par variable et jour

pbcseq_wide <- pbcseq %>%
  pivot_wider(
    id_cols = id,            # identifiant patient
    names_from = day,        # chaque jour devient un suffixe pour la mise en forme au format large 
    values_from = all_of(variables_disponibles),
    names_glue = "{.value}_day{day}"
  )

# Jointure avec pbc en utilisant left_join

pbc_final <- pbc %>%
  left_join(pbcseq_wide, by = "id")

# Aperçu du résultat final
View(pbc_final)
