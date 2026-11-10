# 1. On charge les packages et on les installe si on ne les a pas avec la commande install.packages, on fait aussi quietly = TRUE pour éviter les messages de la librairie 
library(dplyr, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(stringr, quietly = TRUE)

# 2. On définit les fonctions d'abord pour plus de clarté et on les charge avant qu'on les utilise.

texte_en_nombre <- function(x) {
  x <- tolower(x)
  nums <- c(
    "un"=1,"deux"=2,"trois"=3,"quatre"=4,"cinq"=5,"six"=6,
    "sept"=7,"huit"=8,"neuf"=9,"dix"=10,"onze"=11,"douze"=12,
    "treize"=13,"quatorze"=14,"quinze"=15,"seize"=16,"dix-sept"=17,
    "dix-huit"=18,"dix-neuf"=19,"vingt"=20,"trente"=30,"quarante"=40,
    "cinquante"=50,"soixante"=60,"soixante-dix"=70,"quatre-vingt"=80,
    "quatre-vingt-dix"=90
  )
  
  sapply(x, function(y) {
    if (is.na(y)) return(NA_real_)
    y <- tolower(y)
    if (str_detect(y, "-")) {
      parts <- str_split(y, "-", simplify = TRUE)
      return(sum(nums[parts], na.rm = TRUE))
    } else {
      return(nums[y])
    }
  })
}
# 2.1 On définit une autre fonction pour pouvoir faire la conversion de la glycémie 
convert_to_gL <- function(value) {
  if (is.na(value)) {
    return(NA_real_)
  }
  
  mmolL_to_gL <- 0.18
  mgdL_to_gL <- 1/100
  value_num = as.numeric(value)
  
  if (value_num > 50 && value_num <= 600) {
    return(value_num * mgdL_to_gL)
  } else if (value_num >= 3 && value_num <= 25) {
    return(value_num * mmolL_to_gL)
  } else if (value_num < 3 && value_num > 0) {
    return(value_num)
  } else {
    return(NA_real_)
  }
}

# 3.On charge maintenant la base de données dans R

df_patients_tidy <- read.csv("C:/Users/Redouane/Downloads/patients_brut.csv", sep=";", header = TRUE) |>
  
  # 3.1  On sépare la systole de la diastole en deux colonnes "tension_sys" et "tension_dia"
  mutate(Tension_sys_Tension_dia = str_replace(Tension_sys_Tension_dia, "-", "/")) |>
  separate(Tension_sys_Tension_dia, into = c("Tension_sys", "Tension_dia"), sep = "/", convert = TRUE) |>
  
  # 3.2 On standardise  la variable Sexe avec H pour homme et F pour féminin
  mutate(Sexe = case_when(
    Sexe %in% c("homme", "H") ~ "H",
    Sexe %in% c("feminin", "F") ~ "F",
    TRUE ~ Sexe
  )) |>
  
  # 3.3  Ensuite, on fait un nettoyage des valeurs aberrantes en utilisant les quartiles et inter-quartiles pour éviter d'éventuelle erreur de saisie
  mutate(
    Age = ifelse(!is.na(Age) & !str_detect(Age, "^[0-9]+$"), texte_en_nombre(Age), Age),
    Age = as.numeric(Age),
    .Q1 = quantile(Age, 0.25, na.rm = TRUE),
    .Q3 = quantile(Age, 0.75, na.rm = TRUE),
    .IQR = .Q3 - .Q1,
    Age = if_else(
      Age < (.Q1 - 1.5 * .IQR) | Age > (.Q3 + 1.5 * .IQR) | Age > 120,
      NA_real_,
      Age
    )
  ) |> select(-starts_with(".")) |> # Supprime .Q1, .Q3, .IQR
  
  # 3.4 On effectue également un nettoyage des poids pour n'y mettre que des nombres et on convertit les lbs en Kg
  mutate(
    # Nettoyage pour Janv
    Poids_kg_Janv = as.character(Poids_kg_Janv),
    Poids_kg_Janv = str_replace_all(Poids_kg_Janv, "[^0-9,\\.]", "") %>% str_replace(",", ".") %>% as.numeric(),
    
    # Nettoyage pour Juin 
    Poids_lb_Juin = as.character(Poids_lb_Juin),
    Poids_lb_Juin = str_replace_all(Poids_lb_Juin, "[^0-9,\\.]", "") %>% str_replace(",", ".") %>% as.numeric(),
    
    # Conversion des lbs en kg 
    Poids_kg_Juin = round(Poids_lb_Juin * 0.4536, 0),
  ) |> select(-Poids_lb_Juin) |> # Suppression de Poids_lb_Juin
  
  # 3.5  On nettoie et standardise  la glycémie en g/L
  mutate(
    Glycemie1_clean = as.numeric(str_replace(Glycemie1, ",", "."), errors = "coerce"),
    Glycemie2_clean = as.numeric(str_replace(Glycemie2, ",", "."), errors = "coerce"),
    Glycemie1_gL = sapply(Glycemie1_clean, convert_to_gL),
    Glycemie2_gL = sapply(Glycemie2_clean, convert_to_gL),
    
    # Filtrage des glycémies qui pourrait être aberrantes dans notre cas  [0.4 g/L ; 4.0 g/L] et remplacement des anciennes colonnes
    Glycemie1 = if_else(Glycemie1_gL < 0.4 | Glycemie1_gL > 4.0, NA_real_, Glycemie1_gL),
    Glycemie2 = if_else(Glycemie2_gL < 0.4 | Glycemie2_gL > 4.0, NA_real_, Glycemie2_gL)
  ) |> select(-Glycemie_Unite, -ends_with("_clean"), -ends_with("_gL")) |> # Suppression des intermédiaires
  
  # 3.6 On change le format de réponse en oui/non
  separate_rows(maladie, sep=",") |>
  mutate(maladie=trimws(maladie)) |>
  filter(maladie!="") |>
  mutate(valeur="oui") |>
  pivot_wider(names_from=maladie,
              values_from=valeur,
              values_fill="non") |>
  
  # 3.7 On harmonise les noms des variables
  dplyr::rename(Diabete = diabete, Taille_cm = Taille..cm., Glycemie1_gL = Glycemie1, Glycemie2_gL = Glycemie2) |>
  
  # 3.8 On finit par réorganiser les colonnes 
  select(ID_Patient, Sexe, Age, Taille_cm, 
         Poids_kg_Janv, Poids_kg_Juin, 
         Tension_sys, Tension_dia, 
         Glycemie1_gL, Glycemie2_gL, 
         Diabete, HTA, IRC, Asthme,
         everything()) 
write.table(df_patients_tidy, "patients_tidy.csv", sep = ";", row.names = FALSE)
View(df_patients_tidy)

