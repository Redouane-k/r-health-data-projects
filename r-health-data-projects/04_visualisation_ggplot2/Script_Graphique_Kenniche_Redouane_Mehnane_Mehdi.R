#On charge la base de données
data("infert")
#install.packages(ggplot2)
library(ggplot2)
# On a choisi de faire un boxplot pour illustrer la distribution de l'âge des femmes de cette étude en fonction de leur statut d'infertilité
#Ce graphique va donc nous montrer si l'âge a un lien avec le statut d'infertilité 
boxplot(age ~ case, data = infert, 
        main = "Distribution de l'Âge par Statut d'Infertilité",
        xlab = "Statut d'infertilité (0=Contrôle/Fertile, 1=Cas/Infertile)",
        ylab = "Âge")

# On utilise ggsave pour sauvegarder notre graphique en format jpg
# ggsave("Graphique1_kenniche_redouane_mehnane_mehi.jpg")


#Pour le graphique 2 on va faire un diagramme en barre normalisées qui présente la répartition du statut de 'case' (variable binaire) au sein de chaque catégorie de niveau d'éducation (en pourcentages), en se concentrant sur la proportion de femmes.
#On a choisi de le faire en diagramme normalisées pour avoir une représentation équivalente de la répartition du statut d'infertilité selon le niveau d'éducation, pour pouvoir les comparer et regarder si le niveau d'éducation a un lien avec le statut d'infertilité
# On installe les packages si besoin
#install.packages(dplyr)
#install.packages(scales)
library(dplyr)
library(scales) 

# Transformation de la variable 'case' en variable catégorielle pour optimiser la clarté du graphique
infert_factor <- infert %>%
  mutate(
    # Définition de l'ordre des labels pour l'axe X et l'empilement
    case_status = factor(case, labels = c("Contrôle (Fertile)", "Cas (Infertile)")),
    education_label = factor(education, labels = c("0-5 ans", "6-11 ans", "12+ ans"))
  )

# Création du diagramme en barres normalisées
diagramme <- ggplot(infert_factor, aes(x = education_label, fill = case_status)) +
  
  # position="fill" pour la proportion à 100%
  geom_bar(position = "fill", color = "white", alpha = 0.9) +
  
  # On personnalise ici les couleurs
  scale_fill_manual(
    # Attribution du turquoise au Contrôle/Fertile et de l'orange au Cas/Infertile pour bien distinguer les couleurs
    values = c("Contrôle (Fertile)" = "#40E0D0", "Cas (Infertile)" = "#FF8C00"), 
    name = "Statut"
  ) +
  
  # On met l'axe y en pourçentage grâce à la libraire scales
  scale_y_continuous(labels = percent) + 
  
  # On fait ici des ajustement esthétique 
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "bottom"
  ) +
  
  # On va pouvoir ici légender le graphique et nommer les axes
  labs(
    title = "Proportion de Cas d'Infertilité selon le Niveau d'Éducation",
    x = "Niveau d'Éducation",
    y = "Proportion de femmes de l'échantillon"
  )

print(diagramme)
# On utilise ggsave pour sauvegarder notre graphique en format jpg
#ggsave("Graphique2_kenniche_redouane_mehdi_mehnane.jpg", plot = diagramme, width = 7, height = 6)
