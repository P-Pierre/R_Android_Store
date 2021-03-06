library(tidyverse) #charge la librairie Tidyverse

#################################################################################################################
# Charger le fichier CSV et en faire un sous dataset
#################################################################################################################
datastore <- read.csv("~/ISS 2018-2019/RStudio/googleplaystore.csv") #charge le document csv de google play store
#head(datastore) #voir la forme du dataset
expl = sample_n(datastore, 1000) #pour s'entrainer juste avec une petite partie du fichier
testest = sample_n(datastore, 100)

#################################################################################################################
# Voir le nombre de ligne avec des donners manquantes et les affichers ; creer un nouveau dataset avec que des lignes complètes ; 
# Supprimer les lignes avec deux Type en même temps (en attendant de savoir créer deux lignes à partir d'une)
#################################################################################################################
sum(!complete.cases(expl)) #voir le nombre
expl[!complete.cases(expl),] #afficher
explbis = expl[complete.cases(expl),] #creer un nouveau dataset sans les lignes avec données manquantes
sum(!complete.cases(explbis)) # vérifier
explbis = dplyr::filter(explbis, !grepl(';', Genres)) # Supprimer les lignes avec deux Type en même temps

#################################################################################################################
# Changer les colones Price et Installs en donner valides, donc en nombres. Ajout des colones Price2 et Installs2
#################################################################################################################
testest$Price = as.numeric(gsub("[\\$,]","",testest$Price)) # créer la colone Price2 avec les prix en chiffre
testest$Installs = as.numeric(gsub("[\\+,\\,]","",testest$Installs)) #crer la colone Installs2 avec les nombres
#pour creer nouvelle colone ajouter 2 au nom de colone avant le =

#################################################################################################################
# Tests
#################################################################################################################
explbis[explbis$Type=="Paid",] #que des payantes


#################################################################################################################
# Appliquer les traitements des donners aux dataset complet ==> aprés traitement des lignes 37, 38 et 39 plus que 8883 datas
#################################################################################################################
datastore = datastore[complete.cases(datastore),] #creer un nouveau dataset sans les lignes avec données manquantes
datastore = dplyr::filter(datastore, !grepl(';', Genres)) # Supprimer les lignes avec deux Type en même temps
datastore = dplyr::filter(datastore, !grepl('1.9', Category)) # Supprimer la seul ligne avec 1.9 en catégory

#################################################################################################################
# Les plots 

# http://www.sthda.com/french/wiki/ggplot2-barplots-guide-de-demarrage-rapide-logiciel-r-et-visualisation-de-donnees 
# Utiliser + scale_fill_grey() pour avoir des nuances de gris
# Utiliser + coord_flip() pour changer le sens du plot
#################################################################################################################
# Plot du nombre d'apllication Gratuit ou payant en bleu
plot = ggplot(testest, aes(x=Type))+
  geom_bar(stat="count", width=0.2, fill="steelblue")+
  theme_minimal()

# Plot du nombre d'apllication Gratuit ou payant, couleur différentes pour gratuit et payant
plot2 = ggplot(testest, aes(x=Type, fill=Type))+
  geom_bar(stat="count", width=0.2)+
  theme_minimal()

# Plot le nombre de chaque application par genre. Supperposition de couleur en fonction de payant ou gratuit
Plot3 = ggplot(testest, aes(x=Genres, fill=Type))+
  geom_bar(stat="count", width=0.2)+
  theme_minimal()+ 
  coord_flip()
Plot3bis = ggplot(explbis, aes(x=Genres, fill=Type))+
  geom_bar(stat="count", width=0.2)+
  theme_minimal()+ 
  coord_flip()
Plot3data = ggplot(datastore, aes(x=Genres, fill=Type))+geom_bar(stat="count", width=0.2)+
  theme_minimal()+ 
  coord_flip()

# Plot le nombre de chaque application par Category. Supperposition de couleur en fonction de payant ou gratuit
Plot4 = ggplot(testest, aes(x=Category, fill=Type))+
  geom_bar(stat="count", width=0.2)+
  theme_minimal()+ 
  coord_flip()
Plot4bis = ggplot(explbis, aes(x=Category, fill=Type))+
  geom_bar(stat="count", width=0.2)+
  theme_minimal()+ 
  coord_flip()
Plot4data = ggplot(datastore, aes(x=Category, fill=Type))+geom_bar(stat="count", width=0.2)+
  theme_minimal()+ 
  coord_flip()
