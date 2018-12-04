library(tidyverse) #charge la librairie Tidyverse
library("gridExtra") # pour faire le multigraph
library(ggExtra) #test beaux graph
#################################################################################################################
# Charger le fichier CSV et affiche des infos sur le dataset
#################################################################################################################
#charge le document csv de google play store
datastore <- read.csv("~/5AISS/R_Android_Store/googleplaystore.csv") 
database = datastore
#voir la forme du dataset
#head(datastore) 
#voir le résumé du dataset
#summary(datastore) 


#################################################################################################################
# Appliquer les traitements des donners aux dataset complet
# Creer un sous dataset de 1000 donn�es apr�s traitement
#################################################################################################################

#datastore = datastore[complete.cases(datastore),] #creer un nouveau dataset sans les lignes avec données manquantes Na => ne pas appliquer !!
datastore = dplyr::filter(datastore, !grepl(';', Genres)) # Supprimer les lignes avec deux Genres en même temps
datastore = dplyr::filter(datastore, !grepl('1.9', Category)) # Supprimer la seul ligne avec 1.9 en catégory
datastore$Price = as.numeric(gsub("[\\$,]","",datastore$Price)) #  prix en chiffre
datastore$Installs = as.numeric(gsub("[\\+,\\,]","",datastore$Installs)) # Installs avec les nombres
#pour creer nouvelle colone ajouter 2 au nom de colone avant le =
datastore$Reviews = as.numeric(datastore$Reviews)
datastore = datastore %>% filter(Type %in% c("Free", "Paid")) # filtre le dataset, enlève la ligne avec NaN et 0 en Type
#datastore %>%filter(Size %in% "Varies with device") %>% count() # nombre d'entré  avec Varies with device dans $size
#datastore %>%filter(duplicated(datastore)) %>% count() # nombre de ligne dupliqué dans le dataset
datastore = datastore %>%filter(!duplicated(datastore)) # enleve les lignes dupliqué

# Traite la colone Size, passe en numérique, enleve k et passe les données non numériques en Na. Size en Mb
datastore$Size = gsub("['M',\\+,\\,]","",datastore$Size)
datastore$Size = gsub("Varies with device",NA,datastore$Size)
datastore$Size = ifelse(grepl("k", datastore$Size), 0,datastore$Size)
datastore$Size = as.numeric(datastore$Size)

datastore$Population = datastore$Content.Rating
# Supprime les colones inutiles (Last.Updated, Current.Ver, Android.Ver)
datastore$Last.Updated = NULL
datastore$Current.Ver = NULL
datastore$Android.Ver = NULL
datastore$Content.Rating = NULL

#pour s'entrainer juste avec une petite partie du fichier
expl = sample_n(datastore, 1000) 

#################################################################################################################
# Les plots 

# http://www.sthda.com/french/wiki/ggplot2-barplots-guide-de-demarrage-rapide-logiciel-r-et-visualisation-de-donnees 
# Utiliser + scale_fill_grey() pour avoir des nuances de gris
# Utiliser + coord_flip() pour changer le sens du plot
# Utiliser = theme(axis.text.x = element_text(angle=65, vjust=0.6)) pour incliner le texte en bas de l'axe x
#################################################################################################################
# Plot du nombre d'apllication Gratuit ou payant en bleu
plot = ggplot(expl, aes(x=Type))+
  geom_bar(stat="count", width=0.2, fill="steelblue")+
  theme_minimal()

# Plot du nombre d'apllication Gratuit ou payant, couleur différentes pour gratuit et payant
plot2 = ggplot(expl, aes(x=Type, fill=Type))+
  geom_bar(stat="count", width=0.2)+
  theme_minimal()
plot2data = ggplot(datastore, aes(x=Type, fill=Type))+
  geom_bar(stat="count", width=0.2)+
  theme_minimal()

# Plot le nombre de chaque application par genre. Supperposition de couleur en fonction de payant ou gratuit
plot3 = ggplot(expl, aes(x=Genres, fill=Type))+
  geom_bar(stat="count", width=0.2)+
  theme_minimal()+ 
  coord_flip()
plot3data = ggplot(datastore, aes(x=Genres, fill=Type))+geom_bar(stat="count", width=0.2)+
  theme_minimal()+ 
  coord_flip()

# Plot le nombre de chaque application par Category. Supperposition de couleur en fonction de payant ou gratuit
plot4 = ggplot(expl, aes(x=Category, fill=Type))+
  geom_bar(stat="count", width=0.2)+
  theme_minimal()+ 
  coord_flip()
plot4data = ggplot(datastore, aes(x=Category, fill=Type))+geom_bar(stat="count", width=0.4)+
  theme_minimal()+ 
  coord_flip()

#Raiting VS Size
plot5 = ggplot(expl, aes(x=Size, y=Rating)) +
  scale_x_continuous() + #scale_x_continuous(trans='log10') si on veut echelle log
  geom_point(aes(col=Type)) +
  labs(title="Android App Ratings vs App Size", subtitle="Google Playstore Dataset", y="Rating from 1 to 5 stars", x="Size in kb") +
  theme_linedraw()
plot5data = ggplot(datastore, aes(x=Size, y=Rating)) +
  scale_x_continuous() +
  geom_point(aes(col=Type)) +
  labs(title="Android App Ratings vs App Size", subtitle="Google Playstore Dataset", y="Rating from 1 to 5 stars", x="Size in kb") +
  theme_linedraw()

#Size VS Rating
plot6 = ggplot(expl, aes(x=Rating, y=Size)) +
  scale_x_continuous() +
  geom_point(aes(col=Type)) +
  labs(title="Android App Size vs App Ratings", subtitle="Google Playstore Dataset", y="Size in kb", x="Rating from 1 to 5 stars") +
  theme_linedraw()
plot6data = ggplot(datastore, aes(x=Rating, y=Size)) +
  scale_x_continuous() +
  geom_point(aes(col=Type)) +
  labs(title="Android App Size vs App Ratings", subtitle="Google Playstore Dataset", y="Size in kb", x="Rating from 1 to 5 stars") +
  theme_linedraw()

#Raiting VS Category
plot7 = ggplot(expl, aes(x=Category,y=Rating,color=Category)) + 
  geom_boxplot() +
  coord_flip() +
  theme_bw()+
  labs(title="Boxplot of App Rating VS Category",x="Category",y="Raiting") + 
  theme(legend.position="none")
plot7data = ggplot(datastore, aes(x=Category,y=Rating,color=Category)) + 
  geom_boxplot() +
  coord_flip() +
  theme_bw()+
  labs(title="Boxplot of App Rating VS Category",x="Category",y="Raiting") + 
  theme(legend.position="none") #+
#theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())
#################################################################################################################
#Plot [1] : Nb d'aplication par cat�gory
plotNbCat = ggplot(datastore, aes(x=Category, fill=Type))+geom_bar(stat="count", width=0.8)+ 
  coord_flip()
#Plot [2] : Rating par category quartil.. + Installs par Category
plotRatCat = ggplot(datastore, aes(x=Category,y=Rating,fill=Category)) + 
  geom_boxplot() +
  coord_flip() +
  theme_bw()+
  labs(title="Boxplot of App Rating VS Category",x="Category",y="Raiting") + 
  theme(legend.position="none") #+
  #theme(axis.text.x = element_text(angle=90))
plotInsCat = ggplot(datastore, aes(x=Category,y=Installs,fill=Category)) + 
  geom_bar(stat="Identity", width=0.8) +
  coord_flip() +
  theme_bw()+
  labs(title="Installs VS Category",x="Category",y="Installs") + 
  theme(legend.position="none")+ 
  theme( #axis.text.x = element_blank(), 
         axis.title.y = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         axis.line = element_blank())
#plotpart2 = grid.arrange(plotRatCat, plotInsCat, 
#                    ncol=2, nrow=1, widths=c(8, 6), heights=c(4))
#faire un filter surle dataset avec category == GAME....
plotRatCatCut = ggplot(datastore %>% filter(Category %in% c("GAME","COMMUNICATION","SOCIAL","PRODUCTIVITY","PHOTOGRAPHY")), aes(x=Category,y=Rating,fill=Category)) + 
  geom_boxplot() +
  coord_flip() +
  theme_bw()+
  labs(title="Boxplot of App Rating VS Category",x="Category",y="Raiting") + 
  theme(legend.position="none")
plotInsCatCut = ggplot(datastore %>% filter(Category %in% c("GAME","COMMUNICATION","SOCIAL","PRODUCTIVITY","PHOTOGRAPHY")), aes(x=Category,y=Installs,fill=Category)) + 
  geom_bar(stat="Identity", width=0.8) +
  coord_flip() +
  theme_bw()+
  labs(title="Installs VS Category",x="Category",y="Installs") + 
  theme(legend.position="none")+ 
  theme( #axis.text.x = element_blank(), 
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank())
#plotpart2 = grid.arrange(plotRatCatCut, plotInsCatCut, 
#                    ncol=2, nrow=1, widths=c(8, 6), heights=c(4))


#Plot [2bis] : Rating par category quartil.. + Installs par Genres
plotRatGnr = ggplot(datastore, aes(x=Genres,y=Rating,fill=Genres)) + 
  geom_boxplot() +
  coord_flip() +
  theme_bw()+
  #labs(title="Boxplot of App Rating VS Genres",x="Genres",y="Raiting") + 
  theme(legend.position="none") +
  theme( axis.text.x = element_blank(), #
         axis.title.x = element_blank(),
         axis.ticks = element_blank(),
         axis.line = element_blank())#+
#theme(axis.text.x = element_text(angle=90))
plotInsGnr = ggplot(datastore, aes(x=Genres,y=Installs,fill=Genres)) + 
  geom_bar(stat="Identity", width=0.8) +
  coord_flip() +
  theme_bw()+
  #labs(title="Installs VS Genres",x="Genres",y="Installs") + 
  theme(legend.position="none")+ 
  theme( axis.text.x = element_blank(), #
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank())
#plotpart2 = grid.arrange(plotRatGnr, plotInsGnr, plotRatCat, plotInsCat,
#                    ncol=2, nrow=2, widths=c(8, 6), heights=c(4,4))

#plot[3] afficher pour chacune des 5 category choisis Rating Vs Pop et Installs Vs Pop

plotRatPopGame = ggplot(datastore %>% filter(Category %in% c("GAME")), aes(x=Population,y=Rating,fill=Population)) + 
  geom_bar(stat="identity", width=0.8) + # fonctionne pas avec Geom_bar, pourtant m�me type donn�e que avant
  #geom_boxplot() +
  coord_flip() +
  theme_bw()+
  labs(title="Rating by Pop for Game",x="Pop",y="Rating") + 
  theme(legend.position="none")+ 
  theme( #axis.text.x = element_blank(), 
    #axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank())
# When the data contains y values in a column, use stat="identity" library(plyr) 
# Calculate the mean mpg for each level of cyl 
#mm <- ddply(mtcars, "cyl", summarise, mmpg = mean(mpg)) 
#ggplot(mm, aes(x = factor(cyl), y = mmpg)) + geom_bar(stat = "identity")


#################################################################################################################
# Tests
#################################################################################################################
expl[expl$Type=="Paid",] #que des payantes

# Test combining plots ==> fonctionne 
testestestsd = ggMarginal(plot5data, type="histogram") #n�cessite package Extraggplot ??

# Que ggplot
test1 = ggplot(datastore, aes(x=Size,fill=Type))+
  geom_histogram()+
  theme_minimal() +theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank())

test2 = ggplot(datastore, aes(x=Size, y=Rating)) +
  scale_x_continuous() +
  geom_point(aes(col=Type)) +
  theme_linedraw()

test3 = ggplot(datastore, aes(x=Rating,fill=Type))+
  geom_bar(stat="count", width=0.2)+
  theme_minimal()+ 
  coord_flip() +theme(
    axis.text.x = element_blank(), 
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank())
blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(
    plot.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  )
#test = grid.arrange(test1, blankPlot, test2, test3, 
             #ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))
