library(tidyverse) #charge la librairie Tidyverse
library("gridExtra") # pour faire le multigraph
library(ggExtra) #test beaux graph
library(reshape2)
#################################################################################################################
# Charger le fichier CSV et affiche des infos sur le dataset
#################################################################################################################
#charge le document csv de google play store
datastore <- read.csv("~/ISS 2018-2019/R_Android_Store/googleplaystore.csv") #~/5AISS/R_Android_Store/googleplaystore.csv
database = datastore
#voir la forme du dataset
#head(datastore) 
#voir le rÃ©sumÃ© du dataset
#summary(datastore) 


#################################################################################################################
# Appliquer les traitements des donners aux dataset complet
# Creer un sous dataset de 1000 données aprés traitement
#################################################################################################################

#datastore = datastore[complete.cases(datastore),] #creer un nouveau dataset sans les lignes avec donnÃ©es manquantes Na => ne pas appliquer !!
datastore = dplyr::filter(datastore, !grepl(';', Genres)) # Supprimer les lignes avec deux Genres en mÃªme temps
datastore = dplyr::filter(datastore, !grepl('1.9', Category)) # Supprimer la seul ligne avec 1.9 en catÃ©gory
datastore$Price = as.numeric(gsub("[\\$,]","",datastore$Price)) #  prix en chiffre
datastore$Installs = as.numeric(gsub("[\\+,\\,]","",datastore$Installs)) # Installs avec les nombres
#pour creer nouvelle colone ajouter 2 au nom de colone avant le =
datastore$Reviews = as.numeric(datastore$Reviews)
datastore = datastore %>% filter(Type %in% c("Free", "Paid")) # filtre le dataset, enlÃ¨ve la ligne avec NaN et 0 en Type
#datastore %>%filter(Size %in% "Varies with device") %>% count() # nombre d'entrÃ©  avec Varies with device dans $size
#datastore %>%filter(duplicated(datastore)) %>% count() # nombre de ligne dupliquÃ© dans le dataset
datastore = datastore %>%filter(!duplicated(datastore)) # enleve les lignes dupliquÃ©

# Traite la colone Size, passe en numÃ©rique, enleve k et passe les donnÃ©es non numÃ©riques en Na. Size en Mb
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

# Plot du nombre d'apllication Gratuit ou payant, couleur diffÃ©rentes pour gratuit et payant
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
#Plot [1] : Nb d'aplication par catégory
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

#Crée un dataset contenant la moyenne des ratings pour chaque Population dans la category GAME
#datatest = datastore %>% filter(Category %in% c("GAME")) %>% group_by(Population) %>% summarise(MeanRate = mean(Rating, na.rm =TRUE))
#Crée un dataset contenant la moyenne des ratings pour chaque Population pour toute les categorie !!!!
dataplotPop = datastore %>% group_by(Category,Population) %>% summarise(MeanRate = mean(Rating, na.rm =TRUE),MeanRateI = mean(Installs, na.rm =TRUE))


plotRatPopGame = ggplot(dataplotPop %>% filter(Category %in% c("GAME")), aes(x=Population,y=MeanRate,fill=Population)) + 
  geom_bar(stat="identity", width=0.8) + # fonctionne pas avec Geom_bar, pourtant même type donnée que avant
  #geom_boxplot() +
  #coord_flip() +
  theme_bw()+
  labs(title="Rating by Pop for Game",x="Pop",y="Rating") + 
  theme(legend.position="none")+ 
  theme( #axis.text.x = element_blank(), 
    #axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()) +
  coord_cartesian(
    ylim = c(4, 4.65))
plotRatPopCommunication = ggplot(dataplotPop %>% filter(Category %in% c("COMMUNICATION")), aes(x=Population,y=MeanRate,fill=Population)) + 
  geom_bar(stat="identity", width=0.8) + # fonctionne pas avec Geom_bar, pourtant même type donnée que avant
  #geom_boxplot() +
  #coord_flip() +
  theme_bw()+
  labs(title="Rating by Pop for Communication",x="Pop",y="Rating") + 
  theme(legend.position="none")+ 
  theme( #axis.text.x = element_blank(), 
    axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()) +
  coord_cartesian(
    ylim = c(4, 4.65))
plotRatPopSocial = ggplot(dataplotPop %>% filter(Category %in% c("SOCIAL")), aes(x=Population,y=MeanRate,fill=Population)) + 
  geom_bar(stat="identity", width=0.8) + # fonctionne pas avec Geom_bar, pourtant même type donnée que avant
  #geom_boxplot() +
  #coord_flip() +
  theme_bw()+
  labs(title="Rating by Pop for Social",x="Pop",y="Rating") + 
  theme(legend.position="none")+ 
  theme( #axis.text.x = element_blank(), 
    axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()) +
  coord_cartesian(
    ylim = c(4, 4.65))
plotRatPopProductivity = ggplot(dataplotPop %>% filter(Category %in% c("PRODUCTIVITY")), aes(x=Population,y=MeanRate,fill=Population)) + 
  geom_bar(stat="identity", width=0.8) + # fonctionne pas avec Geom_bar, pourtant même type donnée que avant
  #geom_boxplot() +
  #coord_flip() +
  theme_bw()+
  labs(title="Rating by Pop for Productivity",x="Pop",y="Rating") + 
  theme(legend.position="none")+ 
  theme( #axis.text.x = element_blank(), 
    axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()) +
  coord_cartesian(
    ylim = c(4, 4.65))
plotRatPopPhotography = ggplot(dataplotPop %>% filter(Category %in% c("PHOTOGRAPHY")), aes(x=Population,y=MeanRate,fill=Population)) + 
  geom_bar(stat="identity", width=0.8) + # fonctionne pas avec Geom_bar, pourtant même type donnée que avant
  #geom_boxplot() +
  #coord_flip() +
  theme_bw()+
  labs(title="Rating by Pop for Photographie",x="Pop",y="Rating") + 
  theme(legend.position="none")+ 
  theme( #axis.text.x = element_blank(), 
    axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()) +
  coord_cartesian(
    ylim = c(4, 4.65))

plotRatPopGameI = ggplot(dataplotPop %>% filter(Category %in% c("GAME")), aes(x=Population,y=MeanRateI,fill=Population)) + 
  geom_bar(stat="identity", width=0.8) + # fonctionne pas avec Geom_bar, pourtant même type donnée que avant
  #geom_boxplot() +
  #coord_flip() +
  theme_bw()+
  labs(title="Rating by Pop for Game",x="Pop",y="Rating") + 
  theme(legend.position="none")+ 
  theme( #axis.text.x = element_blank(), 
    #axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()) #+
  #coord_cartesian(ylim = c(4, 4.65))
plotRatPopCommunicationI = ggplot(dataplotPop %>% filter(Category %in% c("COMMUNICATION")), aes(x=Population,y=MeanRateI,fill=Population)) + 
  geom_bar(stat="identity", width=0.8) + # fonctionne pas avec Geom_bar, pourtant même type donnée que avant
  #geom_boxplot() +
  #coord_flip() +
  theme_bw()+
  labs(title="Rating by Pop for Communication",x="Pop",y="Rating") + 
  theme(legend.position="none")+ 
  theme( #axis.text.x = element_blank(), 
    axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()) #+
  #coord_cartesian(ylim = c(4, 4.65))
plotRatPopSocialI = ggplot(dataplotPop %>% filter(Category %in% c("SOCIAL")), aes(x=Population,y=MeanRateI,fill=Population)) + 
  geom_bar(stat="identity", width=0.8) + # fonctionne pas avec Geom_bar, pourtant même type donnée que avant
  #geom_boxplot() +
  #coord_flip() +
  theme_bw()+
  labs(title="Rating by Pop for Social",x="Pop",y="Rating") + 
  theme(legend.position="none")+ 
  theme( #axis.text.x = element_blank(), 
    axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()) #+
  #coord_cartesian(ylim = c(4, 4.65))
plotRatPopProductivityI = ggplot(dataplotPop %>% filter(Category %in% c("PRODUCTIVITY")), aes(x=Population,y=MeanRateI,fill=Population)) + 
  geom_bar(stat="identity", width=0.8) + # fonctionne pas avec Geom_bar, pourtant même type donnée que avant
  #geom_boxplot() +
  #coord_flip() +
  theme_bw()+
  labs(title="Rating by Pop for Productivity",x="Pop",y="Rating") + 
  theme(legend.position="none")+ 
  theme( #axis.text.x = element_blank(), 
    axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()) #+
  #coord_cartesian(ylim = c(4, 4.65))
plotRatPopPhotographyI = ggplot(dataplotPop %>% filter(Category %in% c("PHOTOGRAPHY")), aes(x=Population,y=MeanRateI,fill=Population)) + 
  geom_bar(stat="identity", width=0.8) + # fonctionne pas avec Geom_bar, pourtant même type donnée que avant
  #geom_boxplot() +
  #coord_flip() +
  theme_bw()+
  labs(title="Rating by Pop for Photographie",x="Pop",y="Rating") + 
  theme(legend.position="none")+ 
  theme( #axis.text.x = element_blank(), 
    axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()) #+
  #coord_cartesian(ylim = c(4, 4.65))

plotpart3 = grid.arrange(plotRatPopGame, plotRatPopCommunication, plotRatPopSocial, plotRatPopProductivity, plotRatPopPhotography,
                         plotRatPopGameI, plotRatPopCommunicationI, plotRatPopSocialI, plotRatPopProductivityI, plotRatPopPhotographyI,
                         ncol=5, nrow=2, widths=c(7, 6,6,6,6), heights=c(4,4))


# plot[4] pourchaque category, récupérer seulement les colones avec unne valeurs supérieur au 3ème quartil de Rating (Installs) et calculer la moyenne de prix

#avoir le troisième quartil
#summary((datastore%>%filter(Category%in%c("GAME")))$Rating)[5]

#donne un dataset avec 5 lignes, et prix moyen par catégory filtré par le troisième quartil de rating
datatestf = datastore %>% filter(Category %in% c("GAME","COMMUNICATION","SOCIAL","PRODUCTIVITY","PHOTOGRAPHY")) %>% group_by(Category) %>% filter(Rating>=summary(datastore$Rating)[5])%>%summarise(Pricebestf = mean(Price,na.rm=TRUE))
#donne un dataset avec 5 lignes, et prix moyen par catégory 
datatest = datastore %>% filter(Category %in% c("GAME","COMMUNICATION","SOCIAL","PRODUCTIVITY","PHOTOGRAPHY")) %>% group_by(Category) %>%summarise(Pricebest = mean(Price,na.rm=TRUE))
datatestPrice = left_join(datatest,datatestf)

#donne un dataset avec 5 lignes, et prix moyen par catégory filtré par le troisième quartil de installs
datatestIf = datastore %>% filter(Category %in% c("GAME","COMMUNICATION","SOCIAL","PRODUCTIVITY","PHOTOGRAPHY")) %>% group_by(Category) %>% filter(Installs>=summary(datastore$Installs)[5])%>%summarise(PricebestIf = mean(Price,na.rm=TRUE))
datatestPrice = left_join(datatestPrice,datatestIf)

#Trop forte influence de toute les applis gratuites, regarde que les payantes
#summary((datastore%>%filter(Type %in% c("Paid")))$Price)

#donne un dataset avec 5 lignes, et prix moyen par catégory filtré par le troisième quartil de rating sans les gratuites
datatestfp = datastore %>% filter(Category %in% c("GAME","COMMUNICATION","SOCIAL","PRODUCTIVITY","PHOTOGRAPHY"),Type %in% c("Paid")) %>% group_by(Category) %>% filter(Rating>=summary(datastore$Rating)[5])%>%summarise(Pricebestfp = mean(Price,na.rm=TRUE))
#donne un dataset avec 5 lignes, et prix moyen par catégory sans les gratuites
datatestp = datastore %>% filter(Category %in% c("GAME","COMMUNICATION","SOCIAL","PRODUCTIVITY","PHOTOGRAPHY"),Type %in% c("Paid")) %>% group_by(Category) %>%summarise(Pricebestp = mean(Price,na.rm=TRUE))
datatestPricep = left_join(datatestp,datatestfp)

#donne un dataset avec 5 lignes, et prix moyen par catégory filtré par le troisième quartil de installs sans les gratuites
datatestIfp = datastore %>% filter(Category %in% c("GAME","COMMUNICATION","SOCIAL","PRODUCTIVITY","PHOTOGRAPHY"),Type %in% c("Paid")) %>% group_by(Category) %>% filter(Installs>=summary(datastore$Installs)[5])%>%summarise(PricebestIfp = mean(Price,na.rm=TRUE))
datatestPricep = left_join(datatestPricep,datatestIfp)
datatestPrice = left_join(datatestPrice,datatestPricep)
#Mes 0 la ou il y a NA dans les colones avec que des payantes, c'est bien 0 normalement !
datatestPrice[is.na(datatestPrice)]=0.01


#test du plot 
#ggplot(datatestPrice, aes(x=Category)) + geom_bar(aes(y=Pricebestp),stat = "identity",colour="red",width=0.2,position = "dodge") + geom_bar(aes(y=Pricebestfp),stat = "identity",colour="red",width=0.2,position = "dodge") + geom_bar(aes(y=PricebestIfp),stat = "identity",colour="red",width=0.2,position = "dodge")
#datatestPrice = rename(databestPrice, Pricebestp = No_Filter, Pricebestfp = Apps_rating_top_quartil)
datatestPriceResh = melt(datatestPrice,id.vars = 'Category')
plotPrice = ggplot(datatestPriceResh%>%filter(variable %in% c("Pricebestp","Pricebestfp","PricebestIfp")), aes(x=Category,y=value,fill=variable)) + 
  geom_bar(stat="identity", width=0.5,position = "dodge") + # fonctionne pas avec Geom_bar, pourtant même type donnée que avant
  #geom_boxplot() +
  #coord_flip() +
  theme_bw()+
  labs(title="Mean price of paid Apps for each Category with 3 different filter",x="Category",y="Mean Price") + 
  #theme(legend.position="none")+ 
  theme( #axis.text.x = element_blank(), 
    #axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()) #+
#coord_cartesian(ylim = c(4, 4.65))

#filtre les payantes plus tards, aprés la selection par quartil => trop peut de donner payantes au final, genre 10 par catégory
#datastore %>% filter(Category %in% c("GAME","COMMUNICATION","SOCIAL","PRODUCTIVITY","PHOTOGRAPHY")) %>% group_by(Category) %>% filter(Rating>=summary(datastore$Rating)[5])%>%filter(Type %in% c("Paid"))%>%summarise(nb = n())

# plot [5], Rating vs Size et Installs vs Size, plot avec grid arrange plutot que ggMarginal

# Rating vs size
SizeCount = ggplot(datastore, aes(x=Size,fill=Type))+
  geom_histogram()+
  theme_minimal() +theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank())

SizeRating = ggplot(datastore, aes(x=Size, y=Rating)) +
  scale_x_continuous() +
  geom_point(aes(col=Type)) +
  theme_linedraw()

RatingCount = ggplot(datastore, aes(x=Rating,fill=Type))+
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
#SizeVSRating = grid.arrange(SizeCount, blankPlot, SizeRating, RatingCount, 
#                    ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))

# Installs vs size pas représentatif voir pas utile

SizeCount = ggplot(datastore, aes(x=Size,fill=Type))+
  coord_flip() +
  geom_histogram()+
  theme_minimal() +theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank())

SizeInstalls = ggplot(datastore, aes(x=Size, y=Installs)) +
  coord_flip() +
  scale_x_continuous() +
  geom_point(aes(col=Type)) +
  theme_linedraw()

InstallsCount = ggplot(datastore, aes(x=Installs,fill=Type))+
  geom_histogram()+
  theme_minimal()+ 
  #coord_flip() +
  theme(
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
SizeVSInstalls = grid.arrange(InstallsCount, blankPlot, SizeInstalls,SizeCount , 
                    ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))

#plot [bonus] les applis les plus chers
plotpricecountf = ggplot(datastore%>%filter(Type %in% c("Paid")), aes(x=Price)) +
  geom_histogram() +
  theme_linedraw()
#???nom des applis les plus chers
names = (datastore%>%filter(Type %in% c("Paid"),Price>=395))
names = select(names,App,Price,Installs)
#################################################################################################################
# Tests
#################################################################################################################
expl[expl$Type=="Paid",] #que des payantes

# Test combining plots ==> fonctionne 
testestestsd = ggMarginal(plot5data, type="histogram") #nécessite package Extraggplot ??

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
#             ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))
