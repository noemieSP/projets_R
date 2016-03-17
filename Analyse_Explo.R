#Changement de répertoire
setwd('...')

#Chargement des données dans la table arbres
arbres=read.table('gr3_arbres.txt')

####Chargement des packages####
library("rgl", lib.loc="~/R/win-library/3.1")
library("lattice", lib.loc="C:/Program Files/R/R-3.1.1/library")

####Description des données####

#Détail du nombre d'individus et de variables
dim(arbres)
#Nature de chaque variable
str(arbres)

####Conversion des unités en unité métrique####

#pouce en métre
arbres[,1]=arbres[,1]*0.0254
#pied en métre
arbres[,2]=arbres[,2]*0.3048
#pied cube en metre cube
arbres[,3]=arbres[,3]/35.315 

#####Statistiques simples#####
summary(arbres)

##Ecarts type##
#Girth
sd(arbres[,1])
#0.07970872
#Heigh
sd(arbres[,2])
# 1.942129
#Volume
sd(arbres[,3])
# 0.4654636

####Histogrammes####
#Division de la fenêtre de sortie en 3 
layout(matrix(1:3, 1, 3))
#Histogramme de la variable Volume
hist(arbres$Volume, main="Volumes",xlab="Volume (m3) ")
#Histogramme de la variable Girth
hist(arbres$Girth, main="Girth",xlab="Circonférence (m) ")
#Histogramme de la variable Height
hist(arbres$Height, main="Height",xlab="Hauteur (m) ")

####Boxplot####
#Division de la fenêtre de sortie en 3 
layout(matrix(1:3, 1, 3))
#Boxplot de la variable Volume
boxplot(arbres$Volume, main = "Volume", ylab="Volume (m3)")
#Boxplot de la variable Girth
boxplot(arbres$Girth, main = "Girth", ylab="Circonférence (m) ")
#Boxplot de la variable Height
boxplot(arbres$Height, main = "Height", ylab="Hauteur (m)")

####Projection des données par paires de variable####
pairs(arbres,pch=19)

####Matrice de corrélation####
cor(arbres)
#Test de corrélation
cor.test(arbres$Volume,arbres$Girth)
cor.test(arbres$Volume,arbres$Height)
cor.test(arbres$Height,arbres$Girth)

####Droite de régression linéaire pour chaque paire####
dev.off()
#Girth et Height
reg1=lm(arbres$Height~arbres$Girth)
#Pour obtenir les coefficients, p-value et r2
summary(reg1)
plot(arbres[,1:2],xlab="Circonférence (m)",ylab="Hauteur (m)")
abline(reg1$coefficients, col="red")
title("Régression linéaire Girth et Height")
#Girth et Volume
reg2=lm(arbres$Volume~arbres$Girth)
#Pour obtenir les coefficients, p-value et r2
summary(reg2)
plot(arbres[,c(1,3)],xlab="Circonférence (m)",ylab="Volume (m3)")
abline(reg2$coefficients, col="red")
title("Régression linéaire Girth et Volume")
#Heigh et Volume
reg3=lm(arbres$Volume~arbres$Height)
#Pour obtenir les coefficients, p-value et r2
summary(reg3)
plot(arbres[,2:3],xlab="Hauteur (m)",ylab="Volume (m3)")
abline(reg3$coefficients, col="red")
title("Régression linéaire Heigh et Volume")

####Représentation 3D#####
#centrage et réduction
arbresScale=scale(arbres)
#Plot3D des données
plot3d(arbresScale, type='s',size=1.5, col='red')
#Ajout de l'ellipse de corrélation
plot3d(ellipse3d(cor(arbresScale)), col = "grey", alpha = 0.5, add = TRUE)

####regression multiples####
regMultiple=lm(arbres$Volume~arbres$Heigh+arbres$Girth)    
#Pour obtenir les coefficients, p-value et r2
summary(regMultiple)

####coplot#####
#Distribution de la hauteur par equal count
plot(equal.count(arbres$Height,number=6, overlap=0.5), xlab="Hauteur (m)")
#coplot
xyplot(arbres$Volume~arbres$Girth|equal.count(arbres$Height,number=6, overlap=0.5),
       prepanel=function(x,y) prepanel.loess(x, y, span = 4/5, degree = 1),
       panel = function(x, y) {
         panel.grid(h = 2, col = 'black', lty = 3)
         panel.xyplot(x, y)
         panel.loess(x, y, span = 4/5, degree = 1)
       },
xlab="Circonférence (m)", ylab="Volume (m3)", labels="Hauteur (m)")


####Représentation des résidus par rapport à la variable exogène à expliquer####
#Stockage des résidus
res1 <- residuals(reg1)
res2 <- residuals(reg2)
res3 <- residuals(reg3)
#Représentation graphiquer
layout(matrix(1:3, 1, 3))
plot(arbres$Height, res1,xlab="Hauteur (m)",ylab="Résidus ")
title("Height par Girth")
plot(arbres$Volume, res2,xlab="Volume (m3)",ylab="Résidus ")
title("Volume par Girth")
plot(arbres$Volume, res3,xlab="Volume (m3)",ylab="Résidus ")
title("Volume par Height")

##Regression linéaire multiple
dev.off()
resRegMultiple=residuals(regMultiple)
plot(arbres$Volume, resRegMultiple,xlab="Volume (m3)",ylab="Résidus ")
title("Régression multiple")  




