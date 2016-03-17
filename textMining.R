#####################################################################
#########                                           #################
#########       PROJET FOUILLE DE TEXTE             #################
#########    Classification des emails reçus        #################
#########                                           #################
#####################################################################
#########                                           #################
#########         Noémie Salaun-Penquer             #################
#########                                           #################
#####################################################################

####librairies
library(tm)
library(SnowballC)
library(rpart)
library("tm.plugin.mail", lib.loc="...")
library("FactoMineR")
library("rgl")

####Intégration des fichiers####

corpusData = Corpus(DirSource("C:/monChemin/sample enron dataset",recursive = TRUE))

str(corpusData)
#liste de 75210 éléments
####Données trop volumineuses####

####Etude à partir des données de Beck-s ####
DsourceBS=DirSource("C:/monChemin/sample enron dataset/beck-s",recursive = TRUE)
corpusBS = Corpus(DsourceBS)
print(corpusBS)
#Liste de 9156 éléments

#Nombre aléatoire 25% de la base
nbAlea=round(runif(2289, min=1, max=9156), digits=2)

#25% des mails de Becks_s
corpusMin=corpusBS[c(nbAlea)]
print(corpusMin)
#Liste de 2289 éléments

#Liste de préClassification
ClassifInit=DsourceBS$filelist[c(nbAlea)]
ClassifInit=gsub("C:/.../", "", ClassifInit)
ClassifInit=gsub("/", "zzzz", ClassifInit)
ClassifInit=gsub("_", "aaaa", ClassifInit)

ClassifInit=substr(ClassifInit,1,4)
classesCompt=table(ClassifInit)
dim(classesCompt)
#81 classes

#########################
####Preparation du corpus

##Passer toutes les majuscules du texte en minuscule
corpusMin2=tm_map(corpusMin,content_transformer(tolower))

##Suppression des entêtes et des messages originaux
suppressionEnTeteMO= function(A) {
  AA=A$content
  numL=which(AA=="x-origin: beck-s")+2
  if (length(numL)==0){
    numL=1}
  n=which(AA==" -----original message-----" | AA==">" | AA=="----original message-----" )
  nbT=length(AA)
  if (length(n)!=0){
    nbT=n-1}
  res=AA[numL:nbT]
  return (res)
}

#Parcours et suppression des entêtes et original message dans tout le corpus
for(i in 1:length(corpusMin2)) {
  corpusMin2[[i]]$content=suppressionEnTeteMO(corpusMin2[[i]])
}

##Suppression de la ponctuation
corpusMin2 <- tm_map(corpusMin2,removePunctuation)

##Suppression des nombres
corpusMin2 <- tm_map(corpusMin2,removeNumbers)

##Suppression des surplus d'espaces
corpusMin2 <- tm_map(corpusMin2,stripWhitespace)

#########################
####Dictionnaire

#Sauvegarde de la matrice prétraitée
corpusInit=corpusMin2

#Retrait des Stopward
stopM=stopwords('english')
corpusMin2=tm_map(corpusMin2,content_transformer(removeWords),stopM)

#Transformation des stemming
corpusMin2 <- tm_map(corpusMin2,stemDocument)

#matrice creuse
dtm=TermDocumentMatrix(corpusMin2)
print(dtm)
dim(dtm)

#matrice sans les mots éparses
matriceMots = removeSparseTerms(dtm,0.99)
print(matriceMots)
dim(matriceMots)
#1306 mots 

#Afficher le mot dont la fréquence dépasse 500
findFreqTerms(matriceMots, 500)
#74 mots

#matrice inspect
mat.valeurs = inspect(matriceMots)
dim(mat.valeurs)
#transposition de la matrice
transp.mat.valeurs = t(mat.valeurs)
#Transformation en data.frame
data = as.data.frame(transp.mat.valeurs)
print(nrow(data))
print(ncol(data))

dim(data)
#2289 1352
#Nb doc et nb mots 

#calculer le nombre de presence de chaque mot dans l'ensemble des mails
freq = colSums(data)
print(freq)
#effacer les termes trop peu frequents
#retenir ceux qui sont dans au moins 200 mails
k = 200
ok.freq = which(freq >= k)
print(ok.freq)

#filtrer le data.frame
dataFiltre = subset(data,select=ok.freq)
print(nrow(dataFiltre))
print(ncol(dataFiltre))
#250 mots

dim(dataFiltre)

##### Analyse du jeu de données obtenu ####

#Représentation des mails

acp1=PCA(scale(dataFiltre))
plot.PCA(acp1, label="none", axe=c(1,2))
#Retrait des mails extrêmes
mailParticulier = which(acp1$ind$coord[,1]>(40))
acp1=PCA(scale(dataFiltre), ind.sup = mailParticulier)
plot.PCA(acp1, label="none", axe=c(1,2))
#Représentation 3D
plot3d(acp1$ind$coord[,1:3], type='s',size=0.5)
#coloration en fonction de la classification initiale

couleur=ClassifInit[-c(mailParticulier)]
vec.couleur=unique(couleur)
nbC = dim(classesCompt)
for(i in 1:nbC) {
  couleur=gsub(vec.couleur[i],as.numeric(i),couleur)
}
table(couleur)
couleur=as.numeric(couleur)
plot3d(acp1$ind$coord[,1:3], type='s',size=0.5,col=couleur)

##### Classification non supervisée ####
#Avec retrait des mails extrêmes
dataRechercheC=dataFiltre[-mailParticulier,]
#########
##CAH

#Test de toutes les méthodes
#hc_sing <- hclust(dist(scale(dataRechercheC)), method= "single")
hc_comp <- hclust(dist(scale(dataRechercheC)), method= "complete")
#hc_aver <- hclust(dist(scale(dataRechercheC)), method= "average")
#hc_ward <- hclust(dist(scale(dataRechercheC)), method= "ward.D2")

plot(hc_comp, hang = -1, main="Dendodogramme de la CAH complete",
     xlab="groupe",ylab="hauteur")

resCAH=cutree (hc_comp, 10)
table(resCAH)
#Un groupe de 2111 mails
#Mauvais classement

#En tenant compte du nombre de classe comptabilisé initialement
resCAH=cutree (hc_comp, length(table(ClassifInit)))
table(resCAH)
#On a tjs une classe avec 1954 mails

#########
##KMEANS

recherchekmeans = function(donnees) {
  va = c();
  for(i in 1:81) {
    c1 = kmeans(donnees, i);
    va = rbind(va, c1$tot.withinss);
  }
  plot(va);
  return(va);
}

recherchekmeans(dataRechercheC)
#Décroissance jusqu'à 7 groupes puis augmentation et à nouveau décroissance
#donc test à 10
##Test 10 groupes
k1=kmeans(scale(dataRechercheC),10)
k2=kmeans(scale(dataRechercheC),10)
k3=kmeans(scale(dataRechercheC),10)
k4=kmeans(scale(dataRechercheC),10)

#Affichage de la répartition des groupes
table(k1$cluster)
table(k2$cluster)
table(k3$cluster)
table(k4$cluster)

#Enregistrement du clustering proposant une meilleure répartition
resK10=k3$cluster
kFinal=k3
##Test 81 groupes
k1=kmeans(scale(dataRechercheC),dim(classesCompt))
k2=kmeans(scale(dataRechercheC),dim(classesCompt))
k3=kmeans(scale(dataRechercheC),dim(classesCompt))
k4=kmeans(scale(dataRechercheC),dim(classesCompt))

#Affichage de la répartition des groupes
sort(table(k1$cluster))
sort(table(k2$cluster))
sort(table(k3$cluster))
sort(table(k4$cluster))

#Enregistrement du clustering proposant une meilleure répartition
resK81=k3$cluster

#Coloration en fonction de la classification
couleur=resK81
couleur=as.numeric(couleur)
plot3d(acp1$ind$coord[,1:3], type='s',size=0.5, col=couleur+1)
couleur=resK10
couleur=as.numeric(couleur)
plot3d(acp1$ind$coord[,1:3], type='s',size=0.5, col=couleur+1)

classificationK=cbind(dataRechercheC,resK10)

############################
##Interprétation des groupes

##fonction pour sortir les mots les plus fréquents de chaque groupe
MotFreqClass <- function(x,k){
  lignes=which(classificationK$resK10==x)
  gp=classificationK[lignes,]
  #fréquence
  freq = colSums(gp[,-253])
  freq=colSums(gp[,which (freq>k)])
  freq=sort(freq)
  return(freq)
}

###Groupe 1
#33 mails
(FreqGP1=MotFreqClass(1,50))
#finance

###Groupe 2
#20 mails
(FreqGP2=MotFreqClass(2,50))
#energi america

###Groupe 3
#12 mails
(FreqGP3=MotFreqClass(3,50))
#eol

###Groupe 4
#43 mails
(FreqGP4=MotFreqClass(4,50))
#AIde: use / problem / team / respon / book

###Groupe 5
#1545 mails
(FreqGP5=MotFreqClass(5,100))

###Groupe 6
#10 mails
(FreqGP6=MotFreqClass(6,30))
#gas

###Groupe 7
#497 mails
(FreqGP7=MotFreqClass(7,200))

###Groupe 8
#22 mails
(FreqGP8=MotFreqClass(8,30))
#urgent : need / risk / requir / action

###Groupe 9
#103 mails
(FreqGP9=MotFreqClass(9,50))
#finance inovation? new report control

###Groupe 10
#3 mails
(FreqGP10=MotFreqClass(10,20))
#détente

#####################
####Nouveaux mails

####A classer####
nbAlea=round(runif(2289, min=1, max=9156), digits=2)

alea=round(runif(1, min=1, max=9156), digits=2)
ex= corpusBS[alea]
ex[[1]]$content

####Prétraitement####

##fonction pour sortir les mots les plus fréquents de chaque groupe
pretraitement <- function(corpusMin){
  ##Passer toutes les majuscules du texte en minuscule
  corpusMin2=tm_map(corpusMin,content_transformer(tolower))
  ##Suppression des entêtes et des messages originaux
  for(i in 1:length(corpusMin2)) {
    corpusMin2[[i]]$content=suppressionEnTeteMO(corpusMin2[[i]])
  }
  ##Suppression de la ponctuation
  corpusMin2 <- tm_map(corpusMin2,removePunctuation)
  
  ##Suppression des nombres
  corpusMin2 <- tm_map(corpusMin2,removeNumbers)
  
  ##Suppression des surplus d'espaces
  corpusMin2 <- tm_map(corpusMin2,stripWhitespace)
  
  #Retrait des Stopward
  stopM=stopwords('english')
  corpusMin2=tm_map(corpusMin2,content_transformer(removeWords),stopM)
  
  #Transformation des stemming
  corpusMin2 <- tm_map(corpusMin2,stemDocument)
  
  return(corpusMin2)
}

ex2=pretraitement(ex)
ex2[[1]]$content

####recontruction de la matrice####

##A trouver une solution?

####Classification du nouveau mail####

##Fonction qui retourne le classement d'un fichier passé en paramètre 
#(numéro de ligne dans la matrice complétée)
prediction <- function(numLigne){
  nouveauFichier = dataRechercheC[numLigne,]
  centreGravite=kFinal$centers
  Concat=rbind(nouveauFichier,centreGravite)
  A=dist(Concat)
  A=A[1:10]
  groupePred=which(A==min(A))
  return (groupePred)
}

##TEST sur une ligne choisi au hasard
#Nombre aléatoire 
nbLigne=dim(classificationK)[1]
nbAleaT=round(runif(1, min=1, max=nbLigne), digits=2)
test=prediction(nbAleaT)
#Vérif
col=dim(classificationK)[2]
groupeInitKmeans=classificationK[nbAleaT,col]
test==groupeInitKmeans
#true
#OK
