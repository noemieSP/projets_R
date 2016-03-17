##########################
########PROJET############
##########################

##### 1. libraries ####
install.packages("corrgram");
install.packages("ROCR");
install.packages("SDMTools");
install.packages("e1071");
install.packages("randomForest");
install.packages("class");
install.packages("verification");
install.packages("FactoMineR");
install.packages("rgl");
install.packages("rpart");
install.packages("caret");
library("corrgram");
library("MASS");
library("ROCR");
library("SDMTools");
library("e1071");
library("randomForest");
library("class")
library("verification");
library("FactoMineR");
library("rgl");
library("rpart");
library("caret");
##### 2. data import ####
setwd("...");
df = read.table("VisaPremier.txt", header = TRUE, na.strings = ".");

##### 3. fast data description ####
head(df);
str(df);
#5 variables qualitatives / 43 variables quantitatives
dim(df);
#1073 lignes, 48 variables
summary(df);
##Le summary permet de donner quelques indicateurs par variable 
##On constate des variables qui pour chaque individu a la meme valeur
##Ou pour un ou deux individus a une valeur particuliere ou 3
#nbimpaye
#nbbon / nbeparte

##### 4. preprocessing step ####

##### 4.1. dealing with NAs ####

#There is a unreferenced variable named F
#We suppose that it stands for a missing value
#Hence we are going to generate random values for "F." records
df$sitfamil[df$sitfamil == "F."] = NA;

#Clients plus anciens que leur age
#On commence par transformer la var anciente en annee
df$anciente=ceiling(df$anciente/12);
table(df$anciente)
#l ancienete des individus passe de 33 a 60
#on considere donc les individus dont l ancienete est supp
#a 60 comme etant des individus aberrants
df[which(df$anciente>=60), 7] = NA;
#certains individus ont une ancienete supp a leur age
df[which((df$anciente)>df$age),c(5,7)];
#on les considere comme etant des NA
df[which((df$anciente)>df$age),7]=NA;

nas = which(is.na(df), arr.ind = T);
#number of NAs per variable
nas_agg = aggregate(formula = row ~ col, data = nas, FUN = length);
#extracting colnames of these variables
nas_colnames = colnames(df[ ,nas_agg[ ,1]]);
#features with missing values are : departem, codeqlt, agemvt, nbpaiecb

#dealing with variable "departem"

#departement is a nominal value, this means that there is no
#rank differences between values so we are going to generate
#random values given to frenquency table.
#concretly, we are going to give a probability for each value
#depending on her frequency

#replacing NA values by weighted random generated values
df[nas[nas[,2] == 2, 1], 2] = sample(sort(unique(df[which(!is.na(df[,2])), 2])),
                                     nas_agg[1,2],
                                     prob = as.vector(table(df$departem)),
                                     replace = T);

#dealing with variable "codeqlt"

#codeqlt is an ordinal value, this means we can say for example
#that A > B but we can not compute A + B. We can also use functions
#such as median

#replacing NA values by weighted random generated values
df[nas[nas[,2] == 9, 1], 9] = sample(sort(unique(df[which(!is.na(df[,9])), 9])),
                                     nas_agg[4,2],
                                     prob = as.vector(table(df$codeqlt)),
                                     replace = T);

#dealing with "agemvt"

#agemvt is a ratio (discrete) value. We can use operators such as +, *.
#replacing NA values by weighted random generated values
df[nas[nas[,2] == 22, 1], 22] = sample(sort(unique(df[which(!is.na(df[,22])), 22])),
                                       nas_agg[5,2],
                                       prob = as.vector(table(df$agemvt)),
                                       replace = T);

#dealing with "nbpaiecb"
#nbpaiecb is a ratio (discrete) value. We can use operators such as +, *.
#replacing NA values by weighted random generated values
df[nas[nas[,2] == 40, 1], 40] = sample(sort(unique(df[which(!is.na(df[,40])), 40])),
                                       nas_agg[6,2],
                                       prob = as.vector(table(df$nbpaiecb)),
                                       replace = T);

#dealing with "sitfamil"
df[nas[nas[,2] == 6, 1], 6] = sample(sort(unique(df[which(!is.na(df[,6])), 6])),
                                       nas_agg[2,2],
                                       prob = as.vector(table(df$sitfamil))[2:length(as.vector(table(df$sitfamil)))],
                                       replace = T);

#dealing with "ancienete"
df[nas[nas[,2] == 7, 1], 7] = sample(sort(unique(df[which(!is.na(df[,7])), 7])),
                                     nas_agg[3,2],
                                     prob = as.vector(table(df$anciente)),
                                     replace = T);

#free memory
rm(nas, nas_agg, nas_colnames);

#### 4.2. correcting aditionnal nonsenses ####

#Clients possesseurs de carte Visa mais 0 nb carte
table(df[which(df$cartevpr==1),41])
#17 à 0
intersect(which(df$nbcb == 0),which(df$cartevpr==1))
#Tous les individus ayant une carte visa ont au moins une carte dans le champ nbcarte
df[intersect(which(df$nbcb == 0),which(df$cartevpr==1)),41]=1



##### 4.3. dealing with useless or redundant features ####

df = df[ , -c(1, 4, 38, 10, 45, 39, 36, 37)];
#1 : id (useless)
#4 : sex (redundant) with sexer (46)
#38 : nbbon (useless) : only 1 record is equal to 1, rest is 0
#10 : nbimpaye (useless) : every records are equal to 0
#45 : cartevp (redundant) with cartevpr (47)
#39 : mtbon (useless) : only 1 record is equal to 1.95*10^7, rest is 0
#36 : nbeparte (useless) : only 3 records are equal to 1, rest is 0
#37 : mteparte (useless) : only 1 record is equal to 2.1*10^4, rest is 0


#### 4.4. dealing with quali ###

df_quanti = df[ , -c(1, 2, 4, 6, 7, 38)];
#2 : departem
#3 : ptvente
#4 : sex
#6 : sitfamil
#8 : csp
#9 : codeqlt
#45 : cartevp
#46 : sexer
#47 : cartevpr

#### 4.6. Correlations ####

#correlation matrix
cormat = cor(df_quanti);
#set every 1 on diag to 0
diag(cormat) = 0;
#extracting coordinates of variables with high cor
highcor = which(cormat > 0.75);
#extracting coordinates of variables with low cor
#and concatenating with high cor
highcor = union(highcor, which(cormat < -0.75));
#extracting i, j coordinate in the correlation matrix
i = floor(highcor/nrow(cormat)) + 1;
j = highcor %% nrow(cormat);
#extracting variable names for those which have at least one high cor
#with another one
rncors = unique(rownames(cormat[i,]));
cncors = unique(colnames(cormat[,j]));
ncors = union(rncors, cncors);
#building matrix with high cors variables
highcormat = cormat[ncors, ncors]
#ploting the correlogram for high correlation features
corrgram(highcormat);

#free memory
rm(i, j, highcor, ncors, rncors, cncors, cormat, highcormat);

#given to this correlogram, we can see that
#4 variables are highly correlated : moycred3, morcredi, mteparmo, avtscpte.
#plus, nbeparlo is highly correlated with aveparmo which is highly correlated
#with mteparlo.
#plus, engagemt engageml are highly correlated.
#plus, mtvie and aveparfi are highly correlated.

#this bring us to build another data frame with a preselection of our variables.
#In our high correlated matrix we had 11 variables.
#We are going to keep only 4 of them : moycred3, aveparmo, engagemt, mtvie
#So we are going to delete : moycredi, mteparmo, avtscpte, nbeparlo, mteparlo
#engageml and aveparfi.

short_df = df[ ,-c(18, 26, 36, 27, 28, 22, 37)];
short_df_quanti = df_quanti[ , -c(13, 21, 31, 22, 23, 17, 32)];

#### 4.5. clean files export functions ###
write.table(df, file="dfQualiQuanti.csv", sep = ";",
          row.names = FALSE, col.names = TRUE);

write.table(df_quanti, file="dfQuanti.csv", sep = ";",
            row.names = FALSE, col.names = TRUE);

write.table(short_df, file="shortDf.csv", sep = ";",
            row.names = FALSE, col.names = TRUE);

write.table(df_quanti, file="shortDfQuanti.csv", sep = ";",
            row.names = FALSE, col.names = TRUE);

#### 5. Exploratory ####

acp1=PCA(scale(df_quanti));

#Cercle de corr presque illisible -> bcp trop de var
#Fortement corr?l? nbbon/mtbon/moycred3/avtscpte
acp1$eig
#2 axes -> 26.70% de l'info
#3 axes -> 34.799% de l'info

acp1$var$contrib
#AXE1: aveparmo / nbop / nbeparmo / avtscpte
#AXE2: moycred3 / moycredi / mteparmo / avtscpte
#AXE3: nbjdebit / aveparmo / nbop / nbcb

#Repr?sentation 2D
plot.PCA(acp1, label="none", axe=c(1,2))

#1 individu abbérant
indParticulier = which(acp1$ind$coord[,2]<(-40));
#number 2

acp1=PCA(scale(df_quanti), ind.sup = indParticulier);
plot.PCA(acp1, label="none", axe=c(1,2))

#Coloration en fonction de possession de carte
#Representation 3D
couleur=df$cartevpr[-indParticulier];
couleur=as.numeric(couleur)

plot3d(acp1$ind$coord[,1:3], type='s',size=0.5, col=couleur+1)



#####################################
####CLASSIFICATION NON SUPERVISEE####
#####################################

#EN ayant enlevé l'individu un extrême pour toutes les méthodes

#######################
#######KMEANS

#Appel à la fonction Kmeans plusieurs fois
k1=kmeans(scale(df_quanti[-2,]),2)
k2=kmeans(scale(df_quanti[-2,]),2)
k3=kmeans(scale(df_quanti[-2,]),2)
k4=kmeans(scale(df_quanti[-2,]),2)

#Affichage de la répartition des groupes
table(k1$cluster)
table(k2$cluster)
table(k3$cluster)
table(k4$cluster)

#Enregistrement du clustering le plus retrouvé
resK=k1$cluster

#Coloration en fonction de la classification
#Représentation 3D
couleur=resK
couleur=as.numeric(couleur)
plot3d(acp1$ind$coord[,1:3], type='s',size=0.5, col=couleur)


######################
#DBSCAN
#Les paramètres eps et MinPts sont à modifier pour test
#install.packages("fpc")
#library("fpc", lib.loc="~/R/win-library/3.1")
is=dbscan(scale(df_quanti[-2,]), eps=5, MinPts=10)
resD=is$cluster

#Description de la répartition des groupes
table(resD)

#Affichage
couleur=resD
couleur=as.numeric(couleur)
couleur[couleur[]==0]=20

plot3d(acp1$ind$coord[,1:3], type='s',size=0.5, col=couleur)


######################
#PAM 
#Le deuxième paramètre est le nombre de cluster, ici 3
#library("cluster", lib.loc="C:/Program Files/R/R-3.1.1/library")
pam_nut <- pam(scale(df_quanti[-2,]), 2)
resP=(pam_nut$cluster)

table(resP)

#Affichage
couleur=resP
couleur=as.numeric(couleur)

plot3d(acp1$ind$coord[,1:3], type='s',size=0.5, col=couleur)

######################
#CAH
#4 Méthodes testées

hc_sing <- hclust(dist(scale(df_quanti[-2,])), method= "single")
hc_comp <- hclust(dist(scale(df_quanti[-2,])), method= "complete")
hc_aver <- hclust(dist(scale(df_quanti[-2,])), method= "average")
hc_ward <- hclust(dist(scale(df_quanti[-2,])), method= "ward.D2")


plot(hc_ward, hang = -1, main="Dendodogramme de la CAH Ward",
     xlab="groupe",ylab="hauteur")

resCAH=cutree (hc_ward, 2)
table(resCAH)


######################
##Matrice de confusion
##et indicateurs

#library("SDMTools", lib.loc="~/R/win-library/3.1")
obs=VisaPremier$cartevpr[-2]

########
#Kmeans
pred1=resK
pred1[which(pred1==1)]=0
pred1[which(pred1==2)]=1

pred2=resK
pred2[which(pred2==2)]=0

CM1=confusion.matrix(obs,pred1)
CM2=confusion.matrix(obs,pred2)

acc(CM1)
rec(CM1)
fme(CM1)
acc(CM2)
rec(CM2)
fme(CM2)

#########
#DBSCAN
pred1=resD

pred2=resD
pred2[which(pred2==1)]=2
pred2[which(pred2==0)]=1
pred2[which(pred2==2)]=0

CM1=confusion.matrix(obs,pred1)
CM2=confusion.matrix(obs,pred2)

acc(CM1)
rec(CM1)
fme(CM1)
fme(CM2)
rec(CM2)
fme(CM2)

#########
#PAM
pred1=resP
pred1[which(pred1==1)]=0
pred1[which(pred1==2)]=1

pred2=resP
pred2[which(pred2==2)]=0

CM1=confusion.matrix(obs,pred1)
CM2=confusion.matrix(obs,pred2)

acc(CM1)
rec(CM1)
fme(CM1)
acc(CM2)
rec(CM2)
fme(CM2)

#########
#CAH
pred1=resCAH
pred1[which(pred1==1)]=0
pred1[which(pred1==2)]=1

pred2=resCAH
pred2[which(pred2==2)]=0

CM1=confusion.matrix(obs,pred1)
CM2=confusion.matrix(obs,pred2)

acc(CM1)
rec(CM1)
fme(CM1)
acc(CM2)
rec(CM2)
fme(CM2)




#### 6. Classification ###

#Selecting a dataframe for classification

#scaled short df quanti
dfc = as.data.frame(cbind(scale(short_df_quanti[,-26]),short_df_quanti[,26] ));
colnames(dfc)[27] = "cartevpr";

#short df quanti
dfc = short_df_quanti;

#df
dfc = df;

#short df
dfc = short_df;

#df quanti
dfc = df_quanti;

#### 6.1. DF partitioning ###
#freezing samples
set.seed(109123126);
#getting randomly the ID of 70% of the rows of the dataset
train = sample(nrow(dfc), floor(nrow(dfc)*0.7));
#getting all the ID of the rows of the dataset
test = 1:nrow(dfc);
#setting the 30% other rows ID
test = setdiff(test, train);
#setting train values
train = dfc[train, ];
#setting test values
test = dfc[test, ];

#deleting outlier (or not)
#train = train[-which((rownames(train) == 2) ==TRUE), ];

#### 6.2. perf indicators ###

acc = function(cm) return((cm[1,1] + cm[2,2]) / (sum(cm)));
rec = function(cm) return(cm[2,2]/(cm[2,2] + cm[1,2]));
fme = function(cm) return((2*acc(cm)*rec(cm))/(acc(cm)+rec(cm))); 
  
#### 6.3. LDA ###

lda.model = lda(formula = cartevpr ~ . , data = train);
lda.pred = predict(lda.model, test);
lda.cm = confusion.matrix(obs = test$cartevpr, pred = as.numeric(lda.pred$class));
acc(lda.cm);
rec(lda.cm);
fme(lda.cm);
roc.area(obs = test$cartevpr, pred = as.numeric(lda.pred$x));
roc.plot(x = test$cartevpr, pred = as.numeric(lda.pred$x));


#### 6.4. QDA ###

qda.model = qda(formula = cartevpr ~ . , data = train);
qda.pred = predict(qda.model, test);
qda.cm = confusion.matrix(obs = test$cartevpr, pred = as.numeric(qda.pred$class));
acc(qda.cm);
rec(qda.cm);
fme(qda.cm);
roc.area(obs = test$cartevpr, pred = as.numeric(qda.pred$posterior[,2]));
roc.plot(x = test$cartevpr, pred = as.numeric(qda.pred$posterior[,2]));

#### 6.5 KNN ###

#maximizing recall
#no cv
maxrecall = 0;
kindex = -1;
for(i in 1:20) {
  knn.pred = as.numeric(knn(train = train, test = test, k = i, cl = train$cartevpr)) - 1;
  knn.cm = confusion.matrix(obs = test$cartevpr, pred = knn.pred);
  if(rec(knn.cm) > maxrecall) {
    kindex = i;
    maxrecall = rec(knn.cm);
  }
}

knn.pred = as.numeric(knn(train = train, test = test, k = kindex, cl = train$cartevpr)) - 1;
knn.cm = confusion.matrix(obs = test$cartevpr, pred = knn.pred);
acc(knn.cm);
rec(knn.cm);
fme(knn.cm);
roc.area(obs = test$cartevpr, pred = knn.pred);
roc.plot(x = test$cartevpr, pred = knn.pred);

#chosing k on training set with cross validation
maxrecall = 0;
kindex = -1;
for(i in 1:20) {
  knn.pred = as.numeric(knn.cv(train = short_df_quanti, k = i, cl = short_df_quanti$cartevpr)) - 1;
  knn.cm = confusion.matrix(obs = short_df_quanti$cartevpr, pred = knn.pred);
  if(rec(knn.cm) > maxrecall) {
    kindex = i;
    maxrecall = rec(knn.cm);
  }
}

knn.pred = as.numeric(knn(train = train, test = test, k = kindex, cl = train$cartevpr)) - 1;
knn.cm = confusion.matrix(obs = test$cartevpr, pred = knn.pred);
acc(knn.cm);
rec(knn.cm);
fme(knn.cm);
roc.area(obs = test$cartevpr, pred = knn.pred);
roc.plot(x = test$cartevpr, pred = knn.pred);

#### 6.6 Logistic Regression ###

glm.model = glm(cartevpr ~ ., data = train);
summary(glm.model);
#var significatives  :
# age anciente nbopguic engagemt moysold3 nbop mtfactur nbcb nbjdebit
glm.pred = predict(glm.model, test);
glm.cm = confusion.matrix(obs = test$cartevpr, pred = glm.pred);
acc(glm.cm);
rec(glm.cm);
fme(glm.cm);
roc.area(obs = test$cartevpr, pred = glm.pred);
roc.plot(x = test$cartevpr, pred = glm.pred);

#odds ratio
or = exp(coef(glm.model));
or;
#on a 1.39 fois plus de chance de prendre la carte visa premier si on a nbcb>=1
#

#10 fold cross validation (ne semble pas fonctionner...)

tc = trainControl("cv",10,savePred=T)
fit = train(cartevpr~.,data=short_df,method="svm",trControl=tc,family=poisson(link = "log"))
glm.cm = confusion.matrix(obs = short_df$cartevpr, pred = fit$pred$obs);
acc(glm.cm);
rec(glm.cm);
fme(glm.cm);

#### 6.7 SVM ###

svm.model = svm(cartevpr ~ ., data = train);
summary(svm.model);
#kernel = radial, cost = 1, gamma = 0.04, epsilon = 0.1
svm.pred = predict(svm.model, test);
svm.cm = confusion.matrix(obs = test$cartevpr, pred = svm.pred);
acc(svm.cm);
rec(svm.cm);
fme(svm.cm)
roc.area(obs = test$cartevpr, pred = svm.pred);
roc.plot(x = test$cartevpr, pred = svm.pred);

#10 fold cross validation (ne semble pas fonctionner...)

tc = trainControl("cv",10,savePred=T)
fit = train(cartevpr~.,data=short_df,method="svmLinear",trControl=tc)
svm.cm = confusion.matrix(obs = short_df$cartevpr, pred = fit$pred$obs);
acc(svm.cm);
rec(svm.cm);
fme(svm.cm);


#### 6.8 Random Forest ###

rf.model = randomForest(cartevpr ~ ., data = train);
summary(rf.model);
rf.pred = predict(rf.model, test);
rf.cm = confusion.matrix(obs = test$cartevpr, pred = rf.pred);
acc(rf.cm);
rec(rf.cm);
fme(rf.cm);
roc.area(obs = test$cartevpr, pred = rf.pred);
roc.plot(x = test$cartevpr, pred = rf.pred);

#### 6.9 Trees ###

tree.model = rpart(cartevpr ~ ., data = train);
plot(tree.model);
text(tree.model, use.n = TRUE);
tree.pred = predict(tree.model, test);
tree.cm = confusion.matrix(obs = test$cartevpr, pred = tree.pred);
acc(tree.cm);
rec(tree.cm);
fme(tree.cm);
roc.area(obs = test$cartevpr, pred = tree.pred);
roc.plot(x = test$cartevpr, pred = tree.pred);

#Selected attributes: 2,5,9,10,13,15,17,24 : 8
#anciente
#moycred3
#engagemc
#engagemm
#agemvt
#mtfactur
#mtvie
#nbcb

train2 = sample(nrow(short_df_quanti), floor(nrow(short_df_quanti)*0.7));
train2 = short_df_quanti[train2,]
test2 = 1:nrow(short_df_quanti);
#setting the 30% other rows ID
test2 = setdiff(test2, train2);
#setting test values
test2 = short_df_quanti[test2, ];

str(short_df_quanti[,c(2,5,9,10,13,15,17,24)])

tree.model = rpart(cartevpr ~ ., data = train2[,c(2,5,9,10,13,15,17,26)]);
plot(tree.model);
text(tree.model, use.n = TRUE);
tree.pred = predict(tree.model, test2);
tree.cm = confusion.matrix(obs = test2$cartevpr, pred = tree.pred);
acc(tree.cm);
rec(tree.cm);
fme(tree.cm);
roc.area(obs = test2$cartevpr, pred = tree.pred);
roc.plot(x = test2$cartevpr, pred = tree.pred);

glm.model = glm(cartevpr ~ ., data = train2[,c(2,5,9,10,13,15,17,26)]);
summary(glm.model);
#var significatives  :
# age anciente nbopguic engagemt moysold3 nbop mtfactur nbcb nbjdebit
glm.pred = predict(glm.model, test2);
glm.cm = confusion.matrix(obs = test2$cartevpr, pred = glm.pred);
acc(glm.cm);
rec(glm.cm);
fme(glm.cm);
roc.area(obs = test2$cartevpr, pred = glm.pred);
roc.plot(x = test2$cartevpr, pred = glm.pred);

train$cartevpr
write.table(short_df_quanti, file="shortDfQuanti.csv", sep = ";",
            row.names = FALSE, col.names = TRUE);

svm.model = svm(cartevpr ~ ., data = train2[,c(2,5,9,10,13,15,17,26)]);
summary(svm.model);
#kernel = radial, cost = 1, gamma = 0.04, epsilon = 0.1
svm.pred = predict(svm.model, test2);
svm.cm = confusion.matrix(obs = test2$cartevpr, pred = svm.pred);
acc(svm.cm);
rec(svm.cm);
fme(svm.cm)
roc.area(obs = test2$cartevpr, pred = svm.pred);
roc.plot(x = test2$cartevpr, pred = svm.pred);

rf.model = randomForest(cartevpr ~ ., data = train2[,c(2,5,9,10,13,15,17,26)]);
summary(rf.model);
rf.pred = predict(rf.model, test2);
rf.cm = confusion.matrix(obs = test$cartevpr, pred = rf.pred);
acc(rf.cm);
rec(rf.cm);
fme(rf.cm);
roc.area(obs = test2$cartevpr, pred = rf.pred);
roc.plot(x = test2$cartevpr, pred = rf.pred);