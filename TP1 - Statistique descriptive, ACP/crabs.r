library(MASS)
data(crabs)
crabsquant<-crabs[,4:8]
plot(crabsquant)
plot(crabs$sex,crabs$CL)

fem<-subset(crabs, sex=="F")
mal<-subset(crabs, sex=="M")
summary(fem)
summary(mal)

b<-subset(crabs, sp=="B")
o<-subset(crabs, sp=="O")
boxplot(b)
boxplot(o)

######QUESTION 1.2######
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP1/Crabs/sexPlot.png")
plot(crabs[,4:8],bg=c("red","green3")[crabs[,2]],
  pch=c(21,22)[crabs[,2]],  labels=c("Frontal lob size","Rear width",
  "Carapace length","Carapace width", "body depth")
)
dev.off()

png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP1/Crabs/speciesPlot.png")
plot(crabs[,4:8],bg=c("red","green3")[crabs[,1]],
  pch=c(21,22)[crabs[,1]],
  labels=c("Frontal lob size","Rear width",
  "Carapace length","Carapace width", "body depth")
)
dev.off()

cor(crabsquant)
######QUESTION 1.1######
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP1/Crabs/sexBoxplot.png")
boxplot(list(mal$FL,fem$FL, mal$RW, fem$RW, mal$CL, fem$CL, mal$CW, fem$CW, mal$BD, fem$BD), method="jitter", vertical=T, col=c("red", "red", "blue", "blue", "green","green", "yellow", "yellow","pink","pink"), ylab="size(mm)", las = 2, names=c("male", "female","male", "female","male", "female","male", "female","male", "female"))
legend("topright", inset=.05, title="Colors",c("FL","RW","CL","CW","BD"), fill=c("red" ,"blue", "green","yellow","pink"), horiz=FALSE)
dev.off()

png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP1/Crabs/speciesBoxplot.png")
boxplot(list(b$FL,o$FL, b$RW, o$RW, b$CL, o$CL, b$CW, o$CW, b$BD, o$BD), col=c("red", "red", "blue", "blue", "green","green", "yellow", "yellow","pink","pink"), ylab="size(mm)", names=c("B", "O","B", "O","B", "O","B", "O","B", "O"))
legend("topright", inset=.05, title="Colors",
   c("FL","RW","CL","CW","BD"), fill=c("red" ,"blue", "green","yellow","pink"), horiz=FALSE)
dev.off()

######QUESTION 2.1######
#Matrice de l'énoncé
X <- matrix(c(3,1,2,4,4,4,3,1,3,3,6,2), nrow=4)
#Matrice centrée 
M <- matrix(c(0.5,-1.5,-0.5,1.5,1,1,0,-2,-0.5,-0.5,2.5,-1.5), nrow=4)
#Matrice de variance de M
VarM <- (1/4)*(t(M)%*%M)
#Vecteur propre de M
(u <- eigen(VarM)$vectors)
#Composantes principales
c <- M%*%u

######QUESTION 2.2######
cp <- princomp(M)
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP1/Crabs/premierplan.png")
(biplot(cp, ylab = "axe 2", xlab = "axe 1")
abline(h=0,v=0) )
dev.off()

######QUESTION 2.3######
cor <- cor(M, c)
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP1/Crabs/varplan.png")
plot(cor, xlab = "axe 1", ylab = "axe 2", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), col = "blue", pch=c(16,16, 16))
text(cor, pos = 1, labels = c("Var1", "Var2", "Var3"))
abline(h=0,v=0) 
symbols(0, 0, circle = 1, inches = F, add = T)
dev.off()

######QUESTION 2.4#########
exp <- function(k, c, u) {
	m <- 0;
	for (i in k)
		m = m + c[,i]%*%t(u[,i]);
	m
}
######QUESTION 3.1#########
	
 #Création de la matrice de notes
M = matrix(c(6.0, 6.0, 5.0, 5.5, 8.0, 8.0, 8.0, 8.0, 8.0, 9.0, 6.0, 7.0, 11.0, 9.5, 11.0, 14.5, 14.5, 15.5, 15.0, 8.0, 14.0, 14.0, 12.0, 12.5, 10.0, 11.0, 10.0, 5.5, 7.0, 13.0, 5.5, 7.0, 14.0, 11.5, 10.0, 13.0, 12.5, 8.5, 9.5, 12.0, 9.0, 9.5, 12.5, 12.0, 18.0), nrow = 9, byrow = T)
rownames(M) = c("jean", "aline", "annie", "monique", "didier", "andré", "pierre", "brigitte", "evelyne")
colnames(M) = c("math", "scie", "fran", "lati","d-m")

#Centrage de la matrice M en colonne
(X <- scale(M, center=T, scale=F))

#Création de la matrice de variance
(V <- (1/9)*(t(X)%*%X))

#Axes principaux d'inertie
U <- eigen(V)
#Valeurs propres
(ValeursPropres <- U$values)
#Vecteurs propres
(VecteursPropres <- U$vectors)

#Composantes principales
(C <- X%*%U$vectors)

#Plans de représentation
RP <- princomp(X)
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP1/Crabs/repplan.png")
biplot(RP, ylab = "axe 2", xlab = "axe 1", var.axes = FALSE, main="ACP :Exemple des notes"
)
abline(h=0,v=0) 
dev.off()

#Cercle de corrélation
cor <- cor(M, C)

png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP1/Crabs/premplan.png")
biplot(cor, xlab = "axe 1", ylab = "axe 2", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), col = "blue", pch=c(16), main="ACP : Exemple de notes dans le premier plan factoriel")
text(cor, pos = 1, labels = c("math", "scie", "fran", "lati","d-m"))
abline(h=0,v=0) 
symbols(0, 0, circle = 1, inches = F, add = T)
dev.off()

png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP1/Crabs/deuplan.png")
cor2 <- cbind(cor[,1], cor[,3])
plot(cor2, xlab = "axe 1", ylab = "axe 2", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), col = "blue", pch=c(16), main="ACP : Exemple de notes dans le premier plan factoriel")
text(cor2, pos = 1, labels = c("math", "scie", "fran", "lati","d-m"))
abline(h=0,v=0) 
symbols(0, 0, circle = 1, inches = F, add = T)
 dev.off()


######QUESTION 2.3#########
library(MASS)
data(crabs)
crabsquant<-crabs[,4:8]

#Centrage de la matrice M en colonne
(X <- scale(crabsquant, center=T, scale=F))

#Création de la matrice de variance
(V <- (1/200)*(t(X)%*%X))

#Axes principaux d'inertie
U <- eigen(V)
#Valeurs propres
(ValeursPropres <- U$values)
#Vecteurs propres
(VecteursPropres <- U$vectors)

#Composantes principales
(C <- X%*%U$vectors)

#Plans de représentation
RP <- princomp(X)
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP1/Crabs/crabsrepplan.png")
biplot(RP, ylab = "axe 2", xlab = "axe 1", main="ACP sur les données Crabs")
abline(h=0,v=0) 
dev.off()

#Cercle de corrélation
cor <- cor(X, C)

png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP1/Crabs/crabspremplan.png")
plot(cor, xlab = "axe 1", ylab = "axe 2", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), col = "blue", pch=c(16), main="ACP sur les données Crabs")
text(cor, pos = 1,labels = c("FL", "RW", "CL", "CW","BD"))
abline(h=0,v=0) 
symbols(0, 0, circle = 1, inches = F, add = T)
dev.off()

plot(RP$scores,bg=c("red","green3")[crabs[,2]],
  pch=c(21,22)[crabs[,2]],  labels=c("Frontal lob size","Rear width",
  "Carapace length","Carapace width", "body depth")
)
######################

  crabsquant2 = crabsquant / crabsquant$CW
crabsquant2 = crabsquant2[,-4]
RP2 <- princomp(crabsquant2)

png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP1/Crabs/biplotRP2.png")

biplot(RP2, main = "ACP sur les données Crabs après traitement")
abline(h=0,v=0)
dev.off()


png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP1/Crabs/spplotRP2.png")
plot(crabsquant2, bg=c("red","green3")[crabs$sp],
  pch=c(21,22)[crabs[,2]],  labels=c("Frontal lob size","Rear width",
  "Carapace length","Carapace width", "body depth"))
dev.off()

png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP1/Crabs/sexplotRP2.png")
plot(crabsquant2, bg=c("red","green3")[crabs$sex],
  pch=c(21,22)[crabs[,2]],  labels=c("Frontal lob size","Rear width",
  "Carapace length","Carapace width", "body depth"))
  dev.off()
  
  
  
  
  #Centrage de la matrice M en colonne
(X <- scale(crabsquant2, center=T, scale=F))

#Création de la matrice de variance
V <- (1/200)*(t(X)%*%X)

#Axes principaux d'inertie
U <- eigen(V)
#Valeurs propres
(ValeursPropres <- U$values)
#Vecteurs propres
(VecteursPropres <- U$vectors)

#Composantes principales
(C <- X%*%U$vectors)

cor <- cor(X, C)
  
  png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP1/Crabs/RP2premplan.png")
plot(cor, xlab = "axe 1", ylab = "axe 2", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), col = "blue", pch=c(16), main="ACP sur les données Crabs")
text(cor, pos = 1,labels = c("FL", "RW", "CL", "CW","BD"))
abline(h=0,v=0) 
symbols(0, 0, circle = 1, inches = F, add = T)
dev.off()
