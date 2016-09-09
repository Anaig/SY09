####QUESTION 1######
data(iris) 
iris2<-iris[-c(5)]

plan1<-princomp(iris2)

png(filename="/Users/ana/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP2/TP2/irispremplan.png")
biplot(plan1, xlab="axe1", ylab="axe2", pch=16)
abline(h=0, v=0)
dev.off()

png(filename="/Users/ana/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP2/TP2/irisspecie.png")
plot(plan1$scores, pch=16, col=c("blue","red","green","yellow")[iris[,5]])
legend("topright", inset=.05, title="Species", c("Setosa", "Versicolor", "Virginica"), fill=c("blue","red","green"), horiz=FALSE)
abline(h=0,v=0)
dev.off()

####QUESTION 2######
crabs2<-read.csv("crabs2.csv",header=T)
crabsquant<-crabs2[,1:4]
crabsacp <- princomp(crabsquant)
png(filename="/Users/ana/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP2/TP2/crabspremplan.png")
biplot(crabsacp , xlab="axe1", ylab="axe2")
abline(h=0, v=0)
dev.off()

#Crab species
png(filename="Z:/SY09/TP2/crabspecie")
plot(crabsacp$scores, col=c("blue","red")[crabs2[,5]])
#text(crabsacp$scores, labels=crabs2[,5])
legend("topright", inset=.05, title="Species", c("B", "O"), fill=c("blue","red"), horiz=FALSE)
abline(h=0,v=0)
dev.off()


#Crab sex
png(filename="Z:/SY09/TP2/crabsex")
plot(crabsacp$scores, col=crabs2$sex)
#text(crabsacp$scores, labels=crabs2$sex)
#legend(0,0,legend=levels(crabs2[,6]), inset=.05, title="Sex", col=1:2, horiz=FALSE)

legend(0,0,legend=levels(crabs2$sex), col=c(1:2))
abline(h=0,v=0)
dev.off()

#Crab sex + species
png(filename="/Users/ana/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP2/TP2/crabsex_specie.png")
plot(crabsacp$scores, pch=16, col=c("blue","red")[crabs2[,6]])
text(crabsacp$scores, pos=1,labels=crabs2[,5])
legend("topright", inset=.05, title="Species", c("M", "F"), fill=c("blue","red"), horiz=FALSE)
abline(h=0,v=0)
dev.off()


######QUESTION 3#######
library(MASS)
mut<-read.csv("mutations2.csv",header=T,row.names=1)
mut<-as.dist(mut,diag=T,upper=T)
mut2<-cmdscale(mut, k=2)
mutmat <- as.matrix(mut)

png(filename="/Users/ana/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP2/TP2/aftdmut.png")
plot(mut2, pch=16, col=palette(), xlab="Axe 1", ylab="Axe 2")
text(mut2[,1], mut2[,2],labels(mutmat[,1]), pos=1)
abline(h=0,v=0)
dev.off()

s1=Shepard(as.dist(mut),cmdscale(mut,k=2))

png(filename="/Users/ana/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP2/TP2/distance_mut.png")
plot(s1, pch=16, col=palette(), xlab="Dissimilarité", ylab="Distance")
abline(0, 1)
dev.off()

s3=Shepard(as.dist(mut),cmdscale(mut,k=3))
s4=Shepard(as.dist(mut),cmdscale(mut,k=4))
s5=Shepard(as.dist(mut),cmdscale(mut,k=5))

png(filename="/Users/ana/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP2/TP2/shepards.png")
par(mfrow=c(3,1))
plot(s3, pch=16, col=palette(), xlab="Dissimilarité", ylab="Distance", main="Diagramme de Shepard pour d=3")
abline(0, 1)
plot(s4, pch=16, col=palette(), xlab="Dissimilarité", ylab="Distance", main="Diagramme de Shepard pour d=4")
abline(0, 1)
plot(s5, pch=16, col=palette(), xlab="Dissimilarité", ylab="Distance", main="Diagramme de Shepard pour d=5")
abline(0, 1)
dev.off()

#MANQUE LE GRAPHE

###########EXERCIE 2############

#####QUESTION 1######

dendo<-hclust(mut,method="ward.D2") #classification ascendante
dendo2<-hclust(mut,method="single") 
dendo3<-hclust(mut,method="complete") 
dendo4<-hclust(mut,method="average") 
dendo5<-hclust(mut,method="mcquitty") 
dendo6<-hclust(mut,method="median") 
dendo7<-hclust(mut,method="centroid") 

#affichage d'un dendogramme
png(filename="/Users/ana/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP2/TP2/dendo1.png")
plot(dendo, main="Method = ward.D2")
dev.off()
png(filename="/Users/ana/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP2/TP2/dendo2.png")
plot(dendo2, main="Method = single")
dev.off()
png(filename="/Users/ana/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP2/TP2/dendo3.png")
plot(dendo3, main="Method = complete")
dev.off()
png(filename="/Users/ana/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP2/TP2/dendo4.png")
plot(dendo4, main="Method = average")
dev.off()
png(filename="/Users/ana/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP2/TP2/dendo5.png")
plot(dendo5, main="Method = mcquitty")
dev.off()
png(filename="/Users/ana/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP2/TP2/dendo6.png")
plot(dendo6, main="Method = median")
dev.off()
png(filename="/Users/ana/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP2/TP2/dendo7.png")
plot(dendo7, main="Method = centroid")
dev.off()

#####QUESTION 2######
data(iris)
iris<-iris[-c(5)]
d=dist(iris[1:4])*dist(iris[1:4])

dendo2<-hclust(d,method="ward.D2")
png(filename="/Users/ana/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP2/TP2/dendoiris.png")
plot(dendo2)#affichage d'un dendogramme on retrouve bien les 3 especes
rect.hclust(dendo2, 3, border = c("blue", "red", "green"))
dev.off()


#####QUESTION 3######
library(cluster)
dendo3<-diana(d) #classification descendante
png(filename="/Users/ana/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP2/TP2/dendodiris.png")
plot(dendo3)
dev.off()


###########EXERCICE 3############

#on enleve les varibles qualitatives avant de clusteriser

#iris
library(cluster)
data(iris)
iris<-iris[-c(5)]

kmeans(iris,3) # Pour voir comment il clusterise
(k1=kmeans(iris,2))
k2=kmeans(iris,3)
k3=kmeans(iris,4)
plot(iris[1:4],col = c("green","blue")[k1$cluster], pch=16) 

png(filename="/Users/ana/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP2/TP2/iris_k.png")
par(mfrow=c(3,1))
clusplot(iris,k1$cluster,color=TRUE, main="K=2")#affichage avec 2 clusters
clusplot(iris,k2$cluster,color=TRUE, main="K=3")
clusplot(iris,k3$cluster,color=TRUE, main="K=4")
dev.off()

#affichage plusieurs class k=3

k3_1=kmeans(iris,3)
k3_2=kmeans(iris,3)

png(filename="/Users/ana/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP2/TP2/iris_k3.png")
par(mfrow=c(2,2))
clusplot(iris,k3_1$cluster,color=TRUE,main="Classification avec 3 classes")
clusplot(iris,k3_2$cluster,color=TRUE,main="Classification avec 3 classes")
dev.off()



#QUESTION 3





iner_tab<-matrix(0,100,8)
colnames(iner_tab)=c("k=3","k=4","k=5","k=6","k=7","k=8","k=9","k=10")
for (k in 3:10){
	for (i in 1:100){
		iner_tab[i,k-2]=kmeans(iris,k)$tot.withinss
	}
	
}
min_iner=apply(iner_tab,2,min)

iner_tab<-matrix(0,100,10)
colnames(iner_tab)=c("k=1","k=2","k=3","k=4","k=5","k=6","k=7","k=8","k=9","k=10")
for (k in 1:10){
	for (i in 1:100){
		iner_tab[i,k]=kmeans(iris,k)$tot.withinss
	}
	
}
min_iner=apply(iner_tab,2,min)

png(filename="coude_iris_k10.png")
plot(1:10,min_iner,type="l",main="Inertie intra-classe en fonction du nombre de classes",xlab="k",ylab="Inertie minimale")#affichage de l'inertie min en fct de k
dev.off()

iner-exp<-min_iner

#QUESTION 4

data(iris)
k=kmeans(iris[,1:4],3)
png(filename="comparaison_iris.png",width=800)
par(mfrow=c(1,2))
clusplot(iris[,1:4],k$cluster,label=2,color=TRUE,main="Classification avec 3 classes")
clusplot(iris[,1:4],iris$Species,label=2,color=TRUE,main="Classification par espèce")
dev.off()

#CRABS

#QUESTION 1
#on effectue plusieurs classifications avec k=2, on obtient 2 resultats différents
crabs2<-read.csv("crabs2.csv",header=T)
crabsquant<-crabs2[,1:4]
k1=kmeans(crabsquant,2)
k2=kmeans(crabsquant,2)
k3=kmeans(crabsquant,2)
k4=kmeans(crabsquant,2)

png(filename="classifiaction_crabs.png",width=800)
par(mfrow=c(1,2))
clusplot(crabsquant,k1$cluster,diss=F,color=TRUE,main="Classification avec 2 classes")
clusplot(crabsquant,k2$cluster,diss=F,color=TRUE,main="Classification avec 2 classes")
dev.off()

#QUESTION 2

k=kmeans(crabsquant,4)
crabs_type<-apply(crabs2[5:6],1,paste,collapse=" ") #concatene le sexe et l'espece

par(mfrow=c(1,2))
clusplot(crabsquant,k$cluster,diss=F,color=TRUE,main="Classification avec 4 classes")
clusplot(crabsquant,crabs_type,label=2,color=TRUE,main="Classification par espèce et sexe")
#attention ! autre classification possible, voir si ça arrive souvent
#on peut specifier les points de departs dans kmeans avec k= ...




#MUTATIONS

#QUESTION 1
library(MASS)
mut<-read.csv("mutations2.csv",header=T,row.names=1)
mut<-as.dist(mut,diag=T,upper=T)
mut2<-cmdscale(mut,k=5)
k=kmeans(mut2,3)
clusplot(mut2,k$cluster,diss=F,color=TRUE)


#QUESTION 2


stab_mut<-matrix(0,100,4)
colnames(stab_mut)=c("k=2","k=3","k=4","k=5")
	for (k in 2:5){
	for (i in 1:100){
		stab_mut[i,k-1]=kmeans(mut2,k)$tot.withinss
	}
}
boxplot(stab_mut[,1:4])


min_iner_mut=apply(stab_mut,2,min)

table(stab_mut)
#on obtient 6 classifications diff




k1=kmeans(mut,2)
k2=kmeans(mut,3)
k3=kmeans(mut,4)



clusplot(mut,k1$cluster,diss=T,color=TRUE)
clusplot(mut,k3$cluster,diss=T,color=TRUE)

kmeans(mut,3)$withinss #inertie intra classe
#critere de selection, en choisira des clusters avec une inertie intra classse faible



