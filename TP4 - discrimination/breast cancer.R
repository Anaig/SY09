#####Breast cancer Wisconsin#####

Donn <- read.csv("donnees-tp4/bcw.csv", header=T)
X <- Donn[,1:9]
z <- Donn[,10]


####Frontières de décision ####
res<-separ1(X,z)
Xapp <- res$Xapp
zapp <- res$zapp
Xtst <- res$Xtst
ztst <- res$ztst

adq<-adq.app(Xapp,zapp)
adl<-adl.app(Xapp,zapp)
nba<-nba.app(Xapp,zapp)

logapp<-(log.app(Xapp,zapp,intr,epsi))

######Arbre######
res<-separ1(X,z)
Xapp <- res$Xapp
zapp <- res$zapp
zapp <-as.factor(zapp)
data.app<-cbind(Xapp,zapp)
Xtst <- res$Xtst
ztst <- res$ztst

treeapp<-tree(zapp~.,data=data.app,control=tree.control(nobs=dim(Xapp)[1],mindev = 0.0001))
plot(treeapp, uniform=TRUE,main="Classification Tree for Synth1-1000")
cv<-cv.tree(treeapp)

###########adq.app############

adq.app<-function(Xapp,zapp){

param<-NULL 		#Variable qui contiendra les résultats
tablek<-table(zapp) #nb d'individus dans chaque classe
k = length(tablek) 	#nb de classes
p<-dim(Xapp)[2]		#nb param
pik<-vector(length=k)#probabilité a priori
mu<-matrix(nrow=p,ncol=k) #Espérance
vk<-array(dim=c(p,p,k))	#Covariance

for (i in 1:k){
	pik[i]<-tablek[i]/length(zapp)
	mu[,i]<-apply(Xapp[which(zapp==i),],2,mean)
	vk[,,i]<-cov(Xapp[which(zapp==i),])  

}
param$prop<-pik
param$moy<-mu
param$sig<-vk
(param)
}


#######adl.app########

adl.app<-function(Xapp,zapp){

param<-NULL			#Variable qui contiendra les résultats
tablek<-table(zapp)	#nb d'individus dans chaque classe
k = length(tablek)	#nb de classes
n = length (zapp)	#nb d'individus
p<-dim(Xapp)[2]		#nb params
pik<-vector(length=k)#prob a priori	
mu<-matrix(nrow=p,ncol=k)#Esperance
vk<-array(dim=c(p,p,k))#Covariance
x <-0

for (i in 1:k){
	pik[i]<-tablek[i]/length(zapp)
	mu[,i]<-apply(Xapp[which(zapp==i),],2,mean)
	x <- x + (tablek[i]-1)*cov(Xapp[which(zapp==i),]) #calcul par recursivite de sum(nk-1)	
}
for (i in 1:k)
	vk[,,i]<- x/(n-k)
param$prop<-pik
param$moy<-mu
param$sig<-vk
(param)
}

test.tree<-function(X,z){
error=0
for (i in 1:100){
	res<-separ1(X,z)
	Xapp <- res$Xapp
	zapp <- res$zapp
	Xtst <- res$Xtst
	ztst <- res$ztst	
	
	zpredict <- arbre(Xapp,zapp, Xtst, 6)

	error=error+error(ztst,zpredict)
	}
error=error/100
}


test.tree2<-function(X,z){
error=0
for (i in 1:100){
	res<-separ1(X,z)
	Xapp <- res$Xapp
	zapp <- res$zapp
	Xtst <- res$Xtst
	ztst <- res$ztst	
	
	zpredict <- arbre2(Xapp,zapp, Xtst)

	error=error+error(ztst,zpredict)
	}
error=error/100
}

arbre2<-function(Xapp,zapp,Xts){
#K dépend du jeu de données et pas de la separation, 
#donc il faut retrouver K pour chaque nouveau jeu grâce à la fonction cv.tree

zapp <-as.factor(zapp)
data.app<-cbind(Xapp,zapp)
#apprentissage
treeapp<-tree(zapp~.,data=data.app,control=tree.control(nobs=dim(Xapp)[1],mindev = 0.0001)) #contruction arbre d'apprentissage

pre<-predict(treeapp,Xtst)
test=as.vector((pre[,1]>0.5)*1)

}

test.adq<-function(X,z){
error=0
for (i in 1:100){
	res<-separ1(X,z)
	Xapp <- res$Xapp
	zapp <- res$zapp
	Xtst <- res$Xtst
	ztst <- res$ztst	

	param<-adq.app(Xapp,zapp)
	zpost<-ad.val(param,Xtst)$class
	error=error+error(ztst,zpost)
	
	}

error=error/100
}

test.adl<-function(X,z){
error=0
for (i in 1:100){
	res<-separ1(X,z)
	Xapp <- res$Xapp
	zapp <- res$zapp
	Xtst <- res$Xtst
	ztst <- res$ztst	

	param<-adl.app(Xapp,zapp)
	zpost<-ad.val(param,Xtst)$class
	error=error+error(ztst,zpost)
	
	}

error=error/100
}

test.nba<-function(X,z){
error=0
for (i in 1:100){
	res<-separ1(X,z)
	Xapp <- res$Xapp
	zapp <- res$zapp
	Xtst <- res$Xtst
	ztst <- res$ztst	

	param<-nba.app(Xapp,zapp)
	zpost<-ad.val(param,Xtst)$class
	error=error+error(ztst,zpost)
	
	}

error=error/100
}

test.log<-function(X,z){
error=0
intr = 0
epsi = 1e-5
for (i in 1:100){
	res<-separ1(X,z)
	Xapp <- res$Xapp
	zapp <- res$zapp
	Xtst <- res$Xtst
	ztst <- res$ztst	

	beta<-log.app(Xapp,zapp,intr,epsi)$beta
	zpost<-as.vector(log.val(beta, Xtst)$class)
	error=error+error(ztst,zpost)
	
	}

error=error/100
}

########nba.app###

nba.app <- function(Xapp,zapp){

param<-NULL			#Variable qui contiendra les rÃ©sultats
tablek<-table(zapp)	#nb d'individus dans chaque classe
k = length(tablek)	#nb de classes
p<-dim(Xapp)[2]		#nb d'individus
pik<-vector(length=k)#nb params
mu<-matrix(nrow=p,ncol=k)#Esperance
vk<-array(dim=c(p,p,k))#Covariance

for (i in 1:k){
	pik[i]<-tablek[i]/length(zapp)
	mu[,i]<-apply(Xapp[which(zapp==i),],2,mean)
	vk[,,i]<-cov(Xapp[which(zapp==i),])
	vk[,,i]<-diag(diag(vk[,,i]))#On prend la diag de Vk et on en fait une matrice

}
param$prop<-pik
param$moy<-mu
param$sig<-vk
(param)
}



####TEsts####

(test.adq(X,z))
(test.adl(X,z))
(test.nba(X,z))
(test.log(X,z))
(test.tree(X,z))