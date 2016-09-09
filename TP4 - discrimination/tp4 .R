###########adq.app############

adq.app<-function(Xapp,zapp){

param<-NULL 		#Variable qui contiendra les résultats
tablek<-table(zapp) #nb d'individus dans chaque classe
k = length(tablek) 	#nb de classes
p<-dim(Xapp)[2]		#nb param
pik<-vector(length=k)#probabilitÃ© Ã  priori
mu<-matrix(nrow=k,ncol=p) #EspÃ©rance
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
mu<-matrix(nrow=k,ncol=p)#Esperance
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

###########DonnÃ©es############
data<-read.csv("Synth1-1000.txt",sep = " ",header=F)
X<-data[,1:2]
z<-data[,3]
res<-separ1(X,z)
X <- res$Xapp
z <- res$zapp
Xtst <- res$Xtst


########nba.app###

nba.app <- function(Xapp,zapp){

param<-NULL			#Variable qui contiendra les rÃ©sultats
tablek<-table(zapp)	#nb d'individus dans chaque classe
k = length(tablek)	#nb de classes
p<-dim(Xapp)[2]		#nb d'individus
pik<-vector(length=k)#nb params
mu<-matrix(nrow=k,ncol=p)#Esperance
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

##########ad.val########
library(MASS)

ad.val <- function(param, Xtst){
adval <- NULL
k = length(param$prop)
t <- array(dim=c(nrow(Xtst),k))
sum <- 0
for (i in 1:k){
	dens<- mvdnorm(Xtst, param$moy[,i], param$sig[,,i])
	t[,i] <- param$prop[i] * dens
	sum <- sum + t[,i]
}
for (i in 1:k){
	t[,i] <- t[,i] /sum
}

adval$prob <- t
adval$class <- (t[,1] > 0.5)*1
adval
}



(ad <- ad.val(adq, Xtst))
prob.ad(adq, Xtst, ztst, 0.5)


(adq<-adq.app(X,z))
(adl<-adl.app(X,z))
(nba<-nba.app(X,z))

######post.pr######## prob a posteriori

post.pr <- function(x,beta){

n <- dim(x)[1]
matbeta <- matrix(rep(beta,n),nrow=n,byrow=T)
prod <- rowSums(matbeta*x)
res <- exp(prod)/(1+exp(prod))

}

prob<-post.pr

########log.app########## regression logistique

log.app <- function(Xapp,zapp,intr,epsi){

Xapp <- as.matrix(Xapp)
n=dim(Xapp)[1]
p=dim(Xapp)[2]

if(intr==1){
	one<-cbind(rep(1, n))
	Xapp<-cbind(one,Xapp)
	beta<- rep(0,p+1)
}else{
	beta<- rep(0,p)
}
t <-(zapp==1)

niter <- 0
ndiff=epsi+1

while (ndiff>epsi){  

	pq <- post.pr(Xapp,beta)  #prob a posteriori
	gradbeta <- t(Xapp)%*%(t-pq)  #gradient de la log-vraisemblance
	matW <- diag(pq*(1-pq))      
	matH <- -t(Xapp)%*%matW%*%Xapp  #matrice hessienne
	logL <- sum(t*log(pq)+(1-t)*log(1-pq))  #log-vraisemblance

	betaold <- beta
	beta <- beta-ginv(matH)%*%gradbeta

	ndiff <- sum((betaold-beta)^2)
	
	niter <- niter + 1}

log <- NULL
log$beta <- beta
log$niter <- niter
log$logL <- logL
log
}



intr=1
epsi=1e-5
logapp<-(log.app(X,z,intr,epsi))

##########log.val#########

log.val<- function(beta,Xtst){

pbeta<-dim(beta)[1]
pXtst<-dim(Xtst)[2]
n=dim(Xtst)[1]

if (pbeta!=pXtst){
	one<-cbind(rep(1, n))
	Xtst<-cbind(one,Xtst)
	}
logval <- NULL
logval$prob <- array(dim=c(n,2))
logval$prob[,1]<-post.pr(Xtst,beta)
logval$prob[,2]<-1-(post.pr(Xtst,beta))
logval$class <- (logval$prob[,1]>0.5)*1
logval
}

########tree########

data<-read.csv("donnees-tp4/Synth1-1000.txt",sep = " ",header=T)
data<-read.csv("donnees-tp4/Synth2-1000.txt",sep = " ",header=T)
data<-read.csv("donnees-tp4/Synth3-1000.txt",sep = " ",header=T)
X<-data[,1:2]
z<-data[,3]

res<-separ1(X,z)
Xapp <- res$Xapp
zapp <- res$zapp
zapp <-as.factor(zapp)
data.app<-cbind(Xapp,zapp)
Xtst <- res$Xtst
ztst <- res$ztst

library(tree)

#######fonction arbre (bah ouai tree cetait deja pris :(#####

arbre<-function(Xapp,zapp,Xtst,K){
#K dépend du jeu de données et pas de la separation, 
#donc il faut retrouver K pour chaque nouveau jeu grâce à la fonction cv.tree

zapp <-as.factor(zapp)
data.app<-cbind(Xapp,zapp)
#apprentissage
treeapp<-tree(zapp~.,data=data.app,control=tree.control(nobs=dim(Xapp)[1],mindev = 0.0001)) #contruction arbre d'apprentissage

#classification
tree.test<-prune.misclass(treeapp,best=K) 

pre<-predict(treeapp,Xtst)
test=as.vector((pre[,1]>0.5)*1)

}

######
treeapp<-tree(zapp~.,data=data.app,control=tree.control(nobs=dim(Xapp)[1],mindev = 0.0001))
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP4/Graphes/Synth3-1000/arbre.png")
plot(treeapp, uniform=TRUE,main="Classification Tree for Synth1-1000")
dev.off()

cv<-cv.tree(treeapp) # on regarde dev on voit que le mieux est de couper au noeud 5
tree2<-prune.misclass(treeapp,best=6) # on elague a partir de K=5
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP4/Graphes/Synth3-1000/arbreelag.png")
plot(tree2,main="Classification Tree for Synth1-1000")
text(tree2, all=TRUE, cex=.8)
dev.off()
pre<-predict(tree2,Xtst)
test=as.vector((pre[,1]>0.5)*1)
error(ztst,test)


######test######
library(MASS)
source("fonctions-tp4/separ1.r")
source("fonctions-tp4/mvdnorm.r")

error<-function(ztst,zpost){
	ztst=(ztst==1)*1
	compar<-ztst==zpost
	length(which(!compar))/length(compar)
}

test.adq<-function(X,z){
error=0
for (i in 1:20){
	res<-separ1(X,z)
	Xapp <- res$Xapp
	zapp <- res$zapp
	Xtst <- res$Xtst
	ztst <- res$ztst	

	param<-adq.app(Xapp,zapp)
	zpost<-ad.val(param,Xtst)$class
	error=error+error(ztst,zpost)
	
	}

error=error/20
}

test.adl<-function(X,z){
error=0
for (i in 1:20){
	res<-separ1(X,z)
	Xapp <- res$Xapp
	zapp <- res$zapp
	Xtst <- res$Xtst
	ztst <- res$ztst	

	param<-adl.app(Xapp,zapp)
	zpost<-ad.val(param,Xtst)$class
	error=error+error(ztst,zpost)
	
	}

error=error/20
}

test.nba<-function(X,z){
error=0
for (i in 1:20){
	res<-separ1(X,z)
	Xapp <- res$Xapp
	zapp <- res$zapp
	Xtst <- res$Xtst
	ztst <- res$ztst	

	param<-nba.app(Xapp,zapp)
	zpost<-ad.val(param,Xtst)$class
	error=error+error(ztst,zpost)
	
	}

error=error/20
}

test.log<-function(X,z){
error=0
intr = 1
epsi = 1e-5
for (i in 1:20){
	res<-separ1(X,z)
	Xapp <- res$Xapp
	zapp <- res$zapp
	Xtst <- res$Xtst
	ztst <- res$ztst	

	beta<-log.app(Xapp,zapp,intr,epsi)$beta
	zpost<-as.vector(log.val(beta, Xtst)$class)
	error=error+error(ztst,zpost)
	
	}

error=error/20
}

test.logq<-function(X,z){
error=0
intr = 1
epsi = 1e-5
for (i in 1:20){
	res<-separ1(X,z)
	Xapp <- res$Xapp
	zapp <- res$zapp
	Xtst <- res$Xtst
	ztst <- res$ztst	
	quadra <- getquadra(Xapp, Xtst)
	Xtst <- quadra$Xtst
	Xapp <- quadra$Xapp

	beta<-log.app(Xapp,zapp,intr,epsi)$beta
	zpost<-as.vector(log.val(beta, Xtst)$class)
	error=error+error(ztst,zpost)
	
	}

error=error/20
}

data<-read.csv("donnees-tp4/Synth1-1000.txt",sep = " ",header=T)
data<-read.csv("donnees-tp4/Synth2-1000.txt",sep = " ",header=T)
data<-read.csv("donnees-tp4/Synth3-1000.txt",sep = " ",header=T)
X<-data[,1:2]
z<-data[,3]

test.tree<-function(X,z){
error=0
for (i in 1:20){
	res<-separ1(X,z)
	Xapp <- res$Xapp
	zapp <- res$zapp
	Xtst <- res$Xtst
	ztst <- res$ztst	
	
	zpredict <- arbre(Xapp,zapp, Xtst, 6)

	error=error+error(ztst,zpredict)
	}
error=error/20
}




getquadra <- function(Xapp, Xtst){
Xapp2 <- Xapp
Xtst2 <- Xtst
for (p in 1:(dim(Xapp)[2]-1))
{
for (q in (p+1):dim(Xapp)[2])
{
Xapp2 <- cbind(Xapp2, Xapp[,p]*Xapp[,q])
Xtst2 <- cbind(Xtst2, Xtst[,p]*Xtst[,q])
}
}
for (p in 1:dim(Xapp)[2])
{
Xapp2 <- cbind(Xapp2, Xapp[,p]^2)
Xtst2 <- cbind(Xtst2, Xtst[,p]^2)
}
res <- NULL
res$Xapp <- Xapp2
res$Xtst <- Xtst2
res
}

(test.adl(X,z))
(test.adq(X,z))
(test.nba(X,z))
(test.log(X,z))
(test.logq(X,z))
(test.tree(X,z))


