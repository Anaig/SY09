data<-read.csv("donnees-tp4/Synth1-1000.txt",sep = " ",header=T)
data<-read.csv("donnees-tp4/Synth2-1000.txt",sep = " ",header=T)
data<-read.csv("donnees-tp4/Synth3-1000.txt",sep = " ",header=T)
X<-data[,1:2]
z<-data[,3]

(test.log(X,z))
(test.log2(X,z))
(test.logq(X,z))
(test.logq2(X,z))

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
	
	
test.log2<-function(X,z){
error=0
intr = 0
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

test.logq2<-function(X,z){
error=0
intr = 0
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

test.tree<-function(X,z){
error=0
for (i in 1:20){
	res<-separ1(X,z)
	Xapp <- res$Xapp
	zapp <- res$zapp
	Xtst <- res$Xtst
	ztst <- res$ztst	
	
	zpredict <- arbre(X,z)

	error=error+error(ztst,zpredict)
	}
error=error/20}




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

