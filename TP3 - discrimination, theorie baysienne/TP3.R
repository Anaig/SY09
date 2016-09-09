#CLASSIFICATEUR EUCLIDIEN

ceuc.app <- function(Xapp, zapp){#on calcule le centre de gravite des classes
	#Xapp : napp*p, tableau individu-variables des individus d'apprentissage
	#zapp : napp, étiquettes associées aux individus
	#mu : g*p, params estimés du classifieur euclidien
	mu <-matrix(,nrow=length(unique(zapp)),ncol(Xapp))
	for (i in 1:(length(unique(zapp)))){
		mu[i,]<-apply(Xapp[which(zapp==i),],2,mean)
	}
	mu
}





#distXY(mu, Xtst) ou distXY(t(mu), Xtst) ??

ceuc.val <- function(mu, Xtst) {
	etiquette<-matrix(,nrow=nrow(Xtst),1)
	distTab <- distXY(mu, Xtst)
	for (i in 1:(nrow(Xtst))){
		etiquette[i,] <- which.min(distTab[,i])
	}
	etiquette
}

mu=ceuc.app(Xapp,zapp)

zaff<-(ceuc.val(mu,Xtst))

#affichage de la frontiere de decision
png(filename="ceuc_synth_1_40")
front.ceuc(mu, Xtst, zaff)
dev.off()

#####PLUS PROCHES VOISINS########

#Détermine le nombre opptimal de voisins Kppv
kppv.tune <- function(Xapp, zapp, Xval, zval, nppv) {
	Xapp <- as.matrix(Xapp)
	Xval <- as.matrix(Xval)
	zapp <- as.vector(zapp)
	zval <- as.vector(zval)
	res = c(1:length(nppv)) #contiendra le taux d'erreur pour chaque valeur de ppv
	for(i in 1:length(nppv)) { #Pour chaque valeur de ppv
		ztst <- kppv.val(Xapp, zapp, i, Xval) #On cherche le vecteur des étiquettes
		res[i] <- mean(abs(zval-ztst)) #On soustrait les deux vecteurs et on fait la moyenne de la val abs
	}
	res
}


nppv <- c(1:10)

(kppv <- kppv.tune(Xapp ,zapp, Xval, zval, nppv))

kppv <- order(kppv)


kppv.val<-function(Xapp,zapp,K,Xtst){
	Xapp<-as.matrix(Xapp)
	Xtst<-as.matrix(Xtst)
	zapp <- as.vector(zapp)
	napp <- nrow(Xapp) #nb indiv app
	ntst <- nrow(Xtst) #nb indiv test
	
	distance<-distXY(Xtst,Xapp) #distance indiv app-test
	distance2<-t(apply(distance,1,sort)) #distance triée
	dk <- distance2[,K] #kième distance 
	
	distk <- NULL #matrice kième distance
	for(i in 1:napp) {
		distk <- cbind(distk, dk)
	}
	comp <- distance<=distk #matrice booléenne des k indivs les + proches de chaque indiv test

	matzapp <- NULL #matrice des étiquettes des indiv d'apprentissage
	for(i in 1:ntst) {
		matzapp <- rbind(matzapp, zapp)
	}

	ztst <- comp*matzapp #Ne contient plus que les étiquettes des k plus près indiv d'apprentissage 
	z <- NULL #retient l'étiquette en plus grand nombre
	for(i in 1:ntst) {
		if(length(which(ztst[i,]==1))< length(which(ztst[i,]==2)))
			z[i] <- 2
		else
			z[i] <- 1
	}
	z
}
	

K <- kppv[1]
K=2
(zaff <- kppv.val(Xapp, zapp, K, Xtst))

front.kppv(Xapp, zapp, K, X, z)

#######ERREUR#######
error<-NULL
for (i in 1:10){ 
	donn.sep <- separ2(X, z)
	Xapp <- donn.sep$Xapp
	zapp <- donn.sep$zapp
	Xval <- donn.sep$Xval
	zval <- donn.sep$zval
	Xtst <- donn.sep$Xtst
	ztst <- donn.sep$ztst
	ztst <- kppv.val(Xapp, zapp, i, Xval)
	error[i]<-mean(abs(zval-ztst))
}

#######DONNEES TEST########

donn <- read.table("Synth1-40.txt", header=F)
X <- donn[,1:2]
z <- donn[,3]
Xapp <- X[c(1:15,24:35),]
zapp <- z[c(1:15,24:35)]
Xtst <- X[c(16:20,36:40),]
ztst <- z[c(16:20,36:40)]

#FRONTIERES DE DECISION



Kopt <- kppv.tune(Xapp, zapp, Xval, zval, 1:10)
zpred <- kppv.val(Xapp, zapp, Kopt, Xtst)
front.kppv(Xapp, zapp, Kopt, Xapp, zapp)

#SEPARATION DES DONNEES

#La fonction separ1 détermine un ensemble d’apprentissage (de taille napp = 2n=3) et un ensemble
#de test (de taille ntst = n=3).

donn.sep <- separ1(X, z)
Xapp <- donn.sep$Xapp
zapp <- donn.sep$zapp
Xtst <- donn.sep$Xtst
ztst <- donn.sep$ztst


#La fonction separ2 détermine des ensembles d’apprentissage (de taille
#napp = n=2), de validation (de taille nval = n=4) et de test (de taille ntst = n=4).

donn.sep <- separ2(X, z)
Xapp <- donn.sep$Xapp
zapp <- donn.sep$zapp
Xval <- donn.sep$Xval
zval <- donn.sep$zval
Xtst <- donn.sep$Xtst
ztst <- donn.sep$ztst