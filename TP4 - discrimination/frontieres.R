####Frontières de décisions####

#####Synth1-1000#####

data<-read.csv("donnees-tp4/Synth1-1000.txt",sep = " ",header=T)
X<-data[,1:2]
z<-data[,3]
res<-separ1(X,z)
Xapp <- res$Xapp
zapp <- res$zapp
Xtst <- res$Xtst
ztst <- res$ztst

adq<-adq.app(Xapp,zapp)
adl<-adl.app(Xapp,zapp)
nba<-nba.app(Xapp,zapp)

png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP4/Graphes/Synth1-1000/adq.png")
prob.ad(adq, Xtst, ztst, 0.5)
dev.off()
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP4/Graphes/Synth1-1000/adl.png")
prob.ad(adl, Xtst, ztst, 0.5)
dev.off()
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP4/Graphes/Synth1-1000/nba.png")
prob.ad(nba, Xtst, ztst, 0.5)
dev.off()

logapp<-(log.app(Xapp,zapp,intr,epsi))
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP4/Graphes/Synth1-1000/log.png")
prob.log(logapp$beta, Xtst, ztst, 0.5)
dev.off()

X2 <- getquadra(Xapp,Xtst)
logqapp<-(log.app(X2$Xapp,zapp,intr,epsi))
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP4/Graphes/Synth1-1000/logq.png")
prob.log2(logqapp$beta, Xtst, ztst, 0.5)
dev.off()

#prob.log2(logqapp$beta, X2#Xtst, ztst, 0.5)
cov(X[1:501,])
cov(X[502:1000,])

logapp<-(log.app(Xapp,zapp,1,epsi))
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP4/Graphes/Synth1-1000/inter/avec.png")
prob.log(logapp$beta, Xtst, ztst, 0.5)
abline(h=0,v=0)
dev.off()

logapp2<-(log.app(Xapp,zapp,0,epsi))
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP4/Graphes/Synth1-1000/inter/sans.png")
prob.log(logapp2$beta, Xtst, ztst, 0.5)
abline(h=0,v=0)
dev.off()

#####Synth2-1000#####

data<-read.csv("donnees-tp4/Synth2-1000.txt",sep = " ",header=T)
X<-data[,1:2]
z<-data[,3]
res<-separ1(X,z)
Xapp <- res$Xapp
zapp <- res$zapp
Xtst <- res$Xtst
ztst <- res$ztst

adq<-adq.app(Xapp,zapp)
adl<-adl.app(Xapp,zapp)
nba<-nba.app(Xapp,zapp)

png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP4/Graphes/Synth2-1000/adq.png")
prob.ad(adq, Xtst, ztst, 0.5)
dev.off()
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP4/Graphes/Synth2-1000/adl.png")
prob.ad(adl, Xtst, ztst, 0.5)
dev.off()
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP4/Graphes/Synth2-1000/nba.png")
prob.ad(nba, Xtst, ztst, 0.5)
dev.off()

logapp<-(log.app(Xapp,zapp,intr,epsi))
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP4/Graphes/Synth2-1000/log.png")
prob.log(logapp$beta, Xtst, ztst, 0.5)
dev.off()

X2 <- getquadra(Xapp,Xtst)
logqapp<-(log.app(X2$Xapp,zapp,intr,epsi))
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP4/Graphes/Synth2-1000/logq.png")
prob.log2(logqapp$beta, Xtst, ztst, 0.5)
dev.off()

#prob.log2(logqapp$beta, X2#Xtst, ztst, 0.5)
(which(z==1))
cov(X[1:494,])
cov(X[495:1000,])

logapp<-(log.app(Xapp,zapp,intr,epsi))
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP4/Graphes/Synth2-1000/inter/avec.png")
prob.log(logapp$beta, Xtst, ztst, 0.5)
dev.off()

logapp2<-(log.app(Xapp,zapp,0,epsi))
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP4/Graphes/Synth2-1000/inter/sans.png")
prob.log(logapp2$beta, Xtst, ztst, 0.5)
dev.off()


#####Synth3-1000#####

data<-read.csv("donnees-tp4/Synth3-1000.txt",sep = " ",header=T)
X<-data[,1:2]
z<-data[,3]
res<-separ1(X,z)
Xapp <- res$Xapp
zapp <- res$zapp
Xtst <- res$Xtst
ztst <- res$ztst

adq<-adq.app(Xapp,zapp)
adl<-adl.app(Xapp,zapp)
nba<-nba.app(Xapp,zapp)

png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP4/Graphes/Synth3-1000/adq.png")
prob.ad(adq, Xtst, ztst, 0.5)
dev.off()
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP4/Graphes/Synth3-1000/adl.png")
prob.ad(adl, Xtst, ztst, 0.5)
dev.off()
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP4/Graphes/Synth3-1000/nba.png")
prob.ad(nba, Xtst, ztst, 0.5)
dev.off()

logapp<-(log.app(Xapp,zapp,intr,epsi))
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP4/Graphes/Synth3-1000/log.png")
prob.log(logapp$beta, Xtst, ztst, 0.5)
dev.off()

X2 <- getquadra(Xapp,Xtst)
logqapp<-(log.app(X2$Xapp,zapp,intr,epsi))
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP4/Graphes/Synth3-1000/logq.png")
prob.log2(logqapp$beta, Xtst, ztst, 0.5)
dev.off()

#prob.log2(logqapp$beta, X2#Xtst, ztst, 0.5)
(which(z==1))
cov(X[1:517,])
cov(X[518:1000,])

logapp<-(log.app(Xapp,zapp,1,epsi))
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP4/Graphes/Synth3-1000/inter/avec.png")
prob.log(logapp$beta, Xtst, ztst, 0.5)
abline(h=0,v=0)
dev.off()

logapp2<-(log.app(Xapp,zapp,0,epsi))
png(filename="~/Documents/Ana_perso/anaig/Compiègne/GI04/SY09/TP4/Graphes/Synth3-1000/inter/sans.png")
prob.log(logapp2$beta, Xtst, ztst, 0.5)
abline(h=0,v=0)
dev.off()
