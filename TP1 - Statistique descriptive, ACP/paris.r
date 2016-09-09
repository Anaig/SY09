books <- read.csv("anonymous-betting-data.csv")
source("pretraitements.R")
names(books.sel) #Nom des colonnes
attach(books.sel) #Table par défaut

##################QUESTION 1###########################
summary(books.sel)
nlevels(books.sel$match_book_uid) #nombre de paris
nlevels(books.sel$match_uid) #nombre de matchs
length(players.total <- union(loser,winner)) #nombre de joueurs
length(unique(loser)) #nombre perdants
length(unique(winner)) #nombre gagnants
length(unique(book)) #nombre de bookmakers 

##################QUESTION 2###########################
matches <- books.sel[which(!duplicated(books.sel$match_uid)),
-c(1,2,3,4,5,7,8,9,10,11,12)]
matches <- matches[sort.int(as.character(matches$match_uid), index.return=T)$ix,]

win <- aggregate(matches$match_uid~matches$winner, data=matches, FUN=length)
los <-aggregate(matches$match_uid~matches$loser, data=matches, FUN=length)
#ne prend pas en compte les joureurs avec 0 vict ou 0 def, donc pb de dimensions

#Table player_uid/nb match gagnés/nb match perdus
players_data<-data.frame(
	player_uid=factor(levels(matches$winner),levels=levels(matches$winner)),
	player_win=table(matches$winner),
	player_los=table(matches$loser))#petit pb il reste l id des joueurs
	
#Table propension à gagner des matchs par joueur
players <- data.frame(

	players_data[,-c(2,4)],#on enleve les colonnes doublons
	player_level=players_data[3]/(players_data[5]+players_data[3])#colonne repsentant le niveau du joueur (G/(G+P))
)
colnames(players)[2]="victoires" #renomme la colonne
colnames(players)[3]="defaites"
colnames(players)[4]="ratio"
players<-players[order(-players$ratio),]
#affichage du niveau
hist(players$ratio,main="Niveau des joueurs",xlab="Victoires/matchs joués",ylab="Nombre de joueurs",col="darkblue")


##################QUESTION 3###########################

#Match suspects car évolution proba >0.1
books.sel$prob_evo=abs(books.sel$implied_prob_winner_open-books.sel$implied_prob_winner_close)

pari_suspect<-subset(books.sel,books.sel$prob_evo>=0.1 & books.sel$moved_towards_winner==TRUE)
#LE PARI EST SUSPECT SI LA PROB A EVOLUE EN FAVEUR DU GAGNANT
 
dim(pari_suspect) #nbre de pari suspect:2657

##ancienne version##
pari_suspect=(prob_winner_evo >= 0.1) #pari suspect si evo>0.1
table(pari_suspect)#nbre de paris suspects : 4298
##________________##

#Bookmakers suspects
table(pari_suspect$book) #Nombre de matchs suspects / bookmaker 
#les bookmakers A B C sont beaucoup plus concernés que les autres

#Matchs suspects

match_suspect<-unique(pari_suspect$match_uid)
length(match_suspect)#1752 match suspects (2e methode)

##ancienne version##
match_suspect<-books.sel$match_uid[pari_suspect$match_book_uid]
nb_pari<-data.frame(table(match_suspect))
Match_suspect<-subset(nb_pari,Freq>0)#table avec le uid du match suspect et le nombre de paris associés
dim(Match_suspect)#2798 matchs suspects (1ere methode)
##________________##

#le match est vraiment suspect seulement si la cote du gagnant a évolué en sa faveur

#Players suspects
player_suspect <- books.sel$loser[match_suspect] #il est plus facile de faire expres de perdre
length(unique(player_suspect)) #Nombre de players suspects 1e methode : 310 #2e methode : 273

nb_match_suspect <- data.frame(table(player_suspect))
Player_suspect<-subset(nb_match_suspect,Freq>10)#plus de 10 def suspectes
dim(Player_suspect) #1e methode : 145 joueurs suspects #2e methode : 64 !! on est BON

#affichage d'un barplot avec le nbre de def par joueur
display_player<-Player_suspect[order(-Player_suspect$Freq),]#on range par freq (decroissant)
barplot(display_player$Freq,main="Nombre de defaites suspectes par joueur implique",ylab="Matchs suspects",col="darkgreen")
