podtrans <- function(X) {
	t(X)%*%X;
}

 A <- matrix(1:6, nrow=3, byrow=T)
 X <- podtrans(A)
 X
 
 B <- matrix(c(5,3,7,4,6,3,1,6,3,2,8,5), nrow=4, byrow=T)
 Y <- podtrans(B)
 Y
 
 x<-c(2,4,3,7,1)
 A <- matrix(c(1,2,5,3,0,9), nrow=3, byrow=T)
 apply(A,1,max) 
 apply(A,2,max)
 apply(E,3,sum)
 
 centre <- function(X){
 	 rows <- nrow(X);
 	 means <- apply(X,2,mean);
 	 matrixmeans <- matrix(data=1,nrow=rows)%*%means
           X-matrixmeans
 }
 
 covariance <- function(X){
 	rows <- nrow(X);
 	centre <- centre(X);
 	(podtrans(centre))*(rows-1)^-1; 
 }
 
  B <- matrix(c(5,3,7,4,6,3,1,6,3,2,8,5, 1, 4, 5, 3), nrow=4, byrow=T)
  
hist.factor <- function(x, y){
	inter<-seq(min(x), max(x),by=(max(x) - min(x)) /10)
           h<-hist(plot=F, x[y == levels(y)[1]] ,breaks=inter)$count
           for (i in 2:nlevels(y))
      		h <- rbind(h, hist(plot=F, x[y==levels(y)[i]], breaks=inter)$count)
    	barplot(h, main="iris", space=0, legend=levels(y), col=c('green', 'pink', 'blue'))
}
hist.factor(iris$Sepal.Length,iris$Species)


hist.factor<-function(X,Y){
    inter<-seq(min(X),max(X),by=(max(X)-min(X))/10)
    h<-hist(plot=F,X[Y==levels(Y)[1]],breaks=inter)$count
    for (m in 2:nlevels(Y))
      h<-rbind(h,hist(plot=F,X[Y==levels(Y)[m]],breaks=inter)$count)
    barplot(h,space=0,legend=levels(Y),col=palette())
} 
