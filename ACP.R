df<-as.matrix(data.frame(x1=c(0,0,1,1,2,2,1,0,2,1),x2=c(0,0,1,1,2,2,0,1,1,2)))
g<-colMeans(df)
Y<-sweep(x=df,2,g,FUN ='-')
round(x,3)
n=10

D<-(1/n)*diag(rep(1,n))
V<-t(Y)%*% D %*% Y
vp<-eigen(V)
U<-vp$vectors 
ps<- Y%*% U
lambda <- vp$values
etta <- sweep(x=U,2,sqrt(lambda),FUN='*')
library(FactoMineR)
resultat<-PCA(df,scale.unit='False')


