#Figure 5: Effect of market dimension


r1<-c(-0.21,0.09,0.28,0.48) 
r2<-c(0.06,0.18,-0.14,0.1)  #Data of returns

u<-t(c(1,1))
mu<-t(c(mean(r1),mean(r2))) #vector of mean returns (no distribution assumed)
cov12=3/4*cov(r1,r2)        #population covariance; from now own estimates are popultion estimates not sample (why?)
sigma<-c(sqrt(3/4*var(r1)),sqrt(3/4*var(r2)))
corr=cov12/sigma[1]*sigma[2]
C<-matrix(c(sigma[1]^2,cov12,
            cov12,sigma[2]^2)
          ,ncol=2,nrow=2)           #variance-covariance matrix   
invC=solve(C)

weights<-function(x){
  a<-matrix(c(1,u%*%invC%*%t(mu), 
              x,mu%*%invC%*%t(mu)),nrow=2,ncol=2)
  b<-matrix(c(u%*%invC%*%t(u),1, 
              mu%*%invC%*%t(u),x),nrow=2,ncol=2)
  c<-matrix(c(u%*%invC%*%t(u),u%*%invC%*%t(mu), 
              mu%*%invC%*%t(u),mu%*%invC%*%t(mu)),
            nrow=2,ncol=2)
  w=(det(a)*u%*%invC+det(b)*mu%*%invC)/det(c)
  return(w)
}           

t<-seq(0.0,0.5,0.001)
W=matrix(,nrow = length(t), ncol = 2)
for(i in 0:length(t)){
  W[i,]<-t(as.vector(weights(t[i])))
}                         #first we create a length(t)x2-matrix storing the weigths associated with
#different returns

w21<-t(as.vector(W[21,]))
t(w21)
sqrt(w21%*%C%*%t(w21))    #for example, this is the risk associated with weights in row 21

d=matrix(,nrow=501,ncol=2)
for(i in 1:501){
  d[i,]<-(t(as.vector(W[i,])))
}                         #next, we create a matrix d which treats every row as an individual vector of weights

V=matrix(,nrow=501,ncol=1)
for(i in 1:501){
  V[i,]<-sqrt(t(as.vector(d[i,]))%*%C%*%t(t(as.vector(d[i,]))))
}                         #lastly, we create yet another matrix V storing the risk associated with each weight

#Efficient Frontier

r<-as.vector(V)

par(mfrow=c(1,1))

plot(t[0:75]~r[0:75],xlab=expression(sigma),ylab=expression(mu), xlim=c(0,0.4),ylim=c(0.02,0.2),
     type="l",lty="dashed",col="blue")
par(new=TRUE)
plot(t[75:501]~r[75:501],xlab=expression(sigma),ylab=expression(mu), xlim=c(0,0.4),ylim=c(0.02,0.2),
     type="l",lwd="1",col="blue")


#3 assets

r1<-c(-0.21,0.09,0.28,0.48) 
r2<-c(0.06,0.18,-0.14,0.1)  #Data of linear returns
r3<-c(-0.15,0.22,0.07,0.29)
#r4<-c(-0.15,0.22,0.07,0.29)

u<-t(c(1,1,1))
mu<-t(c(mean(r1),mean(r2),mean(r3))) #vector of mean returns (no distribution assumed)
cov12=3/4*cov(r1,r2) 
cov13=3/4*cov(r1,r3)
cov23=3/4*cov(r2,r3)      #population covariance; from now own estimates are popultion estimates not sample (why?)
sigma<-c(sqrt(3/4*var(r1)),sqrt(3/4*var(r2)),sqrt(3/4*var(r3)))
corr12=cov12/sigma[1]*sigma[2]
corr13=cov13/sigma[1]*sigma[3]
corr23=cov23/sigma[2]*sigma[3]
C<-matrix(c(sigma[1]^2,cov12,cov13,
            cov12,sigma[2]^2,cov23,
            cov13,cov23,sigma[3]^2)
          ,ncol=3,nrow=3)           #variance-covariance matrix   
invC=solve(C)

weights<-function(x){
  a<-matrix(c(1,u%*%invC%*%t(mu), 
              x,mu%*%invC%*%t(mu)),nrow=2,ncol=2)
  b<-matrix(c(u%*%invC%*%t(u),1, 
              mu%*%invC%*%t(u),x),nrow=2,ncol=2)
  c<-matrix(c(u%*%invC%*%t(u),u%*%invC%*%t(mu), 
              mu%*%invC%*%t(u),mu%*%invC%*%t(mu)),
            nrow=2,ncol=2)
  w=(det(a)*u%*%invC+det(b)*mu%*%invC)/det(c)
  return(w)
}

t<-seq(0.0,0.5,0.001)
W=matrix(,nrow = length(t), ncol = 3)
for(i in 0:length(t)){
  W[i,]<-t(as.vector(weights(t[i])))
}                         #first we create a length(t)x2-matrix storing the weigths associated with
#different returns

w21<-t(as.vector(W[21,]))
t(w21)
sqrt(w21%*%C%*%t(w21))    #for example, this is the risk associated with weights in row 21

d=matrix(,nrow=501,ncol=3)
for(i in 1:501){
  d[i,]<-(t(as.vector(W[i,])))
}                         #next, we create a matrix d which treats every row as an individual vector of weights

V=matrix(,nrow=501,ncol=1)
for(i in 1:501){
  V[i,]<-sqrt(t(as.vector(d[i,]))%*%C%*%t(t(as.vector(d[i,]))))
}                         #lastly, we create yet another matrix V storing the risk associated with each weight

#Efficient Frontier

r<-as.vector(V)

par(new=TRUE)

plot(t[0:75]~r[0:75],xlab=expression(sigma),ylab=expression(mu), xlim=c(0,0.4),ylim=c(0.02,0.2),
     type="l",lty="dashed",lwd="1",col="red")
par(new=TRUE)
plot(t[75:501]~r[75:501],xlab=expression(sigma),ylab=expression(mu), xlim=c(0,0.4),ylim=c(0.02,0.2),
     type="l",lwd="1",col="red")


legend(0,0.183,"N=2",lty="solid",col="blue",lwd=1)
legend(0,0.2,"N=3",lty="solid",col="red",lwd=1)
mtext("Figure 5: Effect of market dimension",pos = 3,cex=1)