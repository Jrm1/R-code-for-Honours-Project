#Figure 2: MVBs and effect of correlation

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

#1) Minimum Variance Portfolio

n<-c(t((u%*%invC)))
d<-c((u%*%invC%*%t(u)))
w<-t(c(n/d))
w                          #MVP see theory for general derivation given correlation notes I page 31-32
#and pgs. 39-40 for matrix version for more than 2 securities

muv=w%*%t(mu)
muv                        #MVP expected return


sigmav=sqrt(w%*%C%*%t(w))
sigmav                     #MVP risk

#2) Minimum Variance Line

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
}                         #create a function that returns the weights in the MVL
#given a minimum return

w1<-weights(0.2)
w1                        #for example: this is the MVP weights for a returns of 20%
S=sqrt(w1%*%C%*%t(w1))
S                         #associated risk

#Risk for different returns

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


# Perfect positive correlation

r1<-c(-0.21,0.09,0.28,0.48)
r2<-c(0.06,0.18,-0.14,0.1)  #Data of returns

u<-t(c(1,1))
mu<-t(c(mean(r1),mean(r2))) 
sigma<-c(sqrt(3/4*var(r1)),sqrt(3/4*var(r2)))

m<-c()
s<-c()
weight1<-seq(0.0,1,0.001)
for(i in 0:length(weight1)){
  m[i]<-weight1[i]*mean(r1)+(1-weight1[i])*mean(r2)
  s[i]<-weight1[i]*sqrt(3/4*var(r1))+(1-weight1[i])*sqrt(3/4*var(r2))
}

m
s

plot(m~s,type="l")

#Perfect negative correlation

m1<-c()
s1<-c()
weight1<-seq(0.0,1,0.001)
for(i in 0:length(weight1)){
  m1[i]<-weight1[i]*mean(r1)+(1-weight1[i])*mean(r2)
  s1[i]<-weight1[i]*sqrt(3/4*var(r1))-(1-weight1[i])*sqrt(3/4*var(r2))
}

m1
s1

plot(m1~s1,type="l")
m2<-c()
s2<-c()
weight1<-seq(0.0,1,0.001)
for(i in 0:length(weight1)){
  m2[i]<-weight1[i]*mean(r1)+(1-weight1[i])*mean(r2)
  s2[i]<-(-weight1[i]*sqrt(3/4*var(r1))+(1-weight1[i])*sqrt(3/4*var(r2)))
}
m2
s2
abline(coef(line(m2~s2)))
par(mfrow=c(1,1))
plot(t[0:75]~r[0:75], ylab="",xlab="",xlim=c(0,0.4),ylim=c(0.02,0.2),yaxt="n",xaxt="n",type="l",col="black")
par(new=TRUE)
plot(t[75:501]~r[75:501], ylab="",xlab="",xlim=c(0,0.4),ylim=c(0.02,0.2),yaxt="n",xaxt="n",type="l",col="black")
points(sigmav,muv,pch=19)
abline(coef(line(m~s)),type="l")
abline(coef(line(m1~s1)),type="l",col="black")
abline(coef(line(m2~s2)),type="l")
points(sigma[1],mu[1],pch=19,col="black")
points(sigma[2],mu[2],pch=19,col="black")
MVP3=(sigma[2]^2+1*sigma[1]*sigma[2])/(sigma[1]^2+sigma[2]^2+2*sigma[1]*sigma[2])
ER3=MVP3*mu[1]+(1-MVP3)*mu[2]
points(0,ER3,pch=19)
segments(sigma[2],mu[2],1,mu[2],lty="dotted")
segments(sigma[1],mu[1],1,mu[1],lty="dotted")
text(-0.03,mu[1]-0.01, expression(mu[1]), srt = 0, xpd = TRUE, pos = 3,cex=1.5)
segments(-0.03,0.16,sigma[1],mu[1],lty="dotted")
segments(sigma[1],mu[1],sigma[1],0,lty="dotted")
text(-0.03,mu[2]-0.007, expression(mu[2]), srt = 0, xpd = TRUE, pos = 3,cex=1.5)
segments(-0.03,mu[2],sigma[2],mu[2],lty="dotted")
segments(sigma[2],mu[2],sigma[2],0,lty="dotted")
segments(sigmav,muv,-0.03,muv,lty="dotted",lwd=2)
segments(sigmav,muv,sigmav,0,lty="dotted",lwd=2)
text(sigma[2]+0.008,0, expression(sigma[2]), srt = 0, xpd = TRUE, pos = 3,cex=1.5)
text(sigma[1],0, expression(sigma[1]), srt = 0, xpd = TRUE, pos = 3,cex=1.5)
text(0.4,0, expression(sigma[p]), srt = 0, xpd = TRUE, pos = 3,cex=1.5)
text(sigmav-0.001,0, expression(sigma[GMV]), srt = 0, xpd = TRUE, pos = 3,cex=1.5)
text(-0.03,0.2, expression(mu[p]), srt = 0, xpd = TRUE, pos = 3,cex=1.5)
text(-0.035,muv-0.007, expression(mu[GMV]), srt = 0, xpd = TRUE, pos = 3,cex=1.5)
text(sigma[1]-0.05,ER3+0.02, expression(rho[12]==1), srt = 55.89, xpd = TRUE, pos = 3,cex=1)
text(sigma[2]-0.035,ER3-0.0075, expression(abs(rho[12])<1), srt = 0, xpd = TRUE, pos = 3,cex=1)
text(sigma[2]-0.03,ER3+0.025, expression(rho[12]==-1), srt = 30.46, xpd = TRUE, pos = 3,cex=1)
text(sigma[2]-0.06,ER3-0.027, expression(rho[12]==-1), srt = -30.46, xpd = TRUE, pos = 3,cex=1)
mtext("Figure 2: MVBs and effect of correlation",pos = 3,cex=1)
segments(0,0,0,mu[2]+0.0365,lty="dotted")
text(0,0, expression(0), srt = 0, xpd = TRUE, pos = 3,cex=1.5)

