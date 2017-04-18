#Simulation study


set.seed(1)

R1<-c(rnorm(1000,0.09,0.3))
R2<-c(rnorm(1000,0.15,0.25))
R3<-c(rnorm(1000,0.07,0.2))
R4<-c(rnorm(1000,0.18,0.35))

#independent

#CORR12<-c()
#for(i in 1:1000){
#  R1<-c(rnorm(1000,0.09,0.3))
#  R2<-c(rnorm(1000,0.15,0.25))
#  CORR12[i]<-cor(R1,R2)
#}

#hist(CORR12)

#CORR13<-c()
#for(i in 1:1000){
#  R1<-c(rnorm(1000,0.09,0.3))
#  R3<-c(rnorm(1000,0.07,0.2))
#  CORR13[i]<-cor(R1,R3)
#}

#hist(CORR13)


#plot.ts(R1,ylab="Returns",xlab="Time",col="black")
#par(new=TRUE)
#plot.ts(R2,col="green",ylab="",xlab="",xaxt="n",yaxt="n")
#par(new=TRUE)
#plot.ts(R3,col="red",ylab="",xlab="",xaxt="n",yaxt="n")
#par(new=TRUE)
#plot.ts(R4,col="blue",ylab="",xlab="",xaxt="n",yaxt="n")


C<-matrix(c(0.3^2,0,0,0,
            0,0.25^2,0,0,
            0,0,0.2^2,0,
            0,0,0,0.35^2),ncol=4,nrow=4)
invC=solve(C)

u<-t(c(1,1,1,1))

mu<-c(0.09,0.15,0.07,0.18)


rweight<-matrix(runif(1600,0,1),nrow=400,ncol=4)

rwe<-matrix(,nrow=400,ncol=4)
for(i in 1:400){
  rwe[i,]=rweight[i,]/sum(rweight[i,])
}

MU<-matrix(,nrow=400,ncol=1)
SIGMA<-matrix(,nrow=400,ncol=1)

for(i in 1:400){
  MU[i]=rwe[i,]%*%t(t(mu))
  SIGMA[i]=sqrt(rwe[i,]%*%C%*%t(t(rwe[i,])))
}



weights<-function(x){
  a<-matrix(c(1,u%*%invC%*%mu, 
              x,mu%*%invC%*%mu),nrow=2,ncol=2)
  b<-matrix(c(u%*%invC%*%t(u),1, 
              mu%*%invC%*%t(u),x),nrow=2,ncol=2)
  c<-matrix(c(u%*%invC%*%t(u),u%*%invC%*%mu, 
              mu%*%invC%*%t(u),mu%*%invC%*%mu),
            nrow=2,ncol=2)
  w=(det(a)*u%*%invC+det(b)*mu%*%invC)/det(c)
  return(w)
}  




#1) Minimum Variance Portfolio

n<-c(t((u%*%invC)))
d<-c((u%*%invC%*%t(u)))
w<-t(c(n/d))
w                        

muv=w%*%t(t(mu))
muv                   


sigmav=sqrt(w%*%C%*%t(w))
sigmav




#What happens if we estimate mu and C by sample counterparts?


Mat<-matrix(c(var(R1),cov(R1,R2),cov(R1,R3),cov(R1,R4),
              cov(R2,R1),var(R2),cov(R2,R3),cov(R2,R4),
              cov(R3,R1),cov(R3,R2),var(R3),cov(R3,R4),
              cov(R4,R1),cov(R4,R2),cov(R4,R3),var(R4)),ncol=4,nrow=4)
invMat=solve(Mat)

mue<-c(mean(R1),mean(R2),mean(R3),mean(R4))

weights2<-function(x){
  a<-matrix(c(1,u%*%invMat%*%mue, 
              x,mue%*%invMat%*%mue),nrow=2,ncol=2)
  b<-matrix(c(u%*%invMat%*%t(u),1, 
              mue%*%invMat%*%t(u),x),nrow=2,ncol=2)
  c<-matrix(c(u%*%invMat%*%t(u),u%*%invMat%*%mue, 
              mue%*%invMat%*%t(u),mue%*%invMat%*%mue),
            nrow=2,ncol=2)
  w=(det(a)*u%*%invMat+det(b)*mue%*%invMat)/det(c)
  return(w)
}


#MVP


ne<-c(t((u%*%invMat)))
de<-c((u%*%invMat%*%t(u)))
we<-t(c(ne/de))
we                        

muve=we%*%t(t(mue))
muve                   


sigmave=sqrt(we%*%Mat%*%t(we))
sigmave



#Nonparametric bootstrap Asset returns 1

#Based on Michaud (1989): b-th bootstrap has sample mean vector mu.b=c(mean(mu1.ests),mean(mu2.ests),mean(mu3.ests),mean(mu4.ests)

B=999

mu1.ests<-c()
var1.ests<-c()
cov12.ests<-c()
cov13.ests<-c()
cov14.ests<-c()

mu2.ests<-c()
var2.ests<-c()
cov23.ests<-c()
cov24.ests<-c()

mu3.ests<-c()
var3.ests<-c()
cov34.ests<-c()

mu4.ests<-c()
var4.ests<-c()

for(i in 1:B){
  b.r1<-sample(R1,length(R1),replace=TRUE)  #999 bootstrap samples
  b.r2<-sample(R2,length(R2),replace=TRUE)
  b.r3<-sample(R3,length(R3),replace=TRUE)
  b.r4<-sample(R4,length(R4),replace=TRUE)
  
  mu1.ests[i]<-(mean(b.r1)) #without original sample. Add 1000th entry to vector mu.ests equal to mean(R1); the original sample.
  var1.ests[i]<-(var(b.r1))
  cov12.ests[i]<-(cov(b.r1,b.r2))
  cov13.ests[i]<-cov(b.r1,b.r3)
  cov14.ests[i]<-cov(b.r1,b.r4)
  
  mu2.ests[i]<-mean(b.r2)
  var2.ests[i]<-var(b.r2)
  cov23.ests[i]<-cov(b.r2,b.r3)
  cov24.ests[i]<-cov(b.r2,b.r4)
  
  mu3.ests[i]<-mean(b.r3)
  var3.ests[i]<-var(b.r3)
  cov34.ests[i]<-cov(b.r3,b.r4)
  
  mu4.ests[i]<-mean(b.r4)
  var4.ests[i]<-var(b.r4)
}

#hist(mu1.ests)
#hist(mu2.ests)
#hist(mu3.ests)
#hist(mu4.ests)

#hist(var1.ests)
#hist(var2.ests)
#hist(var3.ests)
#hist(var4.ests)

#hist(cov12.ests)
#hist(cov13.ests)
#hist(cov14.ests)
#hist(cov23.ests)
#hist(cov24.ests)
#hist(cov34.ests)


#95% confidence intervals for expected return and variance of asset 1
#Percentile method CIs
alpha <- 0.025
low <- round((B+1)*alpha)
high <- round((B+1)*(1-alpha))

sorted.mu1.ests <- sort(mu1.ests) # sort the estimates of mu1
sorted.mu2.ests <- sort(mu2.ests)
sorted.mu3.ests <- sort(mu3.ests)
sorted.mu4.ests <- sort(mu4.ests)

sorted.var1.ests <- sort(var1.ests) # sort the estimates of sigma1ˆ2
sorted.var2.ests <- sort(var2.ests)
sorted.var3.ests <- sort(var3.ests)
sorted.var4.ests <- sort(var4.ests)

sorted.cov12.ests<-sort(cov12.ests)
sorted.cov13.ests<-sort(cov13.ests)
sorted.cov14.ests<-sort(cov14.ests)
sorted.cov23.ests<-sort(cov23.ests)
sorted.cov24.ests<-sort(cov24.ests)
sorted.cov34.ests<-sort(cov34.ests)

c(sorted.mu1.ests[low],sorted.mu1.ests[high])  # the estimated CI for mu
c(sorted.mu2.ests[low],sorted.mu2.ests[high])
c(sorted.mu3.ests[low],sorted.mu3.ests[high])
c(sorted.mu4.ests[low],sorted.mu4.ests[high])

c(sorted.var1.ests[low],sorted.var1.ests[high])
c(sorted.var2.ests[low],sorted.var2.ests[high])
c(sorted.var3.ests[low],sorted.var3.ests[high])
c(sorted.var4.ests[low],sorted.var4.ests[high])

c(sorted.cov12.ests[low],sorted.cov12.ests[high])
c(sorted.cov13.ests[low],sorted.cov13.ests[high])
c(sorted.cov14.ests[low],sorted.cov14.ests[high])
c(sorted.cov23.ests[low],sorted.cov23.ests[high])
c(sorted.cov24.ests[low],sorted.cov24.ests[high])
c(sorted.cov34.ests[low],sorted.cov34.ests[high])


#Solving the model with bootstrap estimates

#What happens if we estimate mu and C by nonparmetric bootstrap?


Mat.B<-matrix(c(mean(var1.ests),mean(cov12.ests),mean(cov13.ests),mean(cov14.ests),
                mean(cov12.ests),mean(var2.ests),mean(cov23.ests),mean(cov24.ests),
                mean(cov13.ests),mean(cov23.ests),mean(var3.ests),mean(cov34.ests),
                mean(cov14.ests),mean(cov24.ests),mean(cov34.ests),mean(var4.ests)),ncol=4,nrow=4)
              
invMat.B=solve(Mat.B)

mu.b<-c(mean(mu1.ests),mean(mu2.ests),mean(mu3.ests),mean(mu4.ests)) #sample mean vector

weights.b<-function(x){
  a<-matrix(c(1,u%*%invMat.B%*%mu.b, 
              x,mu.b%*%invMat.B%*%mu.b),nrow=2,ncol=2)
  b<-matrix(c(u%*%invMat.B%*%t(u),1, 
              mu.b%*%invMat.B%*%t(u),x),nrow=2,ncol=2)
  c<-matrix(c(u%*%invMat.B%*%t(u),u%*%invMat.B%*%mu.b, 
              mu.b%*%invMat.B%*%t(u),mu.b%*%invMat.B%*%mu.b),
            nrow=2,ncol=2)
  w=(det(a)*u%*%invMat.B+det(b)*mu.b%*%invMat.B)/det(c)
  return(w)
}

#for(i in seq(0.1,0.17,0.0001)){
#  points(sqrt(weights2(i)%*%Mat%*%t(weights2(i))),i,pch=19,col="green")
#}


#MVP


ne.b<-c(t((u%*%invMat.B)))
de.b<-c((u%*%invMat.B%*%t(u)))
we.b<-t(c(ne.b/de.b))
we.b                        

muve.b=we.b%*%t(t(mu.b))
muve.b                   


sigmave.b=sqrt(we.b%*%Mat.B%*%t(we.b))
sigmave.b


#Fitting a confidence interval on bootstrap EF
#For this we need to plot EF with lower bounds of derived 95% CI
#And another EF with upper bounds of derived 95% CI

Mat.BL<-matrix(c(sorted.var1.ests[low],sorted.cov12.ests[low],sorted.cov13.ests[low],sorted.cov14.ests[low],
                 sorted.cov12.ests[low],sorted.var2.ests[low],sorted.cov23.ests[low],sorted.cov24.ests[low],
                 sorted.cov13.ests[low],sorted.cov23.ests[low],sorted.var3.ests[low],sorted.cov34.ests[low],
                 sorted.cov14.ests[low],sorted.cov24.ests[low],sorted.cov34.ests[low],sorted.var4.ests[low]),ncol=4,nrow=4)


invMat.BL=solve(Mat.BL)

mu.bl<-c(sorted.mu1.ests[low],sorted.mu2.ests[low],sorted.mu3.ests[low],sorted.mu4.ests[low])


weights.bl<-function(x){
  a<-matrix(c(1,u%*%invMat.BL%*%mu.bl, 
              x,mu.bl%*%invMat.BL%*%mu.bl),nrow=2,ncol=2)
  b<-matrix(c(u%*%invMat.BL%*%t(u),1, 
              mu.bl%*%invMat.BL%*%t(u),x),nrow=2,ncol=2)
  c<-matrix(c(u%*%invMat.BL%*%t(u),u%*%invMat.BL%*%mu.bl, 
              mu.bl%*%invMat.BL%*%t(u),mu.bl%*%invMat.BL%*%mu.bl),
            nrow=2,ncol=2)
  w=(det(a)*u%*%invMat.BL+det(b)*mu.bl%*%invMat.BL)/det(c)
  return(w)
}





Mat.BH<-matrix(c(sorted.var1.ests[high],sorted.cov12.ests[high],sorted.cov13.ests[high],sorted.cov14.ests[high],
                 sorted.cov12.ests[high],sorted.var2.ests[high],sorted.cov23.ests[high],sorted.cov24.ests[high],
                 sorted.cov13.ests[high],sorted.cov23.ests[high],sorted.var3.ests[high],sorted.cov34.ests[high],
                 sorted.cov14.ests[high],sorted.cov24.ests[high],sorted.cov34.ests[high],sorted.var4.ests[high]),ncol=4,nrow=4)


invMat.BH=solve(Mat.BH)

mu.bh<-c(sorted.mu1.ests[high],sorted.mu2.ests[high],sorted.mu3.ests[high],sorted.mu4.ests[high])


weights.bh<-function(x){
  a<-matrix(c(1,u%*%invMat.BH%*%mu.bh, 
              x,mu.bh%*%invMat.BH%*%mu.bh),nrow=2,ncol=2)
  b<-matrix(c(u%*%invMat.BH%*%t(u),1, 
              mu.bh%*%invMat.BH%*%t(u),x),nrow=2,ncol=2)
  c<-matrix(c(u%*%invMat.BH%*%t(u),u%*%invMat.BH%*%mu.bh, 
              mu.bh%*%invMat.BH%*%t(u),mu.bh%*%invMat.BH%*%mu.bh),
            nrow=2,ncol=2)
  w=(det(a)*u%*%invMat.BH+det(b)*mu.bh%*%invMat.BH)/det(c)
  return(w)
}

#Plots


plot(MU~SIGMA,pch=19,col="blue",xlab=expression(sigma),ylab=expression(mu),xlim=c(sigmav,0.15),ylim=c(0.10,0.14))

for(i in seq(0.11,0.17,0.0001)){
  points(sqrt(weights(i)%*%C%*%t(weights(i))),i,pch=19,col="yellow")
}

for(i in seq(0.1,0.11,0.001)){
  points(sqrt(weights(i)%*%C%*%t(weights(i))),i,pch=19,col="yellow")
}

for(i in seq(0.11,0.17,0.0001)){
  points(sqrt(weights2(i)%*%Mat%*%t(weights2(i))),i,pch=19,col="green")
}

for(i in seq(0.1,0.13,0.001)){
  points(sqrt(weights2(i)%*%Mat%*%t(weights2(i))),i,pch=19,col="green")
}

#for(i in seq(0.11,0.17,0.0001)){
#  points(sqrt(weights.b(i)%*%Mat.B%*%t(weights.b(i))),i,pch=19,col="red")
#}

#for(i in seq(0.1,0.13,0.001)){
#  points(sqrt(weights.b(i)%*%Mat.B%*%t(weights.b(i))),i,pch=19,col="red")
#}

#for(i in seq(0.1,0.17,0.0001)){
#  points(sqrt(weights.bl(i)%*%Mat.BL%*%t(weights.bl(i))),i,pch=".",col="red")
#}


#for(i in seq(0.1,0.17,0.0001)){
#  points(sqrt(weights.bh(i)%*%Mat.BH%*%t(weights.bh(i))),i,pch=".",col="red")
#}

#points(sigmave.b,muve.b,pch=19,col="black")

points(sigmav,muv,pch=19,col="black")

points(sigmave,muve,pch=19,col="black")

mtext("Figure 8: Simulation study",pos = 3,cex=1)

legend(0.1285,0.14,"True EF",lty="solid",lwd="8",col="yellow")
legend(0.134,0.14,"MLE EF",lty="solid",lwd="8",col="green")
legend(0.1285,0.135,"Random Portfs.",pch=19,col="blue")
#legend(0.134,0.135,"Bootstrap EF",lty="solid",lwd="8",col="red")
