#Setting

r1<-c(-0.21,0.09,0.28,0.48) 
r2<-c(0.06,0.18,-0.14,0.1)

m<-c(mean(r1),mean(r2))
sigma<-c(sqrt(var(r1)),sqrt(var(r2)))

cov12=cov(r1,r2,method="pearson") 
corr12=cor(r1,r2,method="pearson")

C<-matrix(c(sigma[1]^2,cov12,
            cov12,sigma[2]^2)
          ,ncol=2,nrow=2) 

invC=solve(C)

#MVB

mu<-c()
sig<-c()
t<-seq(-1.0,1.5,0.001)
s<-seq(-1-0,1,0.001)

for(i in 1:length(s)){
  mu[i]=m[1]*t[i]+m[2]*(1-t[i])
  sig[i]=sqrt(sigma[1]^2*t[i]^2+sigma[2]^2*(1-t[i])^2+2*cov12*t[i]*(1-t[i]))
}

plot(mu~sig,type="l")
points(sigma[1],m[1],pch=19)
points(sigma[2],m[2],pch=19)

#Efficient frontier

wGMV=(sigma[2]^2-cov12)/(sigma[1]^2+sigma[2]^2-2*cov12)

mup<-function(x){
  mp=m[1]*x+m[2]*(1-x)
  return(mp)
}

sigmap<-function(x){
  sp=sqrt(sigma[1]^2*x^2+sigma[2]^2*(1-x)^2+2*cov12*x*(1-x))
  return(sp)
}


mup(wGMV)
sigmap(wGMV)


mu1<-c()
sig1<-c()
for(i in 0:length(t)){
  mu1[i]<-mup(t[i])
  sig1[i]<-sigmap(t[i])
}

lines(mu1[214:length(mu1)]~sig1[214:length(mu1)],type="l",col="blue",lwd="3")


#points(sig1[500],mu1[500],pch=19)

#U=mu1[500]-7/2*sig1[500]^2

#muCE<-c()
#for(i in 214:length(t)){
#  muCE[i]=U+7/2*sig1[i]^2
#}
#lines(muCE~sig1)

A=sigma[1]^2+sigma[2]^2-2*cov12
B=m[2]*sigma[1]^2+m[1]*sigma[2]^2-(m[1]+m[2])*cov12
D=(m[1]-m[2])^2
C=m[2]^2*sigma[1]^2+m[1]^2*sigma[2]^2-2*cov12*m[1]*m[2]

#curve(B/A+sqrt(B^2/A^2+(D/A)*(x^2-(C/D))),xlab=expression(sigma[p]),ylab=expression(mu[p]))
#curve(B/A-sqrt(B^2/A^2+(D/A)*(x^2-(C/D))))

#curve(7*x-((B^2/A^2)+(D/A)*(x^2-(C/D))^(-0.5)*(D/A)*x))
#abline(h = 0, lty = 3)


#fun <- function (x) 7*x-((B^2/A^2)+(D/A)*(x^2-(C/D))^(-0.5)*(D/A)*x)
# curve(fun(x), 0, 8)
#abline(h = 0, lty = 3)
#uni <- uniroot(fun, c(0, 8))$root
#points(uni, 0, pch = 16, cex = 2)

#A=2 and slopes equal at 0.215653

B/A+sqrt(B^2/A^2+(D/A)*(0.215653^2-(C/D)))

mm<-c()
mm1<-c()
for(i in 1:length(t)){
  mm[i]<-B/A+sqrt(B^2/A^2+(D/A)*(t[i]^2-(C/D)))
  mm1[i]<-B/A-sqrt(B^2/A^2+(D/A)*(t[i]^2-(C/D)))
}

plot(mm~t,type="l",ylim=c(0,0.2),xlim=c(0.08,0.3),col="blue",lwd=2,xlab="",ylab="",xaxt="n",yaxt="n")
lines(mm1~t,lty="dashed")

points(sigma[1],m[1],pch=19)
points(sigma[2],m[2],pch=19)
points(sigmap(wGMV),mup(wGMV),pch=19)

#curve(B/A+sqrt(B^2/A^2+(D/A)*(x^2-(C/D))),xlim=c(0,0.4),ylim=c(0,0.2),xlab=expression(sigma[p]),ylab=expression(mu[p]),col="blue",lwd=2)

#slopes equal at sigma= 0.123676
#corresponding return=0.08808104

sigop= 0.123676
mop=B/A+sqrt(B^2/A^2+(D/A)*(0.123676^2-(C/D)))

#Utility level of indifference curve at A=7

U=B/A+sqrt(B^2/A^2+(D/A)*(0.123676^2-(C/D)))-7/2*0.123676^2

#Plot for indifference curve at A=7

muCE<-c()
for(i in 1:length(t)){
  muCE[i]=U+7/2*sig1[i]^2
}
lines(muCE~sig1,col="purple",lwd=2)

points(0.123676,B/A+sqrt(B^2/A^2+(D/A)*(0.123676^2-(C/D))),pch=19,col="purple")
#points(sigma[1],B/A+sqrt(B^2/A^2+(D/A)*(sigma[1]^2-(C/D))),pch=19,col="black")
#points(0.2,0.024,pch=19,col="black")
#points(0.145,0.069,pch=19,col="black")

#(sigma[2],mu[2])

text(0.065,m[2]-0.01, expression(mu[2]), srt = 0, xpd = TRUE, pos = 3,cex=1.5)
segments(0.065,m[2],sigma[2],m[2],lty="dotted")
text(sigma[2]+0.005,-0.025, expression(sigma[2]), srt = 0, xpd = TRUE, pos = 3,cex=1.5)
segments(sigma[2],-0.025,sigma[2],m[2],lty="dotted")

#(sigma[1],mu[1])
text(0.065,m[1]-0.01, expression(mu[1]), srt = 0, xpd = TRUE, pos = 3,cex=1.5)
segments(0.065,m[1],sigma[1],m[1],lty="dotted")
text(sigma[1],-0.025, expression(sigma[1]), srt = 0, xpd = TRUE, pos = 3,cex=1.5)
segments(sigma[1],-0.025,sigma[1],m[1],lty="dotted")

#GMV
text(0.062,mup(wGMV)-0.01, expression(mu[GMV]), srt = 0, xpd = TRUE, pos = 3,cex=1.5)
segments(0.062,mup(wGMV),sigmap(wGMV),mup(wGMV),lty="dotted")
text(sigmap(wGMV)-0.007,-0.025, expression(sigma[GMV]), srt = 0, xpd = TRUE, pos = 3,cex=1.5)
segments(sigmap(wGMV),-0.025,sigmap(wGMV),mup(wGMV),lty="dotted")

#Optimal
#text(0.065,mop-0.01, expression(mu["*"]), srt = 0, xpd = TRUE, pos = 3,cex=1.5)
#segments(0.065,mop,sigop,mop,lty="dotted")
#text(sigop+0.003,-0.025, expression(sigma["*"]), srt = 0, xpd = TRUE, pos = 3,cex=1.5)
#segments(sigop,-0.025,sigop,mop,lty="dotted")

#legend(sigma[1]-0.06,0.024,"Optimal Portfolio",pch=19,col="purple")
mtext("Figure 7: Effect of risk aversion on optimality",pos = 3,cex=1)

#Weights are w1=0.346191 and w2=0.653809

#Portfolios for different levels of risk aversion A=5,3,2


#A=5

sigmop1=0.13156 
mop1=B/A+sqrt(B^2/A^2+(D/A)*(0.13156^2-(C/D)))

U2=mop1-5/2*sigmop1^2

#Plot for indifference curve at A=5

#muCE1<-c()
#for(i in 1:length(t)){
#  muCE1[i]=U2+5/2*sig1[i]^2
#}
#lines(muCE1~sig1,col="green",lwd=2)

#points(sigmop1,mop1,col="green",pch=19)
#legend(sigma[1]-0.06,0.045,"Optimal Portfolio A=5",pch=19,col="green")

#A=3

sigmop2=0.156883
mop2=B/A+sqrt(B^2/A^2+(D/A)*(0.156883^2-(C/D)))

U3=mop2-3/2*sigmop2^2

#Plot for indifference curve at A=3

muCE2<-c()
for(i in 1:length(t)){
  muCE2[i]=U3+3/2*sig1[i]^2
}
lines(muCE2~sig1,col="red",lwd=2)

points(sigmop2,mop2,col="red",pch=19)
#legend(sigma[1]-0.06,0.065,"Optimal Portfolio A=3",pch=19,col="red")

#A=2

sigmop3=0.197176
mop3=B/A+sqrt(B^2/A^2+(D/A)*(0.197176^2-(C/D)))


U4=mop3-2/2*sigmop3^2

#Plot for indifference curve at A=7

muCE3<-c()
for(i in 1:length(t)){
  muCE3[i]=U4+2/2*sig1[i]^2
}
lines(muCE3~sig1,col="orange",lwd=2)

points(sigmop3,mop3,col="orange",pch=19)
#legend(sigma[1]-0.06,0.085,"Optimal Portfolio A=2",pch=19,col="orange")

text(sigma[1]-0.06,0.065, expression(lambda["1"]), col="purple",srt = 0, xpd = TRUE, pos = 3,cex=1.5)
text(sigma[1]-0.054,0.065, expression(">"), col="black",srt = 0, xpd = TRUE, pos = 3,cex=1.5)
text(sigma[1]-0.0465,0.065, expression(lambda["2"]), col="red",srt = 0, xpd = TRUE, pos = 3,cex=1.5)
text(sigma[1]-0.04,0.065, expression(">"), col="black",srt = 0, xpd = TRUE, pos = 3,cex=1.5)
text(sigma[1]-0.0325,0.065, expression(lambda["3"]), col="orange",srt = 0, xpd = TRUE, pos = 3,cex=1.5)
