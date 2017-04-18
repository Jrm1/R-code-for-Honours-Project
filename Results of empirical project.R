#Results of empirical project

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#First we collect weekly pricing data of security universe

library(tseries)  
tckk <-c("IBM","GLD","XOM","AAPL","MSFT","JNJ","PG","BAC","WMT","CVX","T") #ticker names defined  
numtk <-length(tckk);  
ustart <-"2005-01-01";uend <-"2015-01-01" #start and end date  
all_dat<-list()#empty list to fill in the data  
for(i in 1:numtk)  
{  
  all_dat[[i]]<-xxx<-get.hist.quote(instrument = tckk[i], start=ustart, end=uend,quote = c("Close","AdjClose"),provider = "yahoo", compression = "w")  
} 

dat<-data.frame(all_dat)

attach(dat)

keeps <- c("AdjClose","AdjClose.1","AdjClose.2","AdjClose.3","AdjClose.4","AdjClose.5","AdjClose.6","AdjClose.7","AdjClose.8","AdjClose.9","AdjClose.10")

data<-dat[keeps]

colnames(data)<-tckk

any(is.na(data))

attach(data)

#Next we plot the data

#plot.ts(IBM,ylab="Price in $",ylim=range(c(min(IBM,GLD,XOM,AAPL,MSFT),max(IBM,GLD,XOM,AAPL,MSFT))),xaxt="n")
#text(0,-13,"|",srt = 0, xpd = TRUE, pos = 3,cex=1)
#text(0,-20,"2005-01-03",srt = 0, xpd = TRUE, pos = 3,cex=1)
#text(522,-13,"|",srt = 0, xpd = TRUE, pos = 3,cex=1)
#text(522,-20,"2014-12-29",srt = 0, xpd = TRUE, pos = 3,cex=1)
#text(100,-13,"|",srt = 0, xpd = TRUE, pos = 3,cex=1)
#text(100,-20,"2006-12-04",srt = 0, xpd = TRUE, pos = 3,cex=1)
#text(50,-13,"Estimation window",srt = 0, xpd = TRUE, pos = 3,cex=0.9)
#mtext("Figure 10: Price evolution of securities",pos = 3,cex=1)
#par(new=TRUE)
#plot.ts(GLD,col="red",ylim=range(c(min(IBM,GLD,XOM,AAPL,MSFT),max(IBM,GLD,XOM,AAPL,MSFT))),axes=FALSE,xlab="",ylab="")
#par(new=TRUE)
#plot.ts(XOM,col="green",ylim=range(c(min(IBM,GLD,XOM,AAPL,MSFT),max(IBM,GLD,XOM,AAPL,MSFT))),axes=FALSE,xlab="",ylab="")
#par(new=TRUE)
#plot.ts(AAPL,col="blue",ylim=range(c(min(IBM,GLD,XOM,AAPL,MSFT),max(IBM,GLD,XOM,AAPL,MSFT))),axes=FALSE,xlab="",ylab="")
#par(new=TRUE)
#plot.ts(MSFT,col="yellow",ylim=range(c(min(IBM,GLD,XOM,AAPL,MSFT),max(IBM,GLD,XOM,AAPL,MSFT))),axes=FALSE,xlab="",ylab="")
#par(new=TRUE)
#plot.ts(JNJ,col="purple",ylim=range(c(min(IBM,GLD,XOM,AAPL,MSFT),max(IBM,GLD,XOM,AAPL,MSFT))),axes=FALSE,xlab="",ylab="")
#par(new=TRUE)
#plot.ts(PG,col="brown",ylim=range(c(min(IBM,GLD,XOM,AAPL,MSFT),max(IBM,GLD,XOM,AAPL,MSFT))),axes=FALSE,xlab="",ylab="")
#par(new=TRUE)
#plot.ts(BAC,col="orange",ylim=range(c(min(IBM,GLD,XOM,AAPL,MSFT),max(IBM,GLD,XOM,AAPL,MSFT))),axes=FALSE,xlab="",ylab="")
#par(new=TRUE)
#plot.ts(WMT,col="aquamarine",ylim=range(c(min(IBM,GLD,XOM,AAPL,MSFT),max(IBM,GLD,XOM,AAPL,MSFT))),axes=FALSE,xlab="",ylab="")
#par(new=TRUE)
#plot.ts(CVX,col="firebrick",ylim=range(c(min(IBM,GLD,XOM,AAPL,MSFT),max(IBM,GLD,XOM,AAPL,MSFT))),axes=FALSE,xlab="",ylab="")
#par(new=TRUE)
#plot.ts(T,col="orchid",ylim=range(c(min(IBM,GLD,XOM,AAPL,MSFT),max(IBM,GLD,XOM,AAPL,MSFT))),axes=FALSE,xlab="",ylab="")
#legend(0,190,"IBM",lty="solid",col="black")
#legend(105,190,"GLD",lty="solid",col="red")
#legend(0,170,"XOM",lty="solid",col="green")
#legend(105,170,"AAPL",lty="solid",col="blue")
#legend(0,150,"MSFT",lty="solid",col="yellow")
#legend(105,150,"JNJ",lty="solid",col="purple")
#legend(0,130,"PG",lty="solid",col="brown")
#legend(105,130,"BAC",lty="solid",col="orange")
#legend(205,190,"WMT",lty="solid",col="aquamarine")
#legend(205,170,"CVX",lty="solid",col="firebrick")
#legend(0,110,"T",lty="solid",col="orchid")


#Calculate the linear security returns

returns.IBM<-c()
returns.GLD<-c()
returns.XOM<-c()
returns.AAPL<-c()
returns.MSFT<-c()
returns.JNJ<-c()
returns.PG<-c()
returns.BAC<-c()
returns.WMT<-c()
returns.CVX<-c()
returns.T<-c()


for(i in 1:length(IBM)-1){
  returns.IBM[i]=(IBM[i+1]/IBM[i])-1
  returns.GLD[i]=(GLD[i+1]/GLD[i])-1
  returns.XOM[i]=(XOM[i+1]/XOM[i])-1
  returns.AAPL[i]=(AAPL[i+1]/AAPL[i])-1
  returns.MSFT[i]=(MSFT[i+1]/MSFT[i])-1
  returns.JNJ[i]=(JNJ[i+1]/JNJ[i])-1
  returns.PG[i]=(PG[i+1]/PG[i])-1
  returns.BAC[i]=(BAC[i+1]/BAC[i])-1
  returns.WMT[i]=(WMT[i+1]/WMT[i])-1
  returns.CVX[i]=(CVX[i+1]/CVX[i])-1
  returns.T[i]=(T[i+1]/T[i])-1
}

data$IBM[-length(IBM)] <- returns.IBM
data$GLD[-length(IBM)]<- returns.GLD
data$XOM[-length(IBM)]<- returns.XOM
data$AAPL[-length(IBM)]<- returns.AAPL
data$MSFT[-length(IBM)]<- returns.MSFT
data$JNJ[-length(IBM)]<- returns.JNJ
data$PG[-length(IBM)]<- returns.PG
data$BAC[-length(IBM)]<- returns.BAC
data$WMT[-length(IBM)]<- returns.WMT
data$CVX[-length(IBM)]<- returns.CVX
data$T[-length(IBM)]<- returns.T

returns<-data[-522,]

attach(returns)



#Perform clasical/ML input estimation and do mean-variance analysis

u<-t(c(rep(1,11)))

#Vector of mean returns

mu<-t(c(mean(IBM[1:100]),mean(GLD[1:100]),mean(XOM[1:100]),
        mean(AAPL[1:100]),mean(MSFT[1:100]),mean(JNJ[1:100]),
        mean(PG[1:100]),mean(BAC[1:100]),mean(WMT[1:100]),
        mean(CVX[1:100]),mean(T[1:100]))) #vector of mean returns


#Variance-Covariance Matrix

M <- cbind(IBM[1:100],GLD[1:100],XOM[1:100],
           AAPL[1:100],MSFT[1:100],JNJ[1:100],
           PG[1:100],BAC[1:100],WMT[1:100],
           CVX[1:100],T[1:100])

k <- ncol(M) #number of variables
n <- nrow(M) #number of subjects

#create means for each column
M_mean <- matrix(data=1, nrow=n) %*% mu 

#creates a difference matrix
D <- M - M_mean

#creates the covariance matrix
C <- (n-1)^-1*t(D)%*%D

invC=solve(C)

#MVB

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


#MVP

nn<-c(t((u%*%invC)))
d<-c((u%*%invC%*%t(u)))
w<-t(c(nn/d))
w                         

muv=w%*%t(mu)
muv               


sigmav=sqrt(w%*%C%*%t(w))
sigmav 


t<-seq(-0.1,0.1,0.00002)
W=matrix(,nrow = length(t), ncol = 11)
for(i in 0:length(t)){
  W[i,]<-t(as.vector(weights(t[i])))
}                         

w21<-t(as.vector(W[21,]))
t(w21)
sqrt(w21%*%C%*%t(w21))    

d=matrix(,nrow=length(t),ncol=11)
for(i in 1:length(t)){
  d[i,]<-(t(as.vector(W[i,])))
}                         

V=matrix(,nrow=length(t),ncol=11)
for(i in 1:length(t)){
  V[i,]<-sqrt(t(as.vector(d[i,]))%*%C%*%t(t(as.vector(d[i,]))))
}

r<-as.vector(V)


#Plot "plug-in/MLE" estiated efficient frontier

#par(mfrow=c(1,1))
#plot(t[0:10001]~r[0:10001],xlab=expression(sigma),ylab=expression(mu),
#     main="Efficient Frontier",sub="11 Assets",type="l",lwd="3",col="blue",xlim=c(0.0,0.1),ylim=c(-0.002,0.015))
#points(sigmav,muv,pch=19,col="green")
#points(sqrt(C[1,1]),mu[1],pch=19,col="black")
#text(sqrt(C[1,1]),mu[1]-0.00001, "IBM",srt = 0, xpd = TRUE, pos = 3,cex=1,col="black")
#points(sqrt(C[2,2]),mu[2],pch=19,col="red")
#text(sqrt(C[2,2]),mu[2]-0.00001, "GLD",srt = 0, xpd = TRUE, pos = 3,cex=1,col="red")
#points(sqrt(C[3,3]),mu[3],pch=19,col="green")
#text(sqrt(C[3,3]),mu[3]-0.00001, "XOM",srt = 0, xpd = TRUE, pos = 3,cex=1,col="green")
#points(sqrt(C[4,4]),mu[4],pch=19,col="blue")
#text(sqrt(C[4,4]),mu[4]-0.00001, "AAPL",srt = 0, xpd = TRUE, pos = 3,cex=1,col="blue")
#points(sqrt(C[5,5]),mu[5],pch=19,col="yellow")
#text(sqrt(C[5,5]),mu[5]-0.00001, "MSFT",srt = 0, xpd = TRUE, pos = 3,cex=1,col="yellow")
#points(sqrt(C[6,6]),mu[6],pch=19,col="purple")
#text(sqrt(C[6,6]),mu[6]-0.00001, "JNJ",srt = 0, xpd = TRUE, pos = 3,cex=1,col="purple")
#points(sqrt(C[7,7]),mu[7],pch=19,col="brown")
#text(sqrt(C[7,7]),mu[7]-0.00001, "PG",srt = 0, xpd = TRUE, pos = 3,cex=1,col="brown")
#points(sqrt(C[8,8]),mu[8],pch=19,col="orange")
#text(sqrt(C[8,8]),mu[8]-0.00001, "BAC",srt = 0, xpd = TRUE, pos = 3,cex=1,col="orange")
#points(sqrt(C[9,9]),mu[9],pch=19,col="aquamarine")
#text(sqrt(C[9,9]),mu[9]-0.00001, "WMT",srt = 0, xpd = TRUE, pos = 3,cex=1,col="aquamarine")
#points(sqrt(C[10,10]),mu[10],pch=19,col="firebrick")
#text(sqrt(C[10,10]),mu[10]-0.00001, "CVX",srt = 0, xpd = TRUE, pos = 3,cex=1,col="firebrick")
#points(sqrt(C[11,11]),mu[11],pch=19,col="orchid")
#text(sqrt(C[11,11]),mu[11]-0.00001, "T",srt = 0, xpd = TRUE, pos = 3,cex=1,col="orchid")

#Track performance of GMVP as given by w

keeps <- c("AdjClose","AdjClose.1","AdjClose.2","AdjClose.3","AdjClose.4","AdjClose.5","AdjClose.6","AdjClose.7","AdjClose.8","AdjClose.9","AdjClose.10")
prices<-dat[keeps]
attach(prices)
trading.prices<-prices[-seq(1,100,1),]


Capital=100000

positions<-c()

for(i in 1:11){
  positions[i]=(Capital*w[1,i])/trading.prices[1,i]
}


#Buy and Hold strategy; no rebalancing; no updating

Portfolio.value<-c()

for(i in 1:422){
  Portfolio.value[i]=sum(trading.prices[i,]*positions[])
}

require(xts)

#Portfolio cumulative returns

returns.portfolio<-c()

for(i in 1:length(Portfolio.value)-1){
  returns.portfolio[i]=(Portfolio.value[i+1]/Portfolio.value[i])-1
}

cummulative.portfolio.returns<-cumsum(returns.portfolio)


#Sharpe

SR=mean(cummulative.portfolio.returns)/sd(cummulative.portfolio.returns)


#Max drawdown

mdd<-maxdrawdown(Portfolio.value)
MDD=(Portfolio.value[as.numeric(mdd[3])]-Portfolio.value[as.numeric(mdd[2])])/(Portfolio.value[as.numeric(mdd[2])])
MDD*100





#Portfolio for risk averse investor with lambda=7


AA=u%*%invC%*%t(u)*det(C) #correct
BB=u%*%invC%*%t(mu)*det(C) #correct
CC=mu%*%invC%*%t(mu)*det(C) #correct
DD=(AA*CC-BB^2)*(det(C))^-1 #correct


#Need to solve for x in: 7*x-((BB^2/AA^2)+(DD/AA)*(x^2-(CC/DD))^(-0.5)*(DD/AA)*x
#Slopes equal at risk=0.0442336

sigop=0.0442336

muop=BB/AA+sqrt(BB^2/AA^2+(DD/AA)*(sigop^2-(CC/DD))) #corresponding return

CEQ=muop-7/2*sigop^2

wop=weights(muop)

#points(sigop,muop,pch=19,col="purple")



#Performance

Capital=100000

positionsop<-c()

for(i in 1:11){
  positionsop[i]=(Capital*wop[1,i])/trading.prices[1,i]
}


#Buy and Hold strategy; no rebalancing; no updating

Portfolio.value.op<-c()

for(i in 1:422){
  Portfolio.value.op[i]=sum(trading.prices[i,]*positionsop[])
}

require(xts)

#Portfolio cumulative returns

returns.portfolio.op<-c()

for(i in 1:length(Portfolio.value.op)-1){
  returns.portfolio.op[i]=(Portfolio.value.op[i+1]/Portfolio.value.op[i])-1
}

cummulative.portfolio.returns.op<-cumsum(returns.portfolio.op)

#plot.ts(cummulative.portfolio.returns.op,main="Cumulative Porfolio Returns",ylab="Cummulative (weekly) returns")

#Sharpe

SR1=mean(cummulative.portfolio.returns.op)/sd(cummulative.portfolio.returns.op)

#sharpe(cummulative.portfolio.returns.op)

#Max drawdown

mdd.op<-maxdrawdown(Portfolio.value.op)
MDDo=(Portfolio.value.op[as.numeric(mdd[3])]-Portfolio.value.op[as.numeric(mdd[2])])/(Portfolio.value.op[as.numeric(mdd[2])])
MDDo*100


#Bootstrap

set.seed(123)
B=999

muIBM.ests<-c()
muGLD.ests<-c()
muXOM.ests<-c()
muAAPL.ests<-c()
muMSFT.ests<-c()
muJNJ.ests<-c()
muPG.ests<-c()
muBAC.ests<-c()
muWMT.ests<-c()
muCVX.ests<-c()
muT.ests<-c()

for(i in 1:B){
  b.IBM<-sample(IBM,length(IBM),replace=TRUE)  #999 bootstrap samples
  b.GLD<-sample(GLD,length(GLD),replace=TRUE)
  b.XOM<-sample(XOM,length(XOM),replace=TRUE)
  b.AAPL<-sample(AAPL,length(AAPL),replace=TRUE)
  b.MSFT<-sample(MSFT,length(MSFT),replace=TRUE)
  b.JNJ<-sample(JNJ,length(JNJ),replace=TRUE)
  b.PG<-sample(PG,length(PG),replace=TRUE)
  b.BAC<-sample(BAC,length(BAC),replace=TRUE)
  b.WMT<-sample(WMT,length(WMT),replace=TRUE)
  b.CVX<-sample(CVX,length(CVX),replace=TRUE)
  b.T<-sample(T,length(T),replace=TRUE)
  
  muIBM.ests[i]<-mean(b.IBM) 
  muGLD.ests[i]<-mean(b.GLD) 
  muXOM.ests[i]<-mean(b.XOM) 
  muAAPL.ests[i]<-mean(b.AAPL) 
  muMSFT.ests[i]<-mean(b.MSFT) 
  muJNJ.ests[i]<-mean(b.JNJ) 
  muPG.ests[i]<-mean(b.PG) 
  muBAC.ests[i]<-mean(b.BAC) 
  muWMT.ests[i]<-mean(b.WMT) 
  muCVX.ests[i]<-mean(b.CVX) 
  muT.ests[i]<-mean(b.T)
}

mu.b<-t(c(mean(muIBM.ests),mean(muGLD.ests),mean(muXOM.ests),mean(muAAPL.ests),
          mean(muMSFT.ests),mean(muJNJ.ests),mean(muPG.ests),mean(muBAC.ests),
          mean(muWMT.ests),mean(muCVX.ests),mean(muT.ests)))

M <-cbind(b.IBM,  #999 bootstrap samples
          b.GLD,
          b.XOM,
          b.AAPL,
          b.MSFT,
          b.JNJ,
          b.PG,
          b.BAC,
          b.WMT,
          b.CVX,
          b.T)

k <- ncol(M) #number of variables
nn <- nrow(M) #number of subjects

#create means for each column
M_mean <- matrix(data=1, nrow=nn) %*% mu.b 

#creates a difference matrix
D <- M - M_mean

#creates the covariance matrix
CC <- (nn-1)^-1*t(D)%*%D
CC

invCC=solve(CC)


weights.b<-function(x){
  a<-matrix(c(1,u%*%invCC%*%mu.b, 
              x,mu.b%*%invCC%*%mu.b),nrow=2,ncol=2)
  b<-matrix(c(u%*%invCC%*%t(u),1, 
              mu.b%*%invCC%*%t(u),x),nrow=2,ncol=2)
  c<-matrix(c(u%*%invCC%*%t(u),u%*%invCC%*%mu.b, 
              mu.b%*%invCC%*%t(u),mu.b%*%invCC%*%mu.b),
            nrow=2,ncol=2)
  w=(det(a)*u%*%invCC+det(b)*mu.b%*%invCC)/det(c)
  return(w)
}

ne.b<-c(t((u%*%invCC)))
de.b<-c((u%*%invCC%*%t(u)))
we.b<-t(c(ne.b/de.b))
we.b                        

muve.b=we.b%*%t(t(mu.b))
muve.b                   


sigmave.b=sqrt(we.b%*%invCC%*%t(we.b))
sigmave.b


#Performance

Capital=100000

positionsb<-c()

for(i in 1:11){
  positionsb[i]=(Capital*we.b[1,i])/trading.prices[1,i]
}


#Buy and Hold strategy; no rebalancing; no updating

Portfolio.value.b<-c()

for(i in 1:422){
  Portfolio.value.b[i]=sum(trading.prices[i,]*positionsb[])
}

require(xts)

#Portfolio cumulative returns

returns.portfolio.b<-c()

for(i in 1:length(Portfolio.value.b)-1){
  returns.portfolio.b[i]=(Portfolio.value.b[i+1]/Portfolio.value.b[i])-1
}

cummulative.portfolio.returns.b<-cumsum(returns.portfolio.b)

#plot.ts(cummulative.portfolio.returns.b,main="Cumulative Porfolio Returns",ylab="Cummulative returns")

#Sharpe

SR2=mean(cummulative.portfolio.returns.b)/sd(cummulative.portfolio.returns.b)

#sharpe(cummulative.portfolio.returns.b)

#Max drawdown

mdd.b<-maxdrawdown(Portfolio.value.b)
MDDb=(Portfolio.value.b[as.numeric(mdd[3])]-Portfolio.value.b[as.numeric(mdd[2])])/(Portfolio.value.b[as.numeric(mdd[2])])
MDDb*100



#Bootstrap portfolio for risk averse investor with lambda=7


AA=u%*%invCC%*%t(u)*det(CC) #correct
BB=u%*%invCC%*%t(mu)*det(CC) #correct
cc=mu%*%invCC%*%t(mu)*det(CC) #correct
DD=(AA*cc-BB^2)*(det(CC))^-1 #correct


#7*x-((BB^2/AA^2)+(DD/AA)*(x^2-(cc/DD))^(-0.5)*(DD/AA)*x

#Slopes equal at x =0.0424965

sigop.b=0.0424965

muop.b=BB/AA+sqrt(BB^2/AA^2+(DD/AA)*(0.0424965^2-(cc/DD))) #corresponding return

CEQ.op.b=muop.b-7/2*(sigop.b)^2

wop.b=weights(muop.b)


#Performance

Capital=100000

positionsop.b<-c()

for(i in 1:11){
  positionsop.b[i]=(Capital*wop.b[1,i])/trading.prices[1,i]
}


#Buy and Hold strategy; no rebalancing; no updating

Portfolio.value.op.b<-c()

for(i in 1:422){
  Portfolio.value.op.b[i]=sum(trading.prices[i,]*positionsop.b[])
}

require(xts)

#Portfolio cumulative returns

returns.portfolio.op.b<-c()

for(i in 1:length(Portfolio.value.op.b)-1){
  returns.portfolio.op.b[i]=(Portfolio.value.op.b[i+1]/Portfolio.value.op.b[i])-1
}

cummulative.portfolio.returns.op.b<-cumsum(returns.portfolio.op.b)

#plot.ts(cummulative.portfolio.returns.op.b,main="Cumulative Porfolio Returns",ylab="Cummulative returns")

#Sharpe

#sharpe(cummulative.portfolio.returns.op.b)

SR3=mean(cummulative.portfolio.returns.op.b)/sd(cummulative.portfolio.returns.op.b)

#Max drawdown

mdd.op.b<-maxdrawdown(Portfolio.value.op.b)
MDDo.b=(Portfolio.value.op.b[as.numeric(mdd[3])]-Portfolio.value.op.b[as.numeric(mdd[2])])/(Portfolio.value.op.b[as.numeric(mdd[2])])
MDDo.b*100



#Use market index SPY as benchmark

library(tseries)  
tckk <-c("SPY")# ticker names defined  
numtk <-length(tckk);  
ustart <-"2005-01-01";uend <-"2015-01-01" #start and end date  
market.data<-list()#empty list to fill in the data  
for(i in 1:numtk)  
{  
  market.data[[i]]<-xxx<-get.hist.quote(instrument = tckk[i], start=ustart, end=uend,quote = c("Close","AdjClose"),provider = "yahoo", compression = "w")  
} 

dat<-data.frame(market.data)

attach(dat)

keeps <- c("AdjClose")

data<-dat[keeps]

colnames(data)<-tckk

any(is.na(data))

attach(data)



#Weight is w=1

keeps <- c("AdjClose")

prices<-dat[keeps]

attach(prices)

trading.prices<-prices[-seq(1,100,1),]

trading.prices=as.data.frame(trading.prices)

attach(trading.prices)

#Portfolio

Capital=100000
wm<-matrix(c(1),ncol=1,nrow=1)

positions.m<-c()

for(i in 1:1){
  positions.m[i]=(Capital*wm[1,i])/trading.prices[1,i]
}



Portfolio.value.m<-c()

for(i in 1:422){
  Portfolio.value.m[i]=sum(trading.prices[i,]*positions.m[])
}

require(xts)

#Portfolio cumulative returns

returns.portfolio.m<-c()

for(i in 1:length(Portfolio.value.m)-1){
  returns.portfolio.m[i]=(Portfolio.value.m[i+1]/Portfolio.value.m[i])-1
}

cummulative.portfolio.returns.m<-cumsum(returns.portfolio.m)


#Sharpe

mean(cummulative.portfolio.returns.m)/sd(cummulative.portfolio.returns.m)

#Max drawdown

mddm<-maxdrawdown(Portfolio.value.m)
MDDm=(Portfolio.value.m[as.numeric(mdd[3])]-Portfolio.value.m[as.numeric(mdd[2])])/(Portfolio.value.m[as.numeric(mdd[2])])
MDDm*100


#plot(Portfolio.value.m,main="Buy/Hold S&P 500",xlab="Time",ylab="Portfolio value in $",xaxt="n",type="l",col="black")
#text(0,39000, expression("|"), srt = 0, xpd = TRUE, pos = 3,cex=1)
#text(0,35000, expression("2006-12-04"), srt = 0, xpd = TRUE, pos = 3,cex=1)
#text(422,39000, expression("|"), srt = 0, xpd = TRUE, pos = 3,cex=1)
#text(422,35000, expression("2014-12-29"), srt = 0, xpd = TRUE, pos = 3,cex=1)
#mtext("Figure 14: Capital $100,000",pos = 3,cex=1)
#legend(0,170000,expression("SR = 0.9460"))
#legend(0,158000,expression("MDD = -42.507%"))
#legend(0,146000,expression("CEQ = NA"))


#Plot results

#Buy/Hold GMVP MLE

plot(Portfolio.value,main="Strategy: Buy/Hold GMV",xlab="Time",ylab="Portfolio value in $",xaxt="n",type="l",col="black",ylim=c(min(Portfolio.value,Portfolio.value.m),max(Portfolio.value,Portfolio.value.m)))
par(new=TRUE)
plot(Portfolio.value.m,main="",ylim=c(min(Portfolio.value,Portfolio.value.m),max(Portfolio.value,Portfolio.value.m)),xlab="",ylab="",axes=FALSE,type="l",col="red")
mtext("Estimation: Plug-in/MLE; Capital $100,000",pos = 3,cex=1)
text(0,39000, expression("|"), srt = 0, xpd = TRUE, pos = 3,cex=1)
text(0,35000, expression("2006-12-04"), srt = 0, xpd = TRUE, pos = 3,cex=1)
text(422,39000, expression("|"), srt = 0, xpd = TRUE, pos = 3,cex=1)
text(422,35000, expression("2014-12-29"), srt = 0, xpd = TRUE, pos = 3,cex=1)
legend(0,170000,expression("SR = 1.2983"))
legend(0,158000,expression("MDD = -25.046%"))
legend(0,146000,expression("CEQ = NA"))
legend(270,60000,"S&P 500 benchmark",lty ="solid",col="red")

#Buy/Hold Portfolio for lambda=7 MLE

plot(Portfolio.value.op,ylim=c(min(Portfolio.value.op,Portfolio.value.m),max(Portfolio.value.op,Portfolio.value.m)),main="Strategy: Buy/Hold λ=7 Portfolio",xlab="Time",ylab="Portfolio value in $",xaxt="n",type="l",col="black")
par(new=TRUE)
plot(Portfolio.value.m,main="",ylim=c(min(Portfolio.value.op,Portfolio.value.m),max(Portfolio.value.op,Portfolio.value.m)),xlab="",ylab="",axes=FALSE,type="l",col="red")
legend(270,85000,"S&P 500 benchmark",lty ="solid",col="red")
text(0,-2000, expression("|"), srt = 0, xpd = TRUE, pos = 3,cex=1)
text(0,-18000, expression("2006-12-04"), srt = 0, xpd = TRUE, pos = 3,cex=1)
text(422,-2000, expression("|"), srt = 0, xpd = TRUE, pos = 3,cex=1)
text(422,-18000, expression("2014-12-29"), srt = 0, xpd = TRUE, pos = 3,cex=1)
mtext("Estimation: Plug-in/MLE; Capital $100,000",pos = 3,cex=1)
legend(0,600000,expression("SR = 1.527447"))
legend(0,540000,expression("MDD = -27.022%"))
legend(0,480000,expression("CEQ = 0.007736815"))

#Buy/Hold GMVP with Bootstrap

plot(Portfolio.value.b,main="Strategy: Buy/Hold GMV",xlab="Time",ylab="Portfolio value in $",xaxt="n",type="l",col="black",ylim=c(min(Portfolio.value.b,Portfolio.value.m),max(Portfolio.value.b,Portfolio.value.m)))
par(new=TRUE)
plot(Portfolio.value.m,main="",ylim=c(min(Portfolio.value.b,Portfolio.value.m),max(Portfolio.value.b,Portfolio.value.m)),xlab="",ylab="",axes=FALSE,type="l",col="red")
text(0,35000, expression("|"), srt = 0, xpd = TRUE, pos = 3,cex=1)
text(0,30000, expression("2006-12-04"), srt = 0, xpd = TRUE, pos = 3,cex=1)
text(422,35000, expression("|"), srt = 0, xpd = TRUE, pos = 3,cex=1)
text(422,30000, expression("2014-12-29"), srt = 0, xpd = TRUE, pos = 3,cex=1)
mtext("Estimation: Bootstrap; Capital $100,000",pos = 3,cex=1)
legend(0,210000,expression("SR = 1.395217"))
legend(0,193500,expression("MDD = -24.16306%"))
legend(0,177000,expression("CEQ = NA"))
legend(270,62000,"S&P 500 benchmark",lty ="solid",col="red")

#Buy/Hold Portfolio for lambda=7 with Bootstrap

plot(Portfolio.value.op.b,main="Strategy Buy/Hold λ=7 Portfolio",xlab="Time",ylab="Portfolio value in $",xaxt="n",type="l",col="black",ylim=c(min(Portfolio.value.op.b,Portfolio.value.m),max(Portfolio.value.op.b,Portfolio.value.m)))
par(new=TRUE)
plot(Portfolio.value.m,main="",ylim=c(min(Portfolio.value.op.b,Portfolio.value.m),max(Portfolio.value.op.b,Portfolio.value.m)),xlab="",ylab="",axes=FALSE,type="l",col="red")
mtext("Estimation: Bootstrap; Capital $100,000",pos = 3,cex=1)
text(0,-3800, expression("|"), srt = 0, xpd = TRUE, pos = 3,cex=1)
text(0,-19000, expression("2006-12-04"), srt = 0, xpd = TRUE, pos = 3,cex=1)
text(422,-3800, expression("|"), srt = 0, xpd = TRUE, pos = 3,cex=1)
text(422,-19000, expression("2014-12-29"), srt = 0, xpd = TRUE, pos = 3,cex=1)
legend(0,600000,expression("SR = 1.520683"))
legend(0,540000,expression("MDD = -26.96101%"))
legend(0,480000,expression("CEQ = 0.007943266"))
legend(270,85000,"S&P 500 benchmark",lty ="solid",col="red")
