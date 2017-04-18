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

plot.ts(IBM,ylab="Price in $",ylim=range(c(min(IBM,GLD,XOM,AAPL,MSFT),max(IBM,GLD,XOM,AAPL,MSFT))),xaxt="n")
text(0,-13,"|",srt = 0, xpd = TRUE, pos = 3,cex=1)
text(0,-20,"2005-01-03",srt = 0, xpd = TRUE, pos = 3,cex=1)
text(522,-13,"|",srt = 0, xpd = TRUE, pos = 3,cex=1)
text(522,-20,"2014-12-29",srt = 0, xpd = TRUE, pos = 3,cex=1)
text(100,-13,"|",srt = 0, xpd = TRUE, pos = 3,cex=1)
text(100,-20,"2006-12-04",srt = 0, xpd = TRUE, pos = 3,cex=1)
text(50,-13,"Estimation window",srt = 0, xpd = TRUE, pos = 3,cex=0.9)
mtext("Figure 10: Price evolution of securities",pos = 3,cex=1)
par(new=TRUE)
plot.ts(GLD,col="red",ylim=range(c(min(IBM,GLD,XOM,AAPL,MSFT),max(IBM,GLD,XOM,AAPL,MSFT))),axes=FALSE,xlab="",ylab="")
par(new=TRUE)
plot.ts(XOM,col="green",ylim=range(c(min(IBM,GLD,XOM,AAPL,MSFT),max(IBM,GLD,XOM,AAPL,MSFT))),axes=FALSE,xlab="",ylab="")
par(new=TRUE)
plot.ts(AAPL,col="blue",ylim=range(c(min(IBM,GLD,XOM,AAPL,MSFT),max(IBM,GLD,XOM,AAPL,MSFT))),axes=FALSE,xlab="",ylab="")
par(new=TRUE)
plot.ts(MSFT,col="yellow",ylim=range(c(min(IBM,GLD,XOM,AAPL,MSFT),max(IBM,GLD,XOM,AAPL,MSFT))),axes=FALSE,xlab="",ylab="")
par(new=TRUE)
plot.ts(JNJ,col="purple",ylim=range(c(min(IBM,GLD,XOM,AAPL,MSFT),max(IBM,GLD,XOM,AAPL,MSFT))),axes=FALSE,xlab="",ylab="")
par(new=TRUE)
plot.ts(PG,col="brown",ylim=range(c(min(IBM,GLD,XOM,AAPL,MSFT),max(IBM,GLD,XOM,AAPL,MSFT))),axes=FALSE,xlab="",ylab="")
par(new=TRUE)
plot.ts(BAC,col="orange",ylim=range(c(min(IBM,GLD,XOM,AAPL,MSFT),max(IBM,GLD,XOM,AAPL,MSFT))),axes=FALSE,xlab="",ylab="")
par(new=TRUE)
plot.ts(WMT,col="aquamarine",ylim=range(c(min(IBM,GLD,XOM,AAPL,MSFT),max(IBM,GLD,XOM,AAPL,MSFT))),axes=FALSE,xlab="",ylab="")
par(new=TRUE)
plot.ts(CVX,col="firebrick",ylim=range(c(min(IBM,GLD,XOM,AAPL,MSFT),max(IBM,GLD,XOM,AAPL,MSFT))),axes=FALSE,xlab="",ylab="")
par(new=TRUE)
plot.ts(T,col="orchid",ylim=range(c(min(IBM,GLD,XOM,AAPL,MSFT),max(IBM,GLD,XOM,AAPL,MSFT))),axes=FALSE,xlab="",ylab="")
legend(0,190,"IBM",lty="solid",col="black")
legend(105,190,"GLD",lty="solid",col="red")
legend(0,170,"XOM",lty="solid",col="green")
legend(105,170,"AAPL",lty="solid",col="blue")
legend(0,150,"MSFT",lty="solid",col="yellow")
legend(105,150,"JNJ",lty="solid",col="purple")
legend(0,130,"PG",lty="solid",col="brown")
legend(105,130,"BAC",lty="solid",col="orange")
legend(205,190,"WMT",lty="solid",col="aquamarine")
legend(205,170,"CVX",lty="solid",col="firebrick")
legend(0,110,"T",lty="solid",col="orchid")
