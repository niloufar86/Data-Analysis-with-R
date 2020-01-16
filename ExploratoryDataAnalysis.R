
##########################
streamflow = read.table("streamflow_delaware.txt",header=T)
ridership = read.table("ridership_data.txt",header=T)

# Rearrange streamflow data #
ndays = 366
nyrs = nrow(streamflow)/ndays
nsites = ncol(streamflow)

streamflow_yearly = array(NA,c(ndays,nyrs,nsites))
for (i in 1:nsites)
  {
    streamflow_yearly[,,i] = array(streamflow[,i],c(ndays,nyrs)) 
  }

# Rearrange ridership data #
stations = unique(ridership[,1])
nstations = length(stations)
days = 30
stations_data = array(ridership[,5],c(days,nstations))

# Plots to visualize the data #
##############################

# Dot plot and boxplots
library(lattice) # package that has dotplots
par(mfrow=c(2,1))
dotplot(streamflow[,1],xlab="Streamflow for Neversink (CFS)",add=T)
boxplot(streamflow[,1],horizontal=T)

par(mfrow=c(2,1))
dotplot(streamflow_yearly[,1,1],xlab="Streamflow for year Oct 1937 - Sep 1938")
boxplot(streamflow_yearly[,1,1],horizontal=T)


dotplot(stations_data[,4],xlab="October 2011 Ridership data for 1 train 137th Street")
boxplot(stations_data[,4],horizontal=T)

dotplot(ridership[,5],xlab="October 2011 Ridership data for 1 train all stations")
boxplot(ridership[,5],horizontal=T)

# Time series vs. boxplot #
# streamflow
annual.totalflow.neversink = apply(streamflow_yearly[,,1],2,sum,na.rm=T)
par(mfrow=c(1,2))
plot(1938:2001,(annual.totalflow.neversink),type="l",xlab="",ylab="Annual Total Flow cfs (Neversink)",font=2,main="Time Series")
abline(h=median((annual.totalflow.neversink)),lty=2,col="red")
abline(h=quantile(annual.totalflow.neversink, 0.25),lty=2,col="blue")
abline(h=quantile(annual.totalflow.neversink, 0.75),lty=2,col="blue")

boxplot((annual.totalflow.neversink),main="Boxplot")
abline(h=median((annual.totalflow.neversink)),lty=2,col="red")
abline(h=quantile(annual.totalflow.neversink, 0.25),lty=2,col="blue")
abline(h=quantile(annual.totalflow.neversink, 0.75),lty=2,col="blue")


# Seasonality Boxplot #
boxplot(t(log(streamflow_yearly[,,1])),font=2,xlab="Day of the Year (Oct 1 - Sep 30)",ylab="Log(Flow)")

# Boxplots for comparision - Ridership example#
xticks <- stations
boxplot(stations_data,xlab="1 Train Stops", ylab="October 2011 Ridership", outline = FALSE, axes = FALSE, col = F,font=2)
axis(1, at=1:nstations, xticks,las = 1, cex.axis = 0.8,font=2); 
axis(2,at=seq(0,max(stations_data),5000),las=1, cex.axis = 0.8,font=2)
box()

# Histogram #
hist(stations_data,xlab="1 Train Ridership",font=2,main="",cex.axis=2,font.lab=2)
hist(stations_data,breaks=50,xlab="1 Train Ridership",font=2,main="",cex.axis=2,font.lab=2)

par(mfrow=c(1,3))
hist(streamflow_yearly[,,1],xlab="Neversink Streamflow",font=2,main="Raw values",cex.axis=2,font.lab=2)
hist(log(streamflow_yearly[,,1]),xlab="Neversink Streamflow",font=2,main="Log transformed values",cex.axis=2,font.lab=2)
hist(log(streamflow_yearly[,,1]),breaks=60,xlab="Neversink Streamflow",font=2,main="Log transformed values",cex.axis=2,font.lab=2)

par(mfrow=c(1,3))
hist(log(streamflow[,1]))
hist(log(streamflow[,2]))
hist(log(streamflow[,3]))


# Summary Statistics #
##############################
hist(streamflow_yearly[,,1],xlim=c(0,2000),xlab="Neversink Streamflow",font=2,main="",cex.axis=2,font.lab=2)
abline(v=mean(streamflow_yearly[,,1],na.rm=T),col="red",lwd=2)
abline(v=median(streamflow_yearly[,,1],na.rm=T),col="blue",lwd=2)

hist(stations_data[,4],xlab="137 Street, Ridership",font=2,main="Comparing Mean and Median",cex.axis=2,font.lab=2)
abline(v=mean(stations_data[,4],na.rm=T),col="red",lwd=2)
abline(v=median(stations_data[,4],na.rm=T),col="blue",lwd=2)


hist(log(streamflow_yearly[,,1]),xlab="Log(Neversink Streamflow)",font=2,main="Comparing Mean and Median",cex.axis=2,font.lab=2)
abline(v=mean(log(streamflow_yearly[,,1]),na.rm=T),col="red",lwd=2)
abline(v=median(log(streamflow_yearly[,,1]),na.rm=T),col="blue",lwd=2)

hist(log(stations_data[,4]),xlab="log(137 Street, Ridership)",font=2,main="Comparing Mean and Median",cex.axis=2,font.lab=2)
abline(v=mean(log(stations_data[,4]),na.rm=T),col="red",lwd=2)
abline(v=median(log(stations_data[,4]),na.rm=T),col="blue",lwd=2)


hist(log(streamflow_yearly[,,1]),xlab="Log(Neversink Streamflow)",font=2,main="Comparing Mean and Median",cex.axis=2,font.lab=2,col="grey")
abline(v=quantile(log(streamflow_yearly[,,1]),0.25,na.rm=T),col="red",lwd=2)
abline(v=quantile(log(streamflow_yearly[,,1]),0.75,na.rm=T),col="red",lwd=2)

abline(v=mean(log(streamflow_yearly[,,1]),na.rm=T)+sd(log(streamflow_yearly[,,1]),na.rm=T),col="blue",lwd=2)
abline(v=mean(log(streamflow_yearly[,,1]),na.rm=T)-sd(log(streamflow_yearly[,,1]),na.rm=T),col="blue",lwd=2)

abline(v=mean(log(streamflow_yearly[,,1]),na.rm=T)+2*sd(log(streamflow_yearly[,,1]),na.rm=T),col="blue",lty=2)
abline(v=mean(log(streamflow_yearly[,,1]),na.rm=T)-2*sd(log(streamflow_yearly[,,1]),na.rm=T),col="blue",lty=2)


library(moments)
skewness(array(streamflow_yearly[,,1]),na.rm=T)
par(mfrow=c(1,2))
hist(streamflow_yearly[,,1],xlim=c(0,2000),xlab="Neversink Streamflow",font=2,main="",cex.axis=2,font.lab=2)
text(1000,15000,"Skew = 6.95",font=2,col="red")
skewness(array(log(streamflow_yearly[,,1])),na.rm=T)
hist(log(streamflow_yearly[,,1]),xlab="Log(Neversink Streamflow)",font=2,main="",cex.axis=2,font.lab=2)
text(7,3000,"Skew = 0.22",font=2,col="red")

skewness(array(stations_data[,4]),na.rm=T)
hist(stations_data[,4],xlab="137 Street, Ridership",font=2,main="",cex.axis=2,font.lab=2)
boxplot(stations_data[,4],horizontal = T,add=T,col="grey",axes=F)
text(12000,5,"Skew = -0.55",font=2,col="red")

## KDE and Locfit density plots ##

x = log(streamflow_yearly[,,1])
hist(x,xlab="Log(Neversink Streamflow)",font=2,main="",cex.axis=2,font.lab=2,prob=T)
lines(density(x,na.rm=T,bw=0.5))
lines(density(x,na.rm=T,bw=0.01,kernel="triangular"))

library(locfit)
fit <- locfit( ~ lp(array(x), nn=0.05, h=0.8))
plot(fit,get.data=TRUE)

x = stations_data[,4]
hist(x,xlab="137 Street, Ridership",font=2,main="",cex.axis=2,font.lab=2,prob=T)

lines(density(x,na.rm=T))
lines(density(x,na.rm=T,bw=200,kernel="triangular"))

fit <- locfit( ~ lp(x, nn=0.6, h=0.8))
plot(fit,get.data=TRUE)

## Quantile Quantile Plots ##

par(mfrow=c(1,2))
x = (streamflow_yearly[,,1])
qqnorm(x)
qqline(x)

x = log(streamflow_yearly[,,1])
qqnorm(x)
qqline(x)

# testing ridership data for Poisson
x = stations_data[,4]
n = length(x)    # total sample size
p = (1:n)/(n+1)  # Weibull plotting position formula 

plot(qpois(p,(mean(x))),sort(x),xlab="Theoretical Quantiles (Poisson distribution)",ylab="Sample Quantiles",font=2,family="serif") # qqplot 
abline(0,1)

## Bootstrap Confidence Intervals ##
par(mfrow=c(2,1))
x = apply(streamflow_yearly[,,1],2,mean,na.rm=T)
hist(log(x),xlab="Log(Annual Average Neversink Streamflow)",font=2,main="",cex.axis=2,font.lab=2,prob=T,xlim=c(4,6))
lines(density(log(x),na.rm=T))
abline(v=mean(log(x),na.rm=T),col="red",lwd=2)

N=100
interval_mean = matrix(NA,nrow=N,ncol=1)
for (i in 1:N)
{
  xboot = sample(x,replace=T)
  interval_mean[i,1] = mean(log(xboot),na.rm=T)
}
hist(interval_mean,prob=T,xlim=c(4,6),main="")

par(mfrow=c(2,1))
x = stations_data[,4]
hist((x),xlab="October 2011, Ridership 137St)",font=2,main="",cex.axis=2,font.lab=2,prob=T)
lines(density((x),na.rm=T))
abline(v=mean((x),na.rm=T),col="red",lwd=2)

N=100
interval_mean = matrix(NA,nrow=N,ncol=1)
for (i in 1:N)
{
  xboot = sample(x,replace=T)
  interval_mean[i,1] = mean((xboot),na.rm=T)
}
hist(interval_mean,prob=T,xlim=c(6000,18000),main="")


#################### Extreme Value Distributions ########################

# plot to show the annmax values. 
plot(array(streamflow_yearly[,1:5,1]),type="l",font=2,main="",cex.axis=2,font.lab=2,xlab="Days",ylab="Streamflow(cfs)")
abline(v=c(366,(2*366),(3*366),(4*366),(5*366)),col="red",lwd=2)

# Central Limit and Extreme Value Theorem Example #
x1 = apply((streamflow_yearly[,,1]),2,mean,na.rm=T)
y1 = apply((streamflow_yearly[,,1]),2,max,na.rm=T)

par(mfrow=c(3,1))
hist((streamflow_yearly[,,1]),xlim=c(0,7000),xlab="Neversink Streamflow",font=2,main="",cex.axis=2,font.lab=2,prob=T)
lines(locfit(~array(streamflow_yearly[,,1])))

hist((x1),xlim=c(0,7000),xlab="Annual Average of Neversink Streamflow",font=2,main="",cex.axis=2,font.lab=2,prob=T)
lines(locfit(~x1))

hist((y1),xlim=c(0,7000),xlab="Annual Maximum of Neversink Streamflow",font=2,main="",cex.axis=2,font.lab=2,prob=T)
lines(locfit(~(y1[-13])))

# Extreme value distribution simulation experiment
library(logspline)
N = 10000
x1mean = matrix(NA,nrow=N,ncol=1)
x2mean = matrix(NA,nrow=N,ncol=1)
x3mean = matrix(NA,nrow=N,ncol=1)

x1max = matrix(NA,nrow=N,ncol=1)
x2max = matrix(NA,nrow=N,ncol=1)
x3max = matrix(NA,nrow=N,ncol=1)

for (i in 1:N)
{
  x1mean[i,1] = mean(rnorm(100,10,2))
  x2mean[i,1] = mean(rexp(100,.25))
  x3mean[i,1] = mean(runif(100,8,10))
  
  x1max[i,1] = max(rnorm(100,10,2))
  x2max[i,1] = max(rexp(100,.25))
  x3max[i,1] = max(runif(100,8,10))
}
par(mfrow=c(2,3))
hist(x1mean,prob=T,xlab="Average of 100 values from N(10,2)",main="",ylab="f(x)",font=2,family="serif",font.lab=2,cex.lab=1.5)
plot(logspline(x1mean),add=T)

hist(x2mean,prob=T,xlab="Average of 100 values from Exp(1/4)",main="",ylab="f(x)",font=2,family="serif",font.lab=2,cex.lab=1.5)
plot(logspline(x2mean),add=T)

hist(x3mean,prob=T,xlab="Average of 100 values from U[8,10]",main="",ylab="f(x)",font=2,family="serif",font.lab=2,cex.lab=1.5)
plot(logspline(x3mean),add=T)


hist(x1max,prob=T,xlab="Maximum of 100 values from N(10,2)",main="",ylab="f(x)",font=2,family="serif",font.lab=2,cex.lab=1.5)
plot(logspline(x1max),add=T)

hist(x2max,prob=T,xlab="Maximum of 100 values from Exp(1/4)",main="",ylab="f(x)",font=2,family="serif",font.lab=2,cex.lab=1.5)
plot(logspline(x2max),add=T)

hist(x3max,prob=T,xlab="Maximum of 100 values from U[8,10]",main="",ylab="f(x)",font=2,family="serif",font.lab=2,cex.lab=1.5)
plot(logspline(x3max),add=T)


## Estimating tail probability ##
x1 = apply((streamflow_yearly[,,1]),2,max,na.rm=T)

hist((x1),xlim=c(0,7000),xlab="Annual Maximum of Neversink Streamflow",font=2,main="",cex.axis=2,font.lab=2,prob=T)
lines(density((x1[-13])))

abline(v=qnorm(0.99,mean(x1[-13]),sd(x1[-13])),col="red",lwd=2)
abline(v = quantile(x1[-13],0.99),col="blue",lwd=2)

plot(1938:2001,(x1),type="h",xlab="",ylab="Annual Maximum Flow cfs (Neversink)",font=2)
abline(h=quantile(x1[-13],0.90))
dum = x1>quantile(x1[-13],0.90)
t = 1938:2001
t[dum]

# Fit extreme Value Distribution #
library(car)
library(extRemes)
library(logspline)

# Generate Gumbel Random numbers
x = revd(10000,loc=0,scale=1,shape=0)
hist(x,prob=T,xlab="Random Variables from Gumbel (location = 0,scale = 1, shape =0)",main="Gumbel Distribution",ylab="f(x)",font=2,family="serif",font.lab=2,cex.lab=1.5)
plot(logspline(x),add=T)


#____________#
# Generate Frechet Random numbers

# Frechet distribution plot
y = revd(10000,loc=0,scale=1,shape=0.2)
hist(y,prob=T,ylim=c(0,0.4),xlab="Random Variables from Frechet (location = 0,scale = 1, shape =0.2)",main="Frechet Distribution",ylab="f(x)",font=2,family="serif",font.lab=2,cex.lab=1.5)
plot(logspline(y),add=T,col="red")
plot(logspline(x),add=T)


#____________#
# Generate Frechet Random numbers with large shape parameter

# Frechet distribution plot
y = revd(10000,loc=0,scale=1,shape=0.01)
hist(y,prob=T,ylim=c(0,0.4),xlab="Random Variables from Frechet (location = 0,scale = 1, shape =0.2)",main="Frechet Distribution",ylab="f(x)",font=2,family="serif",font.lab=2,cex.lab=1.5)
plot(logspline(y),add=T,col="red")
plot(logspline(x),add=T)

# Generate Weibull Random numbers with large shape parameter

## Weibull Distribution Plot 
z = revd(10000,loc=0,scale=1,shape=-0.6)
hist(z,prob=T,ylim=c(0,0.5),xlab="Random Variables from Weibull (location = 0,scale = 1, shape =-0.6)",main="Weibull Distribution",ylab="f(x)",font=2,family="serif",font.lab=2,cex.lab=1.5)
plot(logspline(z),add=T,col="red")

# fit gev
fit = fevd(x1[-13],type="GEV")
plot(fit)
design_flood=qevd(.99,loc=2459.61,1108.27,-0.137)

#################### Multivariate Data ########################

pairs(streamflow)
pairs(stations_data)

plot(stations_data[,4],stations_data[,2],xlim=c(4000,20000),ylim=c(4000,20000),font=2,xlab="137 St",ylab="116 St")
abline(0,1)
points(stations_data[,4],stations_data[,7],col="red",pch=21)


x = apply((streamflow_yearly[,,1]),2,max,na.rm=T)
y = apply((streamflow_yearly[,,2]),2,max,na.rm=T)

# taking only the complete record for both stations
x = x[14:64]
y = y[14:64] 

par(mfrow=c(2,2))
plot(x,y,xlab="Neversink Flood (cfs)",ylab="Pepacton Flood (cfs)",font=2.)
abline(lm(y~x))
lines(lowess(x,y),col="red")
p = cor(x,y,method="spearman")
s = cor(x,y,method="pearson")
k = cor(x,y,method="kendall")

# Test for significance #
N = 1000
cor.test = matrix(NA,nrow=N,ncol=3)

for(i in 1:N)
{
  xnew = sample(x,replace=T)
  ynew = sample(y,replace=T)
  cor.test[i,1]=cor(xnew,ynew,method="pearson")
  cor.test[i,2]=cor(xnew,ynew,method="spearman")
  cor.test[i,3]=cor(xnew,ynew,method="kendall")
}

k1 = qnorm(0.95,mean(cor.test[,1]),sd(cor.test[,1]))
k2 = qnorm(0.95,mean(cor.test[,2]),sd(cor.test[,2]))
k3 = qnorm(0.95,mean(cor.test[,3]),sd(cor.test[,3]))

hist(cor.test[,1],xlim=c(-1,1))
abline(v=k1,col="blue")
abline(v=p,col="red")

hist(cor.test[,2],xlim=c(-1,1))
abline(v=k2,col="blue")
abline(v=s,col="red")

hist(cor.test[,3],xlim=c(-1,1))
abline(v=k3,col="blue")
abline(v=k,col="red")


# Mutual Information #

library(entropy)

nbins=round(length(x)^(1/3))
grids = discretize2d(x,y,numBins=nbins,numBins2=nbins)
mi = mi.empirical(grids)

# Test for significance #
N = 1000
mi.test = matrix(NA,nrow=N,ncol=1)

for(i in 1:N)
{
  xnew = sample(x,replace=T)
  ynew = sample(y,replace=T)
  
  nbins=round(length(xnew)^(1/3))
  grids = discretize2d(xnew,ynew,numBins1=nbins,numBins2=nbins)
  mi.test[i,1] = mi.empirical(grids)
}

k = quantile(mi.test,.95)
hist(mi.test,xlim=c(0,1))
abline(v=k,col="blue")
abline(v=mi,col="red")


## Tail Dependence Coefficient ##
  tdc = function(x1,x2,k)
  {
    x = cbind(x1,x2)
    N = nrow(x)
    
    r <- x                                # initiating the r matrix
    for (i in 1:ncol(x))
    {
      r[,i] <- rank(x[,i],ties.method = "first")          # stores the rank of the data for each column
    }
    lambda.u = sum(rowSums(r>=(k))==2)/(N-k+1)
    lambda.l = sum(rowSums(r<=(N-k))==2)/(N-k+1)
    
    out = list(lambda.u = lambda.u )
  }

plot(x,y,xlab="Neversink Flood (cfs)",ylab="Pepacton Flood (cfs)",font=2.)
abline(v=quantile(x,0.9))  
abline(h=quantile(y,0.9))  
tail = tdc(x,y,45)

## Lagged Scatter  -- Auto Correlation ##

par(mfrow=c(2,1))
plot(log(streamflow[1:(nrow(streamflow)-1),1]),log(streamflow[2:(nrow(streamflow)),1]),xlab="Neversink Daily Flow (t)",ylab="Neversink Daily Flow (t+1)",font=2,font.lab=2)
acf(na.omit(streamflow[,1]),main="",font=2,font.lab=2,xlab="Lag (days)")
acf(na.omit(stations_data[,4]),main="",font=2,font.lab=2,xlab="Lag (days)")

# partial correlations #
library(ppcor)
par(mfrow=c(1,2))
plot(stations_data[,2],stations_data[,4],xlab="116 Station", ylab="137 Station",font=2,font.lab=2)
text(10000,14000,round(cor(stations_data[,2],stations_data[,4]),4),font=2)

plot(stations_data[,3],stations_data[,4],xlab="125 Station", ylab="137 Station",font=2,font.lab=2)
text(6000,14000,round(cor(stations_data[,3],stations_data[,4]),4),font=2)

pcor.test(stations_data[,2],stations_data[,4],stations_data[,3]) # correlation between 137 and 125 given 116
pcor.test(stations_data[,3],stations_data[,4],stations_data[,2]) # correlation between 137 and 116 given 125


x = apply(streamflow_yearly[,,1],2,mean,na.rm=T)
y = apply(streamflow_yearly[,,2],2,mean,na.rm=T)
z = apply(streamflow_yearly[,,3],2,mean,na.rm=T)

# taking common records #
x = x[14:64]
y = y[14:64]
z = z[14:64]

par(mfrow=c(1,2))
plot(x,y,xlab="Neversink", ylab="Pepacton",font=2,font.lab=2)
text(100,600,round(cor(x,y),4),font=2)

plot(z,y,xlab="Canonsville", ylab="Pepacton",font=2,font.lab=2)
text(200,600,round(cor(z,y),4),font=2)

## Copulas ##
library(copula)
library(mvtnorm)
library(scatterplot3d)

# Normal Copula
normalcop = ellipCopula(family = "normal", dim = 3, dispstr = "un", param = c(1,0.5,0.5))
randomu = (rCopula(20, normalcop))
plot(randomu)
scatterplot3d(randomu)

# backtransform to simulate dependent data #

# all xs have normal distribution
qnorm(randomu)
scatterplot3d(qnorm(randomu))

# xs have different distributions
x1 = qexp(randomu[,1])
x2 = qevd(randomu[,2])
x3 = qnorm(randomu[,3])
scatterplot3d(cbind(x1,x2,x3))


# t Copula
tcop = ellipCopula(family = "t", dim = 3, dispstr = "un", param = c(1,0.5,0.5))
randomu = (rCopula(20, tcop))
plot(randomu)
scatterplot3d(randomu)

# backtransform to simulate dependent data #

# all xs have normal distribution
qnorm(randomu)
scatterplot3d(qnorm(randomu))

# xs have different distributions
x1 = qexp(randomu[,1])
x2 = qevd(randomu[,2])
x3 = qnorm(randomu[,3])
scatterplot3d(cbind(x1,x2,x3))


# Clayton Copula
clatoncop = archmCopula(family = "clayton", dim = 3,param = 10)
randomu = (rCopula(100, clatoncop))
plot(randomu)
scatterplot3d(randomu)
pairs(randomu)

x1 = qexp(randomu[,1])
x2 = qevd(randomu[,2])
x3 = qnorm(randomu[,3])
scatterplot3d(cbind(x1,x2,x3))
pairs(cbind(x1,x2,x3))

# Frank Copula
frankcop = archmCopula(family = "frank", dim = 3,param = 2)
randomu = (rCopula(100, frankcop))
plot(randomu)
scatterplot3d(randomu)
pairs(randomu)

x1 = qexp(randomu[,1])
x2 = qevd(randomu[,2])
x3 = qnorm(randomu[,3])
scatterplot3d(cbind(x1,x2,x3))
pairs(cbind(x1,x2,x3))

# Gumbel Copula
gumbelcop = archmCopula(family = "gumbel", dim = 3,param = 2)
randomu = (rCopula(100, gumbelcop))
plot(randomu)
scatterplot3d(randomu)
pairs(randomu)

x1 = qexp(randomu[,1])
x2 = qevd(randomu[,2])
x3 = qnorm(randomu[,3])
scatterplot3d(cbind(x1,x2,x3))
pairs(cbind(x1,x2,x3))

# Joe Copula
joecop = archmCopula(family = "joe", dim = 3,param = 2)
randomu = (rCopula(100, joecop))
plot(randomu)
scatterplot3d(randomu)
pairs(randomu)

x1 = qexp(randomu[,1])
x2 = qevd(randomu[,2])
x3 = qnorm(randomu[,3])
scatterplot3d(cbind(x1,x2,x3))
pairs(cbind(x1,x2,x3))


## Non-parametric Copula ##

# simplified example #
x = apply((streamflow_yearly[,,1]),2,sum,na.rm=T)
y = apply((streamflow_yearly[,,2]),2,sum,na.rm=T)
z = apply((streamflow_yearly[,,3]),2,sum,na.rm=T)

x = x[14:64] # taking complete data 
y = y[14:64] # taking complete data 
z = z[14:64] # taking complete data 

pairs(cbind(x,y,z))

# reduced pair to explain #
x1 = x[11:20]
y1 = y[11:20]
z1 = z[11:20]

# Step 1: Get the ranks of the data -- this rank matrix is our emperical copula #
rx = rank(x1)
ry = rank(y1)
rz = rank(z1)

# visualize ranks -- emperical copula function #
pairs(cbind(rx,ry,rz))

# Step 2: Fit marginal distributions -- We will use bootstrap sample as a marginal #
x1fit = sample(x1,replace=T)
y1fit = sample(y1,replace=T)
z1fit = sample(z1,replace=T)

# plot original data #
pairs(cbind(x1,y1,z1))

# plot new fits #
pairs(cbind(x1fit,y1fit,z1fit),col="red")

# Step 3: Rearrange to align with the Copula #
x1sort = sort(x1fit)
y1sort = sort(y1fit)
z1sort = sort(z1fit)

x1fitcop = x1sort[rx]
y1fitcop = y1sort[ry]
z1fitcop = z1sort[rz]
pairs(cbind(x1fitcop,y1fitcop,z1fitcop))
pairs(cbind(x1,y1,z1))

# Actual function with copula uncertainty

## SPATIAL SIMULATOR ##
#______________________________________________________________________________#

fuzzy.spatsim <- function(x, xnew, nboot=NULL, lb=NULL, ub=NULL)
{
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------#
  # x     -   the historical data matrix is x (nr = time series length, nc = number of stations or variables in multivariate case)
  
  # xnew  -   is the data matrix on which pdfs have to be fit and spatial structure estimated
  #           nr = time series for each ensemble member, nc = number of stations)
  #           this could be x itself if we just want samples generated from historical data or it could be from a seasonal forecast or climate change run 
  #           which lacks the spatial correlation structure
  #           if length(xnew) < length(x), the code will generate ynew with length of x and discard the remaining values
  
  # lb    -   is lower bound for data distribution
  # ub    -   is upper bound for data distribution
  
  # nboot -   is total size of desired stochastic sample (if number of simulations we desire is 100, nboot = nr*100)
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------#
  nr <- nrow(x)       # number of rows (time length, ensemble length)
  nc <- ncol(x)       # number of columns (stations, variables)
  
  
  if (is.null(nboot)) {nboot <- nr} else {nboot <- nboot}
  
  
  #get ranks of observed data to simplify bootstrap fix later; put in matrix r
  r <- x                                   # initiating the r matrix
  for (i in 1:nc) { r[,i] <- rank(x[,i],ties.method="first") } # stores the rank of the data for each column or stations
  
  
  #candidate data -- fit pdf to each series (this gives a smoothing of the empirical cdf and a tail beyond the data -- using logspline here) ? but one could put an appropriate distbn here
  library (logspline)
  
  nsim <- ceiling(nboot/nr)         # nsim is number of sims of length nr to get desired nboot size
  
  nb <- nsim*nr                     # nb is the total number of samples to be generated such that nboot is achieved
  
  #if you expect data to be lower bounded or upper bounded set bounds (change below or pass in fn call)
  xx <- as.vector(xnew)
  if(is.null(lb)) { lb <- min(xx)-4*sd(xx) } else {lb <- lb}      #lb=0
  if(is.null(ub)) { ub <- max(xx)+4*sd(xx) } else {ub <- ub}      #ub=1e9
  
  xfit <- array(NA,c(nb,nc))    # initializing the xfit matrix
  pp.rank <- array(NA,c(nb,nc)) # fuzzy rank matrix
  ynew <- array(NA,c(nb,nc))    # initializing the ynew matrix
  
  for (j in 1:nc)
  { 	
    for (i in 1:nsim){
      xr <- sample(xnew[,j],replace=TRUE) # this procedure addresses uncertainty in fitting the logspline pdf by also taking a bootstrap sample for xnew
      
      iqr <- quantile(xr,0.75)-quantile(xr,0.25) ; amt <- iqr/(10*length(xr))
      
      xr <- jitter(xr,amount=amt);    
      xr[xr<lb]=lb;xr[xr>ub]=ub
      
      xr.nonzeros = xr[which(xr!=0)]
      nzeros = ((length(xr)-length(xr.nonzeros))/length(xr))
      
      rand = runif(length(xr),0,1)
      
      nrand.nonzeros = length(which(rand>nzeros))
      nrand.zeros = length(xr) - nrand.nonzeros
      
      fdist <- logspline(xr.nonzeros,lb=lb,ub=ub)
      xfit.nonzeros = rlogspline((nrand.nonzeros),fdist)
      
      j1 <- (i-1)*nr+1; j2 <- i*nr
      xfit[j1:j2,j] <- c(matrix(0,nr=(nrand.zeros),nc=1),xfit.nonzeros)
      
      pp.rank[j1:j2,j] <- fuzzy_rank(x[,j])
    }        
  }
  
  # draw a bootstrap sample that preserves the spatial structure, i.e.draw a full year at a time across all stations
  indicator <- 1:nr
  
  for (i in 1:nsim)
  {
    j1 <- (i-1)*nr+1; j2 <- i*nr
    xt <- xfit[j1:j2,]
    indicator.boot <- sample(indicator, replace=TRUE)   # randomizing the years by drawing samples
    rank.dum =  pp.rank[j1:j2,]
    r.boot <- rank.dum[indicator.boot,]                        # r.boot is the bootstrap sample that keeps all locations together and does a bootstrap sample of years
    for (j in 1:nc)
    {
      xt[,j] <- sort(xt[,j])
      ynew[j1:j2,j] <- xt[r.boot[,j],j]
    }
  }
  
  # In the above we fit a logspline density to the new data (each stations) for each ensemble, then we draw a random sample from the fit and then sorted it so ranks are clean
  # we then move the ynew values to the right spot
  
  # done
  yn <- ynew[1:nboot,]
  
  out = list(yn = yn, xfit = xfit)
}


fuzzy_rank = function(xr)
{
  r = xr
  n = length(xr)
  
  for (i in 1:n)
  {
    v = xr[-(which(xr[i]==xr))]     # leave 1 cross validation : leaving the current value from the sample
    sort_v = sort(v)   # sorting the remaining (n-1) values
    locate = which(sort_v>xr[i]) # locate left out value in the sorted matrix
    v_j = locate[1]-1
    
    if (length(locate)==0){r[i] = runif(1,((n-0.5)/(n+1)),1)} else {
      
      if(v_j==0) { r[i] = runif(1,0,(1.5/(n+1))) } else {
        r[i] = runif(1,((v_j-0.5)/(n+1)),((v_j+0.5)/(n+1))) }
    }
    
  }
  return(rank(r))	
}

## Tail Dependence Coefficient ##
tdc = function(x1,x2,k)
{
  x = cbind(x1,x2)
  N = nrow(x)
  
  r <- x                                # initiating the r matrix
  for (i in 1:ncol(x))
  {
    r[,i] <- rank(x[,i],ties.method = "first")          # stores the rank of the data for each column
  }
  lambda.u = sum(rowSums(r>=(k))==2)/(N-k+1)
  lambda.l = sum(rowSums(r<=(N-k))==2)/(N-k+1)
  
  out = list(lambda.u = lambda.u )
}

# apply on flow data #
fullmatrix = cbind(x,y,z)
simulated_matrix = fuzzy.spatsim(fullmatrix, fullmatrix, nboot=(100*nrow(fullmatrix)), lb=0, ub=NULL)

pairs(fullmatrix)
pairs(simulated_matrix$yn)

# colser look# 
plot(simulated_matrix$yn[,1],simulated_matrix$yn[,2])
points(fullmatrix[,1],fullmatrix[,2],col="red",pch=16,xlab="Neversink Total Flow (cfs)",ylab="Pepacton Total Flow (cfs)",font=2)


## Apply on ridership data ##

simualted_ridership = fuzzy.spatsim(stations_data, stations_data, nboot=(100*nrow(stations_data)), lb=0, ub=NULL)

# colser look# 
plot(simualted_ridership$yn[,4],simualted_ridership$yn[,2],xlim=c(4000,25000),ylim=c(4000,25000),font=2,xlab="137 St",ylab="116 St")
abline(0,1)
points(stations_data[,4],stations_data[,2],col="red",pch=16,xlab="Neversink Total Flow (cfs)",ylab="Pepacton Total Flow (cfs)",font=2)




