
############## DIMENSION REDUCTION ##############

# Reading the data files #
##########################
ridership = read.table("ridership_data.txt",header=T)
stocks = read.csv("stocks_close.csv",header=T)
globalfloods = read.table("flood_data.txt",header=T)

# Visual Interpretation of the Covariance Matrix 

## Copulas ##
library(copula)
library(mvtnorm)
library(scatterplot3d)
library(extRemes)

# Normal Copula
normalcop = ellipCopula(family = "normal", dim = 2, dispstr = "un", param = c(0.9))
randomu = (rCopula(100, normalcop))
plot(randomu)

# backtransform to simulate dependent data #
# all xs have normal distribution with mean 10 and variance of 1
x1 = qnorm(randomu[,1],0,1)
x2 = qnorm(randomu[,2],0,3)
x = cbind(x1,x2)

plot(x,xlab="x1",ylab="x2",font=2,font.lab=2,xlim=c(-6,6))
cov(x)

# Correlation Plot #
library(corrplot)
M <- cor(x)
corrplot(M, method="square")

######################### PCA on flood data #########################################

x = globalfloods[,2:29] # select all countries -- not taking the first column (years)
M <- cor(x)
corrplot(M, method="square")

# PCA #
pc_floods = princomp(x,cor=TRUE)

summary(pc_floods)

plot(pc_floods)

pc_floods$loadings

y = cor(x,pc_floods$scores)
corrplot(y, method="circle")

# first pc
plot(globalfloods[,1],pc_floods$scores[,1],type="l") # time series of the first PC
dum = which(y[,1]<(-0.7)) # selecting countries with -0.5 or more corr 
plot.ts((x[,dum]-colMeans(x[,dum]))/apply(x[,dum],2,sd)) # time series of the first country
pc_floods$loadings[dum]

# second pc
plot(globalfloods[,1],pc_floods$scores[,2],type="l") # time series of the second PC
dum = which((y[,2]<(-0.5)) | (y[,2]>0.5) ) # selecting countries with -0.5 or more corr 
plot.ts((x[,dum]-colMeans(x[,dum]))/apply(x[,dum],2,sd)) # time series of the first country

# third pc
plot(globalfloods[,1],pc_floods$scores[,3],type="l") # time series of the third PC
dum = which((y[,3]<(-0.45)) | (y[,3]>0.45) ) # selecting countries with -0.5 or more corr 
plot.ts((x[,dum]-colMeans(x[,dum]))/apply(x[,dum],2,sd)) # time series of the first country

plot(globalfloods[,1],pc_floods$scores[,4],type="l") # time series of the fourth PC
dum = which((y[,4]<(-0.45)) | (y[,4]>0.45) ) # selecting countries with -0.5 or more corr 
plot.ts((x[,dum]-colMeans(x[,dum]))/apply(x[,dum],2,sd)) # time series of the first country


###################### PCA on ridership data ######################
stations = unique(ridership[,1])
nstations = length(stations)
days = 30
stations_data = array(ridership[,5],c(days,nstations))

x = stations_data 
M <- cor(x)
corrplot(M, method="square")

# PCA #
pc_ride = princomp(x,cor=TRUE)

summary(pc_ride)

plot(pc_ride)

pc_ride$loadings

y = cor(x,pc_ride$scores)
corrplot(y, method="circle")

barplot(pc_ride$loadings[,1])

pairs(x[,9:13])

#################### PCA on all ridership data #################
ridership = read.csv("ridership_data_all.csv",header=T)

stations = unique(ridership[,1])
nstations = length(stations)
days = 30
stations_data = array(ridership[,4],c(days,nstations))

x = stations_data 
M <- cor(x)
corrplot(M, method="square")

# PCA #
pc_ride = princomp(x,cor=TRUE)

pc_ride=princomp(covmat=M)

pc_ride$scores=x%*%pc_ride$loadings

summary(pc_ride)

plot(pc_ride)

pc_ride$loadings

y = cor(x,pc_ride$scores)
corrplot(y, method="circle")

par(mfrow=c(2,1))
plot(stations,pc_ride$loadings[,1])

par(mfrow=c(3,1))
plot(stations,y[,1],ylim=c(-1,1))
abline(h=c(-.4,.4))
plot(stations,y[,2],ylim=c(-1,1))
abline(h=c(-.4,.4))
plot(stations,y[,3],ylim=c(-1,1))
abline(h=c(-.4,.4))

########################## PCA on stocks data #######################
x = stocks[4:13]
M <- cor(x)
corrplot(M, method="square")

plot.ts(x[1:3])

# PCA #
pc_stock = princomp(x,cor=TRUE)

summary(pc_stock)

plot(pc_stock)

pc_stock$loadings

y = cor(x,pc_stock$scores)
corrplot(y, method="circle")

# first pc
plot.ts(pc_stock$scores[,1],type="l") # time series of the first PC
dum = which(y[,1]<(-0.8)) # selecting countries with -0.5 or more corr 
plot.ts(pc_stock$scores[,1],type="l") # time series of the first PC
plot.ts(x[,dum])
pc_stock$loadings[dum]

# second pc
plot.ts(pc_stock$scores[,2],type="l") # time series of the first PC
dum = which((y[,2]<(-0.4)) | (y[,2]>0.4)) # selecting countries with -0.5 or more corr 
plot.ts(x[,dum])
pc_stock$loadings[dum]


######################## PCA on copula dependent data ###################

clatoncop = archmCopula(family = "clayton", dim = 4,param = 2)
randomu = (rCopula(100, clatoncop))
plot(randomu)
scatterplot3d(randomu)
pairs(randomu)

x1 = qexp(randomu[,1])
x2 = qevd(randomu[,2])
x3 = qnorm(randomu[,3])
x4 = qnorm(randomu[,4])

pairs(cbind(x1,x2,x3,x4))


x = cbind(x1,x2,x3,x4) 
M <- cor(x)
corrplot(M, method="square")

# PCA #
pc_clayton = princomp(x,cor=TRUE)

summary(pc_clayton)

plot(pc_clayton)

pc_clayton$loadings

y = cor(x,pc_clayton$scores)
corrplot(y, method="circle")


############################# PCA on sine data #########################
x1 = rnorm(100,0,2)
x2 = sin(x1) + rnorm(100,0,.1)
x3 = cos(x1) 

pairs(cbind(x1,x2,x3))
scatterplot3d(cbind(x1,x2,x3))

x = cbind(x1,x2,x3) 
M <- cor(x)
corrplot(M, method="square")

# PCA #
pc_sine = princomp(x,cor=TRUE)

summary(pc_sine)

plot(pc_sine)

pc_sine$loadings

y = cor(x,pc_sine$scores)
corrplot(y, method="circle")
pairs(x)

##############################################################################################################


################################### Robust PCA #####################
library(rpca)
library(Matrix)


### simple 2 d example from clayton copula

clatoncop = archmCopula(family = "clayton", dim = 2,param = 10)
randomu = (rCopula(100, clatoncop))
plot(randomu)
pairs(randomu)

x1 = qnorm(randomu[,1])
x2 = qevd(randomu[,2])

x = cbind(x1,x2)

plot(x)

M <- cor(x)
corrplot(M, method="square")

# PCA #
pc_clayton = princomp(x,cor=TRUE)

summary(pc_clayton)

plot(pc_clayton)

pc_clayton$loadings

y = cor(x,pc_clayton$scores)
corrplot(y, method="circle")

plot(x1,x2)
points(sort(x1),sort(pc_clayton$scores[,1]),col="red",type="l")



# using rpca #
rankMatrix(x) # check the rank of the matrix -- this dataset has full rank 2
xcent = sweep(x,2,colMeans(x)) # center the data 

res <- rpca(xcent) # apply rpca algorithm

summary(res) # we will get L (low rank matrix) + S (sparse matrix) and the svd of L

# check L, S and L + S

res$L 
rankMatrix(res$L) # the L has a rank 2 -- this is the background.

rankMatrix(res$S) # S has full rank -- this is the noise or outliers 

recoverx = res$L + res$S

# Look at the svd of L 

# low rank matrix L has a rank of 2.. svd will decompose this into 2 eigen vectors and 2 eigen values.
res$L.svd

# plot the pcs 
rpc = res$L.svd$u %*% (res$L.svd$d)

plot(x1,x2)
points(sort(x1),sort(rpc[,1]),type="l")

plot(xcent)
points(res$L,col="red")
points(res$S,col="blue")


## Images ##
myImagePlot(x)

myImagePlot(res$L )
myImagePlot(res$S )




#####  using rpca library example #####
data(iris)

M <- as.matrix(iris[,1:4])
pairs(M)

rankMatrix(M) # check the rank of the matrix -- this dataset has full rank 4

Mcent <- sweep(M,2,colMeans(M)) # center the data 


### apply standard PCA first ###
pc_petal = princomp(Mcent,cor=TRUE)

summary(pc_petal)

plot(pc_petal)

pc_petal$loadings

y = cor(Mcent,pc_petal$scores)
corrplot(y, method="circle")
pairs(Mcent)


res <- rpca(Mcent) # apply rpca algorithm

summary(res) # we will get L (low rank matrix) + S (sparse matrix) and the svd of L

# check L, S and L + S

res$L 
rankMatrix(res$L) # the L has a rank 2 -- this is the background.

rankMatrix(res$S) # S has full rank -- this is the noise or outliers 

recoverM = res$L + res$S

# Look at the svd of L 

# low rank matrix L has a rank of 2.. svd will decompose this into 2 eigen vectors and 2 eigen values.
res$L.svd

# plot the pcs 
rpc = res$L.svd$u %*% diag(res$L.svd$d)

plot(jitter(rpc[,1:2],amount=.001),col=iris[,5])

## Compare with classical principal components
pc <- prcomp(M,center=TRUE)
plot(pc$x[,1:2],col=iris[,5])
points(rpc[,1:2],col=iris[,5],pch="+")

## Plot measurements against measurements corrected by sparse components
par(mfcol=c(2,2))
for(i in 1:4) {
  plot(M[,i],M[,i]-res$S[,i],col=iris[,5],xlab=colnames(M)[i])
}

## Images ##
myImagePlot(M)

myImagePlot(res$L)
myImagePlot(res$S)



### simple example to explain low rank matrix ###
a1 = c(1,2,3)
a2 = c(2,4,6)
a3 = c(3,8,7)
a4 = c(5,12,13)
a = cbind(a1,a2,a3,a4)

y = rpca(a)





## Apply RPCA on Flood Data ##

M = as.matrix(globalfloods[,2:29]) # select all countries -- not taking the first column (years)
myImagePlot(M)

rankMatrix(M) # check the rank of the matrix -- this dataset has full rank 28

Mcent <- sweep(M,2,colMeans(M)) # center the data 

res <- rpca(Mcent) # apply rpca algorithm

summary(res) # we will get L (low rank matrix) + S (sparse matrix) and the svd of L

# check L, S and L + S

res$L 
rankMatrix(res$L) # the L has a rank 2 -- this is the background.

rankMatrix(res$S) # S has full rank -- this is the noise or outliers 

recoverM = res$L + res$S

myImagePlot(res$L)

myImagePlot(res$S)

# Look at the svd of L 

# low rank matrix L has a rank of 2.. svd will decompose this into 2 eigen vectors and 2 eigen values.
res$L.svd

# plot the pcs 
rpc = res$L.svd$u %*% diag(res$L.svd$d)
plot(jitter(rpc[,1:2],amount=.001))

y = cor(Mcent,rpc)
corrplot(y, method="circle")

# variance plot #
percentvar = res$L.svd$d/sum(res$L.svd$u)
plot(percentvar)




########## Apply on Transpose of Flood Data (check for years)

M = as.matrix(globalfloods[,2:29]) # select all countries -- not taking the first column (years)
M = t(M)

myImagePlot(M)

rankMatrix(M) # check the rank of the matrix -- this dataset has full rank 28

Mcent <- sweep(M,2,colMeans(M)) # center the data 

res <- rpca(Mcent) # apply rpca algorithm

summary(res) # we will get L (low rank matrix) + S (sparse matrix) and the svd of L

# check L, S and L + S

res$L 
rankMatrix(res$L) # the L has a rank 2 -- this is the background.

rankMatrix(res$S) # S has full rank -- this is the noise or outliers 

recoverM = res$L + res$S

myImagePlot(res$L)

myImagePlot(res$S)

# Look at the svd of L 

# low rank matrix L has a rank of 2.. svd will decompose this into 2 eigen vectors and 2 eigen values.
res$L.svd

# plot the pcs 
rpc = res$L.svd$u %*% diag(res$L.svd$d)
plot(jitter(rpc[,1:2],amount=.001))

y = cor(Mcent,rpc)
corrplot(y, method="circle")

# variance plot #
percentvar = res$L.svd$d/sum(res$L.svd$u)
plot(percentvar)




##### Apply RPCA on Ridership Data ##

ridership = read.csv("ridership_data_all.csv",header=T)

stations = unique(ridership[,1])
nstations = length(stations)
days = 30
stations_data = array(ridership[,4],c(days,nstations))

x = log(stations_data) 

myImagePlot(x)

M = x # select all countries -- not taking the first column (years)

rankMatrix(M) # check the rank of the matrix -- this dataset has full rank 28

Mcent <- sweep(M,2,colMeans(M)) # center the data 

res <- rpca(Mcent) # apply rpca algorithm

summary(res) # we will get L (low rank matrix) + S (sparse matrix) and the svd of L

# check L, S and L + S

res$L 
rankMatrix(res$L) # the L has a rank 2 -- this is the background.

rankMatrix(res$S) # S has full rank -- this is the noise or outliers 

recoverM = res$L + res$S

myImagePlot(res$L)

myImagePlot(res$S)

myImagePlot(res$L.svd$vt)
################################### MVU #####################










################################### Frequency Domain #####################

library(multitaper)

# 1.Spectral Analysis on Sine Waves

# define modes of variability
f1 = (72)*pi/180    # frequency -- 360/72 = 5 year cycle
f2 = (36)*pi/180    # frequency -- 360/36 = 10 year cycle
f3 = (18)*pi/180    # frequency -- 360/18 = 20 year cycle
f4 = (12)*pi/180    # frequency -- 360/12 = 30 year cycle

###################################################
# define coefficients - this is the amplitute
a = 0                    
b1 = 2                 
b2 = 1                 
b3 = 0.5               
b4 = 1                 

#initialize time 
t = 1:120

x1 = b1*sin(f1*t)
x2 = b2*sin(f2*t)
x3 = b3*sin(f3*t)
x4 = b4*sin(f4*t)
error = rnorm(length(t),0,2)

x = x1 + x2 + x3 + x4 + error

plot(t,x,type="l",col="grey")
lines(t,x1,col="red",lwd=2)
lines(t,x2,col="blue",lwd=2)
lines(t,x3,col="grey",lwd=2)
lines(t,x4,col="brown",lwd=2)

par(mfrow=c(2,1))
y = spec.pgram(x,plot=F)
plot(y,ci=0.95,log="yes",type="l",ci.col="blue",ci.lty=3)

y = spec.ar(x,plot=F)
plot(y,ci=0.95,log="yes",type="l",ci.col="blue",ci.lty=3)

# 2.Spectral Analysis on Triangle Waves

x = c(1,2,3,2,1,0,0,0,0,0,0,0) # create a triangle wave for one year 
x =rep(x,10) # repeat for 10 years

plot.ts(x)

par(mfrow=c(2,1))
y = spec.pgram(x,plot=F)
plot(y,ci=0.95,log="yes",type="l",ci.col="blue",ci.lty=3)

y = spec.ar(x,plot=F)
plot(y,ci=0.95,log="yes",type="l",ci.col="blue",ci.lty=3)


# 3.Spectral Analysis on Ridership Data

## Read Ridership Data ##
ridership = read.table("ridership_data.txt",header=T)
ridership = ridership[1:30,]
ridership = rbind(ridership[1,], ridership[12,], ridership[23,], ridership[25:30,],ridership[2:11,],ridership[13:22,],ridership[24,])

x = ridership[,5]

# periodogram of ridership #
par(mfrow=c(2,1))
y = spec.pgram(x,plot=F)
plot(y,ci=0.95,log="yes",type="l",ci.col="blue",ci.lty=3)

y = spec.ar(x,plot=F)
plot(y,ci=0.95,log="yes",type="l",ci.col="blue",ci.lty=3)


## hourly ridership data ##
rider_hour = read.csv("hourly_ridership_4stations.csv",header=T)

plot.ts(rider_hour[,4])

x = rider_hour[,2]

par(mfrow=c(2,1))
y = spec.pgram(x,plot=F)
plot(y,ci=0.95,log="yes",type="l",ci.col="blue",ci.lty=3)

y = spec.ar(x,plot=F)
plot(y,ci=0.95,log="no",type="l",ci.col="blue",ci.lty=3)


# 4.Spectral Analysis on Flood Data

x = globalfloods[,2]
par(mfrow=c(2,1))
y = spec.pgram(x,plot=F)
plot(y,ci=0.95,log="yes",type="l",ci.col="blue",ci.lty=3)

y = spec.ar(x,plot=F)
plot(y,ci=0.95,log="no",type="l",ci.col="blue",ci.lty=3)


# 5.Spectral Analysis on Streamflow Data

#### streamflow ###
streamflow = read.table("streamflow_delaware.txt",header=T)

# Rearrange streamflow data #
ndays = 366
nyrs = nrow(streamflow)/ndays
nsites = ncol(streamflow)

streamflow_yearly = array(NA,c(ndays,nyrs,nsites))
for (i in 1:nsites)
{
  streamflow_yearly[,,i] = array(streamflow[,i],c(ndays,nyrs)) 
}

x = apply(streamflow_yearly[,,3],2,sum,na.rm=T)

par(mfrow=c(2,1))
y = spec.pgram(x,plot=F)
plot(y,ci=0.95,log="yes",type="l",ci.col="blue",ci.lty=3)

y = spec.ar(x,plot=F)
plot(y,ci=0.95,log="no",type="l",ci.col="blue",ci.lty=3)

# 6.Spectral Analysis on Climate Data

# Climate data #
enso = read.table("ENSO.txt",header=F)
amo = read.table("AMO.txt",header=F)
nao = read.table("NAO.txt",header=F)
pdo = read.table("PDO.txt",header=F)


x = as.matrix(enso)

par(mfrow=c(2,1))
y = spec.pgram(x,plot=F)
plot(y,ci=0.95,log="no",type="l",ci.col="blue",ci.lty=3)

y = spec.ar(x,plot=F)
plot(y,ci=0.95,log="no",type="l",ci.col="blue",ci.lty=3)


x = as.matrix(amo)

par(mfrow=c(2,1))
y = spec.pgram(x,plot=F)
plot(y,ci=0.95,log="no",type="l",ci.col="blue",ci.lty=3)

y = spec.ar(x,plot=F)
plot(y,ci=0.95,log="no",type="l",ci.col="blue",ci.lty=3)



x = as.matrix(nao)

par(mfrow=c(2,1))
y = spec.pgram(x,plot=F)
plot(y,ci=0.95,log="yes",type="l",ci.col="blue",ci.lty=3)

y = spec.ar(x,plot=F)
plot(y,ci=0.95,log="no",type="l",ci.col="blue",ci.lty=3)


x = as.matrix(pdo)

par(mfrow=c(2,1))
y = spec.pgram(x,plot=F)
plot(y,ci=0.95,log="no",type="l",ci.col="blue",ci.lty=3)

y = spec.ar(x,plot=F)
plot(y,ci=0.95,log="no",type="l",ci.col="blue",ci.lty=3)

y = 10 + 2*enso + amo + nao + pdo + rnorm(length(enso),0,1)

par(mfrow=c(2,1))
y1 = spec.pgram(y,plot=F)
plot(y1,ci=0.95,log="yes",type="l",ci.col="blue",ci.lty=3)

y1 = spec.ar(ts(y),plot=F)
plot(y1,ci=0.95,log="yes",type="l",ci.col="blue",ci.lty=3)
spec.mtm(ts(y),log="no")

#################################################################################

######################## Wavelets ######################################
library(biwavelet)


# sine example to start with # 
# define modes of variability
f1 = (72)*pi/180    # frequency -- 360/72 = 5 year cycle
f2 = (36)*pi/180    # frequency -- 360/36 = 10 year cycle
f3 = (18)*pi/180    # frequency -- 360/18 = 20 year cycle

#initialize time 
t1 = 1:40
t2 = 41:80
t3 = 81:120
t = 1:120

x1 = sin(f1*t1)
x2 = sin(f2*t2)
x3 = sin(f3*t3)


## Very Basic Understanding of Wavelet Transforms ##

trend = 0
error = trend*t + rnorm(length(t),0,0.5)

x = c(x1,x2,x3) + error

plot(t,x,type="l",col="grey")
lines(t1,x1,col="red",lwd=2)
lines(t2,x2,col="blue",lwd=2)
lines(t3,x3,col="brown",lwd=2)


yr1 = 1
yr2 = 120
wlt=wavelet(x)
Cw=CI(0.9,x,"w")
C=CI(0.9,x,"r")
#above gets global wavelet white and red noise conf limts

par(mfrow=c(2,2));par(mar=c(4,2,3,0.5))
plot(yr1:yr2,x,xlab="Year",ylab="",main="");
lines(lowess(yr1:yr2,x,f=1/15),lwd=2,col="red");
plot(0,type='n',axes=FALSE,ann=FALSE)
wt1=wt(cbind(t,x));
plot(wt1, type="power.corr.norm", xlab="Year",main="");
plot(wlt$period,wlt$p.avg,xlim=c(0,32),main="Global Wavelet Spectrum",xlab="Period",ylab="Variance",type="l"); 
lines(wlt$period,Cw$sig);
lines(wlt$period,C$sig,col="red");


# wavelet on ridership data #

ridership = read.table("ridership_data.txt",header=T)
ridership = ridership[1:30,]
ridership = rbind(ridership[1,], ridership[12,], ridership[23,], ridership[25:30,],ridership[2:11,],ridership[13:22,],ridership[24,])

x = ridership[,5]

yr1 = 1
yr2 = 30

######
wlt=wavelet(x)
Cw=CI(0.9,x,"w")
C=CI(0.9,x,"r")
#above gets global wavelet white and red noise conf limts

par(mfrow=c(2,2));par(mar=c(4,2,3,0.5))
plot(yr1:yr2,x,xlab="Year",ylab="",main="");
lines(lowess(yr1:yr2,x,f=1/15),lwd=2,col="red");
plot(0,type='n',axes=FALSE,ann=FALSE)
wt1=wt(cbind(t,x));
plot(wt1, type="power.corr.norm", xlab="Year",main="");
plot(wlt$period,wlt$p.avg,xlim=c(0,32),main="Global Wavelet Spectrum",xlab="Period",ylab="Variance",type="l"); 
lines(wlt$period,Cw$sig);
lines(wlt$period,C$sig,col="red");
######


rider_hour = read.csv("hourly_ridership_4stations.csv",header=T)

plot.ts(rider_hour[,4])

x = rider_hour[,2]
yr1 = 1
yr2 = length(x)
t = yr1:yr2
######
wlt=wavelet(x)
Cw=CI(0.9,x,"w")
C=CI(0.9,x,"r")
#above gets global wavelet white and red noise conf limts

par(mfrow=c(2,2));par(mar=c(4,2,3,0.5))
plot(yr1:yr2,x,xlab="Year",ylab="",main="");
lines(lowess(yr1:yr2,x,f=1/15),lwd=2,col="red");
plot(0,type='n',axes=FALSE,ann=FALSE)
wt1=wt(cbind(t,x));
plot(wt1, type="power.corr.norm", xlab="Year",main="");
plot(wlt$period,wlt$p.avg,xlim=c(0,32),main="Global Wavelet Spectrum",xlab="Period",ylab="Variance",type="l"); 
lines(wlt$period,Cw$sig);
lines(wlt$period,C$sig,col="red");
######

# wavelet on CLIMATE data #
x = as.matrix(enso)

yr1 = 1901
yr2 = 2015
t = yr1:yr2
######
wlt=wavelet(x)
Cw=CI(0.9,x,"w")
C=CI(0.9,x,"r")
#above gets global wavelet white and red noise conf limts

par(mfrow=c(2,2));par(mar=c(4,2,3,0.5))
plot(yr1:yr2,x,xlab="Year",ylab="",main="");
lines(lowess(yr1:yr2,x,f=1/15),lwd=2,col="red");
plot(0,type='n',axes=FALSE,ann=FALSE)
wt1=wt(cbind(t,x));
plot(wt1, type="power.corr.norm", xlab="Year",main="");
plot(wlt$period,wlt$p.avg,xlim=c(0,32),main="Global Wavelet Spectrum",xlab="Period",ylab="Variance",type="l"); 
lines(wlt$period,Cw$sig);
lines(wlt$period,C$sig,col="red");
######


x = as.matrix(nao)

yr1 = 1901
yr2 = 2015
t = yr1:yr2
######
wlt=wavelet(x)
Cw=CI(0.9,x,"w")
C=CI(0.9,x,"r")
#above gets global wavelet white and red noise conf limts

par(mfrow=c(2,2));par(mar=c(4,2,3,0.5))
plot(yr1:yr2,x,xlab="Year",ylab="",main="");
lines(lowess(yr1:yr2,x,f=1/15),lwd=2,col="red");
plot(0,type='n',axes=FALSE,ann=FALSE)
wt1=wt(cbind(t,x));
plot(wt1, type="power.corr.norm", xlab="Year",main="");
plot(wlt$period,wlt$p.avg,xlim=c(0,32),main="Global Wavelet Spectrum",xlab="Period",ylab="Variance",type="l"); 
lines(wlt$period,Cw$sig);
lines(wlt$period,C$sig,col="red");
######

x = as.matrix(amo)

yr1 = 1901
yr2 = 2015
t = yr1:yr2
######
wlt=wavelet(x)
Cw=CI(0.9,x,"w")
C=CI(0.9,x,"r")
#above gets global wavelet white and red noise conf limts

par(mfrow=c(2,2));par(mar=c(4,2,3,0.5))
plot(yr1:yr2,x,xlab="Year",ylab="",main="");
lines(lowess(yr1:yr2,x,f=1/15),lwd=2,col="red");
plot(0,type='n',axes=FALSE,ann=FALSE)
wt1=wt(cbind(t,x));
plot(wt1, type="power.corr.norm", xlab="Year",main="");
plot(wlt$period,wlt$p.avg,xlim=c(0,64),main="Global Wavelet Spectrum",xlab="Period",ylab="Variance",type="l"); 
lines(wlt$period,Cw$sig);
lines(wlt$period,C$sig,col="red");


x = as.matrix(pdo)

yr1 = 1901
yr2 = 2015
t = yr1:yr2
######
wlt=wavelet(x)
Cw=CI(0.9,x,"w")
C=CI(0.9,x,"r")
#above gets global wavelet white and red noise conf limts

par(mfrow=c(2,2));par(mar=c(4,2,3,0.5))
plot(yr1:yr2,x,xlab="Year",ylab="",main="");
lines(lowess(yr1:yr2,x,f=1/15),lwd=2,col="red");
plot(0,type='n',axes=FALSE,ann=FALSE)
wt1=wt(cbind(t,x));
plot(wt1, xlab="Year",main="");
plot(wlt$period,wlt$p.avg,xlim=c(0,32),main="Global Wavelet Spectrum",xlab="Period",ylab="Variance",type="l"); 
lines(wlt$period,Cw$sig);
lines(wlt$period,C$sig,col="red");


# Streamflow #
#### streamflow ###
streamflow = read.table("streamflow_delaware.txt",header=T)

# Rearrange streamflow data #
ndays = 366
nyrs = nrow(streamflow)/ndays
nsites = ncol(streamflow)

streamflow_yearly = array(NA,c(ndays,nyrs,nsites))
for (i in 1:nsites)
{
  streamflow_yearly[,,i] = array(streamflow[,i],c(ndays,nyrs)) 
}

x = apply(streamflow_yearly[,,3],2,sum,na.rm=T)

yr1 = 1
yr2 = length(x)
t = yr1:yr2
######
wlt=wavelet(x)
Cw=CI(0.9,x,"w")
C=CI(0.9,x,"r")
#above gets global wavelet white and red noise conf limts

par(mfrow=c(2,2));par(mar=c(4,2,3,0.5))
plot(yr1:yr2,x,xlab="Year",ylab="",main="");
lines(lowess(yr1:yr2,x,f=1/15),lwd=2,col="red");
plot(0,type='n',axes=FALSE,ann=FALSE)
wt1=wt(cbind(t,x));
plot(wt1, xlab="Year",main="");
plot(wlt$period,wlt$p.avg,xlim=c(0,32),main="Global Wavelet Spectrum",xlab="Period",ylab="Variance",type="l"); 
lines(wlt$period,Cw$sig);
lines(wlt$period,C$sig,col="red");










#################################################################################

######################## Hypothesis Tests ######################################


### Data to be used: 1) Flood data for USA and 2) Parking Violations for NYC ###

# Floods #
time = globalfloods[,1]
x = globalfloods[,2]

# simple time series plot #
plot(time,x,font=2,xlab="Years",ylab="Number of Floods",font.lab=2)
lines(lowess(time,x),col="red",lwd=2,lty=3)
abline(lm(x~time),col="grey")
points(time[1:15],x[1:15],pch=17,col="grey")
points(time[16:31],x[16:31],pch=17,col="black")


# Rank Sum Test on data before and after #
floods_before = x[1:15]
floods_after = x[16:31]

boxplot(cbind(floods_before,floods_after))

# 2 sample t-test
t.test(floods_before,floods_after)

# rank sum test 
wilcox.test(floods_before,floods_after)

# bootstrap test
x1 = floods_before
x2 = floods_after

N = 10000
S = matrix(0,nrow=N,ncol=1)

for (i in 1:N)
{
  x1boot = sample(x1,replace=T)
  x2boot = sample(x2,replace=T)
  if(mean(x1boot)>mean(x2boot)) { S[i,1]=1 }
}
p_value = sum(S)/N
print(p_value)
boxplot(cbind(x1,x2))


# Parking Violations #
parking_violate = read.csv("ParkingViolation_nyct2010_15CTs.csv",header=T)

# extracting zipcode data #
# 10016 - east side 27 to 40
# 10018 - Bryant Park
# 10028 - east 81
# 10128 - east 87 - 96
dum1 = which(parking_violate$zip == 10028)
dum2 = which(parking_violate$zip == 10128)

parking_1 = parking_violate[dum1,]
parking_2 = parking_violate[dum2,]

# create a series of daily violations #

parking_daily = matrix(NA,nrow = 30,ncol = 2)
for (i in 1:30)
{
  dum = which(parking_1$Data==i)
  parking_daily[i,1] = length(dum)
  
  dum = which(parking_2$Data==i)
  parking_daily[i,2] = length(dum)
}

# rank sum test 
wilcox.test(parking_daily[,1],parking_daily[,2])

# bootstrap test
x1 = parking_daily[,1]
x2 = parking_daily[,2]

N = 10000
S = matrix(0,nrow=N,ncol=1)

for (i in 1:N)
{
  x1boot = sample(x1,replace=T)
  x2boot = sample(x2,replace=T)
  if(mean(x1boot)>mean(x2boot)) { S[i,1]=1 }
}
p_value = sum(S)/N
print(p_value)
boxplot(cbind(x1,x2))


#################################################################################

######################## Regression Basics ######################################

parking = read.table("parking.txt",header=T)
lake = read.table("lake.txt",header=T)

#################### 1: Population Density - Parking Price Model #################
#################### BUILDING A GOOD REGRESSION MODEL #####################

data = parking

x = data[,1]                          # predictor variable 
y = data[,2]                          # predictand variable 

#################### Step 1: Plot the Data ######################

plot(data,font=2,cex.axis=1.5,family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2)   # scatter plot for the data
abline(lm(y~x),lwd=2)                # plots the trend line linear line 
lines(lowess(data),col="red",lwd=2)  # plots the smooth line between x and y to check for non-linearity

#################### Step 2: Transformation (using Bulging Rule) if the relation looks non-linear ######################
x = log10(data[,1])                          # predictor variable 
y = data[,2]                          # predictand variable 


plot(x,y,font=2,cex.axis=1.5,family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2)   # scatter plot for the data
abline(lm(y~x),lwd=2)                # plots the trend line linear line 
lines(lowess(x,y),col="red",lwd=2)  # plots the smooth line between x and y to check for non-linearity

#################### Step 3: Build the Regression Model ######################

model = lm (y~x)        # linear regression model on y using x

# Regression basic outputs
summary(model)

#################### Step 4: Regression Diagnostics ######################

#### Residual Plots #### 
residuals = residuals(model)   # residuals 
yhat = predict(model)          # predicted values - yhat 

par(mfrow=c(2,2))
#### Residuals Vs. Yhat ####
plot(yhat,residuals,font=2,cex.axis=1.5,family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2) ; abline(h=0,lwd=2)     

#### Residuals Vs. x ####
plot(x,residuals,font=2,cex.axis=1.5,family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2) ; abline(h=0,lwd=2)                                    

#### Residuals Vs. Time ####
plot(1:length(x),residuals,type="o",font=2,cex.axis=1.5,xlab="Time",family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2) ; abline(h=0,lwd=2)                                    

#### ACF of Residuals ####
acf(residuals,font=2,cex.axis=1.5,main="",family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2) ; abline(h=0,lwd=2)                                    

#### Residuals - Normal Distribution Test - QQ plot and KS Test ####
goodness(residuals)



#### Leverage, Influence and Outlier Plots #### 

### Leverage ###
h = hatvalues(model)

### Influence ###
d = cooks.distance(model)

### Outliers ###
es = rstandard(model)

par(mfrow=c(3,2))

###### Leverage on XY Plot #####
plot(x,y,font=2,cex.axis=1.5,family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2)   # scatter plot for the data
symbols(x,y,circles=h,add=T,inches=1/6, ann=F, bg="steelblue2", fg=NULL)
#text(x,y,labels=round(h,2),cex=0.5,font=2)
abline(lm(y~x),lwd=2)

###### Influence on XY Plot #####
plot(x,y,font=2,cex.axis=1.5,family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2)   # scatter plot for the data
symbols(x,y,circles=d,add=T,inches=1/6, ann=F, bg="red", fg=NULL)
#text(x,y,labels=round(d,2),cex=0.5,font=2)
abline(lm(y~x),lwd=2)

###### Standardized Residuals Vs. X with Leverage #####
plot(x,es,font=2,cex.axis=1.5,,ylab="Standardized Residuals",family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2)
symbols(x,es,circles=h,add=T,inches=1/6, ann=F, bg="steelblue2", fg=NULL)
#text(x,y,labels=round(h,2),cex=0.5)
abline(h=0,lwd=2)

###### Standardized Residuals Vs. X with Influence #####
plot(x,es,font=2,cex.axis=1.5,,ylab="Standardized Residuals",family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2)
symbols(x,es,circles=d,add=T,inches=1/6, ann=F, bg="red", fg=NULL)
#text(x,y,labels=round(d,2),cex=0.5)
abline(h=0,lwd=2)

###### Standardized Residuals Vs. Leverage with Influence #####
plot(h,es,font=2,cex.axis=1.5,ylab="Standardized Residuals",family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2,xlab="Leverage")
symbols(h,es,circles=d,add=T,inches=1/4, ann=F, bg="red", fg=NULL)
abline(h=0,lwd=2)


#################### Step 5: Predictions ######################

####   Plot confidence intervals and predictions intervals for the model #### 

dummyx = seq(min(x),max(x),by=0.05)
fitted.values.conf = predict(model,data.frame(x=dummyx),interval="confidence",level=0.95)
fitted.values.pred = predict(model,data.frame(x=dummyx),interval="prediction",level=0.95)

plot(x,y,font=2,cex.axis=1.5,family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2)   # scatter plot for the data
abline(lm(y~x),lwd=2)

lines(dummyx,fitted.values.conf[,2],col="blue",lwd=2)
lines(dummyx,fitted.values.conf[,3],col="blue",lwd=2)

lines(dummyx,fitted.values.pred[,2],col="red",lwd=2)
lines(dummyx,fitted.values.pred[,3],col="red",lwd=2)


####  Use the model to make predictions ####

# for new value #
xnew <- data.frame((x=log10(1000)))

ynew <- predict(model,xnew,interval="prediction",level=0.95,se.fit=TRUE)

plot(log10(data[,1]),y,xlim=c((min(log10(data[,1]))-sd(log10(data[,1]))),(max(log10(data[,1]))+sd(log10(data[,1])))),ylim=c((min(y)-sd(y)),(max(y)+sd(y))),font=2,cex.axis=1.5,family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2)   # scatter plot for the data
boxplot(t(ynew$fit),boxwex=0.25,add=T,at=as.numeric(xnew))
points(xnew,ynew$fit[1],pch=19,col="red",cex=2)

lines(dummyx,fitted.values.conf[,1],col="black",lwd=2)

lines(dummyx,fitted.values.conf[,2],col="blue",lwd=2)
lines(dummyx,fitted.values.conf[,3],col="blue",lwd=2)

lines(dummyx,fitted.values.pred[,2],col="red",lwd=2)
lines(dummyx,fitted.values.pred[,3],col="red",lwd=2)


# standard error of the predicted mean = se.fit
# standard deviations of the residuals = residual.scale

prediction.sd = sqrt((ynew$se.fit)^2 + (ynew$residual.scale)^2)

threshold = 1
# Probability of DO greater than k mg/L  
prob.demand = pnorm(threshold,ynew$fit[,1],prediction.sd)

# simulated data #
simulated.ynew = rnorm(100000,ynew$fit[,1],prediction.sd)

hist((simulated.ynew),main="",font=2,cex.axis=1.5,family="serif",font.lab=2,cex.lab=1.5,xlab="Predicted ynew Distribution");abline(v=threshold,lwd=2,col="red")



#################### 2: Lake Area - Water Depth Model #################
#################### BUILDING A GOOD REGRESSION MODEL #####################

data = lake

x = data[,1]                          # predictor variable 
y = data[,2]                          # predictand variable 

#################### Step 1: Plot the Data ######################

plot(data,font=2,cex.axis=1.5,family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2)   # scatter plot for the data
abline(lm(y~x),lwd=2)                # plots the trend line linear line 
lines(lowess(data),col="red",lwd=2)  # plots the smooth line between x and y to check for non-linearity

#################### Step 2: Transformation (using Bulging Rule) if the relation looks non-linear ######################
x = log10(data[,1])                          # predictor variable 
y = data[,2]                          # predictand variable 


plot(x,y,font=2,cex.axis=1.5,family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2)   # scatter plot for the data
abline(lm(y~x),lwd=2)                # plots the trend line linear line 
lines(lowess(x,y),col="red",lwd=2)  # plots the smooth line between x and y to check for non-linearity

#################### Step 3: Build the Regression Model ######################

model = lm (y~x)        # linear regression model on y using x

# Regression basic outputs
summary(model)

#################### Step 4: Regression Diagnostics ######################

#### Residual Plots #### 
residuals = residuals(model)   # residuals 
yhat = predict(model)          # predicted values - yhat 

par(mfrow=c(2,2))
#### Residuals Vs. Yhat ####
plot(yhat,residuals,font=2,cex.axis=1.5,family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2) ; abline(h=0,lwd=2)     

#### Residuals Vs. x ####
plot(x,residuals,font=2,cex.axis=1.5,family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2) ; abline(h=0,lwd=2)                                    

#### Residuals Vs. Time ####
plot(1:length(x),residuals,type="o",font=2,cex.axis=1.5,xlab="Time",family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2) ; abline(h=0,lwd=2)                                    

#### ACF of Residuals ####
acf(residuals,font=2,cex.axis=1.5,main="",family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2) ; abline(h=0,lwd=2)                                    

#### Residuals - Normal Distribution Test - QQ plot and KS Test ####
goodness(residuals)



#### Leverage, Influence and Outlier Plots #### 

### Leverage ###
h = hatvalues(model)

### Influence ###
d = cooks.distance(model)

### Outliers ###
es = rstandard(model)

par(mfrow=c(3,2))

###### Leverage on XY Plot #####
plot(x,y,font=2,cex.axis=1.5,family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2)   # scatter plot for the data
symbols(x,y,circles=h,add=T,inches=1/6, ann=F, bg="steelblue2", fg=NULL)
#text(x,y,labels=round(h,2),cex=0.5,font=2)
abline(lm(y~x),lwd=2)

###### Influence on XY Plot #####
plot(x,y,font=2,cex.axis=1.5,family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2)   # scatter plot for the data
symbols(x,y,circles=d,add=T,inches=1/6, ann=F, bg="red", fg=NULL)
#text(x,y,labels=round(d,2),cex=0.5,font=2)
abline(lm(y~x),lwd=2)

###### Standardized Residuals Vs. X with Leverage #####
plot(x,es,font=2,cex.axis=1.5,,ylab="Standardized Residuals",family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2)
symbols(x,es,circles=h,add=T,inches=1/6, ann=F, bg="steelblue2", fg=NULL)
#text(x,y,labels=round(h,2),cex=0.5)
abline(h=0,lwd=2)

###### Standardized Residuals Vs. X with Influence #####
plot(x,es,font=2,cex.axis=1.5,,ylab="Standardized Residuals",family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2)
symbols(x,es,circles=d,add=T,inches=1/6, ann=F, bg="red", fg=NULL)
#text(x,y,labels=round(d,2),cex=0.5)
abline(h=0,lwd=2)

###### Standardized Residuals Vs. Leverage with Influence #####
plot(h,es,font=2,cex.axis=1.5,ylab="Standardized Residuals",family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2,xlab="Leverage")
symbols(h,es,circles=d,add=T,inches=1/4, ann=F, bg="red", fg=NULL)
abline(h=0,lwd=2)


#################### Step 5: Predictions ######################

####   Plot confidence intervals and predictions intervals for the model #### 

dummyx = seq(min(x),max(x),by=0.05)
fitted.values.conf = predict(model,data.frame(x=dummyx),interval="confidence",level=0.95)
fitted.values.pred = predict(model,data.frame(x=dummyx),interval="prediction",level=0.95)

plot(x,y,font=2,cex.axis=1.5,family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2)   # scatter plot for the data
abline(lm(y~x),lwd=2)

lines(dummyx,fitted.values.conf[,2],col="blue",lwd=2)
lines(dummyx,fitted.values.conf[,3],col="blue",lwd=2)

lines(dummyx,fitted.values.pred[,2],col="red",lwd=2)
lines(dummyx,fitted.values.pred[,3],col="red",lwd=2)


####  Use the model to make predictions ####

# for new value #
xnew <- data.frame((x=log10(12.5)))

ynew <- predict(model,xnew,interval="prediction",level=0.95,se.fit=TRUE)

plot(log10(data[,1]),y,xlim=c((min(log10(data[,1]))-sd(log10(data[,1]))),(max(log10(data[,1]))+sd(log10(data[,1])))),ylim=c((min(y)-sd(y)),(max(y)+sd(y))),font=2,cex.axis=1.5,family="serif",font.lab=2,cex.lab=1.5,pch=19,cex=2)   # scatter plot for the data
boxplot(t(ynew$fit),boxwex=0.25,add=T,at=as.numeric(xnew))
points(xnew,ynew$fit[1],pch=19,col="red",cex=2)

lines(dummyx,fitted.values.conf[,1],col="black",lwd=2)

lines(dummyx,fitted.values.conf[,2],col="blue",lwd=2)
lines(dummyx,fitted.values.conf[,3],col="blue",lwd=2)

lines(dummyx,fitted.values.pred[,2],col="red",lwd=2)
lines(dummyx,fitted.values.pred[,3],col="red",lwd=2)


# standard error of the predicted mean = se.fit
# standard deviations of the residuals = residual.scale

prediction.sd = sqrt((ynew$se.fit)^2 + (ynew$residual.scale)^2)

threshold = 75
# Probability of DO greater than k mg/L  
prob.demand = pnorm(threshold,ynew$fit[,1],prediction.sd)

# simulated data #
simulated.ynew = rnorm(100000,ynew$fit[,1],prediction.sd)

hist((simulated.ynew),main="",font=2,cex.axis=1.5,family="serif",font.lab=2,cex.lab=1.5,xlab="Predicted ynew Distribution");abline(v=threshold,lwd=2,col="red")



#################################################################################

######################## Formal Trend Analysis ######################################

#Trend 1 EXAMPLES 
#SLIDE #297
#Generate 100 RN  N(0,1)
y=rnorm(100,0,1)
x=seq(1,100,1)
model1=lm(y~x)
summary(model1)

par(mfrow=c(2,1))
# See if the time series have any autocorrelation structure. Plot ACF
plot(acf(y,plot=F), main="Autocorrelation Function",lwd=2)

plot(x,y, main="Normally Distributed Random Numbers")

#Check if the slope and intercept are significant. Plot the line
abline(model1$coefficients,lt=2,lw=3)
# and the LOWESS
lines(lowess(y,f=2/3),lw=2,col=2)


#_____________________________________________________
#exponentially distributed data with no trend
#SLIDE # 298
# Generate 100 exponentially distributed random numbrs (mean=1)
y=rexp(100,1)
hist(y,freq=F);lines(density(y))

par(mfrow=c(2,1))
# Plot these against time (x in this case)
plot(x,y, main="Exponentially Distributed Random Numbers")
# Fit linear model and check if the parameters (particularly the slope) are significant
model1=lm(y~x)

summary(model1)
abline(model1$coefficients,lt=2,lw=3)
lines(lowess(y,f=2/3),lw=2,col=2)

plot(acf(y,plot=F), main="Autocorrelation Function",lwd=2)

#Check the distribution of the residuals against normal QQ plot
plot(model1$residuals)
par(mfrow=c(1,2))
hist(model1$residuals,freq=F);lines(density(model1$residuals))
qqnorm(model1$residuals);qqline(model1$residuals)


#____________________________________________________________
#model b AR(1) process
#SLIDE #301
#Generate a series with lag 1 autoregressive values with small noise
x=c(seq(1,100,1))
y=c(seq( 1, 100, 1))
y[1]=rnorm(1)
for(i in 2:100)
{
  y[i]=0.9*y[i-1]+rnorm(1,0,0.19)
}

par(mfrow=c(2,1))
plot(x,y, main="Stationary AR(1) process, r=0.9",xlab="t",ylab="y")
modelb=lm(y~x)

summary(modelb)
abline(modelb$coefficients,lt=2,lw=3)
lines(lowess(y,f=2/3),lw=2,col=2)

#Check ACF

plot(acf(y,plot=F), main="Autocorrelation Function",lwd=2)

#residuals from best fit

plot(x,modelb$residuals,main="Residuals From Best Linear Fit",ylab="Residuals")
modelbres=lm(modelb$residuals~x)
summary(modelbres)


abline(modelbres$coefficients,lt=2,lw=3)
lines(lowess(modelb$residuals,f=2/3),lw=2,col=2)

#lines(lowess(acf.value,f=2/3),lw=2,col=2))

plot(acf(modelb$residuals,plot=F), main="Autocorrelation Function",lwd=2)


#________________________________________________________________
#fit composite model
#first fit a regression line with lag= 1 value of y(t-1)and 
# time t as predictors. y(t)= a +b*y(t-1)+t
# xx is the data to be fitted
# xxx is the data with lag . i.e., y(t-1)
# yy is the time from 2 to 100
yy=y[2:100]
yyy=y[1:99]
xx=x[2:100]
modelb2=lm(yy~xx+yyy)

#observe the coefficient of time which is xx here  ... 
summary(modelb2)
# note that trend is not significant (large p value) but lagged data yyy is 
#change model to to fit only the lag 1 data (yyy)
modelb2a=lm(yy~yyy)
#observe the t values.. 
summary(modelb2a)

# see that lag 1 coefficient  significantly differs from zero (small p value)
#plot residuals

plot(xx,modelb2a$residuals,main="Residuals After Fitting Lag 1 Regression to AR1 Data ",ylab="Residuals ", xlab="t")
lmres=lm(modelb2a$residuals~xx)
summary(lmres)
abline(lmres$coefficients,lt=2,lw=3)
lines(lowess(modelb2a$residuals,f=2/3),lw=2,col=2)

# acf of residuals show no autocorrelation
acf.value=acf(modelb2a$residuals,main="Autocorrelation Function of Residuals",lwd=2)


#______________________________________________________________

# EXAMPLE c-1 LINEAR TREND
# SLIDE 305
# y= 0.01t+N(0,1) Noise
# Generate 100 values
x=c(seq(1:100))
y=c(seq(1:100))
noise=rnorm(100,0,1)
y=0.01*x+noise

par(mfrow=c(2,1))
plot(x,y,main="Trend with 0.01 slope and N(0,1) Noise",ylab="Y")
lines(x,y,lt=2)
modelc1=lm(y~x)
summary(modelc1)
abline(modelc1$coefficients,lw=2,lt=2)
lines(lowess(y,f=2/3),lw=2,col=2)
plot(acf(y,plot=F), main="Autocorrelation Function",lwd=2)

# Reduce the standard deviation by tenfold
#SLIDE # 306
y2=c(seq(1:100))
noise2=rnorm(100,0,0.1)
y2=0.01*x+noise2

par(mfrow=c(2,1))
plot(x,y2,main="Trend with 0.01 slope and N(0,0.1) Noise",ylab="Y")
modelc1a=lm(y2~x)
summary(modelc1a)
abline(modelc1a$coefficients,lw=2,lt=2)
lines(lowess(y2,f=2/3),lw=2,col=2)

plot(acf(y2,plot=F), main="Autocorrelation Function",lwd=2)

# Show that the decrease in acf is linear
m=acf(y2,plot=F)
names(m)
#  acf values..
m$acf
# lag values
m$lag
# fit a line
modelmm=lm(m$acf~m$lag)

summary(modelmm)
#plot the line on top of acf plot
abline(modelmm$coefficients,col="red")


#___________________________________________________________
#EXAMPLE C-2
# SLIDE #308
#Model is  y=0.01t+ sin(13.5 *(pi/180)*t) +N(0,1) Noise
t=c(seq(1:100))
yc2=c(seq(1:100))
noise=rnorm(100,0,1)
yc2[t]=0.01*t+sin(13.5*(pi/180)*t)+noise
yc2

par(mfrow=c(2,1))
plot(t,yc2,main="Sine wave + linear trend with N(0,1) Noise & slope=0.01",ylab="Y")
lines(t,yc2,lt=3)
modelc2=lm(yc2~t)
summary(modelc2)
abline(modelc2$coefficients,lw=2,lt=2)
lines(lowess(yc2,f=2/3),lw=2,col=2)
# Add linear and sine basis functions

modelc2a=lm(yc2~t+sin(13.5*(pi/180)*t))
summary(modelc2a)
curve(modelc2a$coefficients[1]+modelc2a$coefficients[2]*x+modelc2a$coefficients[3]*sin(13.5*(pi/180)*x),
      1,100,add=T,lt=4,lw=3)
lines(lowess(yc2,f=.2),lw=2,col=4)

plot(acf(yc2,plot=F), main="Autocorrelation Function Before model fit",lwd=2)
plot(acf(modelc2a$residuals,plot=F),main="Autocorrelation Function of Residuals" )



#_____________________________________________________________________________
# example c.3 Step : change in mean at a step t=50 (Step size = 1)
# SLIDE 309
# generate data from
# y=-1+ds(n=50) + Normal (0,1) noise
# t is defined earlier
t=c(seq(1:100))
yc3=c(seq(1:100))
noise=rnorm(100,0,1)
ds=1
for (i in 1:100){
  if (i<50) yc3[i]=-1+noise[i]
  if(i>=50)yc3[i]=-1+ds+noise[i]
}

yc3
step=c(rep(0,49),rep(1,51))

par(mfrow=c(2,1))
plot(t,yc3,ylab="y",main="Step Function, Step=1, Noise SD =1")
#fit a line to all data

modelc3a=lm(yc3~t)
summary(modelc3a)
abline(modelc3a$coefficients,lt=2)
lines(lowess(yc3,f=2/3),lw=2,col=2)



modelc3=lm(yc3~t+step)

summary(modelc3)
#plot the step function
#define line segments
x0=1
y0=modelc3$coefficients[1]
x1=49
y1=modelc3$coefficients[1]+modelc3$coefficients[2]*x1
x2=49
y2=modelc3$coefficients[1]+modelc3$coefficients[2]*x2+modelc3$coefficients[3]
x3=100
y3=modelc3$coefficients[1]+modelc3$coefficients[2]*x3+modelc3$coefficients[3]

segments(x0,y0,x1,y1,lt=4,lw=2)
segments(x1,y1,x2,y2,lt=4,lw=2)
segments(x2,y2,x3,y3,lt=4,lw=2)


plot(acf(yc3,plot=F), main="Autocorrelation Function",lwd=2)
plot(acf(modelc3$residuals))
#............
modelc4=lm(yc3~step)
summary(modelc4)

plot(acf(modelc4$residuals))


#_______________________________________________________________________________
#Trend + exogenous factor
# SLIDE 311
# Example d.1
#create the y variable
# the model is: y=0.01t+10sqrt(z)+noise(N(0,1))
# z= exp(1)
noise=rnorm(100,0,1)
yd1=c(seq(0,100))
z=rexp(100,1)
yd1=0.01*t+10*z^0.5+noise

par(mfrow=c(2,1))
plot(t,yd1,main="Linear trend + Exogeneous Variable + Noise N(0,1)",lwd=2,ylab="y")
lines(t,yd1,lt=4,lw=1)
modeld1=lm(yd1~t)
summary(modeld1)
abline(modeld1$coefficients,lt=2,lwd=2)

lines(lowess(yd1,f=2/3),lw=2,col=2)

#Inroduce the exogenous predictor (exponential square root)
zz=sqrt(z)
modeld1a=lm(yd1~t+zz)
summary(modeld1a)


plot(acf(yd1,plot=F), main="Autocorrelation Function",lwd=2)

#-------------------------------------
# SLIDE 29
# Plot y against z to see if there is a hint of a relationship
par(mfrow=c(2,1))
plot(z,yd1,ylab="y",main="Scatterplot y against z, Suggest transformations")

#transform
#zz=sqrt(z)

modeld1b=lm(yd1~zz)
summary(modeld1b)

curve(modeld1b$coefficients[1]+modeld1b$coefficients[2]*sqrt(x),0,5,
      add=T,lwd=2)
#remove trend
R=yd1-modeld1b$coefficients[1]-modeld1b$coefficients[2]*zz


modeld1c=lm(R~t)
summary(modeld1c)
plot(t,R,main="Linear trend  after removing exogenous variable's effect",ylab="y" )
abline(modeld1c$coefficients,lt=2,lw=2)
lines(lowess(R,f=2/3),lw=2,col=2)

acf(R)


#______________________________________________________
#EXAMPLE d.2
# SLIDE # 313
# Linear trend + AR(1) noise
# Model yt=0.01t+et
# et=0.9e(t-1)+sqrt(0.19)*N(0,1)

yd2=c(rep(0,100))
yd2
eps=c(rep(0,100))
for (i in 2:100)
{
  eps[i]=eps[i-1]*0.9+sqrt(0.19)*rnorm(1,0,1)
}

acf(eps)

yd2=0.01*t+eps

plot(t,yd2,ylab="y",main="Linear Trend + AR1 Correlated Noise")
lines(t,yd2,lt=3)
modeld2a=lm(yd2~t)
summary(modeld2a)
abline(modeld2a$coefficients,lt=2,lw=2)
lines(lowess(yd2,f=2/3),lw=2,col=2)
acf.value=acf(yd2, main="Autocorrelation Function",lwd=2)

# Build a model with y(t-1) and t as predictors
#d2a: values from t=2 to 100 
yd2a=c(yd2[-(1:1)])
#yd2b: Values from t=1 to 99
yd2b=c(yd2[-100])
tt=c(seq(from=1 , to =99))
modeld2b=lm(yd2a~tt+yd2b)
summary(modeld2b)

# calculate the predicted time series
predict=c(rep(0,99))
for(i in seq(from=2,to=100))
{
  predict[i]=modeld2b$coefficients[1]+ modeld2b$coefficients[2]*i+modeld2b$coefficients[3]*yd2b[i-1]
}

predict[1]

#remove the 1st element 
predict=c(predict[-1])

ttt=c(seq(from=1, to=99 ))
length(ttt)
length(predict)
plot(ttt,predict)  #   predicted values using the model d2b
lines(ttt,predict,lt=2)
tt=c(seq(from=2,to=100))
length(yd2a)

lines(tt,yd2a,col=2)  # observed data

# Check if the trend is removed in the reesiduals: predict-yd2a
acf.value=acf((predict-yd2a), main="Autocorrelation Function",lwd=2)



#_________________________________________
#
#The Last Example d.3
# SLIDE # 315
# Generate  8 figures by selecting r
yd3=c(rep(0,100))
#Try different values of r
par(mfrow=c(4,2))
R=c(1,1.2,1.5,1.8,1.9,1.95,1.99,2)
for (j in 1:8){r=R[j]
yd3[1]=rnorm(1)
yd3[2]=rnorm(1)
for(i in 3:100){
  yd3[i]=r*yd3[i-1]-yd3[i-2]}

plot(t,yd3,ylab="y")
lines(t,yd3)}


##### Trend 
#########################################

#Problem 1: Trend : Change in Properties of Series at a Particular Point

#...........................................................#
#Rank sum test Slide 326
#Example 1 data 

x=c(1.8,-2.1,0.1,-0.4)
y=c(-1.2,3.0,1.1,1.5,0.6)

ww=wilcox.test(x,y,mu=0,exact=TRUE,conf.int=TRUE,paired=FALSE)
names(ww)
summary(ww)
ww$p.value
ww$statistic

#example 2 data

xx=c(4.8,5.1,11.2,3.1)
yy=c(0.1,9.6,2.9,1.8,1.4)

par(mfrow=c(2,1))
plot.ts(c(x,y),type="o");abline(v=5)
plot.ts(c(xx,yy),type="o");abline(v=5)

#For example 1 data we have
z=wilcox.test(x,y)
zz=wilcox.test(xx,yy)

#--------------------------------------------------------
# What kind of information is in z or zz

names(z)
# W values calculated. See notes for Slide 16

##  W = n1* n2 + n1(N1+1)/2 -R1
###   R1 = 16......Wrs value calculated according to slide 16

z$statistic
zz$statistic

z$p.value
zz$p.value

## Try different sets corresponding to different change points

# One step earlier

x_1_back=c(1.8,-2.1,0.1)
y_1_back=c(-0.4, -1.2,3.0,1.1,1.5,0.6)
wilcox.test(y_1_back,x_1_back)

# Two steps earlier
x_2_back=c(1.8,-2.1)
y_2_back=c(0.1,-0.4, -1.2,3.0,1.1,1.5,0.6)
wilcox.test(y_2_back,x_2_back)

# One step later
x_1_later=c(1.8,-2.1,0.1,-0.4,-1.2)
y_1_later=c(3.0,1.1,1.5,0.6)
wilcox.test(y_1_later,x_1_later)

# two steps later
x_2_later=c(1.8,-2.1,0.1,-0.4,-1.2,3.0)
y_2_later=c(1.1,1.5,0.6)
wilcox.test(y_2_later,x_2_later)
#####################
#Rank sum test for example 2
zz=wilcox.test(xx,yy)

######################################

#Example 2 boxplots
par(mfrow=c(1,2))
boxplot(xx,yy,names=c("x", "y"),main="Original Data")
boxplot(log(xx),log(yy),names=c("x", "y"),main="Log-Transformed")
##################################################################

#****************************************************************
#Regression framework example

### Input data for the examples in slide 332
# we are generating a time series with a step at t=50
# no trend, and a more complex one with two trends
y=c(rep(0,100))
yy=c(rep(0,100))

par(mfrow=c(1,1))
a0=0
a1=0.0025
a2=0.06
a3=-0.005
step=c(rep(0,50), rep(1,50))
e=rnorm(100,0,.09)
t=c(seq(1:100))


#simple case: just a step function with noise
for(i in 1:100)
{
  y[i]=a0+a2*step[i]+e[i]
  #complex case   with trend and step and second slope
  yy[i]=a0+a1*t[i]+a2*step[i]+a3*step[i]*t[i]+e[i]
}
y


plot(t,y,main="Simple Case")
lines(t,y,main="Simple Case")

segments(0,0,50,0,lw=2)
segments(50,0,50,a2,lw=2)
segments(50,a2,100,a2,lw=2)

plot(t,yy,main="Complex Case")
lines(t,yy)

segments(0,0,50,50*a1,lw=2)
segments(50,50*a1,50,(a0+a1*50+a2+a3*50),lw=2)
segments(50,(a0+a1*50+a2+a3*50),100,(a0+a1*100+a2+a3*100),lw=2)
step

# Model based on slope and step and second slope with step

v=step*t
model=lm(yy~t+v)
summary(model)
acf(yy)
acf(model$residuals)
## plot the model results
curve(model$coefficients[1]+model$coefficients[2]*x+model$coefficients[3]*v[x], 1, 100, add=T,col=2)



########## Trend Analysis using Regression Smoothers ##########

#***********************************************************
#Slide 349 script

# Model: y(t)=0.9y(t-1)+sqrt(0.19)*N(0,1)
# Create a time series based on the above model...

n=100
t=c(1:n)
y=c(rep(0,n))
for (i in 2:n)
  y[i]=0.9*y[i-1]+sqrt(0.19)*rnorm(1,0,1)
yy=y[2:n]
plot(t,y)
P=0
M=0
Z=0
for (i in 1:(n-1))
  for (j in (i+1):n)
    if (y[i] - y[j] >0) M=M+1  else P=P+1
S=P-M
S
#Calculate Z
V=(n/18)*(n-1)*(2*n+5)
V 
if(S>0) u=-1 else u=1
Z=(S+u)/sqrt(V)
Z
if(Z > 0) p.value=(1-pnorm(Z,0,1))*2 else p.value=pnorm(Z,0,1)*2
p.value

### If p.value < Type I error, we reject the null hypothesis of No-Trend..
####.............................


acf(y, main="Slide 16-17")
A=acf(y)
#correlation coefficient of lag 1 A[1]
# use this and build an AR(1) model
a=A$acf[2]
#************************
A$acf

#Slide 17

curve(A$acf[2]^x,0,100,n=100,add=T,col=2,lt=2,lwd=2)

### What does this signify??

pacf(y)
# After removing the AR(1)what is left??
B=pacf(y)
B$acf

plot(pacf(y))

mod1=lm(y~t)
summary(mod1)

# See help  AIC and extract AIC
AIC(mod1)
extractAIC(mod1)



#  This gives two numbers: first is the equivalend degrees of freedom 
###  for the fit
### The second is the AIC for the fit

### Note that AIC and extract AIC calculate AIC values differently ..
#*************************************************

#*******************************
mod2=ar(y,aic=TRUE,order.max=3)
# Observe the order . type mod2
mod2
summary(mod2)
names(mod2)
mod2$resid
acf(mod2$resid,na.action=na.pass)
mod2$ar


#********************************************
#        slide 352 ... GLOBAL SMOOTHERS......

plot(t,y)
# Line
lin=lm(y~t)
summary(lin)
extractAIC(lin)

abline(lin$coefficients,lt=2,lw=2)


# Fit second order
t2=t^2
sec=lm(y~t+t2)
summary(sec)
extractAIC(sec)
# AIC is smaller..
curve(sec$coefficients[1]+sec$coefficients[2]*x+sec$coefficients[3]*x^2,0,100,n=100,add=T,col=2,lt=2,lw=2)

#  Cubic

t3=t^3
cub=lm(y~t+t2+t3)
summary(cub)
extractAIC(cub)

curve(cub$coefficients[1]+cub$coefficients[2]*x+
        cub$coefficients[3]*x^2+ cub$coefficients[4]*x^3,0,100,n=100,add=T,col=3,lt=2,lw=2)


par(mfrow=c(4,1))
acf(lin$residuals,main="linear")

acf(sec$residuals,main="quadratic")
acf(cub$residuals,main="cubic")
acf(mod2$resid,na.action=na.pass,main="AR(1)")


#*********************************************************


#*********************************************************




#Slide 22 script  Tests for smoothers

### Friedman's superSmoother: running lines smoother
## Running lines are symmetric with k/2 points 
#  on each side of the predicted point
#   and values of k as 0.5*n, 0.2*n and 0.05*n where n= number of data points
#   if span is specified, a single smoother with span*n is used

# see the details in help file for supsmu

par(mfrow=c(1,1))
plot(t,y)
u1=supsmu(t,y)
y1=u1$y
lines(y1,lt =1, lw=2) # default
## check acf acf(u$y)

u2=supsmu(t,y,span=0.01)
y2=u2$y
lines(y2,lt=3,lw=3,col=2)
names(u2)
###acf(y2)


###    Span = 0.05 
u3=supsmu(t,y,span=0.05)
y3=u3$y
lines(y3,lt=3,lw=3,col=3)

# **************************************

### To see the effect of smoothing for ech span value
### acf(y1)
### D= ar(y1)  
##    acf(D$resid, na.action=na.pass)
##****************************************

u4=supsmu(t,y,span=0.1)
y4=u4$y
lines(y4,lt=3,lw=3,col=4)



u5=supsmu(t,y,span=0.5)
y5=u5$y
lines(y5,lt=3,lw=3,col=5)
#  Check acf for each case:

A1=acf(y1)

A2=acf(y2)
A3=acf(y3)
A4=acf(y4)
A5=acf(y5)
B1=ar(y1)
C1=acf(B1$resid,na.action=na.pass)
B2=ar(y2)
C2=acf(B2$resid,na.action=na.pass)
B3=ar(y3)
C3=acf(B3$resid,na.action=na.pass)
B4=ar(y4)
C4=acf(B4$resid,na.action=na.pass)
B5=ar(y5)
C5=acf(B5$resid,na.action=na.pass)




par(mfrow=c(1,2))

plot(A1); plot(C1,main="Default Span")
plot(A2);plot(C2,main="Span= 0.01")
plot(A3);plot(C3,main="Span= 0.05")
plot(A4);plot(C4,main="Span= 0.1")
plot(A5);plot(C5,main="Span= 0.5")




par(mfrow=c(1,1))

### Kernel smnoothing, slide 22 ############################
plot(t,y)
lines(y1,lt =1, lw=2) 
lines(y2,lt=3,lw=3,col=2)
lines(y3,lt=3,lw=3,col=3)
lines(y4,lt=3,lw=3,col=4)
lines(y5,lt=3,lw=3,col=5)



k1=kernel("daniell" ,3)
u1=kernapply(y,k1)
lines(u1,lw=4)
acf(u1)
acf(ar(u1)$resid,na.action=na.pass)
####...................................


#Plot spline and lowess Slide #26 ###########################
## Local smoothers

plot(t,y)
lw=lowess(y,f=2/3)
smsp=smooth.spline(t,y,cv=TRUE)
names(smsp)

lines(lowess(y,f=2/3),lt=2)
lines(smooth.spline(t,y,cv=TRUE))
#Plot residuals after smoothing
reslw=lw$y-y
acf(reslw)
ressm=smsp$y-y
acf(ressm)


#*******************************************

#get residuals of AR(1) fit to y

mod3=ar(y,aic=FALSE,order.max=1)
summary(mod3)
mod3$x.mean
new=mod3$resid
new=c(new[-1])
tt=c(t[-100])
plot(tt,new,type="line")
lines(tt,new)
mod3l=lm(new~tt, na.action=na.exclude)
summary(mod3l)
abline(mod3l$coefficients)

acf(new,na.action=na.pass)
#************


#Man-Kendall
n=99
P=0
M=0
Z=0
for (i in 1:(n-1))
  for (j in (i+1):n)
    if (new[i] - new[j]>0) M=M+1  else P=P+1
S=P-M
S
#Calculate Z
V=(n/18)*(n-1)*(2*n+5)
V 
if(S>0) u=-1 else u=1
Z=(S+u)/sqrt(V)
Z
if(Z> 0) p.value = (1-pnorm(Z,0,1))*2 else p.value = pnorm(Z,0,1)*2
p.value
#********************************
# Alternate model: trend and AR(1)
# y(t)= a+ bt +cy(t-1)
yy=c(y[-1])
yy
yyy=c(y[-100])
mod4=lm(yy~tt+yyy)
summary(mod4)
acf(mod4$residuals)


#*********************************************************



############### Rainfall Counts Example ###############

# Libraries #
library(MASS)
library(relaimpo)
library(leaps)
library(gdata)
library(Kendall)
library('rjags')
library(R2jags)
library("rworldmap")
library(maps)
library(maptools)
library(RColorBrewer)
library(classInt)
library(gpclib)
library(mapdata)


# rainfall counts data #
rainfall_data = read.table("Rjagdata.txt",header=F,sep=",")

# time trend data #
time = 1900:2014

# global temperature data #
globaltemp = as.matrix(read.table("Temp.txt",header=F))

# climate data #
enso = as.matrix(read.table("ENSO.txt",header=F))
nao  = as.matrix(read.table("NAO.txt",header=F))
amo  = as.matrix(read.table("AMO.txt",header=F))
pdo  = as.matrix(read.table("PDO.txt",header=F))

# Lat long data #
lat = as.matrix(read.table("lat.txt",sep=",",header=F))
long = as.matrix(read.table("lon.txt",header=F,sep=","))


## Simple Trend Model ##

nstations = ncol(rainfall_data)
nyrs = nrow(rainfall_data)

betapval_time = matrix(NA,nrow=nstations,ncol=1)
beta_time = matrix(NA,nrow=nstations,ncol=1)

for (i in 1:nstations)
{
  y = rainfall_data[,i]
  
  mod0 = glm(y ~ time, family = poisson())
  betapval_time[i,1] = coef(summary(mod0))["time","Pr(>|z|)"]
  beta_time[i,1] = mod0$coefficients[2]
}

dum = which(betapval_time < 0.01)

plotvar <- beta_time

nclr <- 7 # Define number of colours to be used in plot
plotclr <- brewer.pal(nclr,"RdBu") # Define colour palette to be used

# Define colour intervals and colour code variable for plotting
class <- classIntervals(plotvar, nclr, style = "pretty")
colcode <- findColours(class, plotclr)

map('usa')
points(long, lat, pch = 16, col= colcode, cex = 1)
points(long[dum],lat[dum],cex=1,lwd=3)
title("Trend in Rainfall Counts")
legend("bottomleft", legend = names(attr(colcode, "table")), fill = attr(colcode, "palette"), cex = 0.7, bty = "n")


# some stations: 304, 250, 10
i = 500

map('usa')
points(long, lat, pch = 16, col= colcode, cex = 1)
points(long[dum],lat[dum],cex=1,lwd=3)
points(long[i],lat[i],cex=2,col="red",pch=13,lwd=2)


y = rainfall_data[,i]
# y for acf model (if required)
yacf = c(NA,y[1:(length(y)-1)])

# Step 1: Exploratory Analysis # 

# 1.1 Smoothed scatterplot #

plot(time,y,font=2,type="o",col="grey")
abline(lm(y~time))
lines(lowess(time,y),col="red")
lines(lowess(y,f=2/3),lt=2)
lines(smooth.spline(time,y,cv=TRUE))
# shows some trend # 

# 1.2 acf plot #
acf(y)
# no auto-correlation

# 1.3 spectrum #
spec.ar(y)
spec.pgram(y)
# no periodicity

# model 0
mod0 = glm(y ~ time, family = poisson())
summary(mod0)
plot(mod0)
acf(residuals(mod0))

# model 1 : test with global average temperature #
mod1 = glm(y ~ globaltemp, family = poisson())
summary(mod1)
plot(mod1)
acf(residuals(mod1))

# model 2: test with climate predictors #
mod2 = glm(y ~ enso + nao + pdo + amo, family = poisson())
summary(mod2)
plot(mod2)
acf(residuals(mod2))

# model 3: test with GST and climate predictors #
mod3 = glm(y ~ globaltemp + enso + nao + pdo + amo, family = poisson())
summary(mod3)
plot(mod3)
acf(residuals(mod3))

# model 4: test with acf #
mod4 = glm(y ~ yacf, family = poisson())
summary(mod4)
plot(mod4)
acf(residuals(mod4))

# model 5: test with acf + GST + Climate #
mod5 = glm(y ~ yacf + globaltemp + enso + nao + pdo + amo , family = poisson())
summary(mod5)
plot(mod5)
acf(residuals(mod5))

R = residuals(mod5)
plot.ts(R)
plot(time,R,font=2,type="o",col="grey")
abline(lm(R~time))
lines(lowess(time,R),col="red")

mod4 = glm(R ~ time, family = gaussian())
summary(mod4)




########### Nepal NDVI Data ##########
library(R.matlab)

ndvi = readMat("ndvi_nepal.mat")

ndvi_data = ndvi$ndvi.all

i = 1

time = 1:828
y = ndvi_data[i,]

# y for acf model (if required)
yacf = c(NA,y[1:(length(y)-1)])

# Step 1: Exploratory Analysis # 

# 1.1 Smoothed scatterplot #

plot(time,y,font=2,type="o",col="grey")
abline(lm(y~time))
lines(lowess(time,y),col="red")
lines(lowess(y,f=2/3),lt=2)
lines(smooth.spline(time,y,cv=TRUE))
# shows some trend # 

# 1.2 acf plot #
acf(y)
# no auto-correlation

# 1.3 spectrum #
spec.ar(y)
spec.pgram(y)
# no periodicity

# model 0
mod0 = glm(y ~ time, family = gaussian())
summary(mod0)
plot(mod0)
acf(residuals(mod0))

# model 1 : test with periodicity ; there are 3 cycles, 8, 12 and 24 months #

# define periodic modes of variability
f1 = (45)*(pi/180)    # frequency of interannual variability term    -- 360/45 = 8 month cycle
f2 = (30)*(pi/180)    # frequency of decadal variability term        -- 360/30 = 12 month cycle
f3 = (15)*(pi/180)    # frequency of multidecadal variability term   -- 360/15 = 24 month cycle


p1 = sin(f1*time)
p2 = sin(f2*time)
p3 = sin(f3*time)

mod1 = lm(y ~ time + yacf + p3 + p4 + p5)
summary(mod1)
R = residuals(mod1)
acf(R)
plot(mod1)

plot(time,R,font=2,type="o",col="grey")
abline(lm(R~time))
lines(lowess(time,R),col="red")
lines(lowess(R,f=2/3),lt=2)
lines(smooth.spline(time,R,cv=TRUE))
acf(R)

### all grids ###
ngrids = nrow(ndvi_data)
nmonths = ncol(ndvi_data)

time = 1:828

betapval_time = matrix(NA,nrow=ngrids,ncol=1)
beta_time = matrix(NA,nrow=ngrids,ncol=1)

for (i in 1:ngrids)
{
  y = ndvi_data[i,]
  # y for acf model (if required)
  yacf = c(NA,y[1:(length(y)-1)])
  
  mod1 = lm(y ~ time + yacf + p3 + p4 + p5)
  betapval_time[i,1] = coef(summary(mod1))["time","Pr(>|t|)"]
  beta_time[i,1] = mod1$coefficients[2]
}

dum = which(betapval_time < 0.01)

plotvar <- beta_time

nclr <- 7 # Define number of colours to be used in plot
plotclr <- brewer.pal(nclr,"RdBu") # Define colour palette to be used

# Define colour intervals and colour code variable for plotting
class <- classIntervals(plotvar, nclr, style = "pretty")
colcode <- findColours(class, plotclr)

plot(ndvi$xp, ndvi$yp, pch = 16, col= colcode, cex = 1,xlab="Longitude",ylab="Latitude")
points(ndvi$xp[dum],ndvi$yp[dum],cex=1,lwd=3)
title("Trend in NDVI")
legend("bottomleft", legend = names(attr(colcode, "table")), fill = attr(colcode, "palette"), cex = 0.7, bty = "n")

# plot with elevation #
plot(ndvi$zp,beta_time,xlab="Elevation",ylab="Time Trend",font=2)
points(ndvi$zp[dum],beta_time[dum],col="red")




# ----- Define a function for plotting a matrix ----- #
myImagePlot <- function(x, ...){
  min <- min(x)
  max <- max(x)
  yLabels <- rownames(x)
  xLabels <- colnames(x)
  title <-c()
  # check for additional function arguments
  if( length(list(...)) ){
    Lst <- list(...)
    if( !is.null(Lst$zlim) ){
      min <- Lst$zlim[1]
      max <- Lst$zlim[2]
    }
    if( !is.null(Lst$yLabels) ){
      yLabels <- c(Lst$yLabels)
    }
    if( !is.null(Lst$xLabels) ){
      xLabels <- c(Lst$xLabels)
    }
    if( !is.null(Lst$title) ){
      title <- Lst$title
    }
  }
  # check for null values
  if( is.null(xLabels) ){
    xLabels <- c(1:ncol(x))
  }
  if( is.null(yLabels) ){
    yLabels <- c(1:nrow(x))
  }
  
  layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))
  
  # Red and green range from 0 to 1 while Blue ranges from 1 to 0
  
  
  ColorRamp <- rgb( seq(0,1,length=256),  # Red
                    seq(0,1,length=256),  # Green
                    seq(1,0,length=256))  # Blue
  ColorLevels <- seq(min, max, length=length(ColorRamp))
  
  # Reverse Y axis
  reverse <- nrow(x) : 1
  yLabels <- yLabels[reverse]
  x <- x[reverse,]
  
  # Data Map
  par(mar = c(3,5,2.5,2))
  image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="",
        ylab="", axes=FALSE, zlim=c(min,max))
  if( !is.null(title) ){
    title(main=title)
  }
  axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
  axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,
       cex.axis=0.7)
  
  # Color Scale
  par(mar = c(3,2.5,2.5,2))
  image(1, ColorLevels,
        matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
        col=ColorRamp,
        xlab="",ylab="",
        xaxt="n")
  
  layout(1)
}
# ----- END plot function ----- #






