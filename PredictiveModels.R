
# Exhaustive Model Selection #
x1 <-rnorm(100,0,1)
x3<-rnorm(100,0,1)
x4<-rnorm(100,0,1)
x2<-0.8*x1+rnorm(100,0,1)
y <- 0.5 + 0.1*x1 + 0.2*x2 + 0.3*x3 + 0.1*x4 + rnorm(100,0,0.5) 

x<-cbind(x1,x2,x3,x4)

min.yr = 1; max.yr = 100

pred.select(x,y,min.yr,max.yr,model="LM") #[ or replace LM with "KNN"]

############## Predicting Frogs ##############

# Experiment with GLM using logistic transform
# from Data Analysis and Graphics Using R
# John Maindonald & John Braun

#Data from library DAAG
library(DAAG)
data(frogs)
plot(northing~easting,data=frogs,pch=c(1,16)[frogs$pres.abs+1],
     xlab="Meters east of reference point",ylab="Maters North",font=2)

pairs(frogs[,4:10],oma=c(2,2,2,2),cex=0.5,font=2)
# see the data
frogs

# The distributions of explanatory variables:

par(mfrow=c(3,3))
for (nam in c("distance","NoOfPools","NoOfSites") )
{
  y=frogs[,nam]
  plot(density(y),main=" ",xlab=nam)
}
# sqrt transform
for (nam in c("distance","NoOfPools","NoOfSites") )
{
  y=sqrt(frogs[,nam])
  z=paste("sqrt(",nam, ")")
  plot(density(y),main="",xlab=z)
}

# and logs
for (nam in c("distance","NoOfPools") )
{
  y=log(frogs[,nam])
  z=paste("log(",nam, ")")
  plot(density(y),main="",xlab=z)
}
y=log(frogs[,"NoOfSites"]+1)
z="log(NoOfSites +1)"
plot(density(y),main="",xlab=z)

attach(frogs)
pairs(cbind(altitude,log(distance),log(NoOfPools),NoOfSites),
      panel=panel.smooth,labels=c("altitude","log(distance)",
                                  "log(NoOfPools)","NoOfSites"))
detach(frogs)


y=(frogs[,"NoOfSites"])^2
z="(NoOfSites2)"
plot(density(y),main="",xlab=z)

# simple model
frogs.glm0=glm(formula=pres.abs~altitude+log(distance)
               +log(NoOfPools) + NoOfSites+avrain+meanmin+
                 meanmax,family=binomial,data=frogs)

summary(frogs.glm0)

# consider the variables with small p values, also meanmax

frogs.glm=glm(formula=pres.abs~log(distance)+log(NoOfPools)+
                meanmin+meanmax,family=binomial,data=frogs)
summary(frogs.glm)

# Cross Validation #

#the frogs presence absence example with  bootstrap

ns=100 # no of bootstrap samples

nf=10 # of obs reserved for prediction

nfit=212-nf

xpred=matrix(nr=ns,nc=nf)

ind1=1:212

ind =  sample(ind1,nf,replace=F)

ff=frogs[ind,]

ind2=setdiff(ind1,ind)

ff0=frogs[ind2,]

# fit and predict original sample
sglm=step(glm(pres.abs ~ altitude + avrain + distance + easting + 
                    meanmax + meanmin + NoOfPools + NoOfSites + northing, 
                  family=binomial(logit), data=ff0))

predorig=plogis(predict(sglm,ff))

#above breaks into a predict piece and a fit piece   and then predicts


for (i in 1:ns){
  fff=ff0[sample(1:202,202,replace=T),]

  sglm=step(glm(pres.abs ~ altitude + avrain + distance + easting + 
                      meanmax + meanmin + NoOfPools + NoOfSites + northing, 
                    family=binomial(logit), data=fff))
  xpred[i,]=plogis(predict(sglm,ff))}

xm=colMeans(xpred)
plot(1:nf,predorig,pch="o",ylim=c(0,1))
lines(1:nf,xm,lwd=3)
for (i in 1:ns)points(1:nf,xpred[i,],col="red",pch=i,cex=0.5)

#above shows uncertainty across bootstrap predictions and mean of bootstrap

############## Predicting CSO ################

cso_data = data.frame(read.csv("CSO data_Log.csv",header=T))

y = cso_data[,12]


# simple model
cso.glm0 = glm(formula = cso_data$rain_occurance ~ cso_data$Hours + cso_data$Inches + cso_data$Volume.in.Storage.at.12AM, family=binomial)
summary(cso.glm0)
pred_cso = plogis(predict(sglm))

plot(pred_cso,type="l",col="red")
points(cso_data$rain_occurance,type="l",col="grey")


############## Subway Ridership Models ##############
setwd("/Users/devineni/work/teaching/cuny/advanced_data_analysis/Spring2017/lectures/4_Models/example1_ridershipmodels/")

# Read the data files #
  daily_rain = read.csv("dailyrain.csv")
  daily_ridership = read.csv("subwayridership_daily.csv")
  station_data = read.csv("stations.csv")

# Fit the models for 1 stations - test#
  y = daily_ridership$V61
  x1 = daily_rain$V61
  x2 = daily_ridership$Weekend
  x3 = x1*x2
  
  par(mfrow=c(1,2))
  plot(x1,y)
  plot(x2,y)
  
  
  mod0 = glm(y~x1+x2+x3,family="poisson")
  summary(mod0)
  
  mod1 = step(glm(y~x1+x2+x3,family="poisson"))
  
  plot(y,col="grey",type="l")
  points(pred_ride,type="l",lty=2)
############## Subway Ridership Models ##############

  
  
  
############## LOCFIT MODELS ##############
  setwd("/Users/devineni/work/teaching/cuny/advanced_data_analysis/Spring2017/lectures/4_Models/")
  
  library(locfit)
  
  floods = read.csv("MRB_floods.csv",header=T)
  attach(floods)
  
  plot(GPHd, Qf)
  lines(locfit(Qf ~ GPHd), lwd=3)
  
  # now check effect of changing the bandwidth
  a = seq(0.1,1,.1)
  gcv_save = a
  for (i in 1 : 5)
  {
    
    lines(locfit(Qf ~ GPHd,alpha=a[i]),lty=i)
    d = gcv(Qf ~ GPHd,alpha=a[i])
    gcv_save[i] = d[4]
  }
  plot(a,gcv_save,type="o")
  
  mod = locfit(Qf ~ GPHd, alpha = 0.5)
  plot(GPHd, Qf)
  lines(mod, lwd=3)
  
  GPHd_new = c(-0.2, 0.25, 0.5)
  Qf_pred = preplot(mod, GPHd, where="fitp", se.fit=TRUE,band="pred")
  
  plot(GPHd, Qf)
  plot(Qf_pred,add=T)

  
  ## Trying a different family ##
  mod = locfit(Qf ~ GPHd, family = "log", alpha = 0.5)
  plot(GPHd, Qf)
  lines(mod, lwd=3)
  
  GPHd_new = c(-0.2, 0.25, 0.5)
  Qf_pred = preplot(mod, GPHd, where="fitp", se.fit=TRUE,band="pred")
  
  plot(GPHd, Qf)
  plot(Qf_pred,add=T)
  
  plot(Qf,type="o")
  lines(exp(Qf_pred$fit),col="red")
  
  #LOCFIT EXAMPLES
  attach(ethanol)
  plot(E,NOx);lines(locfit(NOx~E),lwd=3)
  # now check effect of changing the bandwidth
  for (i in 1 : 5){a=0.2*i;lines(locfit(NOx~E,alpha=a),lty=i)}
  cpplot(NOx~E,alpha=seq(0.1,1,0.1))
  gcvplot(NOx~E,alpha=seq(0.1,1,0.1))
  
  #made up data
  x=rnorm(100);y=x^2+rnorm(100);plot(x,y)
  
  for (i in 1:5){a=0.2*i;lines(locfit(y~x,alpha=a),lty=i)}
  cpplot(y~x,alpha=seq(0.1,1,0.1))
  gcvplot(y~x,alpha=seq(0.1,1,0.1))
  summary(gcvplot(y~x,alpha=seq(0.1,1,0.1)))
  plot(seq(0.1,1,0.1),summary(gcvplot(y~x,alpha=seq(0.1,1,0.1)))[,2],type="o")
      
  
  #example with circular /periodic data  -- this will fit the best seasonal cycle for instance but not vary its amplitude from year to year
  # generate an x variable, and a response with period 0.2
  x <- seq(0,1,length=200)
  y <- sin(10*pi*x)+rnorm(200)/5
  # compute the periodic local fit. Note the scale argument is period/(2pi)
  fit <- locfit(y~ang(x,scale=0.2/(2*pi)))
  plot(fit,xlim=c(0,1),ylim=c(-1.5,1.5))  ;points(x,y)
  
  #now let us try a crazier function
  
  y <- x*sin(10*pi*x)+rnorm(200)/5
  fit <- locfit(y~ang(x,scale=0.2/(2*pi)))
  plot(fit,xlim=c(0,1),ylim=c(-1.5,1.5))  ;points(x,y)
  lines(locfit(y~x),lwd=2)
  gcvplot(y~x,alpha=seq(0.1,1,0.1))
  plot(fit,xlim=c(0,1),ylim=c(-1.5,1.5))  ;points(x,y)
  lines(locfit(y~x,alpha=0.2),col="red",lwd=2)
  
  
  
  ######## Knn ############
  Qf_knn = knnreg(Qf,GPHd,GPHd,15)
  plot(Qf,type="o")
  lines(Qf_knn,col="red")
  
  # ethanol data #
  x = cbind(E,C)
  ethanol_knn = knnreg(NOx,x,x,5)
  
  plot(E,NOx)
  plot(NOx,ethanol_knn)
  
  points(E,ethanol_knn,col="red")
  

  
  ######## Missing Data Imputation ############
  
  original_sample_mu = 10
  original_sample_sd = 1
  n = 100
  nmiss = 10
  
  x = rnorm(n,original_sample_mu,original_sample_sd)
  xmiss = x
  
  index = 1:n
  randomsample = sample(index,nmiss)
  xmiss[randomsample]=NA
  
  # parameters #
  
  
  sample_mean = mean(xmiss,na.rm=T)
  sample_sd = sd(xmiss,na.rm=T)
  
  nmiss = length(randomsample)
  dum = which(is.na(xmiss)==T)
  
  niter = 100
  
  tolerance = matrix(NA,nrow=niter,ncol=1)
  
  xfill_init = rnorm(nmiss, sample_mean, sample_sd)
  
  for (i in 1:niter)
  {
    xfill = rnorm(nmiss, sample_mean, sample_sd)
    xmiss[dum] = xfill
    sample_mean_new = mean(xmiss)
    sample_sd_new = sd(xmiss)
    tolerance[i] = sum((xfill_init - xfill)^2)
    
    sample_mean = sample_mean_new
    sample_sd = sample_sd_new
    xfill_init = xfill
  }
  
  par(mfrow=c(1,2))
  plot(tolerance); lines(lowess(tolerance),col="red")
  plot(x,xmiss)
  points(x[dum],xmiss[dum],col="red")
  
  sample_mean
  sample_sd
  
  
  
  library(mice)
  library(missForest)
  
  #load data
  data <- iris
  
  #Get summary
  summary(iris)
  
  #Generate 10% missing values at Random 
  iris.mis <- prodNA(iris, noNA = 0.1)
  
  #Check missing values introduced in the data
  summary(iris.mis)
  
  
  #remove categorical variables
  iris.mis <- subset(iris.mis, select = -c(Species))
  summary(iris.mis)
  
  md.pattern(iris.mis)
  
  library(VIM)
  mice_plot <- aggr(iris.mis, col=c('navyblue','yellow'),
                      numbers=TRUE, sortVars=TRUE,
                      labels=names(iris.mis), cex.axis=.7,
                      gap=3, ylab=c("Missing data","Pattern"))
  
  imputed_Data <- mice(iris.mis, m=5, maxit = 50, method = 'pmm', seed = 500)
  summary(imputed_Data)
  
  
  #check imputed values
  imputed_Data$imp$Sepal.Width
  
  #get complete data ( 2nd out of 5)
  completeData <- complete(imputed_Data,2)
  
  plot(iris$Sepal.Length,completeData$Sepal.Length)
  
  
  
  
  ############################# GIBBS SAMPLER ########################
  x <- c (1,2,3,4,5,6,7,8,9,10)
  y <- c (1,1,1,0,1,1,0,0,0,0)
  n <- length (y)
  sigma <- 0.5
  n.iter <- 100
  a <- rnorm (1,0,10)  # starting value
  b <- rnorm (1,0,10)  # starting value
  plot(a,b)
  for (i in 1:n.iter){
    var.a <- 1/(1/10^2 + n/sigma^2)
    E.a <- (sum(y-b*x)/sigma^2)*var.a
    a <- rnorm (1, E.a, sqrt(var.a))
    var.b <- 1/(1/10^2 + sum((x/sigma)^2))
    E.b <- (sum((y-a)*x)/sigma^2)*var.b
    b <- rnorm (1, E.b, sqrt(var.b))
    cat (paste ("a =", a, "b =", b, "\n"))
    points(a,b,col="red")
  }
  
  
  gibbs1 <- function (x, y, sigma, n.iter){
    parameters <- c ("a","b")
    output <- array (NA, c(n.iter, length(parameters)))
    colnames (output) <- parameters
    n <- length (y)
    a <- rnorm (1,0,10)  # starting value
    b <- rnorm (1,0,10)  # starting value
    for (i in 1:n.iter){
      var.a <- 1/(1/10^2 + n/sigma^2)
      E.a <- (sum(y-b*x)/sigma^2)*var.a
      a <- rnorm (1, E.a, sqrt(var.a))
      var.b <- 1/(1/10^2 + sum((x/sigma)^2))
      E.b <- (sum((y-a)*x)/sigma^2)*var.b
      b <- rnorm (1, E.b, sqrt(var.b))
      output[i,] <- c (a, b)
    }
    return (output)
  }
  x<-c(1,2,3,4,5,6,7,8,9,10); y<-c(1,1,1,0,1,1,0,0,0,0); sigma<-.5
  out = gibbs1 (x, y, sigma, 100)
  plot(out)
  plot(out[51:100,])
  par(mfrow=c(2,1))
  plot(out[,1])
  plot(out[,2])
  
  
  
  ############################# Running Multilevel Models ########################
  library(runjags)
  library(rjags)
  library(R2jags)
  library(MCMCpack)
  library(parallel)
  library(mcmcplots)
  library(superdiag)
  
  # Create Predictors #
   N = 100
  x1 = rnorm(N, mean = 5, sd = 2)
  x2 = rnorm(N, mean = 5, sd = 2)
  e = rnorm(N, mean = 0, sd = 1)
  
  # Create Model: y = a + b1*x1 + b2*x2 + e
  b1 = 1.2
  b2 = -3.1
  a = 1.5
  
  y = a + b1*x1 + b2*x2 + e
  
  mod1 = lm(y~x1)
  AIC(mod1)
  
  mod2 = lm(y~x1+x2)
  AIC(mod2)
  
  ## Jags format for input dat ##
  predictand = as.matrix(y)
  predictor = as.matrix(cbind(x1,x2))
  ntimes = N
  npred = 2
  
  jags.data <- list('predictand' = predictand,'predictor' = predictor,'ntimes'=ntimes,'npred'=npred)
  jags.params <- c('predictand.rep','mu.predictand','sigma.predictand','alpha','betas')
  jags.inits <- NULL
  
  #=============#
  # using jags  #
  #=============#
  jagsfit <- jags(data=jags.data, inits=jags.inits, jags.params,
                  n.iter=2000, model.file='model.bug')
  
  # attach jags object into search path see "attach.bugs" for details
  attach.jags(jagsfit)
  
  hist(alpha); 
  
  abline(v = 1.5,col="red",lwd=2)
  hist(betas[,1,1]); abline(v = 1.2,col="red",lwd=2)
  hist(betas[,1,2]); abline(v = -3.1,col="red",lwd=2)
  plot(betas[,1,1])
  
  
  ######### Experiment to show partial pooling  #########
  
  # Create Predictors #
  N = 100
  
  # single predictor #
  x = rnorm(N, mean = 5, sd = 2)
  e = rnorm(N, mean = 0, sd = 1)
  
  # Create Models: y1 = a1 + b1*x1  + e1
  a1 = 1.5
  a2 = 1.6
  a3 = 1.7
  a4 = 1.8
  a5 = 1.9
  a6 = 2.0
  
  b1 = 0.8
  b2 = 0.9
  b3 = 1.0
  b4 = 1.1
  b5 = 1.2
  b6 = 1.15
  
  y1 = a1 + b1*x + e
  y2 = a2 + b2*x + e
  y3 = a3 + b3*x + e
  y4 = a4 + b4*x + e
  y5 = a5 + b5*x + e
  y6 = a6 + b6*x + e
 
  plot(x,y1); abline(lm(y1~x)) 
  points(x,y2,col="red"); abline(lm(y2~x),col="red") 
  points(x,y3,col="blue"); abline(lm(y3~x),col="blue") 
  points(x,y4,col="grey"); abline(lm(y4~x),col="grey") 
  points(x,y5,col="green"); abline(lm(y5~x),col="green") 
  points(x,y6,col="pink"); abline(lm(y6~x),col="pink") 
  
  
  ## Call Jags No Pooling ##
  
  ## Jags format for input dat ##
  predictand = as.matrix(cbind(y1,y2,y3,y4,y5,y6))
  predictor = as.matrix(cbind(x))
  ntimes = N
  npred = ncol(predictor)
  nsites = ncol(predictand)
  
  jags.data <- list('predictand' = predictand,'predictor' = predictor,'ntimes'= ntimes,'npred'=npred, 'nsites' = nsites)
  jags.params <- c('predictand.rep.np','sigma.predictand.np','alpha.np','betas.np')
  jags.inits <- NULL
  
  #=============#
  # using jags  #
  #=============#
  jagsfit.np <- jags(data=jags.data, inits=jags.inits, jags.params,
                  n.iter=2000, model.file='model_nopool.bug')
  
  # attach jags object into search path see "attach.bugs" for details
  attach.jags(jagsfit.np)
  slopes.np = betas.np
 
  boxplot(alpha.np[,,1])
  boxplot(betas.np[,,1])
  
  
  ## Call Jags Full Pooling ##
  
  ## Jags format for input dat ##
  predictand = as.matrix(cbind(y1,y2,y3,y4,y5,y6))
  predictor = as.matrix(cbind(x))
  ntimes = N
  npred = ncol(predictor)
  nsites = ncol(predictand)
  
  jags.data <- list('predictand' = predictand,'predictor' = predictor,'ntimes'= ntimes,'npred'=npred, 'nsites' = nsites)
  jags.params <- c('predictand.rep.fp','sigma.predictand.fp','alpha.fp','betas.fp')
  jags.inits <- NULL
  
  #=============#
  # using jags  #
  #=============#
  jagsfit.fp <- jags(data=jags.data, inits=jags.inits, jags.params,
                     n.iter=2000, model.file='model_fullpool.bug')
  
  # attach jags object into search path see "attach.bugs" for details
  attach.jags(jagsfit.fp)
  slopes.fp = betas.fp
  
  boxplot(alpha.fp[,1])
  abline(h = c(a1,a2,a3,a4,a5,a6),lty=4)
  boxplot(betas.fp[,1],ylim=c(0.8,1.3))
  abline(h = c(b1,b2,b3,b4,b5,b6),lty=4)
  
  
  
  
  ## Call Jags Partial Pooling ##
  
  ## Jags format for input dat ##
  predictand = as.matrix(cbind(y1,y2,y3,y4,y5,y6))
  predictor = as.matrix(cbind(x))
  ntimes = N
  npred = ncol(predictor)
  nsites = ncol(predictand)
  
  jags.data <- list('predictand' = predictand,'predictor' = predictor,'ntimes'= ntimes,'npred'=npred, 'nsites' = nsites)
  jags.params <- c('predictand.rep.pp','sigma.predictand.pp','alpha.pp','betas.pp','mu.beta.pp')
  jags.inits <- NULL
  
  #=============#
  # using jags  #
  #=============#
  jagsfit.pp <- jags(data=jags.data, inits=jags.inits, jags.params,
                     n.iter=2000, model.file='model_parpool.bug')
  
  # attach jags object into search path see "attach.bugs" for details
  attach.jags(jagsfit.pp)
  slopes.pp = betas.pp
  
  boxplot(alpha.pp)
  points(c(a1,a2,a3,a4,a5,a6),col="red",pch=20)
  boxplot(betas.pp[,,1])
  points(c(b1,b2,b3,b4,b5,b6),col="red",pch=20)
  
  
  # All slopes #
  par(mfrow=c(1,3))
  boxplot(slopes.np[,,1])
  boxplot(slopes.pp[,,1])
  boxplot(cbind(slopes.fp,mu.beta.pp))
  
  boxplot(cbind(slopes.np[,1,1],slopes.pp[,1,1]))
  
  ## Real Data -- Network Model ##
  
  floods = read.csv("MRB_floods.csv",header=T)
  attach(floods)
  
  floods1 = floods[14:49,]
  attach(floods1)

  
  y = log(Fd+1)
  x1 = log(Qf)
  z1 = GPHd
  z2 = log(WV)
  z3 = DIV
  
  ## Call Jags Bayes Network ##
  
  ## Jags format for input dat ##
  y = as.matrix(log(Fd+1))
  x1 = as.matrix(log(Qf))
  z1 = as.matrix(GPHd)
  z2 = as.matrix(log(WV))
  z3 = as.matrix(DIV)
  nyrs = length(y)
 
  jags.data <- list('y' = y,'x1' = x1, 'z1' = z1, 'z2' = z2, 'z3' = z3, 'nyrs'= nyrs)
  jags.params <- c('y.rep','x1.rep','alpha','beta','a','b1', 'b2', 'b3')
  jags.inits <- NULL
  
  #=============#
  # using jags  #
  #=============#
  jagsfit.network <- jags(data=jags.data, inits=jags.inits, jags.params,
                     n.iter=5000, model.file='network_model.bug')
  
  # attach jags object into search path see "attach.bugs" for details
  attach.jags(jagsfit.network)
  plot(jagsfit.network)
  
  boxplot(y.rep[,,1])
  lines(y,col="red")
  
  boxplot(x1.rep[,,1])
  lines(x1,col="red",type="o")
  
  
  
  
    #=====================
  # Function KNN 
  #
  #=====================
  
  knnreg=function(y,x,xtest,k,w=NULL){
    x=as.matrix(x)
    xtest=as.matrix(xtest)
    y=as.matrix(y)
    if(is.null(w))w=rep(1,ncol(x))
    if(nrow(y)!= nrow(x))
      print('error: lengths of y and x differ')
    if(ncol(x)!= ncol(xtest))
      print('error: col lengths of x and xtest differ')
    
    na=rep(NA,nrow(xtest))
    yknn=matrix(NA,nrow(xtest),ncol(y))
    
    yk=na
    
    yr=seq(1,nrow(y))
    
    for(i in 1:nrow(xtest)){
      a=matrix(NA,nrow(x),ncol(x))
      for(n in 1:ncol(x))
        a[,n]=100*w[n]*(x[,n]- xtest[i,n])^2
      
      c=rowSums(a,na.rm=T)
      
      yk[rank(c,ties.method='first')]=yr
      
      j=rank(c)		#equal prob for equidistant neighbours
      sj=sum(j[1:k]^(-1))
      pj=(j[1:k]^(-1))/sj
      
      ynp=sample(yk[1:k],1000,replace=T,prob=pj)
      for(p in 1:ncol(y)) 
        yknn[i,p]=mean(y[ynp,p],na.rm=T)
      
    }
    return(yknn)
  }
  #=====================
  
  
  ## More analysis ##
  
  qf = as.matrix(read.csv("qf.csv",header=T))
  gph = as.matrix(read.csv("gph.csv",header=T))
  
  plot(rev(gph[,4:40]),rev(qf[,4:40]))
  
  gph_1 = apply(gph[,4:40], 2, rev) 
  qf_1 =  apply(qf[,4:40], 2, rev)  
  
  i = 40
  par(mfrow=c(2,1))
  plot(gph_1[i,],type="l"); abline(h=0)
  lines(qf_1[i,],col="red")
  plot(gph_1[i,],qf_1[i,]); abline(lm(qf_1[i,]~gph_1[i,]))
  
  