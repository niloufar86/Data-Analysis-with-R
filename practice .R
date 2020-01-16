

# Random vs. Deterministic Process # 

# Rolling a dice #
##################
dice = c(1,2,3,4,5,6) # create a dice with 6 numbers

N = 1000 # roll the dice 1000 times

# drawing a random number from the 6 numnbers in dice 1000 times with replacement #
roll1 = sample(dice,N,replace=T)  
roll2 = sample(dice,N,replace=T)

x = roll1 + roll2 # adding the numbers from the 2 rolls

y=table(x)/length(x) # prepare the output table 

barplot(y,xlab="Sum of Rolls",ylab="Probability of Occurence",font=2) # plot the frequency values

# Seasonal Temperature #
##################
ndays = 360 # number of days in 1 year
nyrs = 10   # number of years

days = 1:(ndays*nyrs) # generate days

A = 10 # amplitude of the sine wave (-10 to 10)

temperature = A*sin(days*pi/180) # generate temperature data from sine wave

plot(days,temperature,type="l",font=2)

plot(density(temperature),xlab="Temperature",font=2,main="")

# Test for independence #
plot(temperature[1:((ndays*nyrs)-1)],temperature[2:(ndays*nyrs)],ylim=c(-10,10),xlim=c(-10,10),xlab="Temperature day=t", ylab="Temperature day=t+1")
abline(v=0)

plot(temperature[1:((ndays*nyrs)-1)],temperature[2:(ndays*nyrs)],ylim=c(-1,1),xlim=c(-1,1),xlab="Temperature day=t", ylab="Temperature day=t+1")
abline(v=0)

library(scatterplot3d)

temporig = temperature

x=temperature[1:((ndays*nyrs)-2)]

z=temperature[3:(ndays*nyrs)]

y=temperature[2:((ndays*nyrs)-1)]

scatterplot3d(x, y, z, highlight.3d=TRUE)

# Log Likelihood Under Dependence #
##################

#read in the data
x=c(1,0,0,0,1,0,0,1,1,1,1,1,1,1,0,0,0,0,1,0)

n=length(x)

p=0.5

pd=c(0.6, 1/3)  

# I am defining the p11 and p10 since the others are 1-these

#initialize log likelihood
if(x[1]==1)logl=log10(p) else logl =log10(1-p)

#Note that inside the if statement you need 2= signs to denote a test for an equality
# You can have <= for less than equal to and >= for greater than equal to

#run the loop to calculate logl using the conditional probabilities
for (i in 2:n) {
  # the { denotes a section of a script and is closed by }
  if(x[i-1]==1){
    if(x[i]==1)logl=logl+log10(pd[1])    # p11
    else   logl=logl+log10(1-pd[1])}   #  p01
  # this completes the definition if the previous value is a 1
  else 
    if(x[i]==1)logl=logl+log10(pd[2])    #p10
  else   logl=logl+log10(1-pd[2])}   #p00

# this completes the definition when the previous value is a 0
# now type out logl
logl



