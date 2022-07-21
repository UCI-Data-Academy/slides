### Resampling and Simulation Based Statistics 
##### COSMOS 2022
#### R Code by Sam Behseta and Babak Shahbaba


### Back to vol or NACCICV 
#### vol is highly symmetric
length(vol)

hist(vol,xlab="Volume",breaks=10)

vol.m=mean(vol)
vol.sd=sd(vol)

vol.m
vol.sd

#### let's do some probability calculations with vol
length(vol[vol>1000])
length(vol[vol<=1000])

## probability of all vol values less or equal than 1000
length(vol[vol<=1000])/length(vol)

### probability of all vol values between 1300 and 1600
l=length(vol)
length(vol[vol>=1300 & vol <=1600])/l


#### Now let's consider a theoretical normal distribution with the 
#### mean vol.m and standard deviation vol.sd. Let's calculate the similar areas under
### this theoretical curve. 
pnorm(1000,vol.m,vol.sd)

### slightly different but close! it's ok. 

pnorm(1600,vol.m,vol.sd)-pnorm(1300,vol.m,vol.sd)
#### closER!

### Now let's suppose the world was theroetical!
### let's simulate normal or Gaussian data from the 
### theoretical normal curve

vol.sim=rnorm(100000,vol.m,vol.sd)

par(mfrow=c(1,2))
hist(vol,xlab="Volume",breaks=10)
hist(vol.sim,xlab="Simulated Volume",breaks=10)

### not bad! But may be this is an illusion
### Did we do well?

par(mfrow=c(1,1))

vol.frame <- data.frame(vol)
vol.sim.frame <- data.frame(vol.sim)
colnames(vol.sim.frame)[colnames(vol.sim.frame)=="vol.sim"]<-"vol"

vol.frame$tag<-"real data"
vol.sim.frame$tag<-"simulated data"

volTot<-rbind(vol.frame,vol.sim.frame)
head(volTot)

ggplot(volTot, aes(vol, fill = tag))+ 
 geom_density(alpha = 0.5)

### what is the problem? 
### remember how sharp the ceneter of vol is?

hist(vol,xlab="Volume",breaks=15)

#### while normal provides a decent simulation
#### but real life can we more complex
#### we'll discuss that later


### there is hope!
vol.log=log(vol)
hist(vol.log,xlab="Log Volume",breaks=15)


##### before closing this simulation story, let's
##### verigy the 68-95-99.7 rule with our simulated data

m.sim=mean(vol.sim)
sd.sim=sd(vol.sim)
l.sim=100000

length(vol.sim[vol.sim>=m.sim-sd.sim & vol.sim<=m.sim+sd.sim])/l.sim
length(vol.sim[vol.sim>=m.sim-2*sd.sim & vol.sim<=m.sim+2*sd.sim])/l.sim
length(vol.sim[vol.sim>=m.sim-3*sd.sim & vol.sim<=m.sim+3*sd.sim])/l.sim



############ Simulations from the Uniform Distribution
### simulating from a uniform(0,10)
a=runif(10000,0,10)
hist(a,breaks=10)

##### discrete uniform simulation with k=10
b=seq(1,10,1)
c<-sample(b,1000000,replace=TRUE)
d=table(c)
d
barplot(d)


#### Bernoulli
### a seq of 1's and 0's with p=0.5
bern<-rbinom(1000,1,0.5)
bern.table<-table(bern)
barplot(bern.table)


###### Some simulations associated with binomial

### let's start by caluclating the proportion of females in the 
### dataset! 

### Approach 1
female<-as.factor(female)
summary(female)
table(female)
prob.female<-length(female[female==1])/length(female)
prob.female

### Approach 2
library(janitor)
tabyl(female)



#### playing a bit more with binomial 
### but first, lest's start with a simple binomial 
### with p=0.5, and n=4
dbinom(0,5,.5)
dbinom(0,5,0.5)+dbinom(1,5,0.5)
pbinom(1,5,0.5)
qbinom(0.1875,5,0.5)
pbinom(-1:5,5,0.5)
plot(0:5,dbinom(0:5,5,0.5),type="h",ylab="probability",xlab="number of successes")

