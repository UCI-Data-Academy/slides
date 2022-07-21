#### Central Limit Theorem

log.csfvol=log(csfvol)
hist(csfvol,breaks=20)
hist(log.csfvol,breaks=20)
mean(log.csfvol)
sd(log.csfvol)

simulations=matrix(0,nrow=1000,ncol=1000)

for(i in 1:1000)
{
  simulations[i,]<-rnorm(1000,5.83,0.18)
}

sample.mean<-numeric(1000)

sample.mean<-apply(simulations,1,mean)
hist(sample.mean,breaks=15)

mean.xbar=mean(sample.mean)
sd.xbar=sd(sample.mean)

mean.xbar
sd.xbar
0.18/sqrt(1000)


hist(sample.mean,breaks=15)
abline(v = mean.xbar,col = "red", lwd = 3)
text(x =mean.xbar+0.005 , y = mean.xbar*24, paste("Mean =",5.83),col ="red", cex = 1.4)



########
### The Idea Behind the 95% Confidence Interval 
z_star_95 <- qnorm(0.975)
z_star_95

library(statsr)
library(dplyr)
library(ggplot2)

alz<-alzheimer_data
attach(alz)

n <- 100
samp.alz <- sample_n(alz, n)
dim(samp.alz)

samp.alz %>%
  summarise(lower = mean(vol) - z_star_95 * (sd(vol) / sqrt(n)),
            upper = mean(vol) + z_star_95 * (sd(vol) / sqrt(n)))


params <- alz %>%
  summarise(mu = mean(vol))
params


ci <- alz %>%
  rep_sample_n(size = n, reps = 100, replace = TRUE) %>%
summarise(lower = mean(naccicv) - z_star_95 * (sd(naccicv) / sqrt(n)),
            upper = mean(naccicv) + z_star_95 * (sd(naccicv) / sqrt(n)))

ci %>%
  slice(1:10)

ci <- ci %>%
  mutate(capture_mu = ifelse(lower < params$mu & upper > params$mu, "yes", "no"))

ci_data <- data.frame(ci_id = c(1:100, 1:100),
                      ci_bounds = c(ci$lower, ci$upper),
                      capture_mu = c(ci$capture_mu, ci$capture_mu))


ggplot(data = ci_data, aes(x = ci_bounds, y = ci_id, 
                           group = ci_id, color = capture_mu)) +
  geom_point(size = 2) +  # add points at the ends, size = 2
  geom_line() +           # connect with lines
  geom_vline(xintercept = params$mu, color = "darkgray") # draw vertical line


######### Univariate inference with R

vol.sample=sample(vol,100,replace=FALSE)
t.test(vol.sample,mu=1376,alternative="two.sided",conf.level=0.95)


####### Or we can write our own function
my.confidence<-function(dat,conf.level)
{
  n<-length(dat)
   z_star_cl<-qnorm(1-(1-conf.level)/2)
  lower<- mean(vol) - z_star_cl * (sd(dat) / sqrt(n))
  upper<-mean(vol) + z_star_cl *(sd(dat) /sqrt(n))
            ci<-c(lower,upper)
            return(ci)
  }

my.confidence(vol.sample,0.95)


###### Inference for the population proportion
female<-as.factor(female)
fem.counts<-summary(female)[2]
prop.test(fem.counts,2700)


### Or write our own function
my.prop.confidence<-function(counts,tot,conf.level)
{
  z_star_cl<-qnorm(1-(1-conf.level)/2)
  p<-counts/tot
  se.dat<-sqrt((p*(1-p))/tot)
  lower<- p - z_star_cl * se.dat
  upper<- p + z_star_cl * se.dat 
  ci<-c(lower,upper)
  return(ci)
}

my.prop.confidence(1549,2700,0.95)

