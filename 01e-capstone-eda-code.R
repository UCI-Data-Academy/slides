####
####
######## R codes for Summer COSMOS program
#### Demonstrating data analysis through Alzheimer's data 
##########
######################  Lesson 1: Univariate Data Analysis
###################

##### Basics, data set, and simple manipulations of data
alz=alzheimer_data
dim(alz)
attach(alz)
length(hrate)


str(alz)
head(alz)


alz[2,]
alz[3:4,1:5]


### base R
alz.test=alz[,c("id","diagnosis","educ")]
head(alz.test)

### tidy
alz.test=select(alz,id,diagnosis,educ)
head(alz.test)

###### "diagnosis" is a categorical variable with three outcomes 
##### 0= normal cognitive ability, 1= mild symptoms, and 1= strong symptoms. 

diag=as.factor(diagnosis)
summary(diag)


##### simple barplots

## base R
plot(diag,xlab="Category", ylab="Frequency", ylim=c(0,1600))

## ggplot2

library(ggplot2)
counts=c(1534,613,553)
cat=c("normal","low","severe")
cat=as.factor(cat)
df=data.frame(counts,cat)

p<-ggplot(data=df, aes(x=cat, y=counts)) +
  geom_bar(stat="identity")
p + scale_x_discrete(limits=c("normal", "low", "severe"))



r.f=counts/sum(counts)
df=data.frame(r.f,cat)
p<-ggplot(data=df, aes(x=cat, y=r.f)) +
  geom_bar(stat="identity")
p + scale_x_discrete(limits=c("normal", "low", "severe"))


###### naccicv feature is a numerical variable, overall representing the volume of 
###### the brain. For the patients with more progressive AlZheimer's the volume is 
##### typically smaller.

#### histogram of one of volume features
vol<- naccicv
summary(vol)
hist(vol,xlab="Volume",breaks=15)
abline(v=mean(vol),col="red",lwd=2)
abline(v=median(vol),col="blue",lwd=2)



##### naccicv boxplot 

#base R
boxplot(vol,col ="blue",pch=16)

### ggplot2
vol1=data.frame(vol)

ggplot(data = vol1, aes(x = "", y = vol)) + 
  geom_boxplot(fill="blue") + 
  coord_cartesian(ylim = c(1000, 2000))

####### more on boxplots: identifying distribution's quantile,
####### outlying thresholds and other goodies! 
q.vol=quantile(vol)
q.vol
range(vol)
inter.quartile=IQR(vol)
outlier.lower.threshold=q.vol[2]-1.5*inter.quartile
outlier.upper.threshold=q.vol[4]+1.5*inter.quartile
thresholds=c(outlier.lower.threshold,outlier.upper.threshold)
thresholds

boxplot(vol,col ="blue",horizontal=TRUE,axes=FALSE,
        pch=16,main="Distribution of Intracarnial Volume")
text(x=fivenum(vol), labels=round(fivenum(vol)),
     y=1.25,cex=0.6)



