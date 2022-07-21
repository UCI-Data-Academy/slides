###### Summer 2022 
###### R Code for Linear and Logistic Regression



###### Linear Regression 
#### let's plot left hippocampus volume versus age and 
#### education separately

plot(age,lhippo,pch=16,col="red")
plot(educ,lhippo,pch=16,col="blue")


#### linear model with age only
lm.age<-lm(lhippo~age)
summary(lm.age)
plot(age,lhippo,pch=16,col="red")
pred.age<-predict(lm.age)
lines(age,pred.age,lwd=3)

#### linear model with education only
lm.educ<-lm(lhippo~educ)
summary(lm.educ)
plot(educ,lhippo,pch=16,col="blue")
pred.educ<-predict(lm.educ)
lines(educ,pred.educ,lwd=3)


#### linear model with age and education
lm.AgeEduc<-lm(lhippo~age+educ)
summary(lm.AgeEduc)



####
####
####   logistic regression
####
####



## Let's treat the binarized version of diagnosis as the response.
alz<-alzheimer_data
attach(alz)
summary(diagnosis)
head(diagnosis)

### let's collapse the two factors of 1 and 2 into a `1`
### There are many ways to do this in R
### one of the most straightforward methods is to use the command "recode"
### in the package "car":
library(car)
diagnosis.new<-recode(diagnosis,"c(1,2)='1';else='0'")
diag.new<-as.factor(diagnosis.new)
summary(diag.new)

#### let's build a logistic regression for diag.new as the response and 
#### educ, age, naccicv, and female as predictors

fem<-as.factor(female)

diag.logistic<-glm(diag.new~educ+age+naccicv+fem,family=binomial)
summary(diag.logistic)






#####
##### cross-validation for model validation
######



#### create training and testing data

#### library(tidyverse)
#### library(caret)

set.seed(1234)
a=seq(1,2700,1)
b=sample(a,2200,replace = F)

alz.logistic<-alz[,c("educ","age","naccicv")]
alz.logistic<-cbind(diag.new,alz.logistic,fem)
train.data<-alz.logistic[b,]
test.data<-alz.logistic[-b,]

#### run the model on training data
diag.logistic<-glm(diag.new~educ+age+naccicv+fem,family=binomial,data=train.data)


#### obtain the probability of diagnosis for the subjects in test data
probability<-diag.logistic %>% predict(test.data,type="response")
head(probability)


#### Set the threshold as 0.5 for positive diagnosis 
predicted.classes <- ifelse(probability > 0.5, "1", "0")





#### calculate the accuracy of your model's predicition
mean(predicted.classes == diag.new[-b])
