#####
########   R Code for NACC data analysis 
#######           Summer 2022
########       Shahbaba and Behseta 
######





attach(alz)

diag=as.factor(diagnosis)
summary(diag)

female=as.factor(female)
summary(female)




t<- table(diagnosis,female)
t
#female
#diagnosis    0    1
#0  529 1005
#1  327  286
#2  295  258


tab_sum <- addmargins(t, FUN = sum)       
tab_sum

tab_sum


t.rate=t/sum(t)
t.rate
tab_rate <- addmargins(t.rate, FUN = sum)       
tab_rate 


counts<-c(529,1005,327,286,295,258)
diagnosis<-c("normal","mild","severe")
female<-c("no","yes")
new.data<-data.frame(counts,diagnosis,female)
new.data

##### plotting the two variables

ggplot(data = new.data, aes(x = diagnosis, y = counts, fill = female)) + 
  geom_bar(stat = "identity") + 
  labs(x = "\n diagnosis", y = "Counts \n", 
       title = "Diagnosis By Female \n",
       fill = "Female") +
  
  
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(face="bold", colour="blue", size = 12),
        axis.title.y = element_text(face="bold", colour="blue", size = 12),
        legend.position = "bottom")


#### chi square test via a built-in function in R
c<-chisq.test(t)
c
c$observed
c$expected


### double checking the value of chisq.test and its pvalue

my.chisq=sum((c$observed-c$expected)^2/c$expected)
my.chisq

1-pchisq(my.chisq,2)


#### Activity 1
educ.cat=numeric(2700)
educ.cat[educ>=0 & educ <=12]<-1
educ.cat[educ>12 & educ <=17]<-2
educ.cat[educ>17 & educ <=25]<-3
educ.cat<-as.factor(educ.cat)
head(educ.cat) 

educ1=data.frame(educ)

library(magrittr)

educ1.cat <- educ1 %>% 
  mutate(category=cut(educ, breaks=c(0,12,17,25), labels=c("1","2","3")))



summary(vol[diag==0])
summary(vol[diag==1])
summary(vol[diag==2])

boxplot(vol~diag1,col="blue",pch=16,xlab="diagnosis", 
        ylab="intracarnial volume")


######## t-test for vol of female versus others 
#### suppose we would like to test the hypothesis that the naccicv for "female" and the other group are the same.
## We can accomplish that with a simple t test. In R, the command is t.test

vol.f<-vol[female=="yes"]
vol.o<-vol[female=="no"]

summary(vol.f)
summary(vol.o)


t.test(vol.f,vol.o)



#### Let's consider the subjects' left hippocampus volume versus their right hippocamus volume. There 
### should be a fairly large correlation between the two
plot(lhippo,rhippo,pch=16,xlab="left hippocampus volume",ylab="right hippocampus volume")
cor(lhippo,rhippo)
cor.test(lhippo,rhippo)

plot(age,naccmmse,pch=16)
summary(lm(naccmmse~educ+age+vol))
