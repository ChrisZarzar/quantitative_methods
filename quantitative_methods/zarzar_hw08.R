#1 Testing whether the number of wind reports in the first 25 years of this
#dataset the same as the remaining years in the dataset (alhpa at 0.1: two sided).
#I know that I will need to use a Mann-Whitney U test

#Ho: There are the same number of wind reports in the first 25 years of this dataset as there are in the remaining years of the dataset
#Ha: There are not the same number of wind reports in the first 25 years of this dataset as there are in the remaining years of the dataset

#Executing the Mann-Whitney U test
first.25 <- wind.data[1:89,5]
remain.yrs <- wind.data[90:137,5]
all.ranks <- rank(cbind(first.25,remain.yrs))
all.ranks

all.ranks <- matrix(all.ranks,ncol=2,byrow=F) #you do byrow=F because R writes in columns, so if you go from a vector to a matrix this will have to be set to false, if you are scanning in an already set up matrix you do byrow=T
all.ranks
u.first.25 <- sum(all.ranks[,1])-(89/2)*90
u.first.25
u.remain.yrs <- sum(all.ranks[,2]) -(48/2)*49
u.remain.yrs

mean.u <- (89*48)/2
mean.u
sd.u <- sqrt((89*48*(89+48+1))/12)
sd.u
#The smaller U for these datasets is u.first.25
#Now calculate the z score z= U of the smaller dataset - sample population mean / population sd

z.score <- (u.first.25-mean.u)/sd.u
z.score
pnorm(z.score)
#OUTPUT: 4.677182e-10 This is essentially zero, so we should be able to reject the null that the two sets of data have an equal number of tornado reports.

#2 The task now is to create a 95% confidence intervals for the first 25 years and the remaining years of data for all the variabels in the dataset using bootstrap methods


#Calculate the confidence intervals for Wind
first.25.wind <- wind.data[1:89,5]
remain.yrs.wind <- wind.data[90:137,5]


boot.first25.wind <- boot(first.25.wind,mean.boot,R=5000)
ci.first25.wind <- quantile(boot.first25.wind$t,probs=c(0.025,0.5,0.975)) 
ci.first25.wind

boot.remain.yrs.wind <- boot(remain.yrs.wind,mean.boot,R=5000)
ci.remain.yrs.wind <- quantile(boot.remain.yrs.wind$t,probs=c(0.025,0.5,0.975)) 
ci.remain.yrs.wind

#Calculate the confidence intervals for Hail
first.25.hail <- wind.data[1:89,4]
remain.yrs.hail <- wind.data[90:137,4]


boot.first25.hail <- boot(first.25.hail,mean.boot,R=5000)
ci.first25.hail <- quantile(boot.first25.hail$t,probs=c(0.025,0.5,0.975)) 
ci.first25.hail

boot.remain.yrs.hail <- boot(remain.yrs.hail,mean.boot,R=5000)
ci.remain.yrs.hail <- quantile(boot.remain.yrs.hail$t,probs=c(0.025,0.5,0.975)) 
ci.remain.yrs.hail

#Calculate the confidence intervals for Tornadoes
first.25.torn <- wind.data[1:89,3]
remain.yrs.torn <- wind.data[90:137,3]

boot.first25.torn <- boot(first.25.torn,mean.boot,R=5000)
ci.first25.torn <- quantile(boot.first25.torn$t,probs=c(0.025,0.5,0.975)) 
ci.first25.torn

boot.remain.yrs.torn <- boot(remain.yrs.torn,mean.boot,R=5000)
ci.remain.yrs.torn <- quantile(boot.remain.yrs.torn$t,probs=c(0.025,0.5,0.975)) 
ci.remain.yrs.torn


#Calculate the confidence intervals for Ranking Index
first.25.ri <- wind.data[1:89,2]
remain.yrs.ri <- wind.data[90:137,2]

boot.first25.ri <- boot(first.25.ri,mean.boot,R=5000)
ci.first25.ri <- quantile(boot.first25.ri$t,probs=c(0.025,0.5,0.975)) 
ci.first25.ri

boot.remain.yrs.ri <- boot(remain.yrs.ri,mean.boot,R=5000)
ci.remain.yrs.ri <- quantile(boot.remain.yrs.ri$t,probs=c(0.025,0.5,0.975)) 
ci.remain.yrs.ri


#Now I will cbind the different confidence intervals together so I can make plots of the two confidence intervals for each variable
#plot wind ci
plotmat.wind <- cbind(ci.first25.wind, ci.remain.yrs.wind)
plotmat.wind
plot.ci(plotmat.wind)
abline(h=25.98)
abline(h=131.83)


#plot hail ci
plotmat.hail <- cbind(ci.first25.hail, ci.remain.yrs.hail)
plotmat.hail
plot.ci(plotmat.hail)
abline(h=4.00)
abline(h=19.60)

#plot tornado ci
plotmat.torn <- cbind(ci.first25.torn, ci.remain.yrs.torn)
plotmat.torn
plot.ci(plotmat.torn)
abline(h=12.07)
abline(h=14.21)

#plot ranking index ci
plotmat.ri <- cbind(ci.first25.ri, ci.remain.yrs.ri)
plotmat.ri
plot.ci(plotmat.ri)
abline(h=0.55)
abline(h=0.43)

#3 Now we are going to redo number 1 by conducting a permutation test (alpha = 0.05: 1 sided)
#Fortunantely, we have this nice function that has already been created and makes this job rather simple
permutationTestMeans(first.25.wind,remain.yrs.wind,B=10000)
#Output: 0
#Therefore, we can again reject the null that the first 25 years of data and the remaining 25 years of data have an equal number of tornado reports

