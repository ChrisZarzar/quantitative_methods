nytorns <- scan('E:/Rworkspace/datasets/nytorns.txt')
mean(nytorns)
sd(nytorns)
#Now we can calculate the lower limit of the confidence interval
ci.lower <- mean(nytorns)+qt(0.025,df=39)*(sd(nytorns)/sqrt(40))
#qt gives you the quartiles for the normal t distribution
ci.lower

#Now we can calculate the upper limit of the confidence interval. WE just have to change 0.025 to 0.975
ci.upper <- mean(nytorns)+qt(0.975,df=39)*(sd(nytorns)/sqrt(40))
#qt gives you the quartiles for the normal t distribution
ci.upper

ci.upper-mean(nytorns)

#So this tells use that the confidence interval is 5.225 +- 0.846 for a 95% confidence interval 
#So there is a 95% confidence that any value from the nytorns dataset will lie withing 5.225 +- 0.846


#TASK 6

#Compute confidence intervals for the ithaca and Canadaigua high temperature
#data. Compare the two and determine if they are statistically significantly
#different. Assume normality (alpha=0.05)

cana.data <- canadaigua_data
mean.cana.hi <- mean(cana.data[,3])
mean.ithaca.hi <- mean(ithaca.data[,3])
sd.cana.hi <- sd(cana.data[,3])
sd.ithaca.hi <- sd(ithaca.data[,3])

ci.lower.cana <- (mean.cana.hi+qt(0.025,df=30)*(sd.cana.hi)/sqrt(31))
ci.upper.cana <- (mean.cana.hi+qt(0.975,df=30)*(sd.cana.hi)/sqrt(31))
ci.lower.ithaca <- (mean.ithaca.hi+qt(0.025,df=30)*(sd.ithaca.hi)/sqrt(31))
ci.upper.ithaca <- (mean.ithaca.hi+qt(0.975,df=30)*(sd.ithaca.hi)/sqrt(31))

ci.lower.cana 
ci.upper.cana 
ci.lower.ithaca 
ci.upper.ithaca 
mean.cana.hi
mean.ithaca.hi

ci.upper.cana - mean.cana.hi

ci.upper.ithaca - mean.ithaca.hi


#CI for canadaigua highs is 31.77 +- 2.88
#CI for ithaca highs is 29.23 +- 3.34

#See if the mean of canadaigua highs could fall within the ithaca high confidence interval

31.77 - 2.88 = 28.89

#see if the mean of ithaca highs could fall within the canadaiqua high confidence interval

29.23 +3.34 = 32.57

#Result: Because both fall within the others 95% confidence interval, we cannot say with statistical significance that the average of the two are significantly different at p=0.05
#We cannot reject the null hypothesis that the two are the same

#To view this graphically...

source('plot.ci.R')
ci.ith <- c(25.9,29.2,32.6)
ci.can <- c(28.9,31.8,34.7)
plot.ci(cbind(c.ith,ci.can))
abline(h=29.2)
abline(h=31.8)

all.ranks <- rank(cbind(ithaca.data[,3],cana.data[,3]))
all.ranks

all.ranks <- matrix(all.ranks,ncol=2,byrow=F) #you do byrow=F because R writes in columns, so if you go from a vector to a matrix this will have to be set to false, if you are scanning in an already set up matrix you do byrow=T
u.ith <- sum(all.ranks[,1])-(31/2)*32
u.ith
u.can <- sum(all.ranks[,2]) -(31/2)*32
u.can

mean.u <- (31^2)/2
mean.u
sd.u <- sqrt((31*31*63)/12)
sd.u
#Now calculate the z score z= U of the smaller dataset - sample population mean / population sd

z.score <- (u.ith-mean.u)/sd.u
z.score
pnorm(z.score) #we use pnorm because the Z score is normalized by standard deviation so we use the probability for a normal distribution. 


