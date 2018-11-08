#3.17.15

#Hypothesis Testing

q.critical <- (qt(0.05,df=39)*2.75)+6
q.critical
1.366593
#critcal value

#what is the prob of commiting a type 2 error?

prob.type2 <- 1-pt((q.crit-6)/(2.75/sqrt(40)),df=39) #not working ????
prob.type2
#need to transform into t stat
1
liklihood of commiting a type 2 error is 100%
hist(nytorns)
#not a normal distribution and outliers are pulling up the mean


#task 2
#Does NY have more than 8 tornadoes in a year?
#one sided test
#T test and alpha 0.05

nytorns <-scan("nytorns.txt")
#

t.stat <- (mean(nytorns)-8)/(sd(nytorns)/sqrt(40))
-6.633823
#number of sd below test mean
#p value is small so not reject

pt(t.stat,df=39)
3.422959e-08
#39 data points
#T stat tells us where sample lies on distr. 
#popl. mean of 8

mean.hi.ithaca <- mean(ithaca.data[,3])
mean.hi.canada <- mean(canada.data[,3])

var.hi.ithaca <- var(ithaca.data[,3])
var.hi.canada <- var(canada.data[,3])

t.stat <- (mean.hi.ithaca-mean.hi.canada)/sqrt((var.hi.ithaca+var.hi.canada)/31)
t.stat

pt(t.stat,df=30)

#task 3
#if the average # of tornadoes in NY has changed btwn the first 2 decades of the dataset 
#and the second 2 decades alpha=0.05

#first 2 decades would be first 2 datapoints
# 1:20 splits the dataset into 2
mean.1 <- mean(nytorns[1:20])
mean.2 <- mean(nytorns[21:40])

var.1 <- var(nytorns[1:20])
var.2 <- var(nytorns[21:40])

t.stat <- (mean.1-mean.2)/sqrt((var.1+var.2)/20)
t.stat
-2.629582

pt(t.stat,df=19)
0.008253879
#n-1  which is why df=19
#reject null hypothesis tells us that they likely come from a different population
#beginning of distribution up to that point
#95% confidence and small type 1 error


#F statistic
