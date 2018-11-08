#Working with Gamma distributions
set.seed(10)
hist(rgamma(100,5,0.5)) #Generate 100 random values in a gamma distribution

set.seed(10)
hist(rgamma(100000,10,2)) #The first value is the numbers, the midde is alpha (shape) the thirs is beta (scale)
#THis one makes a distribution more similar to a normal distributions

set.seed(10)
hist(rgamma(100000,.5,(1/10)))
#THis kind of makes your classic gamma distribution with a long right tail

set.seed(10)
hist(rgamma(100000,10,(1/10)))
#This looks the same as teh first one we tried where alpha =10 and beta = 2, howver the spread on the x axis is much larger and the magnitudes of the data are much greater

#Example
#WIth out last distribution, alpha = 6.25, beta = 0.8, what is the probability
#our distribution is greater than 5?
#To calculate this in R....
1-pgamma(5,6.25,(1/0.8))#We us p rather than d because it is a continous distribution
#OUTPUT: 0.4467776
hist(rgamma(100000,6.25,(1/0.8)))0
#Lets plot the PDF for this example
quants <- seq(0,20,by=0.001)
plot(quants,dgamma(quants,6.25,(1/0.8)),type='l')

##TASK 6##
#probability that ithaca precip will be greater than 0.2 inches in January
alpha.ithaca <- mean(ithaca.data[,2])^2/var(ithaca.data[,2])
beta.ithaca <- var(ithaca.data[,2])/mean(ithaca.data[,2])
1-pgamma(0.2,alpha.ithaca,(1/beta.ithaca))
#OUTPUT:  0.1451161

