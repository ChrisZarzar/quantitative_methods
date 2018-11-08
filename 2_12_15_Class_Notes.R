# What is the probability the a day with a low greater than zero occurs on days when there is precipitation


ithaca.data <- ithaca_data
# So to calculate this percentage in R
pr.e2 <- sum(ifelse(ithaca.data[,2]>0, 1, 0))/length(ithaca.data[,2])
# Probability of E2 happening = 15/31 ==> 48.4%
pr.e2
pr.e1.int.e2 <- sum(ifelse(ithaca.data[,2] >0 & ithaca.data[,4]>0,1,0))/length(ithaca.data[,2])
# Probability both E1 and E2 happening = 14/31 ==> 45.2 %
pr.e1.int.e2
# For the conditional probability.
pr.e1.int.e2/pr.e2
# Conditional probability of E1 and E2 happening and standardized by E2 = 93%

#****Task8*****
# As meteorologist, we know that occurrences in the atmopshere are not independent. However, let's assume for a moment that precipitation and low temperature are independent. How does you conditional probability result change in that case?  

pr.e1 <- sum(ifelse(ithaca.data[,4]>0,1,0))/length(ithaca.data[,4])
pr.e1
#Based on climatology, there is a 74 % Chance the temperature will be above 0 degrees. 
#So if we know it has been raining and take it into account, we can forcast with better skill that
#the low may be greater than 0 becase there is a 93% chance the low will be greater than 0 if there 
#is precip that day. 


#NOW LETS TRY, WHAT IS THE PROBABILITY OF PRECIITATION > 0 GIVEN A LOW OF ZERO DEGREE FARENHEIT
#USE BAYS THEROM TO INVERT IT  

# So to calculate this percentage in R
pr.e2 <- sum(ifelse(ithaca.data[,2]>0, 1, 0))/length(ithaca.data[,2])
# Probability of E2 happening = 15/31 ==> 48.4%
pr.e2
pr.e2.int.e1 <- (.9333333)*(pr.e2/pr.e1)
# Probability both E1 and E2 happening = 14/31 ==> 45.2 %
pr.e2.int.e1
#So the probability that the rain will be greater than zero if the low is greater than zero is only 61%


#***TASK 9
#PRove that the Bayes theorem answer we just got what you get from doing it th original way. In other words,
#Without using Bayes theorem, what is the probability of precipiation greater than 0 give a temperature greater than o

pr.e2.e1 <- pr.e1.int.e2/pr.e1
pr.e2.e1

#**TASK 1
#What is the expected value from throwing 2 dice?
#Guess what it would be for 10 dice

#A simpler way to do this task
x.vals <- c(2:12)
probs.x <- c(seq((1/36),(6/36),by=(1/36)),seq((5/36),(1/36),by=-(1/36)))
sum(x.vals*probs.x)
