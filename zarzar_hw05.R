#The first step is to figure out the probability of an oubreak with a ranking index of > 0.5 occuring in the wind_outbreaks.txt dataset
wind.data <- matrix(scan("F:/RWorkspace/datasets/wind_outbreaks.txt"), ncol = 5, byrow=T)

wind.grt.pt5.vec <- ifelse(wind.data[,2]>0.5,1,0) #convert all values >0.5 to a 
wind.grt.pt5.vec
pr.grt.pt5 <- sum(ifelse(wind.data[,2]>0.5,1,0)/length(wind.data[,2]))
pr.grt.pt5
#OUTPUT: 0.3138686


#1a
#probability the next 3 outbreaks have a ranking index of greater than 0.5
dbinom(3,3,pr.grt.pt5)
#OUTPUT: 0.0309203

#1b
#We are assuming a binomial distribution, so we would assume that it should be 50%
dbinom(3,10,pr.grt.pt5)
#OUTPUT: 0.2656293

#1c
#Now I need to figure out the probabiity that 15 out of the next 18 outbr3eaks have a ranking index of less than 0.5 
dbinom(15,18,(1-pr.grt.pt5))
#OUTPUT: 0.08872441

#1d
set.seed(7)
sum(rbinom(75,1,pr.grt.pt5))
#OUTPUT: 24

#2a
#Probability of getting 70 wind reports in a wind outbreak
wind.mean <- mean(wind.data[,5])
wind.mean
#OUTPUT: 64.0292
dpois(70,wind.mean) 
#OUTPUT: 0.0363523

#2b
dpois(60,wind.mean)
#OUTPUT:  0.04518367

#2c
dpois(500,wind.mean)
#OUTPUT: 1.972843e-259 

#2d 
sum(dpois(0:75,wind.mean))
#OUTPUT: 0.9213064
