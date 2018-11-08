#2-17-15 Class notes

#Working with probabilities for binomial distributions
set.seed(6)
rbinom(1000,1,0.5)->test.vec #Test vec because a list of 1000 numbers where 1 are considered success and 0 are failures
hist(test.vec)
sum(test.vec)/1000 #Here, you add up the values (so you add up the ones) then you divide by 1000 to ge tthe percentatge
#OUTPUT 
#[1] 0.497
#To figure out  the probabilty of having 5 head in 9 coin flips...

dbinom(5,9,0.5)

#OUTPUT
#[1] 0.2460938

#TASK 2
#Say we are gambling on coin fliips and we have a weighed coin that lands on its
#heads 65% of the time. Set your random number seed to 35 and generate 1000
#coiflips with this probability using the rbinom command. How many ead and how
#many tails do you get? (Set 1=heads, 0=tails)
set.seed(35)
rbinom(1000,1,0.65) -> flip.vec
print(paste0('You get ', sum(flip.vec), ' heads and ', abs(sum(flip.vec)-1000), ' tails'))
#OUTPUT
#[1] 653

#Now figure out what the probability of getting exactly 10 heads (i.e. successes) out of 15 trials is.
dbinom(10,15,0.65)
#OUTPUT
#[1] 0.2123387

#Working with the Poisson distribution 
nytorns <- scan('datasets/nytorns.txt')
mean(nytorns)

#So what is the probability of getting 5 tornadoes per year
pr.5 <- ((5.225^5)*exp(-5.225))/factorial(5)
pr.5
#OUTPUT
#[1] 0.1717706

#A nicer way to get the above answer without having to type in the equation is below
dpois(5,5.225) 

#TASK 3
#Let's see if the probabilty of tornadoes has changed over the dataset. Break the
#dataset into two pieces one with the first 20 data rows and one with the
#second. Determine the probability of 5 tornadoes in a given year for each of
#these dataets
torn.1 <- nytorns[1:20]
torn.2 <- nytorns[21:40]
torn.1
torn.2

print(paste0('The mean of the first 20 values in the NY Tornado dataset is: ', mean(torn.1)))
print(paste0('The mean of the last 20 values in the NY Tornado dataset is: ', mean(torn.2)))

print(paste0('The probability of getting 5 tornadoes in a year from the distribution of the first 20 values in the NY Tornado dataset is: ', dpois(5,4.2)))
print(paste0('The probability of getting 5 tornadoes in a year from the distribution of the last 20 values in the NY Tornado dataset is: ',dpois(5,6.25)))

quantiles <- seq(-3,3,by=0.01)
pdf.norm <- dnorm(quantiles)
plot(quantiles, pdf.norm, type='l')
