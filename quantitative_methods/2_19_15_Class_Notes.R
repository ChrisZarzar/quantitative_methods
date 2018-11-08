#PLotting CDF 

quants <- seq(-3,3, by=0.001)
cdf.vals <- pnorm(quants)
plot(quants, cdf.vals, type ='l')

#Calculating the PDF for a unified distribution

dunif(10,0,100)
# OUTPUT [1] 0.01

#Make a histogram of aunified distribution with random values
hist(runif(1000,0,100))
#Lets see what the probabiity of us getting a value between 0-20 with a unified distribution

#WAYS OF INVESTIGATING CDF
punif(20,0,100) #here, we give it a place on the x axis and ask for what proability that would be associated with
#OUTPUT [1] 0.2 #So 20 percent.

qunif(0.2,0,100) #This is the inverse of punif. This give it a probability and ask it to tell us what value on the x axis this probabilty corresponds to


###TASK $#####
#What is the probability of getting a uniform number value of 35 or larger on a
#distribution that extends from -50 to 50? Also, use an ifelse statement to
#count up how many, out of 1000, random numbers you get thtat are greater than
#35 for this distribution. Are these two numbers consistent.
set.seed(10)
unif.vec <- runif(1000,-50,50)
prob.35 <- punif(35,-50,50)
prob.grt.equ.35 <- (1-prob.35)
prob.grt.equ.35
sum(ifelse(unif.vec >=35,1,0))
#OUTPUT [1] 0.15
#[1] 163 #You would expect 150 based on your percentage, so 1000 random numbes is getting pretty close


#HOW MERCER DID THE TASK
1-punif(35,-50,50)
#OUTPUT [1] 0.15
#what if you want to find the percentage that lies between -40 and 35?
punif(35,-50,50)-punif(-40,-50,50)
#OUTPUT [1] 0.75

#NORMAL DISTRIBUTIONS
pnorm(3)-pnorm(-3) #Determining what probability of the normal distribution falls within 3 standard deviations
#OUTPUT [1] 0.9973002
pnorm(2)-pnorm(-2) #What probability of the normal distribution of data falls within 2 standard deviations
#OUTPUT  [1] 0.9544997  #This is where the 95% rule comes


#To generate random standard numbers in a normal distriution 
hist(rnorm(10000))

#To figure out the percentage without having to calculate the standardized Z score by hand 
pnorm(2,5,2) #So we want to find the percentage of <=2 if we have a mean of 5 and a standard deviation of 2
# OUTPUT [1] 0.0668072

1-pnorm(2,5,2) #if we want to find the percentage of values that lie >=2
#OUTPUT [1] 0.9331928

###TASK 5###
#Assume we have collected 50 pressure values that are followin a normal
#ditribution. THeir average is 998 mb, and their variance is 16 mb. One day
#after collecting these 50 values, we observd a low value of 975 mb. What is the
#probbility of this value or lower occuring?
pnorm(975,998,4) #So the chance of getting a low value of <=975 mb when the mean is 998 mb and the variance is 16 mb, so standard deviation is sqrt 16 mb or 4 mb
#OUTPUT [1] 4.462172e-09

#ANother way that this could be done is to first calcuate the z score
z.score <- (975-998)/4
z.score
#OUTPUT  [1] -5.75
pnorm (z.score)
#OUTPUT [1] 4.462172e-09
