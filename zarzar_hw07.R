#Zarzar HW 7
# We will be using the wind_outbreaks.txt and the hail_outbreaks.txt datasets to
# determine the average ranking index of both, the variance of both, the type of
# distribution of both, and the implications of the results found in the 
# assignment.
source('E:/RWorkspace/rcodes/chisquare.R')

wind.data <- matrix(scan("G:/RWorkspace/datasets/wind_outbreaks.txt"), ncol = 5, byrow=T)
hail.data <- matrix(scan("G:/RWorkspace/datasets/hail_outbreaks.txt"), ncol = 4, byrow=T)

#The ranking index is column 2 of wind.data and column 1 for hail.data

#Now I will use a t-test to test whether the hail ranking index average is greater than 0.5
#This will be a one sided hypothesis test because we are only looking to find whether it is greater than a certain value
#Ho: The hail average ranking index is less than or equal to 0.5
#Ha: The hail average ranking index is greater to 0.5
mean.hail <- mean(hail.data[,1])
sd.hail <- sd(hail.data[,1])
#Now that we have the mean and sd, we calculate the t statistic
t.stat <- ((mean.hail-0.5)/sd.hail)/sqrt(length(hail.data[,1]))
t.stat
#Now calculate the probability associated
pt(t.stat,(length(hail.data[,1])-1))
#Because we are testing at a p=0.05 and we got p=0.499, we cannot reject the null hypothesis 

#Now I will test whether the average ranking indicies for hail and wind outbreaks are equal
#Ho: The average of the hail and wind outbreak ranking indicies are equal
#Ha: The avearge of the hail and wind outbreak ranking indicies are not equal
mean.wind <- mean(wind.data[,2])
var.wind <- var(wind.data[,2])
var.hail <- var(hail.data[,1])
n.hail <- length(hail.data[,1])
n.wind <- length(wind.data[,2])

#Now we set up the test
t.stat <- (mean.hail-mean.wind)/sqrt((var.hail/n.hail)+(var.wind/n.wind))
t.stat
pt(t.stat,(n.hail+n.wind)-1)

#Because we are testing at a p=0.1 and we get p=0.305, we cannot reject the null hypothesis 


#Now I will test whether the variance of the hail reports and the wind reports are equal.
#I will need to use the f test for this
#Hail reports in the hail data set are column 3
#Wind reports in the wind dataset are column 5
#Ho: The variance of hail reports in the hail dataset and the wind reports in the wind dataset are equal
#Ha: The variance of hail reports in the hail dataset and the wind reports in the wind dataset are not equal
f.stat <- var(hail.data[,3])/var(wind.data[,5])
f.stat
#f.stat = 1.754
pf(f.stat,n.hail-1,n.wind-1)

#Because we are testing at a p=0.01 and we get p=0.0096, we can reject the null hypothesis


#Now I need to determine if these follow a gamma distribution using the chisquared 
sd.wind <- sd(wind.data[,2])

alpha.wind <- mean.wind^2/sd.wind^2

beta.wind <- sd.wind^2/mean.wind
rate.wind <- 1/beta.wind

chi.square(wind.data[,2],dist.name="gamma",n.bins=6,dist.params=c(alpha.wind,rate.wind))

#Now I need to investigate whether the ranking index for the wind data and hail data follow a gamma (or normal) distribution.
#I will do this by simply plotting the columns and I will save those plots as images in the word document
hist(wind.data[,2])

#This does not follow a gamma distribution.

alpha.hail <- mean.hail^2/sd.hail^2

beta.hail <- sd.hail^2/mean.hail
rate.hail <- 1/beta.hail

chi.square(hail.data[,2],dist.name="gamma",n.bins=6,dist.params=c(alpha.hail,rate.hail))
hist(hail.data[,1])
#This does not follow a gamma distribution. 

#Each of these test were developed with the assumption that the data follows a
#gamma distribution. It must follow a gamma distribution for these hypotheses
#test to be relavent. They do not follow a gammma distribtion and the results
#are, therefore, not valid for a proper hypothesis test. These data will require
#non parametric methods for the hypothesis testing.
