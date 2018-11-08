hist(ithaca_data)

#plot high temperature data
hist(ithaca_data[,3])
hist(ithaca_data[,3], nclass=27) #you can add more bins by identify the number of classes 

#plots precipitation
hist(ithaca_data[,2]) 
boxplot(ithaca_data[,2])
title("Precipitation")

#Measuring skewness for precipitation
source('rcodes/skewness.R')
skewness(ithaca_data[,2])
#Output: [1] 3.016924

#measuring skewness for high temperatures
skewness(ithaca_data[,3])
#Output: [1] -0.5250288

#solve kurtosis
source('rcodes/kurtosis.R')
kurtosis(rnorm(100))
#Output: [1] 2.59578 is near 3 because 100 random numbers should be near a normal distribution

set.seed(100)
sample1 <- rnorm(500)
sample2 <- rlnorm(500) #rl norm generates random numbers from a log normal distribution
hist(sample1)
hist(sample2)

skewness(sample1)
kurtosis(sample1)

skewness(sample2)
kurtosis(sample2)

# Output > skewness(sample1)
# [1] -0.1032239
# > kurtosis(sample1)
# [1] 3.363089
# > 
#   > skewness(sample2)
# [1] 3.873081
# > kurtosis(sample2)
# [1] 23.79949

#Figureing out how to add lines to the histogram
hist(ithaca_data[,3])
lines(density(ithaca_data[,3]) # add a density estimate with defaults
lines(density(ithaca_data[,2], adjust=2), lty="dotted")

#TASK4##/
#Compute the skewness and the kurtosis of the second and third columns of the ithaca data. What do they tell you about the data

skewness(ithaca_data[,2])
kurtosis(ithaca_data[,2])

skewness(ithaca_data[,3])
kurtosis(ithaca_data[,3])

# Output
# > skewness(ithaca_data[,2])
# [1] 3.016924
# > kurtosis(ithaca_data[,2])
# [1] 11.69976
# > 
#   > skewness(ithaca_data[,3])
# [1] -0.5250288
# > kurtosis(ithaca_data[,3])
# [1] 5.678223
# >

#Plotting in R
plot(ithaca_data[,3], ithaca_data[,4], xlab="Ithaca high", ylab="Ithaca low", main = "Ithaca highs vs low") #first values is your x coordinate, the second value us your y coordiante

