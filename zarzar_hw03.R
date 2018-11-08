#This homework uses the hail_outbreaks.txt dataset
#The first task in this assignment is to calculate the standard deviation and variance o the ranking index for both the
#wind_outbreaks.txt dataset from assignment two and the severety ranking for 
#the hail_outbreaks.txt dataset
zarzar.hw03 <- function(){
  
wind.data <- matrix(scan('datasets/wind_outbreaks.txt'),ncol=5, byrow=T)
hail.data <- matrix(scan('datasets/hail_outbreaks.txt'),ncol=4, byrow=T)

print('Standard deviation and variance of the ranking index in wind.data')
wind.rank.sd <- sd(wind.data[,2])#Calculate the standard devition for ranking index
wind.rank.var <- var(wind.data[,2])#Calculate the variance for the ranking index
print(paste0("The wind dataset ranking index standard deviation is: ", wind.rank.sd))
print(paste0("The wind dataset ranking index variance is: ", wind.rank.var))
      
print('Standard deviation and variance of the ranking index in hail.data') 
hail.rank.sd <- sd(hail.data[,1])#Calculate the standard devition for ranking inde
hail.rank.var <- var(hail.data[,1])#Calculate the variance for ranking index
print(paste0("The hail dataset standard deviation of the ranking index is: ", hail.rank.sd))
print(paste0("The hail dataset variance of the ranking index is: ", hail.rank.var))

wind.hail.data <- c(wind.data[,2],hail.data[,1]) #Combine the ranking indexes for the two datasets into a single vecctor

print('Mean and standard deviation of combined wind/hail dataset. Therefore, these are the global values')

#find the mean and standard deviations for this newly combined dataset
global.mean <- mean(wind.hail.data)
global.sd <- sd(wind.hail.data)
print(paste0("The global mean is: ", global.mean))
print(paste0("The global standard deviation is: ",global.sd))

#create vectors that that will hold those significant outbreaks that are greater than 2 standard deviations about the global mean
wind.sig <- c()
hail.sig <- c()
wind.nonsig <- c()
hail.nonsig <- c()

two.sd <- (2*global.sd) + global.mean  #This calculated what the 2 standard deviation above the mean threshold is that I can compare the wind and hail values to
print(paste0("The 2 standard deviation threshold is: ", two.sd)) #This bit simply prints the value so I can manually check that it is correct.

#run a loop and conditional to determine what values for the wind dataset and hail dataset are two deviationss about the global mean
for (n in 1:dim(wind.data)[1]) {
  if (wind.data[n,2] > two.sd) {
    wind.sig <- c(wind.sig, 1)
  } else{
    wind.nonsig <- c(wind.nonsig, 1) 
  }
}
for (n in 1:dim(hail.data)[1]) {
  if (hail.data[n,1] > two.sd) {
  hail.sig <- c(hail.sig, 1)
  } else {
    hail.nonsig <- c(hail.nonsig, 1)
  }
}
sum.wind.sig <- sum(wind.sig)
sum.wind.nonsig <- sum(wind.nonsig)
sum.hail.sig <- sum(hail.sig)
sum.hail.nonsig <- sum(hail.nonsig)
print(paste0("There are ", sum.wind.sig, " significant wind events and ",sum.wind.nonsig," non-significant wind events" ))
print(paste0("There are ", sum.hail.sig, " significant hail events and ",sum.hail.nonsig," non-significant hail events" ))

#Now compute the four moment statistics for the wind and hail reports for both datassets

#calculate first moment statistic, mean
hail.wind.mean <- mean(wind.data[,4])
wind.wind.mean <- mean(wind.data[,5])
hail.hail.mean <- mean(hail.data[,3])
wind.hail.mean <- mean(hail.data[,4])

print(paste0("The mean for hail reports in the wind dataset is: ", hail.wind.mean))
print(paste0("The mean for wind reports in the wind dataset is: ", wind.wind.mean))
print(paste0("The mean for hail reports in the hail dataset is: ", hail.hail.mean))
print(paste0("The mean for wind reports in the hail dataset is: ", wind.hail.mean))

#calculate second moment statistic, variance
hail.wind.var <- var(wind.data[,4])
wind.wind.var <- var(wind.data[,5])
hail.hail.var <- var(hail.data[,3])
wind.hail.var <- var(hail.data[,4])

print(paste0("The variance for hail reports in the wind dataset is: ", hail.wind.var))
print(paste0("The variance for wind reports in the wind dataset is: ", wind.wind.var))
print(paste0("The variance for hail reports in the hail dataset is: ", hail.hail.var))
print(paste0("The variance for wind reports in the hail dataset is: ", wind.hail.var))

#calculate third moment statistic, skewness
hail.wind.skew <- skewness(wind.data[,4])
wind.wind.skew <- skewness(wind.data[,5])
hail.hail.skew <- skewness(hail.data[,3])
wind.hail.skew <- skewness(hail.data[,4])

print(paste0("The skewness for hail reports in the wind dataset is: ", hail.wind.skew))
print(paste0("The skewness for wind reports in the wind dataset is: ", wind.wind.skew))
print(paste0("The skewness for hail reports in the hail dataset is: ", hail.hail.skew))
print(paste0("The skewness for wind reports in the hail dataset is: ", wind.hail.skew))

#calculate fourth moment statistic, kurtosis
hail.wind.kur <- kurtosis(wind.data[,4])
wind.wind.kur <- kurtosis(wind.data[,5])
hail.hail.kur <- kurtosis(hail.data[,3])
wind.hail.kur <- kurtosis(hail.data[,4])

print(paste0("The kurtosis for hail reports in the wind dataset is: ", hail.wind.kur))
print(paste0("The kurtosis for wind reports in the wind dataset is: ", wind.wind.kur))
print(paste0("The kurtosis for hail reports in the hail dataset is: ", hail.hail.kur))
print(paste0("The kurtosis for wind reports in the hail dataset is: ", wind.hail.kur))
}
#Plot histogram for hail reports in wind.data
hist(wind.data[,4])
#plot histogram for wind reports in wind.data
hist(wind.data[,5])
#plot histogram for hail reports in hail.data
hist(hail.data[,3])
#plot histogram for wind reports in hail.data
hist(hail.data[,4])
