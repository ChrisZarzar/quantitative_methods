source('rcodes/permutationTestMeans.R')

permutationTestMeans(ithaca.data[,4],cana.data[,4],B=2000)
#OUTPUT: 0.024
sample1 <-c(10,10,11,12,13)
sample2<- c(0,1,1,1,2)
permutationTestMeans(sample1,sample2,B=10000)
#OUTPUT: 9e-4

sample1 <- c(0,1,1,2)
sample2 <- c(0,1,2,2)
permutationTestMeans(sample1,sample2,B=10000)
#OUTPUT: 0.8218

#****TASK 10******#
#PErform a similar permuatation test on Ithaca high temp and precipitation data,
#comparing it to cana. WHich, if any are statistically significant different
permutationTestMeans(ithaca.data[,3],cana.data[,3],B=2000)
#OUTPUT: 0.2355
permutationTestMeans(ithaca.data[,2],cana.data[,2],B=2000)
#OUTPUT:0.637
#Conclusion, you cannot reject the Ho on either of that datasets that the means are equal to each other

month.highs <- matrix(scan('datasets/month_highs.txt'),ncol=2,byrow=T)

#Now lets take this dataset and go about calculating a regression the hard way
#x is the number of minutes of sunlight
#y is the average monthly high
term1 <- 12*sum(month.highs[,2]*month.highs[,1])
term1

term2 <- sum(month.highs[,2])*sum(month.highs[,1])
term2

term3 <- 12*sum(month.highs[,2]^2)
term3

term4 <- sum(month.highs[,2])^2
term4

beta1 <- (term1-term2)/(term3-term4)
beta1

#beta 1 gives us the slope, so to find the intercept we can do some alegrab and
#get the mean of the first column and then subtract it from the slope and mean
#of the second column

beta0 <- mean(month.highs[,1])-beta1*mean(month.highs[,2])
beta0

#So now the regression equation is y=24.44706 + 0.0644238x

y.hat <- beta0+beta1*month.highs[,2] #These are the predictions of the temperatures based on the number of minutes of sunlight
y.hat 
month.highs[,1] #lets see how the model compares?

sum(month.highs[,1]-y.hat) #This calculates the least squares fit to see if the sum of the observed and predicted is zero

lm.obj <- lm(month.highs[,1]~month.highs[,2])
lm.obj

plot(month.highs[,2],month.highs[,1],xlab="Minutes of Sunlight",ylab="High Temperatures")
abline(lm.obj)


#*****TASK 1*****#

#Grab the file, june_ecuador.txt. The file has four columns(year, average june
#temperature, June precip in mm, average June mean sea level presure). Use
#temperature as a predictor to try to predict June precipitaion. PLot a
#scatterplot with the line you fit t the ata to see how you did

ecuador.data <- matrix(scan('datasets/june_ecuador_data.txt'), ncol=4, byrow=T)
cor(ecuador.data[,3],ecuador.data[,2])
#OUTPUT: 0.706
cor(ecuador.data[,3],ecuador.data[,2],method="spearman") #There are some obvious outliers, so a spearman correlation is probably more appropriate
#OUTPUT:0.594
lm.obj <- lm(ecuador.data[,3]~ecuador.data[,2])
lm.obj
plot(ecuador.data[,2],ecuador.data[,3],xlab="Ecuador Average June Temperature (C)",ylab="Ecuador June Precipitation (mm)")
abline(lm.obj)