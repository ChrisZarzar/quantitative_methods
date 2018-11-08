#Setting up bootstrapping for different statistics so that they can be used in a bootstrap
mean.boot <- function(x,d){
  return (mean(x[d]))
}

var.boot <- function(x,d){
  return (var(x[d]))
}

sd.boot <- function(x,d){
  return (sd(x[d]))
}

IQR.boot <- function(x,d){
  return (IQR(x[d]))
}

mode.boot <- function(x,d){
  return (Mode(x[d]))
}

median.boot <- function(x,d){
  return (median(x[d]))
}

min.boot <- function(x,d){
  return (min(x[d]))
}

max.boot <- function(x,d){
  return (max(x[d]))
}

kurtosis.boot <- function(x,d){
  return (kurtosis(x[d]))
}

skewness.boot <- function(x,d){
  return (skewness(x[d]))
}

#now create a data vector and run a bootstrap to resample it and to calculate the mean on it

datavec <- c(10,6,11,4,1,12,3,5,7,8)
boot.obj <- boot(datavec,mean.boot,R=10000) #Run a bootstrap to resample and calculate means. Set that output to boot.obj. the input is the datavec, the statistic we want to calculate is the mean, and we want to resample this 10,000 times
boot.obj$t #by typing the object that holds the resample means calculated and then put a dollar sign plus 't' on the end, it will print out the 10,000 means that were calclated in the resampling
hist(boot.obj$t) #This will provide a histogram of all of those calculated means from the 10,000 samples  
#looks like a normal distribution in the histogram

quantile(boot.obj$t, probs=c(0.025,0.5,0.975)) # This will give you your 95% confidence interval
boot.obj #This provides the information about the bootstrap that you ran. 


#****TASK 8****#
#Set your seed to 100. COmpare the actual calclated mean of a random set of 1000
#gamma distributed datapoints with alpha=1 and beta=2 to the bootstrap mean and
#median of these same data. That is, compute the median and mean of the boostrap
#replicates.
set.seed(100)
data.gamma <- rgamma(1000,1,1/2)
data.gamma.mean <- mean(data.gamma)
data.gamma.median <- median(data.gamma)
boot.gamma.mean <- boot(data.gamma,mean.boot,R=1000)
#boot.gamma.median <- boot(data.gamma,median.boot,R=1000) #He was not asking for this in the question
boot.mean <- mean(boot.gamma.mean$t)
#boot.median <- median(boot.gamma.median$t) #He was not asking for this in the question
boot.mean.median <- median(boot.gamma.mean$t)

data.gamma.mean
data.gamma.median
boot.mean
#boot.median #He was not asking for this in the question
boot.mean.median


boot.ith <- boot(ithaca.data[,2],mean.boot,R=2000)
boot.cana <- boot(cana.data[,2],mean.boot,R=2000)

ci.ith <- quantile(boot.ith$t,probs=c(0.025,0.5,0.975)) #get the confidence interval for the ithaca bootstrapped mean
ci.cana <- quantile(boot.cana$t,probs=c(0.025,0.5,0.975)) #get the confidence intervale for the cana bootstrapped mean
ci.ith
ci.cana
#If we had set up a hypothesis test and set the null to say that the means are equal, then we cannot reject the null because the means of both lie within the confidence interval of the other 

#***Task 9*****#
# use bootstrap confidence intervals to determine if climate change caused a
# change in Ithaca rainfall between the first 25 years and second 25 years of
# the Ithaca precipitation dataset. Plot your CIs using the plot.ci funtion to
# visually assess

ith.precip.data <- matrix(scan('datasets/ithaca_precip.txt'),ncol = 2,byrow = T)
first.25 <- ith.precip.data[1:25,2]
second.25 <- ith.precip.data[25:50,2]
boot.first <- boot(first.25,mean.boot,R=2000)
boot.second <- boot(second.25,mean.boot,R=2000)
first.ci <- quantile(boot.first$t, probs=c(0.025,0.5,0.975))
second.ci <- quantile(boot.second$t, probs=c(0.025,0.5,0.975))
first.ci
second.ci

source('rcodes/plot.ci.R')

plot.ci
#1st row is the Lower CI
#2nd row is median
#3rd row is upper limit
# OUTPUT:
#   function(x,barwidth=0.1,min.y=NULL,max.y=NULL,xlabel=NULL,ylabel=NULL) {
#     ## This function expects a matrix with 3 rows and a number
#     ## of columns corresponding to the number of values you're comparing
#     ## The function assumes the first row is the lower limit, the middle
#     ## row is the median, and the top row is the upper limit
#     if (barwidth > 0.5) {
#       warning("Warning:  Error bars will overlap unless you reduce barwidth")
#     }
#     
#     n.plots <- length(x[1,])
#     
#     ## Determine maximum and minimum values of x for the plotting routine.
#     ## The function figures this out if it is not specified.
#     
#     if (is.null(min.y) && is.null(max.y)) {
#       min.y <- min(x)-2*sd(x[2,])
#       max.y <- max(x)+2*sd(x[2,])
#     }
#     
#     plot(x[2,],xlab=xlabel,ylab=ylabel,ylim=c(min.y,max.y),pch=16)
#     
#     for (i in 1:n.plots) {
#       lines(c(i-barwidth,i+barwidth),c(x[1,i],x[1,i]))
#       lines(c(i-barwidth,i+barwidth),c(x[3,i],x[3,i]))
#       lines(c(i,i),c(x[1,i],x[3,i]))
#       
#     }
#     
#   }


#USING PLOT.CI

plotmat<- cbind(first.ci, second.ci)
plotmat
plot.ci(plotmat)

# OUTPUT:
# > plotmat<- cbind(first.ci, second.ci)
# > plotmat
# first.ci second.ci
# 2.5%   1.55356  1.561144
# 50%    1.87240  2.022115
# 97.5%  2.18284  2.558971
# > plot.ci(plotmat)