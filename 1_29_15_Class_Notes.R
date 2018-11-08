#Talking about quartiles and box plots
boxplot(temp.data) #How to create a box plot.
boxplot (c(temp.data,1))# An example of an outlier. We just add 1 to the vector of values

#Format for getting the five number summary: 
#quantile(dataset,probs=c(0,0.25,0.50,0.75,1))
quantile(ithaca_data[,3], probs=c(0,0.25,0.50,0.75,1)) #five number summary of ithaca.data column 3
quantile(ithaca_data[,4], probs=c(0,0.25,0.50,0.75,1)) #five number summary of ithaca.data column 4
boxplot(ithaca_data[,3],ithaca_data[,4]) #make a box plot of each dataset, they 
#are together in the boxplot function because they will be plot in the same winndow

#Now lets put those values into two sepearte variables and lets then combine then so we can see 
#see the numebrs side by side
##TASK 2##
ithaca_hi <- quantile(ithaca_data[,3], probs=c(0,0.25,0.50,0.75,1))
ithaca_lo <- quantile(ithaca_data[,4], probs=c(0,0.25,0.50,0.75,1))
cbind(ithaca_hi,ithaca_lo) #This aligns them vertically side by side (columns)
rbind(ithaca_hi, ithaca_lo) #This aligns them horizontally top and bottom (rows)

# OUTPUT:
#   > cbind(ithaca_hi,ithaca_lo) #This aligns them vertically side by side
# ithaca_hi ithaca_lo
# 0%           2     -13.0
# 25%         26       1.0
# 50%         30      19.0
# 75%         33      24.5
# 100%        53      31.0
# > rbind(ithaca_hi, ithaca_lo) #This aligns them horizontally top and bottom
# 0% 25% 50%  75% 100%
# ithaca_hi   2  26  30 33.0   53
# ithaca_lo -13   1  19 24.5   31

# ithaca_hi has a very small IQR and therefore it is expected that there will be outliers. It 
# turns out that this is the case when looking at the boxplot. You can expect to have relatively 
# similar mean and median.
# ithaca_lo has a greater spread in the IQR and therefore there are no outliers. The median is
# shifted very far up so you can expect the median and mean to be pretty far off.

title("Boxplot for Task #2", xlab="Dataset",ylab="Temperature") #To add a title, x, and y to your boxplot

#Going to Variance and Standard Deviations
var(temp.data) #Variance
sd(temp.data) #Standard Deviation

#sidework
> plot(temp.data) #Scatterplot of the data
> hist(temp.data) #histogram of the data

#Calculating anomalies
unstand.anom <- temp.data-mean(temp.data)
stand.anom <- (temp.data-mean(temp.data))/sd(temp.data)

#OUTPUT:
#  > unstand.anom #These valuess are all real values. They are actually the distance from the mean
# [1]  -6.6  -8.6  -1.6   4.4  13.4 -11.6 -21.6  15.4   7.4   9.4
# 
# > stand.anom #these values are all deviations from the mean
# [1] -0.5490687 -0.7154532 -0.1331076  0.3660458  1.1147759 -0.9650299 -1.7969522  1.2811604  0.6156225
# [10]  0.7820070

##TASK3##
#Compute the standard deviation of the ithaca highs. Using that value, convert the ithaca highs
#into standardized anomalies. 
#Show that their mean is 0 and standard deviation is 1. 
ithaca_hi <- ithaca_data[,3]
sd_ithaca_hi <- sd(ithaca_hi)
standanom_ithaca_hi <- ((ithaca_hi-mean(ithaca_hi))/sd_ithaca_hi)