#Homework #2
wind_outbreaks <- matrix(scan('datasets/wind_outbreaks.txt'), ncol=5, byrow=T) #Bring in the dataset

#setting up a function to calculate basic statistics to help me interpret the output and to 
#make the process a bit simpler in the future
basic_stats <- function(x) {
  print ('the mean is:')
  print (mean(x))
  print ('the median is:')
  print (median(x))
  print ('the mode is:')
  print (Mode(x))
  print ('the five number summary is:')
  print (quantile(x,prob=c(0,0.25,0.5,0.75,1)))
}

#PART A
#Find mean, median, and mode of severe wind reports (column 5)
basic_stats(wind_outbreaks[,5])

#Find mean, median, and mode of severe ranking index (column 2)
basic_stats(wind_outbreaks[,2])

boxplot(wind_outbreaks[,5])
title('Severe Wind Reports')

hist(wind_outbreaks[,5])
title('Severe Wind Reports')

plot(wind_outbreaks[,5])
title('Severe Wind Reports')
  
boxplot(wind_outbreaks[,2])
title('Severe Ranking Index')

hist(wind_outbreaks[,2])
title('Severe Ranking Index')

plot(wind_outbreaks[,2])
title('Severe Ranking Index')

#PART B

basic_stats(wind_outbreaks[,3])
boxplot(wind_outbreaks[,3])
title('Tornado Reports')

basic_stats(wind_outbreaks[,4])
boxplot(wind_outbreaks[,4])
title('Severe Hail Reports')

basic_stats(wind_outbreaks[,5])
boxplot(wind_outbreaks[,5])
title('Severe Wind Reports')



#PART C

major_events <- c()
nonmajor_events <- c()


for (n in 1:dim(wind_outbreaks)[1]){
  if (wind_outbreaks[n,2]>0.5) {
    major_events <- c(major_events,wind_outbreaks[n,5])
} else {
    nonmajor_events <- c(nonmajor_events, wind_outbreaks[n,5])
}
}


hist(major_events)

boxplot(major_events)
title('Major Events Severe Wind Reports')
basic_stats(major_events)
print (sum(major_events))


hist(nonmajor_events)

boxplot(nonmajor_events)
title('Non Major Events Severe Wind Reports')
basic_stats(nonmajor_events)
print (sum(nonmajor_events))