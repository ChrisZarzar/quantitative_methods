#Calcuate the correlation and covariance for [,3] and [,4] for the canadaigua_data.txt dataset

canadaigua_data <- matrix(scan('datasets/canadaigua_data.txt'),ncol=4, byrow=T) #Read in the dataset. You always need to do the matrix function or else it will read it in as one long vecto

var(canadaigua_data[,3], canadaigua_data[,4]) #Calculate the Covariance 
cor(canadaigua_data[,3], canadaigua_data[,4]) #Calculate the Correaltion 

plot(canadaigua_data[,3], canadaigua_data[,4], xlab='High Temperatures', ylab='Low Temperatures', main= 'Canadaigua High vs Low')

abline(lm(canadaigua_data[,4] ~ canadaigua_data[,3]),col="red") #This add a best line fit to the scatterplot. TO write this, you do the y values then you need a squigly line, then the x values


#Using spearman correlations now when you have outliers
test.set.1<-c(2,3,4,5,6,7,8,9,10,20)
test.set.2<- c(8,4,9,2,4,5,3,1,7,17)
plot(test.set.1,test.set.2)

cor(test.set.1,test.set.2)

#Setting up a spearman correlations
rank.1 <- rank(test.set.1)
rank.1

rank.2 <- rank(test.set.2)
rank.2

#now do a scatterplot of the ranks

plot(rank.1, rank.2)
cor(rank.1,rank.2)


# An easier way to do a spearman correlation is to add method='spearman' to the correlation functions
cor(test.set.1, test.set.2, method="spearman")

#Working on autocorrelations

lag0.ithaca <- ithaca_data[2:31,3] # This sets the data one day ahead
lag1.ithaca <- ithaca_data[1:30,3] #THis is essentially the day of. THe data is not adjust, but we do have to take off the last data so that the number of rows are the same for both columns
cbind(lag0.ithaca, lag1.ithaca) #This combines the two vectors together into a two column matrix. We could set it equal to some matrix and hten correlate the columns or we could just do the corrrelation of the vectors 
cor(lag0.ithaca, lag1.ithaca)

#Now lets try it with a 2 day offset
lag0.ithaca <- ithaca_data[3:31,3] #Start on day three
lag2.ithaca <- ithaca_data[1:29,3] #Start from day 1 so we are testing a two day lag
cor(lag0.ithaca, lag2.ithaca)

"#TASK 6
Compare the spearman correlation of the Ithaca high and low temperatures to the ordinary correation
you computed in task #5. Also, compute a lag-2 autocorrelation for Ithaca low temparture data. 
"

cor(ithaca_data[,3], ithaca_data[,4]) #regular pearson correlation
cor(ithaca_data[,3],ithaca_data[,4],method="spearman") #calculate the spearman correaltion

"
OUTPUT:

> cor(ithaca_data[,3], ithaca_data[,4]) #regular pearson correlation
[1] 0.7518199
> cor(ithaca_data[,3],ithaca_data[,4],method="spearman") #calculate the spearman correaltion
[1] 0.7878613
> 

Because the correlations are so similar, there should not big substantial outliers

"
lag0.ithacalow<-(ithaca_data[3:31,4])
lag2.ithacalow<-(ithaca_data[1:29,4])
cor(lag0.ithacalow,lag2.ithacalow)

"
OUTPUT
> cor(lag0.ithacalow,lag2.ithacalow)
[1] 0.3729389

"
source('rcodes/autocor_lag.R')# Bring in mercers lag correlations
       