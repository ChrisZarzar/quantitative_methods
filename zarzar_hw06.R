#a)
#I am going to plot the PDF 
quantiles <- seq(0,5, by =0.01)
pdf.exp <- dexp(quantiles, rate=(1/2))
plot(quantiles, pdf.exp, type='l', main = "PDF: Beta = 2")

#I am going to plot the CDF
quantiles <- seq(0,5, by =0.01)
cdf.exp <- pexp(quantiles, rate=(1/2))
plot(quantiles, cdf.exp, type ='l', main = "CDF: Beta = 2")

#b)
# I need to  generate a random exponential dataset with 1500 values at a rate of 3.5
set.seed(35)
exp.dist <- rexp(1500, rate=(1/3.5))
exp.dist

hist(exp.dist)

#c)
#Now I need to sort the data using the sort() function. 
sort.exp.dist <- sort(exp.dist)

#Calculate the probability between the 1st and 750th datapoints
(pexp(sort.exp.dist[750],rate=(1/3.5))) - (pexp(sort.exp.dist[1],rate=(1/3.5))) #probability between the 1st to 750th column
#OUTPUT: 0.4768198

#Calculate the probability 50th and 350th datapoints
(pexp(sort.exp.dist[350],rate=(1/3.5))) - (pexp(sort.exp.dist[50],rate=(1/3.5)))  ##probability betwee 50th and 350th. You have to subtract the probability from the 1st to the 50th to geth the probability for just tha region between
#OUTPUT:  0.1828813
#Calculate the probability 1000th and 1250th datapoints
(pexp(sort.exp.dist[1250],rate=(1/3.5))) - (pexp(sort.exp.dist[1000],rate=(1/3.5))) #probability betwee 1000th and 1250th. You have to subtract the probability from the 1st to the 1000th to geth the probability for just tha region between
#OUTPUT: 0.183537

#Determine the probability of getting a random value greater than the maximum in the dataset
1-(pexp(sort.exp.dist[1500],rate=(1/3.5)))
#OUTPUT: 0.0005217617

#d)
#Determining the quantile values for the 0.025 and 0.975 probabilities. 
#Determine, using ifelse stamtements, what percentage of your random number data fall outside of this range
#What percent should it be?
low.limit <- qexp(0.025,rate=(1/3.5))
low.limit
#OUTPUT: 0.08861233
upper.limit <- qexp(0.975, rate=(1/3.5))
upper.limit
#OUTPUT: 12.91108
low.out.prob <- sum(ifelse(sort.exp.dist<low.limit,1,0))/1500
up.out.prob <- sum(ifelse(sort.exp.dist>upper.limit,1,0))/1500
low.out.prob
#OUTPUT: 0.02533333
up.out.prob
#OUTPUT:0.02933333
