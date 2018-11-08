
#zarzarhw4 <- function(){
# Correltion between ranking index and number of wind reports
#And the same correlationbut for hail reports

print(paste0('The ranking index/wind reports correlation is: ', cor(wind.data[,2],wind.data[,5])))
print(paste0('The ranking index/hail reports correlation is: ', cor(hail.data[,1],hail.data[,3])))

#Now plot both variables for
plot(wind.data[,5], wind.data[,2], xlab="Wind Reports", ylab="Ranking Index", main="Wind Reports vs Ranking Index")
plot(hail.data[,3], hail.data[,2], xlab="Hail Reports", ylab="Ranking Index", main="Hail Reports vs Ranking Index")

#Now do the same correlations but use the spearman correlation
print(paste0('The ranking index/wind reports Spearman correlation is: ', cor(wind.data[,2],wind.data[,5], method="spearman")))
print(paste0('The ranking index/hail reports Spearman correlation is: ', cor(hail.data[,1],hail.data[,3], method="spearman")))

lag0.wind <- wind.data[2:137,5]
lag1.wind <- wind.data[1:136,5]
print(paste0('The wind reports one day lag correlation is: ',cor(lag0.wind, lag1.wind)))


#PROBABILITY

#1
#Now to work on some probabilities
#find prob that a hail outbreak contains more than 10 tornado AND more than 10 wind reports
pr.10torn.wind <- sum(ifelse(hail.data[,2]>10 & hail.data[,4]>10,1,0))/length(hail.data[,2])
print(paste0('The probability that a hail outbreak contains more than 10 tornado and more than 10 wind reports is: ', pr.10torn.wind)) 


#2
#Find probability that a ranking index of 1 or higher will be observed, given the number of hail reports observed was larger than 100
pr.hail100 <- sum(ifelse(hail.data[,3]>100,1,0))/length(hail.data[,3])
pr.rank1.int.hail100 <- sum(ifelse(hail.data[,1]>=1 & hail.data[,3]>100,1,0))/length(hail.data[,1])
pr.rank1.int.hail100
print(paste0('The probability that you have a ranking index of 1 or higher given there are 100 hail reports is: ', pr.rank1.int.hail100/pr.hail100))

#3

#Find the probability that the number of hail reports is larger than 100, given that the ranking index of 1 or higher was observed?
#Apply Bayes Theorem to the previous problem (i.e. inverting problem 2) to get this answer
pr.rank1 <- sum(ifelse(hail.data[,1]>=1,1,0))/length(hail.data[,1])

pr.hail100.int.rank1 <- (0.189781) *(pr.hail100/pr.rank1)
print(paste0('The probability that you have 100 hail reports given your ranking index is 1 or higher is: ',pr.hail100.int.rank1))

#You could also do this by just inverting the problem
pr.hail100.int.rank1 <- sum(ifelse(hail.data[,3]>100 & hail.data[,1]>=1,1,0))/length(hail.data[,1])

pr.rank1.int.hail100

pr.hail100.int.rank1/pr.rank1

#}