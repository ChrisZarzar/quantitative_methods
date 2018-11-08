#3.19.15

#Hypothesis Testing


#Step 1: Identify test stat F stat
#S2: identify null and alternative
#S3: Ho same variance
#   Ha not same variance
#S4: what is null distribtuion
#S5: complete test, draw speculative distrubtion
#S6: calculate 
# reject or accept

f.stat <- var(ithaca.data[,4])/var(canada.data[,4])
f.stat
2.500956

pf(f.stat,30,30) 
0.9928234
#degrees of freedom
#reject bc falls outside

f.stat <- var(canada.data[,4])/var(ithaca.data[,4])
f.stat
0.399847
pf(f.stat,30,30)
0.007176625  
#still reject
#cannot do interchange if different lengths

var(canada.data[,4])
77.58065
var(ithaca.data[,4])
194.0258

#higher var = more spread
#var is spread in values
#one has higher variability and is near a great lake 
#canada is closer to lake so T is moderated


#Task 4 F-test
#determine if the variability in the # of tornadoes in NY has changed btwn 
#the first 2 decades of the dataset and second two decades, alpha=0.05

nytorns1 <- nytorns[1:20]
nytorns2 <- nytorns[21:40]

f.stat <- var(nytorns2)/var(nytorns1)
f.stat
1.648509

1/f.stat
0.6066087
pf(f.stat,19,19) 
0.8576373
pf(1/f.stat,19,19)
0.1423627
#cannot reject the null bc have same variance
#ave. # of tornadoes went up but the variance went up
#mean has increased btwn the 2 decades but variability btwn did not change
#trend is going upward but spread is same


#Chi square
#better served in discrete distribution, group data into different bins
#hist of one dataset is same as hist of another dataset

#Mercer wrote function that will do the chi square test for us
source('chisquare.R')
#calculate number of df and will fit data to distribultion

ithaca.precip <-matrix(scan('ithaca_precip.txt'),ncol=2,byrow=T)

#distribution parameters
mean.ith <- mean(ithaca.precip[,2])
sd.ith <- sd(ithaca.precip[,2])

chi.square(ithaca.precip[,2],dist.name="norm",n.bins=6,dist.params=c(mean.ith,sd.ith))
0.9999929

#Pvalue will give different answers based on use of random #

#reject null

#is it gamma distributed?
#need to calculate gamma fit by getting rate and alpha

alpha.ith <- mean.ith^2/sd.ith^2

beta.ith <- sd.ith^2/mean.ith
rate.ith <- 1/beta.ith

chi.square(ithaca.precip[,2],dist.name="gamma",n.bins=6,dist.params=c(alpha.ith,rate.ith))
0.5868228 #p value
#could not reject based on p value bc not larger than 0.95
#one sided test bc cant get a negative bc of squared and only want to reject if numerator is large and positive
#only reject if on far right side of distribution


#task 5 
#determine if the Ithaca low T data follow a normal distribution. Use 4 bins. 
#assume alpha = 0.5 is not given

mean.itha <- mean(ithaca.data[,4])
mean.itha
13.32258

sd.itha <- sd(ithaca.data[,4])
sd.itha
13.92931

chi.square(ithaca.data[,4],dist.name="norm",n.bins=4,dist.params=c(mean.itha,sd.itha))
0.9213232
#cant reject null

hist(ithaca.data[,4])
#its safer to not reject than to reject when you are on edge Ex. 0.95




