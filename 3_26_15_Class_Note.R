##TASK &##
# Are teh average low temperatures equal between Canadaigua and Ithaca? Compare
# the reslts of a t-test with a Mann-Whitney test.

#Doing a Mann-Whitney test
all.ranks <- rank(cbind(ithaca.data[,4],cana.data[,4]))
all.ranks

all.ranks <- matrix(all.ranks,ncol=2,byrow=F) #you do byrow=F because R writes in columns, so if you go from a vector to a matrix this will have to be set to false, if you are scanning in an already set up matrix you do byrow=T
u.ith <- sum(all.ranks[,1])-(31/2)*32
u.ith
u.can <- sum(all.ranks[,2]) -(31/2)*32
u.can

mean.u <- (31^2)/2
mean.u
sd.u <- sqrt((31*31*63)/12)
sd.u
#Now calculate the z score z= U of the smaller dataset - sample population mean / population sd

z.score <- (u.ith-mean.u)/sd.u
z.score
pnorm(z.score) #we use pnorm because the Z score is normalized by standard deviation so we use the probability for a normal distribution. 

#OUTPUT
# t.stat:-1.752782
# pt: 0.03981968


#Now to do this using a t-test
mean.ith <- mean(ithaca.data[,4])
mean.can <- mean(cana.data[,4])
var.ith <- var(ithaca.data[,4])
var.can <- var(cana.data[,4])
n.ith <- length(ithaca.data[,4])
n.can <- length(cana.data[,4])

#Now we set up the test
t.stat <- (mean.ith-mean.can)/sqrt((var.ith/n.ith)+(var.can/n.can))
t.stat
pt(t.stat,(30))

# #OUTPUT
# t.stat:-2.332184
# pt: 0.01329614 

#Testing at a alpha=0.05, we can reject the Ho that the two location average low
#temperatures are not equal for the t-test, but because we are testing as a
#2-sided test we cannot reject for the Mann-Whitney U test because the p values
#would need to be less than 0.025 for a two sided test.



#JACKKNIFING
datavec <- c(10,6,11,4,1,12,3,5,7,8)
jack1 <- datavec[-10]
jack2 <- datavec[-9]
jack3 <- datavec[-8]
jack4 <- datavec[-7]
jack5 <- datavec[-6]
jack6 <- datavec[-5]
jack7 <- datavec[-4]
jack8 <- datavec[-3]
jack9 <- datavec[-2]
jack10 <- datavec[-1]
jackmat <- cbind(jack1,jack2,jack3,jack4,jack5,jack6,jack7,jack8,jack9,jack10)
dim(jackmat)
jackmat

means.jack <- apply(jackmat,2,mean)
mean(datavec)

#So with the jacksknife method, it is turning the data to more normally
#distributed and we can say with confidence tha tthe population mean lies
#somewhere in the range of 6.1111 to 7.3333

#lets install the package for bootstrapping
install.packages("boot")

#Now load up the library
library('boot')

#and now call the boot library
boot


#in the bootstrap in R, we write boot(dataset,statistic,R=1000). But we need to
#create a function so that we can actually run the statistic we want, because R
#will not recognize 'statistic' as it is now. We will need a function lke the
#mean function below. And we can do this for any basic statistic, like sd, or
#var, but we will need to do a bit more for something like a correlation.

#Write a function that will be needed for bootstrapping the mean of the dataset.
#In this function, we will give the x and R will figure out the d
mean.boot <- function(x,d){
  return (mean(x[d]))
}
