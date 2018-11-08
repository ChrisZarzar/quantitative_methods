
predictand <- ithaca.data[,4]
predictor.mat <- cbind(ithaca.data[,3],cana.data[,3],cana.data[,4]) #This is how you tie together the predictors in the multivariate regressssion
lm.obj <- lm(predictand~predictor.mat)
lm.obj

anova(lm.obj)

#in this case, the df for predictors is 3 because we have three predicting variables 
# the df for the residuals is 27 because it is total n, minus the numner of precictor (3) minus 1, so n-Nx-1 == 31-3-1=27 
summary(lm.obj)
#From the summary, omly one predictor is significant telling us the other two predictors can be thrown out

lm.obj2 <- lm(predictand~cana.data[,4])   
lm.obj2

anova(lm.obj2)

summary(lm.obj2)
#eventhough the R squared went down slightly, it is not enoufh to want to use that earier and more complicated model

source('rcodes/stepwise.R')

stepwise(predictand,predictor.mat)
# 
# OUTPUT:
#   V1     V2        V3     V4
# 1 Var #    MSE R-squared F-stat
# 2     3 31.279     0.844 157.09
# 3     1  30.35     0.849 78.586
# 4     2 31.353     0.849 50.674
# This list is set up so everything above a varible. so first, it is only predictor 3, then preditor 3 and 1, then predictor 3,1,and 2.

cor(predictor.mat)

# 
# OUTPUT:
#   [,1]      [,2]      [,3]
# [1,] 1.0000000 0.9112218 0.7711347
# [2,] 0.9112218 1.0000000 0.8101689
# [3,] 0.7711347 0.8101689 1.0000000

source('rcodes/predictlm.R')


canada.new <- matrix(scan('datasets/canadaigua_new.txt'), ncol=4, byrow=T)

y.hat <- predictlm(lm.obj2,canada.new[,4]) # using the newdata to run the lm model and predict the output

y.hat

ithaca.new <- matrix(scan('datasets/ithaca_new.txt'),ncol=4,byrow=T)
rmse <- sqrt(mean((ithaca.new[,4]-y.hat)^2)) #see how the model did with brand new data
rmse

median(ithaca.new[,4]-y.hat) # see how far off the median was in the precition. maybe we coukd add or subttct the median difference found andget a better overall prediction




#########TASK 3#########
#conduct a stepwise regression on the june ecuador data, using both MSLP and
#temperature tp predict precipitation. Which variables should you keep? Grab the
#file test_ecuador.txt and redict precip valuez from that file using the
#stepqise model. How does it do?

#temp is column 2
#Mslp is column 4
# precip is column 3
june.ecu <- matrix(scan('datasets/june_ecuador_data.txt'),ncol=4,byrow=T)
test.ecu <- matrix(scan('datasets/test_ecuador.txt'),ncol=4,byrow=T)
predictand <- (june.ecu[,3])
predictmat <- cbind(june.ecu[,2],june.ecu[,4])
stepwise(predictand,predictmat)

cor(predictmat)

lm.ecu <- lm(predictand~predictor[,1]) #I have decided to only keep the second predictor, temp
lm.ecu
anova(lm.ecu)
summary(lm.ecu)

#so thats the model, now lets try to see how it does predicting with brand new data

do the y hat stuff, the predict function, then see what the rmse is