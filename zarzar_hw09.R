# 1)

wind.data <- wind.data.orig[,-1] #Delete the years from the matrix

#Set up the matricies
set.seed(100)
train.labs <- sample(c(1:137),100)
test.labs <- c(1:137)[-train.labs]
train.wind <- wind.data[train.labs,]
test.wind <- wind.data[test.labs,]
train.labs <- sample(c(1:363),300)
test.labs <- c(1:363)[-train.labs]
train.hail <- hail.data[train.labs,]
test.hail <- hail.data[test.labs,]



# for the multivariate regressions, I am going to start with a correlation
# between the predictors to get some idea of what can probably be left out.
# Then, I will run a  stepwise regression which will help me finalized what
# predictors I will keep in the final regression. I am then going to run a
# regression with those predictors I deem useful. ANOVA and Sumary output will
# provide information about how well each regression did. A cross-validation
# will be run to see how the model performs in predicting the ranking index and
# determine its bias in predictions. Finally, calculating the RMSE will provide
# the final bit of information needed to determine which model performs best

source('rcodes/stepwise.R')

#Column 2 is Tornado, 3 is Hail, 4 is Wind

#Creating a model for the wind.data
predictand <- train.wind[,1]
predictors <- cbind(train.wind[,2],train.wind[,3],train.wind[,4])
cor(predictors)

#Looks like hail and wind have a pretty strong correlation, so using both of these variable may be unecessary

stepwise(predictand, predictors)

#After running stepwise, it is obvious I do not need wind AND hail, but one of
#them. Also, I do not necessary need either of them, it looks like tornadoes
#predict a large amount of variation in the ranking index without hail or wind
#included. This pushes me towards only keep tornadoes as a predictor, however,
#the fact that R squared goes from .474 to .492 when hail is included, and the
#fact that there is only a moderate correaltion between tornadoes and hail, i
#will keep hail in the final linear regression

predictors <- cbind(train.wind[,2],train.wind[,3])

lm.obj.wind <- lm(predictand~predictors)

#Now lets see how the model did. 
anova(lm.obj.wind)
#The anova test shows that the model is a good statistical fit.

summary(lm.obj.wind)

source('rcodes/predictlm.R')

y.hat.wind <- predictlm(lm.obj.wind,cbind(test.wind[,2],test.wind[,3])) # using the test data to run the lm model and predict the output

test.rank <- test.wind[,1]
rmse <- sqrt(mean((test.rank-y.hat.wind)^2)) #see how the model did predicting by comparing to the test data
rmse 

median(test.rank-y.hat.wind) # see how far off the y.hat.wind mediam is from the actual values to investigate bias.

#There is the test for wind. Now I need to do the same for hail, interpret the output, then I will be done with question 1. 

#Creating a model for the hail.data
predictand <- train.hail[,1]
predictors <- cbind(train.hail[,2],train.hail[,3],train.hail[,4])
cor(predictors)

#Looks like hail and wind have a pretty strong correlation, so using both of
#these variable may be unecessary. Also, the tornado columns has very weak
#correlations with the other two columsn, so one of the other two variabels
#should add more information to this model than they did for the model created
#with wind.

stepwise(predictand, predictors)

#After running stepwise, it looks like the hail column needs to be left out of
#the model. Also interesting is that the R-squared value is greater than the
#R-squared from the wind model. Therefore, this model should do a better job
#predicting the ranking index using the number of tornado and wind reports as
#predictors.

predictors <- cbind(train.hail[,2],train.hail[,4])

lm.obj.hail <- lm(predictand~predictors)

#Now lets see how the model did. 
anova(lm.obj.hail)
#The anova test shows that the model is a good statistical fit.

summary(lm.obj.hail)
#The summary table indicated that both prodictors make important contributions
#to the model. It also shows us that teh model appears to be a better fit to the
#data that the values from the wind dataset did.



y.hat.hail <- predictlm(lm.obj.hail,cbind(test.hail[,2],test.hail[,4])) # using the test data to run the lm model and predict the output

test.rank <- test.hail[,1]
rmse <- sqrt(mean((test.rank-y.hat.hail)^2)) #see how the model did predicting by comparing to the test data
rmse #Got a value of 0.594569

median(test.rank-y.hat.hail) # see how far off the y.hat.wind mediam is from the actual values to investigate bias.



# 2)

#Setting up the predictors
train.wind.new <- cbind(train.wind[,1],train.wind[,2],train.wind[,4])
train.hail.new <- cbind(train.hail[,1],train.hail[,2],train.hail[,4])

predictors <-rbind(train.wind.new, train.hail.new)

#Now we will set up a matrix that will be equilivalent to 100 wind values set to 1 and 300 hail values set to 0
predictand <- matrix(c(rep(1,100),rep(0,300)),ncol=1, byrow=T)

logr.obj.train <- glm(predictand~predictors,family=binomial(link='logit'))
logr.obj.train$fitted.values


y.hat <- ifelse(logr.obj.train$fitted.values>0.5,1,0)
y.hat

#Setting up the predictors
test.wind.new <- cbind(test.wind[,1],test.wind[,2],test.wind[,4])
test.hail.new <- cbind(test.hail[,1],test.hail[,2],test.hail[,4])

obs <- matrix(c(rep(1,37),rep(0,63)),ncol=1,byrow=T)
    
new.predictors <-rbind(test.wind.new, test.hail.new)

exponents <- predictlm(logr.obj.train, new.predictors)

new.probs <- exp(exponents)/(1+exp(exponents))

y.hat.new <- ifelse(new.probs>0.5,1,0)
y.hat.new


#We now need to make the contigency tables and calculate the statistics

source('rcodes/contable.R')
source('rcodes/table.stats.R')


contab.out <- con.table(obs, y.hat.new)

contab.out


table.stats(contab.out)

#Now to run the Brier skill score

source('rcodes/brier.R')
brier(obs, new.probs)


# So our model did not do very well because our brier skill score is low, .29



