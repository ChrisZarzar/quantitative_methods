#CLASSIFICATION
#This is an REEP approach
predictand <- ifelse(ithaca.data[,4]<10,1,0)
predictand

reep.obj <- lm(predictand~cbind(ithaca.data[,3],cana.data[,3],cana.data[,4]))
reep.obj

reep.obj$fitted.values
#These values are no longer binary because of the predictors, so now we need to round them back to 1 and 0

y.hat <- ifelse(reep.obj$fitted.values>0.5,1,0)
y.hat

rbind(predictand,y.hat) #Shows you in rows rather than in columns
#So it only missed two times. This means it hit 29/31 times, 90ish percent. So pretty darn good. 

# ****TASK 5******# 
# Using the ecuador dataset, formulate a model that predicts
# the probability of 1cm or more of rainfall per June using average monthly
# temperature as a predictor. Look     at the    fitted values and explain twhat
# you seen

june.ecuador <- matrix(scan('datasets/june_ecuador_data.txt'),ncol=4,byrow=T)
predictand <- ifelse(june.ecuador[,3]>=10,1,0)
ecuador.reep <- lm(predictand~june.ecuador[,2])
ecuador.reep$fitted.values
y.hat <- ifelse(ecuador.reep$fitted.values>0.5,1,0)
rbind(predictand,y.hat)

#Now lets do a better approach, the logistic approach

predictors <- cbind(ithaca.data[,3],cana.data[,3],cana.data[,4])
predictand <- ifelse(ithaca.data[,4]<10,1,0)
logr.obj <- glm(predictand~predictors,family=binomial(link='logit'))
logr.obj
logr.obj$fitted.values
#so the output is saying that, for example, "on the 29th, there was an 89 percent chance that there was less than 10 cm of rain.

#****TASK 6******#
#Repeat task 5 uaing a logitic regression instead of REEP

june.ecuador <- matrix(scan('datasets/june_ecuador_data.txt'),ncol=4,byrow=T)
predictand <- ifelse(june.ecuador[,3]>=10,1,0)
ecuador.logit <- glm(predictand~june.ecuador[,2],family=binomial(link='logit'))
ecuador.logit$fitted.values
y.hat <- ifelse(ecuador.logit$fitted.values>0.5,1,0)
rbind(predictand,y.hat)


