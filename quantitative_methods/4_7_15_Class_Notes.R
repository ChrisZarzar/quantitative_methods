lm.obj <- lm(month.highs[,1]~month.highs[,2])
lm.obj

lm.obj$fitted.values #List the predicted values

lm.obj$residuals #List the residuals

sum(lm.obj$residuals) #Add up the residuals
 
lm.obj$coef #give the intercept and then the slope

anova(lm.obj)

pf(40.334,1,10) #This is the probabilty value for f 

1-pf(40.224,1,10) #This would be the test p test value. 

# If the 1-pf is less than 0.05, then we can reject the null hyothesis that teh MSE is larger than the MSR. 
#You just need to know if the F statistic if it is significant or not

summary(lm.obj) #Provides a nice output of all informaiton you would be interested in from the regression.

#*****TASK 2******#

#From the regression object you created for the Ecuador data, do an ANOVA and a
#summary. Also, look at the residuals(plot them in a histogram). Interpret your
#results. Redraw your scatterplot for some perspective

ecuador.data <- matrix(scan('datasets/june_ecuador_data.txt'), ncol=4, byrow=T)
cor.test(ecuador.data[,3],ecuador.data[,2], alternative = "two.sided") #correation between precip and temperature
#OUTPUT: 0.706
cor.test(ecuador.data[,3],ecuador.data[,2],method="spearman", alternative="two.sided") #There are some obvious outliers, so a spearman correlation is probably more appropriate
#OUTPUT:0.594
lm.ecu <- lm(ecuador.data[,3]~ecuador.data[,2])
lm.ecu
plot(ecuador.data[,2],ecuador.data[,3],xlab="Ecuador Average June Temperature (C)",ylab="Ecuador June Precipitation (mm)")
abline(lm.ecu)
anova(lm.ecu)
summary(lm.ecu)

lm.ecu$residuals

hist(lm.ecu$residuals)

#From his summary, we get a significant F value and we also get a significant t
#statistic which would suggest that we have a good fit (statistically speaking). 
#However, the R-squared value (0.4985) is rather low, so while the model is
#significant, it  does not do very well explaining the variance in y

median(lm.ecu$residuals)

predictand <- ithaca.data[,4]
predictor.mat <- cbind(ithaca.data[,3],cana.data[,3],cana.data[,4]) #This is how you tie together the predictors in the multivariate regressssion

lm.obj <- lm(predictand~predictor.mat)
lm.obj


anova(lm.obj)
summary(lm.obj)
#From the summary table, it is apparent that predictor 3, the cana lows are the only predictor that is significant. So we should throw out the other two predictors. 

