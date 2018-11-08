pairs(ithaca.data[,2:4],labels=c("Ithaca precip","Ithaca hi","Ithaca lo"))
#There looks to be a nonlinear relationship between highs and lows that we may be able to to fit a nonliner model to 
#lets just start with a regular linear model to see how bad it does
lm.obj <- lm(ithaca.data[,3]~ithaca.data[,4])
#now lets try a logarithmic model
nlm.obj <- lm(log(ithaca.data[,3])~ithaca.data[,4])

summary(lm.obj)
summary(nlm.obj)

#The logarithmic model actually made it worse

#take a look at the linear model
plot(ithaca.data[,4],ithaca.data[,3],xlab="lows", ylab="highs")
abline(lm.obj)

#Tae a look at the logarithmic model 
plot(ithaca.data[,4],log(ithaca.data[,3]),xlab="lows", ylab="Log highs")
abline(nlm.obj)

#Now we will try to add higher order polynomials to better the fit
ith <- ithaca.data #just to make typing go faster

nlm.obj <- lm(ith[,3]~ith[,4]+I(ith[,4]^2)+I(ith[,4]^3))
nlm.obj
summary(nlm.obj)

#Lets take a look at it
plot(ithaca.data[,4],ithaca.data[,3],xlab="lows", ylab="highs")
curve(nlm.obj$coef[1]+nlm.obj$coef[2]*x+nlm.obj$coef[3]*x^2+nlm.obj$coef[4]*x^3,add=T) #using #coef pulls out the coefficients needed


#****TASK 4****#
#Grab the file td_e.txt, which ontains laboratory controlled dewpoint
#temperature data (1st columns) and measured vapor pressure data (2nd column).
#The relationship between these is well known, yet the data dont directly
#conform to it. Treat dewpoint as the predictor and vapor pressure as the
#preditand. so, formulate a nonlinear regression on these daa and diagnose your
#fit. To do this, do each of the Following:
#-linear fit 
#- log transform fit
#-polynomial fit 
#Provide scatterplots showing the quality of each fit
td_e.data <- matrix(scan('datasets/td_e.txt'),ncol=2,byrow=T)

plot(td_e.data[,2],td_e.data[,1],xlab="Vapor Pressure", ylab="Dewpoint")
lm.obj <- lm(td_e.data[,1]~td_e.data[,2])
logm.obj <- lm(td_e.data[,1]~log(td_e.data[,2])
polm.obj <- lm(td_e.data[,1]~td_e.data[,2]+I(td_e.data[,2]^2)+I(td_e.data[,2]^3))
summary(lm.obj)
summary(logm.obj)
summary(polm.obj)