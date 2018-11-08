#Set up a sequence of numbers from -3 to 3
quants<- seq(-3,3,by=0.01)
#Now plot the sequnce 
plot(quants, dt(quants,df=10),type='l')
#Now lets add a new line to the plot and see what it looks like with a df of 1
points(quants,dt(quants,df=1),type='l',col='red')
points(quants,dt(quants,df=5),type='l',col='blue')
points(quants,dt(quants,df=30),type='l',col='purple')
points(quants,dt(quants,df=100),type='l',col='green')
points(quants,dnorm(quants),type='l',col='gold')