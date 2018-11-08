source('rcodes/plot.weathermap.R')
source('rcodes/congruence.R')

hgtdata <- matrix(scan('datasets/hgtdata.txt'),ncol=493,byrow=T)

#Procedure for a PCA
# 1. Compute Z from your data
# 2. correlation matrix R
# 3. Eigen analysis V D 
# 4. A=VD^0.5
# 5. F=Z(A^T)^-1

#There is no reaason to not rotate you PCA. If the original distribution of the data is best, the the algorithm will simply leave the PCA nonrotated
#You can use varimax() to do an orthoganal rotation or promax to do n oblique rotation. Mercer likes to start 
#with promax and then if any PC correlation is >0.33, then he switches to varimax 

install.packages('maps')
library('maps')
map("usa")
map("state")
map('state','Mississippi')

lons <- seq(-130, -60, by=2.5)
lons
lats<- seq(20,60,by=2.5)
lats

dim(hgtdata)

#3653 is 10 years of daily data where the three is from the leaps years 
plotmat <- matrix(hgtdata[1,],ncol=29, byrow=T)
length(lats)*length(lons)
plot.weathermap(lons,lats,plotmat,interval=50)

#DDo that PCA analysis yo!
z.mat <- scale(hgtdata)
hgt.mean <- apply(z.mat,2,mean)
hgt.mean
hgt.sd <- apply (z.mat,2,sd)
hgt.sd
#boom, so this tells us that we have calulated Z because we have means near zero and SD of 1

cor.mat <- cor(z.mat)
cor.mat

eigen.mat <- eigen(cor.mat)

#So far we have Z, R, V, and D

#Now lets check that scree plot homie
plot(eigen.mat$values[1:15]) # We do 15 becasue this typically gauruntees you will get all of the PCs that you will need

load.mat <- eigen.mat$vectors[,1:6]%*%sqrt(diag(eigen.mat$values[1:6]))

#Now we need to rotate the loading matrix

rot.obli <- promax(load.mat)$loadings
rot.obli

#Z dimensions is 3653x4983
#R dimensions is 493 x 493
#V dimensions is 493 x 6
#D dimensions is 6 x 6
#A dimensions is 493 x 6
#B dimensions is 493 x 6

# next we do the congruence test to see if 6 PCs were the right number to keep
congruence(cor.mat, rot.obli)

#Yuck, none exceed .81 so we do not want to keep nearly as many PCs as we kept
#lets go back and try 4 PCs, so now we hav eto recalculate the loading matrix

load.mat <- eigen.mat$vectors[,1:4]%*%sqrt(diag(eigen.mat$values[1:4]))

#Now we need to rotate the loading matrix

rot.obli <- promax(load.mat)$loadings
rot.obli

#Z dimensions is 3653x4983
#R dimensions is 493 x 493
#V dimensions is 493 x 4
#D dimensions is 4 x 4
#A dimensions is 493 x 4
#B dimensions is 493 x 4


# next we do the congruence test to see if 4 PCs were the right number to keep
congruence(cor.mat, rot.obli)

#Better but not there yet


load.mat <- eigen.mat$vectors[,1:2]%*%sqrt(diag(eigen.mat$values[1:2]))

#Now we need to rotate the loading matrix

rot.obli <- promax(load.mat)$loadings
rot.obli

#Z dimensions is 3653x4983
#R dimensions is 493 x 493
#V dimensions is 493 x 2
#D dimensions is 2 x 2
#A dimensions is 493 x 2
#B dimensions is 493 x 2


# next we do the congruence test to see if 4 PCs were the right number to keep
congruence(cor.mat, rot.obli)

#There we go, 2 PCs looks pretty good.


#F dimesnsions is 3653 x 2

#lets solve for F
score.mat <- z.mat %*% rot.obli %*% solve(t(rot.obli)%*%rot.obli)
dim(score.mat)

#S-mode, colums of B are the dominant spatial patterns within R
#Columns of F are the scores which represent the relative match between any time and the coluns of B

plotmat <-matrix(cor.mat[200,],ncol=29,byrow=T) #IT is off the coast of NC, so this is showing the correlation of that one gridpoint compared to the other values, so at the station it is 1 and the correlations drops as you move away. 
plot.weathermap(lons,lats,plotmat,interval=0.1)

plotmat<-matrix(rot.obli[,1],ncol=29,byrow=T)
plot.weathermap(lons,lats,plotmat,interval=0.1) 
#So here, we have taken those 493 grid points and turned them into 1 map with this first PC


plotmat<-matrix(rot.obli[,2],ncol=29,byrow=T)
plot.weathermap(lons,lats,plotmat,interval=0.1) 
#So here, we have taken those 493 grid points and turned them into 1 map with this first PC

#The signs on this map output are irrelevant becuse it can change how R on different computers computes the eigenvalues

var.explained <- apply(rot.obli^2/493,2,sum)
var.explained

#How do we use the scores?

#how do we relate to score informaiton in the weather map back to reality

plot(score.mat[,1],type='l')

which(score.mat[,1]==max(score.mat[,1])) #Day where it si large negative


which(score.mat[,1]==min(score.mat[,1])) #Day where it is large negative

score.mat[107,1] #Day where it is near zero

plotmat<-matrix(hgtdata[10,],ncol=29,byrow=T)
plot.weathermap(lons,lat,plotmat,interval=50)


plotmat<-matrix(z.mat[10,],ncol=29,byrow=T)
plot.weathermap(lons,lats,plotmat,interval=0.1)


plotmat<-matrix(z.mat[204,],ncol=29,byrow=T)
plot.weathermap(lons,lats,plotmat,interval=0.1)


plotmat<-matrix(z.mat[107,],ncol=29,byrow=T)
plot.weathermap(lons,lats,plotmat,interval=0.1)


