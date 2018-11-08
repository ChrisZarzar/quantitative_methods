mat.b <- matrix(c(4,2,3,7),ncol=2,byrow=T)
eig.b <- eigen(mat.b)
eig.b
eig.b$vectors
eig.b$values
mat.b %*% eig.b$vectors
eig.b$vectors[,1]*eig.b$values[1]
eig.b$vectors[,2]*eig.b$values[2]


#*******TASK2*********#
#Compute the eienvectors and eigenvalues for the column correlation matrix of CB.
#verify that the first eigenvector and eigenvalue satisfy the relationship for
#eigenvalues and eigenvectors.

#Recall how to generate the matricies
set.seed(15)
matrix.a <- matrix(runif(15,0,10),ncol=3, byrow=T)
matrix.b <- matrix(runif(15,0,10),ncol=3, byrow=T)
matrix.c <- matrix(runif(15,0,10),ncol=3, byrow=T)

mat.cb <- matrix.c%*%matrix.b 
eig.cb <- eigen(mat.cb)

cor(mat.cb)%*%eig.cb$vectors

eig.cb$values[1]*eig.cb$vectors[,1]


#Running a PCA 


#first thing to do is standardize the values
z.ith <- scale(ithaca.data[,4])
z.can <- scale(cana.data[,4])
z.mat <- cbind(z.ith,z.can)

#NOw we need to calculte R, the correlation matrix of Z

cor.mat <- cor(z.mat)
cor.mat

#now we get the eigen vectors and eigen values
eigen.mat <- eigen(cor.mat)
eigen.mat

#D
# $values
# [1] 1.91878261 0.08121739

#V
# $vectors
# [,1]       [,2]
# [1,] 0.7071068 -0.7071068
# [2,] 0.7071068  0.7071068

#Calculate the loading matrix
load.mat <- eigen.mat$vectors %*% sqrt(diag(eigen.mat$values))

#Now we need to calculate F by multiplying the loading matrix by Z

#Solve does the inverse something or another...
score.mat <- z.mat %*% solve(t(load.mat))
score.mat

cor(score.mat)
# So we have taken the very correlated values from z.mat and we have adjusted them so that they are no longer correlated and the most information is moved into the first PC

#Why do this, lets first check out a plot of the original values
plot(z.mat[,1],z.mat[,2])

#now lets overlay the eigen vectors on this plot THe first should point in the
#direction of the most variance of data and the second should be orthogonal to
#that

abline(0,eigen.mat$vectors[2,1]/eigen.mat$vectors[1,1]) #This is simply the slope, rise over run (y/x)
abline(0,eigen.mat$vectors[2,2]/eigen.mat$vectors[1,2])


#What happens when you take the eigen vectors and make them the new axis (Slide 34). You see that there appears to be no correlations which is what PCA does, it takes a correlated dataset and makes it uncorrelated

#Variance explained...
var.explained <- eigen.mat$values/sum(eigen.mat$values)
var.explained
# OUTPUT: [1] 0.9593913 0.0406087
#So the first PC is telling us about 96% of the varince in the data and the second PC is telling us only 4 percent

  