colsummax <- function(x){  #this sets up the function, colsummax
  dummy <- -1000000  #set up a dummy variable to compare each column to
  for (n in 1:ncol(x)){ #using ncol tells the for loop to loop from 1 to whatever number of columns there are in the matrix
  colsum <- sum(x[,n])     #This sums up the column
    if (colsum > dummy) {
      dummy <- colsum
      columnnum <- n 
    } else{
      dummy <- dummy
    } 
  }
  return(paste0('The column with the largest sum is column ',columnnum, ' with a total of ',dummy)) #This will spit out the column with the maximum sum.  
}

set.seed(45)
datamat <- matrix(rnorm(100000),ncol=1000,byrow=T)
colsummax(datamat)

# OUTPUT: [1] "The column with the largest sum is column 132 with a total of 36.1331785032335"