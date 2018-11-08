#----------------NOTES------------------------------

#1-19-15
#This code is for assignment 1. I have so far figured out how to loop through however many 
#columns are in a given matrix and then find the sum of each column. Now I need to figure out 
#how to compare those sums and figure out which is largest. 

#1-20-15
#Just talked to Mercer and he helped me figure out what I need to do to finish the code. 
#What I need to do now is set up a dummy variable with a really low value, like -1,000,000
#then, I will use an if statement to compare each column sum to that dummy variable, if the 
#column sum is greater than the variable, it will replace the variable. I will also need another
#variable to record the column number in so I at the end I can report back the column number with the 
#greatest sum


#---------------NOTES--END--------------------------


colsummax <- function(x){  #this sets up the function, colsummax
  dummy <- -1000000
  for (n in 1:ncol(x)){ #using ncol tells the for loop to loop from 1 to whatever number of columns there are in the matrix
  colsum <- sum(x[,n])     #This sums up the thrid column
    if (colsum > dummy) {
      dummy <- colsum
      columnnum <- n 
    } else{
      dummy <- dummy
    } 

  }
  print(paste0('The column with the largest sum is column ',columnnum, ' with a total of ',dummy)) #This will spit out the column with the maximum sum.  
}

# The code below is the matrix that Mercer wants us to run the function on.
set.seed(45)
datamat <- matrix(rnorm(100000),ncol=1000,byrow=T)
colsummax(datamat)

# [1] "The column with the largest sum is column 132 with a total of 36.1331785032335"