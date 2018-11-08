## TASK 6 ## 
# We know the value of the sums of each column of inputdata.txt. Use a for loop
# to verify they are correct
inputdata <- matrix(scan('datasets/inputdata.txt'), ncol=3, byrow=T)
for (i in 1: dim(inputdata) [2]) { #I used ncol before, but you could use dim and after the matrix name add [1] to loop through rows or [2] to loop through columns
    if (i==1) { #Create the vector
      colsumvec <- sum(inputdata[,i])
    } else { #append onto colsumvec if i != 1
      colsumvec <- c(colsumvec, sum(inputdata[,i]))
    }#end if statment
    print(colsumvec)
}

##ANOTHER WAY TO WRITE THIS CODE...

for (i in 1: ncol(inputdata)) {#The [2] in the original code above is what calls the number of columns rather than using ncol
  if (i==1) { #Create the vector
    colsumvec <- sum(inputdata[,i])
  } else { #append onto colsumvec if i != 1
    colsumvec <- c(colsumvec, sum(inputdata[,i]))
  }#end if statment
  print(colsumvec)
}

##END TASK 6##


#creating a function
sum.new <- function(x) { #x is the inpt vetor we are summing
  sum.x <- 0
  for (i in 1:length(x)){
    sum.x <- sum.x + x[i]
  }
  return(sum.x) #return is different from print because print just put the numbers on the screen but return outputs the values in a way that you can use the values that 'return' output. 
}


##TASK 7##

det.rows <- function(x){ #create a function that will determine the number of rows in a matrix
#  num.rows <- dim(x)[1] #I had this in there but it was really not necessary
  return(dim(x)[1])
}

#you could also doooooooooooooooooo

det.rows2 <- function(x){
  return(nrow(x))
}
##END TASK 7##

