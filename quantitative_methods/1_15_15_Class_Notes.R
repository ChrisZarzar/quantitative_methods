grades <- c(100,90,80,70,60) #create a vector called grades with those 5 numbers. 'c' is an operator that creates a vector
grades #print the vector that was just created
grades[1] #print only the first value in the vector
grades[1:3] #print the first three values of the vector
grades.positions <- c(2,4) #CONFUSED ABOUT WHAT THIS DOES, ASK #takes the vector 'grade' and based off of the second values, creat a vector that includes that second value and the fourth value only
grades.positions

grades2 <- c(50,40,30,20,10) #create a second vector called grades
grades+grades2 #adding the two vectors will add each values together with the values in the same relative position in the other vector

#TASK 2

temp.data <- c(85,79,77,76,78,81,82,90,89,96)
temp.data
temp.matrow <- matrix(temp.data,ncol=2,byrow=T) #Make a new matrix called temp.mat using the temp.data dataset. Set up this matrix with two columns and fill in by row, not by column.
temp.matrow
temp.matcol <- matrix(temp.data,ncol=2,byrow=F) #Make a new matrix called temp.mat using the temp.data dataset. Set up this matrix with two columns and fill in by column, not by row
temp.matcol
#
#
temp.matrow[1,2]
temp.matrow[,2]
temp.matrow[1,]

#TASK 3
task3.vec <- c(10,11,13,14,16,20,19,16,17,11,9,14) #Create the vector
task3.vec
task3.mat <- matrix(task3.vec,ncol=3,byrow=T) # Turn the vector into a matrix with three columns which fills in by row
task3.mat
task3.matsub <- task3.mat[,2] - task3.mat[,3] #subtrat the second an third columns and put that into a new vector
task3.matsub
dim(task3.mat)
length(task3.matsub)


scan("datasets/inputdata.txt") #This is how you bring the data in, but it comes into vector for so you need instead do the command below to bring it in as a matrix
inputdata.file <- matrix(scan("datasets/inputdata.txt"),ncol=3,byrow=T)
dim(inputdata.file) #find out the dimensions of the file
inputdata.file[1,] #pull out the first row from the matrix


#Examples of the different types of possible varibles from lecture 1 slide 21
example.int <- c(1,2,3,4)
example.int
example.real <- c(1.1,2.2,3.3,4.4)
example.real
example.char <- c("I", "love", "stats!")
example.char
example.logic <- c(TRUE,FALSE,TRUE,FALSE)
example.logic
example.logic <- c(T,F,T,T)
example.logic

#examples of converting between data types
example <- sqrt(2) #one way to do the square root
example <- 2^0.5 #a second way to do the square root
example
as.character(example) #convert to a character
as.character(example)^2 #this will not work because the format has gone to character, so it cannot do calculations on the valus
as.double(as.character(example))^2 #now convert the values that are in character back to double so you can do a calculation


#Trying out some logical operators

3<4
4>3
3<=4
4>=3
4==4
4!=3
3!=4&&4!=3
3!=4||4==4

#Tring out an if statement
if (4>3) {
  x <- 4 - 3 
} #if (4<3) **adding this comment is nice because it reminds you that this currly brace goes with the above if statement. It could otherwise get confusing if you have multiple if statements.
x
