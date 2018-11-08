#Class notes taken January 20th 2015. **Need to change the filename to 2015, not 2014

if (4>3) { #Set up a logical test, if it is true, run the statement in the curly brackets
  x <- 4-3 #Set x equal to...
  y <- 5-3 #Set y equal to..
}
x #Ouput the value of x
y #Output the value of y
# In the console, we just did rm(x) and rm(y) to remove those variables from the environment memory

if (4==3) { #Set up a logical test, if it is true, run the statement in the curly brackets. If it is not true, do not run it
  x <- 4-3 #Set x equal to...
  y <- 5-3 #Set y equal to..
}
x #Ouput the value of x
y #Output the value of y

# #This time, we got 
#> x #Ouput the value of x
# Error: object 'x' not found
# > y #Output the value of y
# Error: object 'y' not found
# >
#WE got ths because the test was false and therefore the statement was not executed


if (4==3) { #Set up a logical test, if it is true, run the statement in the curly brackets. If it is not true, do not run it
  x <- 4-3 #Set x equal to...
  y <- 5-3 #Set y equal to..
} else {   #Turns out that else should be on the same line as the if statement closed curly bracket
  x <- 4^2
  y <- 5^2
}        
x #Ouput the value of x
y #Output the value of y

#This tine around, we got x = 16 and y = 25 because the if statement was false, so it ran the else statement


if (4!=3) { #Set up a logical test, if it is true, run the statement in the curly brackets. If it is not true, do not run it
  x <- 4-3 #Set x equal to...
  y <- 5-3 #Set y equal to..
} else {   #Turns out that else should be on the same line as the if statement closed curly bracket
  x <- 4^2
  y <- 5^2
}        
x #Ouput the value of x
y #Output the value of y

#This time x =1 and y = 2 because the if statement was true so it executed that statement and skipped the else statement. 

#If you need more test, you can use else if...
if (4==3) { #Set up a logical test, if it is true, run the statement in the curly brackets. If it is not true, do not run it
  x <- 4-3 #Set x equal to...
  y <- 5-3 #Set y equal to..
} else if (4<3) {   #Turns out that else should be on the same line as the if statement closed curly bracket
  x <- 4^2
  y <- 5^2
} else if (4!=3){
  x=10
  y=12
}     

x #Ouput the value of x
y #Output the value of y

##TASK 4##
inputdata <- matrix(scan('datasets/inputdata.txt'), ncol=3, byrow=T)
columnsum1 <- sum(inputdata[,1]) #find the sum of each column and assign each total to a variable
columnsum2 <- sum(inputdata[,2])
columnsum3 <- sum(inputdata[,3])

print (columnsum1)
print (columnsum2)
print (columnsum3)

dummy <- -100000000000000000000
colnum <- 0
if (columnsum1 > dummy) {
  dummy <- columnsum1
  colnum < -1
}
if (columnsum2 > dummy) {
  dummy <- columnsum2
  colnum <- 2
}
if (columnsum3 > dummy) {
  dummy <- columnsum3
  colnum <- 3
}
print(paste0( 'Column ',colnum, ' has the largest sum with a total of ',dummy))

#This gave the output...
#[1] "Column 2 has the largest sum with a total of 119700"



##END TASK 4##

##**HOW MERCER DID TASK 4**##
inputdata <- matrix(scan('datasets/inputdata.txt'), ncol=3, byrow=T)
col1.sum <- sum(inputdata[,1])
col2.sum <- sum(inputdata[,2])
col3.sum <- sum(inputdata[,3])

## Start with the first column ##
if (col1.sum > col2.sum && col1.sum > col3.sum){  #If this is true, then column 1 is the column with the largest sum
  print('Column 1 is the largest')
} 

## Second Column ##
if (col2.sum > col1.sum && col2.sum > col3.sum) { #If this is true, then column 2 is the column with the largest sum
  print('Column 2 is the largest')
}

## Third Column ##
if (col3.sum > col1.sum && col3.sum > col2.sum) { #If this is true, then column 2 is the column with the largest sum
  print('Column 3 is the largest')
}

#This gave the output...
#[1] "Column 2 is the largest"

##END MERCERS WAY OF DOING TASK 4##

example.data <- c(-10:10) #this will create a vector with a sequence of numbers from -10 to 10
example.data
ifelse(example.data<0,'N','P') #using ifelse is a nice way to set up a logical test that will scan 
#through the example.data vector and if they meet the criteria, they will be assigned a N for Negative. 
#If they do not meet the test criteria they will be assigned a P  for Positive. 
#HERE IS WHAT THE PROGRAM SPITS OUT

# [1] -10  -9  -8  -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7   8   9  10
# 
# [1] "N" "N" "N" "N" "N" "N" "N" "N" "N" "N" "P" "P" "P" "P" "P" "P" "P" "P" "P" "P" "P"



#How could you count up how many values in this list are negative?
ifelse(example.data < 0, 1, 0))
#The output of this is:  [1] 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0
#SO that assigned all the negative a value of 1. I can then add those up to see how many
#values are negative. EVen better, I can make this into a one line command with the script below.
sum(ifelse(example.data < 0, 1, 0))
#The output is:    [1] 10

#The ifelse statement is a very important statement in programming

##TASK 5##
#Find out how many times there are values in inputdata.txt that are larger than 1000

sum(ifelse(inputdata.txt > 1000, 1, 0)) #This scans inputdata.txt and looks for any values greater than 1000
#if the value is greater than 1000, it will assign it a value of 1, otherwise it will be zero. 
#the command then sums up the values, because they have been reassigned, the total will be the 
#number of values that were over 1000
#The output was: [1] 2099
##END TASK 5##

#When working wiht loops, you will start the loop with a counter variable (interger)
#So you will need a start values, an end values, and a command that repeats
#All of this stuff fits into a for statement 
for (counter in 1:#) { #Where the '#' is however many iterations you want to do
       Execute stuff
}

##   EXAMPLE  ##

for (counter in 1:50) { #This will loop 50 times. 
  print (counter*2) #THis will multiply the counter value by 2
}
#This printed from 2 to 100
