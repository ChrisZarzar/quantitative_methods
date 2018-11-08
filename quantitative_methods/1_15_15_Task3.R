#TASK 3
task3.vec <- c(10,11,13,14,16,20,19,16,17,11,9,14) #Create the vector
task3.vec
task3.mat <- matrix(task3.vec,ncol=3,byrow=T) # Turn the vector into a matrix with three columns which fills in by row
task3.mat
task3.matsub <- task3.mat[,2] - task3.mat[,3] #subtrat the second an third columns and put that into a new vector
task3.matsub
dim(task3.mat)
length(task3.matsub)
