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
