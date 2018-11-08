#Class Notes from 1-27-15

temp.data <- c(70,68,75,81,90,65,55,92,84,86)

mean (temp.data)
median (temp.data)
mode(temp.data) #This tells us what type of data we have

# > mean (temp.data)
# [1] 76.6
# > mean (temp.data)
# [1] 76.6
# > median (temp.data)
# [1] 78
# > mode(temp.data)
# [1] "numeric"
# >

source('rcodes/mode.R') #This is a function from Mercer that finds the true mode of the a dataset
#It is important to rememebr that with this mode function from mercer, you have to capatalize it, Mode

Mode(temp.data)
# > Mode(temp.data)
# [1] 55
# > 

## TASK 1##
ithaca_data <- matrix(scan('datasets/ithaca_data.txt'),ncol=4,byrow=T)
mean(ithaca_data[,3]) #mean for column 3
median(ithaca_data[,3]) #median for column 3
Mode(ithaca_data[,3]) #mode for column 3
mean(ithaca_data[,4]) #mean for column 4
median(ithaca_data[,4]) #median for column 4
Mode(ithaca_data[,4]) #mode for column 4

# > mean(ithaca_data[,3])
# [1] 29.22581
# > median(ithaca_data[,3])
# [1] 30
# > Mode(ithaca_data[,3])
# [1] 30
# > mean(ithaca_data[,4])
# [1] 13.32258
# > median(ithaca_data[,4])
# [1] 19
# > Mode(ithaca_data[,4])
# [1] 29

