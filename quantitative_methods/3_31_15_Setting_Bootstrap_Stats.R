

#Chris Zarzar
#3-31-15
#Setting up all statistical variables that I may want to use in a bootstrap 

mean.boot <- function(x,d){
  return(mean(x[d]))
}

var.boot <- function(x,d){
  return(var(x[d]))
}

sd.boot <- function(x,d){
  return(sd(x[d]))
}

IQR.boot <- function(x,d){
  return(IQR(x[d]))
}

mode.boot <- function(x,d){
  return(Mode(x[d]))
}

median.boot <- function(x,d){
  return(median(x[d]))
}

min.boot <- function(x,d){
  return(min(x[d]))
}

max.boot <- function(x,d){
  return(max(x[d]))
}

kurtosis.boot <- function(x,d){
  return(kurtosis(x[d]))
}

skewness.boot <- function(x,d){
  return(skewness(x[d]))
}

#Chris Zarzar
#3-31-15
#Working on setting up the bootstrap covariance statistic


covar.boot <- function(x,v,d){ #Bootstrap Covariance
  return(var(x[d],v[d]))
}  

#Chris Zarzar
#3-31-15
#Working on setting up the bootstrap pearson correlation statistic

cor.boot <- function(x,v,d){
  return(cor(x[d],v[d]))
}

#Chris Zarzar
#3-31-15
#Working on setting up the bootstrap spearman correlation statistic

spearman.boot <- function(x,v,d){
  return(cor(x[d],v[d],method="spearman"))  
}
