#-----------#
# JosephKBS #
#-----------#
# 1
cdf_dis_function <- function(xrange, fun){
  lower <- c(min(xrange)-1)
  upper <- c(max(xrange)+1)
  xrange_ext <- as.matrix((c(lower,xrange, upper) ))

  # empty data frame
  table <- data.frame(matrix(nrow = dim(xrange_ext)[1]*2 , ncol = 2))
  # loop
  for (i in 1:dim(xrange_ext)[1]){
  table[2*i,1] = t(xrange_ext)[i]
  table[2*i+1,1] = t(xrange_ext)[i+1]-0.001
  table[1,1] = t(xrange_ext)[1]-1
  
  table[2*i,2] = fun*(i-1)
  table[2*i+1,2] = fun*(i-1)
  
  table[1,2] <- 0
  table[2,2] <- 0
  table[dim(xrange_ext)[1]*2  ,2] <- 1
  table[dim(xrange_ext)[1]*2-1,2] <- 1
  }  
  plot(table[,1], table[,2], type='l', col="blue", main="CDF plot", xlab="X variables", ylab="Cumulative probabilities", lty=2) 
    # pointer creation
  for (j in 2:(dim(xrange_ext)[1]-1)){
   points(table[2*j,1], table[2*j,2], cex=1.2, pch=16, col="red")
   points(table[2*j-1,1], table[2*j-1,2],cex=1.2, pch=1, col="red")
   
   #points(table[2,1], table[2,2],cex=1.5, pch=1, col="red")
   points(table[dim(xrange_ext)[1]*2-2 ,1], table[dim(xrange_ext)[1]*2-2, 2], cex=1.2, pch=16, col="red")
   }
  # line highlight (only for 5 x_range)
  lines(table[1:3,1], table[1:3,2], type='l', col="red")
  lines(table[4:5,1], table[4:5,2], type='l', col="red")
  lines(table[6:7,1], table[6:7,2], type='l', col="red")
  lines(table[8:9,1], table[8:9,2], type='l', col="red")
}

#xrange <- c( -1, 0, 1)
#fun <- (1/3)
#cdf_dis_function(xrange, fun)

#3
cdf_function_num4 <- function(xrange, coef){
  low <- c(min(xrange)-1)
  upper <- c(max(xrange)+1)
  xrange_ext <- as.data.frame( c(low, xrange, upper) )
  # empty data frame
  table <- data.frame(matrix(nrow = dim(xrange_ext)[1]*2 , ncol = 3))
  
  # loop
  for (i in 1:dim(xrange_ext)[1]){
  # first column (X value)
  table[2*i-1,1] = t(xrange_ext)[i]
  table[2*i,1] = t(xrange_ext)[i+1]-0.001
  table[1,1] = 0 # t(xrange_ext)[1]-1
  table[2,1] = 0.9999 # t(xrange_ext)[1]-0.001
  table[dim(xrange_ext)[1]*2 ,1] = t(xrange_ext)[i]+0.0001
  
  # second col (Y value)
  table[1,2] <- 0
  table[2,2] <- 0 
    if (i >1) {
    table[2*i-1,2] <- coef*(table[2*i-1,1]) + (table[2*i-2, 2])
    table[2*i,2]   <- table[2*i-1, 2]
    }
  table[(dim(xrange_ext)[1]*2)-3 ,2] <- 1
  table[(dim(xrange_ext)[1]*2)-2 ,2] <- 1  

  table[(dim(xrange_ext)[1]*2)-1 ,2] <- 1
  table[dim(xrange_ext)[1]*2 ,2] <- 1
  
  table[2*i ,3] <- ("include")
  table[2*i-1 ,3] <- ("exclude")
  }
  plot(table[,1], table[,2],  type='l', 
       col="blue", main="CDF plot", xlab="X variables", ylab="Cumulative probabilities", lty=2)
  # pointer creation
  for (j in 2:(dim(xrange_ext)[1]-2)){
   points(table[2*j,1], table[2*j,2], cex=1.2, pch=1, col="red")
   points(table[2*j-1,1], table[2*j-1,2],cex=1.2, pch=16, col="red")
   
   points(table[2,1], table[2,2],cex=1.5, pch=1, col="red")
   points(table[dim(xrange_ext)[1]*2-3 ,1], table[dim(xrange_ext)[1]*2-3, 2], cex=1.2, pch=16, col="red")}
  # line highlight (only for 5 x_range)
  lines(table[1:2,1], table[1:2,2], type='l', col="red")
  lines(table[3:4,1], table[3:4,2], type='l', col="red")
  lines(table[5:6,1], table[5:6,2], type='l', col="red")
  lines(table[7:8,1], table[7:8,2], type='l', col="red")
  lines(table[9:10,1], table[9:10,2], type='l', col="red")
  lines(table[11:12,1], table[11:12,2], type='l', col="red")
}

# 4 factorial

factorial <- function(n) {
  if(n <= 1) {return(1)
  } else { 
    return(n * factorial(n-1)) }  }

# 5 poisson random variable
poisson_var <- function(mean, x){
  exp(-mean)*mean^(x)/factorial(x) }

# 6 negative binomial dist
# r sucess on the x trial, p=possibility
negative_binom <- function(p, r, x){
  choose(x-1, r-1)*( (1-p)^(x-r) )*p^(r)  }

# 7 hypergeometric calculator
# N is total number
# randomly drawing n elements
# r is tagged
# x is number of drawing from one side. (so the other will be n-x)

hyper_geo <- function (N, r, n, x) {
  choose(r,x)*choose(N-r, n-x)/choose(N, n)
}

# example 
# There are 20 cards (6red, 14 black). 5 cards is drawn without replacement.
# what is prob of drawing 4 red card?

#  hyper_geo(16,6,5,3)


#10 Limusine question
limu_trip <- function(num_show, prob){
  ( (prob)^(num_show) )*( (1-prob)^(6-num_show) ) * choose(6,num_show) 
}