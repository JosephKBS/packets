# --------------Geomtetric distribution-------------------

geometric = function(N, p){
  results = rep(0, N)
  for(i in 1:N){
   x = 0
   k = 0
   while(x == 0){
      x = sample(c(0,1), size = 1, replace = TRUE, prob = c(1-p, p))
      k = k + 1
   }
  results[i] = k
  }
  results
}

y = geometric(10000, 0.7)
table(y)/10000
plot(table(y)/10000, ylab = "Probability")
plot(ecdf(y))

# --------------Nega_dist real function using rbin-------------------

N <- 10000
p <- 0.7
r <- 5
results = rep(0, N)
  for(i in 1:N){
   x = 0
   k = 0
   while(x < r){
      x = sample(c(0,1), size = 1, replace = TRUE, prob = c(1-p, p))
      k = k + 1
   }
  results[i] = k
}

plot(ecdf(rnbinom(10000, 5, 0.7)))
plot(ecdf(results))


#------------------------------------------
#------------------------------------------
geometric = function(N, p){
  results = rep(0, N)
  for(i in 1:N){
   x = 0
   k = 0
   while(x == 0){
      x = sample(c(0,1), size = 1, replace = TRUE, prob = c(1-p, p))
      k = k + 1  }
  results[i] = k } 
  results }
# y = geometric(10000, 0.7)
# table(y)/10000
#------------------------------------------
add_all <- function(k){
  for (k in 1:k){
    result <- sum(1:k) }
   result }
#--------------negaative_binomial ----------
y <- as.data.frame(geometric(10000, 0.7))
table(y)

table1 <- data.frame(matrix(NA, nrow=10, ncol=3 ))
for (i in 2:10){
  table1[1,1] <- length(which(y == 1) )
  table1[1,2] <- table1[1,1]
  table1[i,1] <- length(which(y == i) )
  table1[i,2] <- table1[i,1] + table1[i-1,2]
  table1[1,3] <- 1
  table1[i,3] <- i
}
print(paste("P(x=1) is :", table1[1,2]/10000) )
print(paste("P(x=5) is :", table1[5,2]/10000) )
#----------------plotting process-------------------------                   
table2 <- data.frame(matrix(NA, nrow=22, ncol=3 ))
for (i in 1:10){
  table2[2*i-1, 1] <- i -1
  table2[2*i,1] <- i+0.999999 -1
  
  table2[1,2] <- 0
  table2[2,2] <- 0
  table2[2*i-1,2] <- table1[i,2]
  table2[2*i,2] <- table1[i,2]
}
 plot(table2[,1], table2[,2], type='l', col="blue", 
     main="CDF plot", xlab="X variables", 
     ylab="Cumulative probabilities", lty=2)
for (j in 1:10 ) {
   points(table2[2*j,1], table2[2*j,2], cex=1.2, pch=1, col="red")
   points(table2[2*j-1,1], table2[2*j-1,2],cex=1.2, pch=16, col="red")  
   }   
# lines( table2[2*j-1:2*j, 1] , table2[2*j-1:2*j, 2] , type='l', col="red" )
lines( table2[1:2, 1] , table2[1:2, 2] , type='l', col="red" )
lines( table2[3:4, 1] , table2[3:4, 2] , type='l', col="red" )
lines( table2[5:6, 1] , table2[5:6, 2] , type='l', col="red" )
lines( table2[7:8, 1] , table2[7:8, 2] , type='l', col="red" )
lines( table2[9:10, 1] , table2[9:10, 2] , type='l', col="red" )
lines( table2[11:12, 1] , table2[11:12, 2] , type='l', col="red" )


r = 5
p = 0.70
n = 5 - r
# exact
dnbinom(x = n, size = r, prob = p)
mean(rnbinom(n = 10000, size = r, prob = p) == n)

library(dplyr)
library(ggplot2)

data.frame(x = 0:10, prob = dnbinom(x = 0:10, size = r, prob = p)) %>%
  mutate(Failures = ifelse(x == n, n, "other")) %>%
ggplot(aes(x = factor(x), y = prob, fill = Failures)) +
  geom_col() +
  geom_text(
    aes(label = round(prob,2), y = prob + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Probability of r = 3 Successes in X = 7 Trials",
       subtitle = "NB(3,.2)",
       x = "Failed Trials (X - r)",
       y = "Probability") 
# plot------------------------------------------------
plot(table(y)/10000, ylab = "geometric: Probability")
plot(table(y_bi)/10000, ylab = "negative-binom Probability")
plot(ecdf(y))
plot(ecdf(y_bi))
#--------------------------------------------------

```{r}
########################################
# N is total number 
# n is trial
# p is probability
binomial = function(N, n, p){
  results = rep(0, N)
  for(i in 1:N){
    results[i] = sum(sample(c(0,1), n, replace = T, prob = c(1-p, p)))
  }
   results }
x = binomial(10000, 5, 0.7)
x <- as.data.frame(x)

for (m in 1:length(uniqe(x)) ){
  table2[m,1] <- uniqie(x)[m]
  table2[m,2] <- 
}

#( table(x)/10000 )
plot(x)
############################################
N <- 10000
table1 <- data.frame(matrix(nrow = N, ncol = 3))

negative_binom <- function(p,r,x){
  choose(x-1, r-1)*( (1-p)^(x-r) )*p^r
}

x_ran_trial <- function(range){
  sample(range, 1 , replace=T)
}

p <- 0.7
r <- 5

for (i in 2:N){
  table1[1,1] <- negative_binom(p,r,5)
  table1[1,2] <- table1[1,1]
  
  table1[i,1] <- negative_binom(p,r,5)
  table1[i,2] <- table1[i-1,1] + table1[i,1]
  table1[i,3] <- as.numeric(i)  
}

par(mfrow=c(2,2))
plot(density(table1[,1]))
plot(density( table[,3] , table1[,2]))

#----------------all function from the beginning-----------------------------------
#----------------all function from the beginning-----------------------------------
n = 5 # size
p = 0.8 # prob of success

#----------function-----------------
berllouni <- function(n,p){
  sample(0:1, size=n, replace=TRUE, prob=c(1-p,p))
}

berllouni(10, 0.8) # test
#----------binomial dist------------------
binomial <-  function(N, n, p){
  results = rep(0, N)
  for(i in 1:N){
    results[i] = sum(sample(c(0,1), n, replace = T, prob = c(1-p, p))) # counting how many success
  }
   results 
}

unique(binomial(3000, 10, 0.7))
hist( binomial(3000, 10, 0.7) )

# x = binomial(10000, 5, 0.7)
# x <- as.data.frame(x)
# table(x)/10000

# plot(table(x)/10000, ylab = "Probability")
# plot(ecdf(x))
#----------geometric dist------------------
geometric = function(N, p){
  results = rep(0, N)
  for(i in 1:N){
   x = 0 # happen in coin flipped ( o or 1)
   k = 0 # num of fail
   while(x == 0){
      x = sample(c(0,1), size = 1, replace = TRUE, prob = c(1-p, p))
      k = k + 1
   }
  results[i] = k # num of fail 
  }
  results
}
hist( geometric(1000, 0.7) )
#----------negative dist------------------
nega_bin <- function(N, r, p){
  results = rep(0, N)
  
  for(i in 1:N){
   x = 0  # num of success
   k = 0  # num fail before success
   while(x < r){
      x = sum(sample(c(0,1), size = r, replace = TRUE, prob = c(1-p, p)))
      k = k + 1  
      }
  results[i] = k # num of fail 
  }
  return(results)
}
