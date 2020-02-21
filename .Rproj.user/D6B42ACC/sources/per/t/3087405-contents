library(limSolve)
library(tidyverse)
library(reshape2)

n <- readline(prompt = "Input sampling density: ")
n <- as.integer(n)

e1 <- function(x) {
  if (0 < x && x <= 2/3) {
    return (3*x/2)
  } else if (2/3 < x && x < 4/3) {
    return ((4 - 3*x)/2)
  } else {
    return (0)
  } 
}

e2 <- function(x) {
  if (2/3 < x && x <= 4/3) {
    return ((3*x - 2)/2)
  } else if (4/3 < x && x < 2) {
    return ((6 - 3*x)/2)
  } else {
    return (0)
  } 
}

e3 <- function(x) {
  if (4/3 < x && x <= 2) {
    return ((3*x - 4)/2)
  } else {
    return (0)
  } 
}

"%f*x%" <- Vectorize(function(f, n) {
  force(f)
  force(n)
  function(x) f(x) * n
})

addFunctions <- function(f1,f2) {
  force(f1)
  force(f2)
  function(x) f1(x) + f2(x)
}

B <- matrix(rep(0, len = 9),nrow = 3, ncol = 3)
B[1,1] <- 73/27
B[2,1] <- -29/18
B[3,1] <- 0
B[1,2] <- B[2,1]
B[2,2] <- 35/9
B[3,2] <- -41/18
B[1,3] <- B[3,1]
B[2,3] <- B[3,2]
B[3,3] <- 59/54

L <- c(0, 0, 0)

W <- Solve(B,L)

u <- c(e1, e2, e3)
u <- u %f*x% W
u <- Reduce(addFunctions, u)

temp <- matrix(rep(0, len = n), ncol = n, nrow = 1)
for (i in 1:n) {
  x <- (2*(i-1))/n
  temp[1, i] <- u(x)
}

colnames(temp) <- seq(from=0,by=2/n, length.out = n)
longData <- melt(temp)

ggplot(longData, aes(x = Var2, y = value)) + 
  geom_line() + labs(x = "x", y = "y", title = "Vibration")

