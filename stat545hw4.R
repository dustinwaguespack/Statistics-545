###################################################
#Dustin Waguespack                                #
#Stat 545                                         #
#2/25/16                                          #
#This program corresponds to assignment 2.2       #
#This program uses the Newton-Raphson method to   #
#approximate the roots of two equations           #
#with two unknowns                                #
###################################################
newton.raphson <- function(init.x,init.y){
  f1 <- function(x1,x2){
    return(3*(x1)^2 + 2*x1*x2 + 5*(x2)^2 - 69)
  }
  f2 <- function(x1,x2){
    return(4*(x1)^2 - 3*x1*x2 + (x2)^2 - 7)
  }
  x <-c(init.x,init.y)
  i <- 1
  repeat{
    jacobian <- t(matrix(c(6*x[1]+2*x[2],10*x[2]+2*x[1],
                           8*x[1]-3*x[2],2*x[2]-3*x[1]),2,2))
    x <- x - solve(jacobian,c(f1(x[1],x[2]),f2(x[1],x[2]))) 
    i <- i+1
    if(abs(f1(x[1],x[2])) <= 1.0e-7 & abs(f2(x[1],x[2])) <= 1.0e-7 || i >= 100){break}
  }
  print(x)
}
newton.raphson(1,2)
newton.raphson(.1,6)
