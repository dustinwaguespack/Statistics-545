###################################################
#Dustin Waguespack                                #
#Stat 545                                         #
#2/25/16                                          #
#this program corresponds to assignment 2.1 #1    #
#in which the root for the funtcion f is computed #
#using the Illinois method                        #
###################################################
illinois.method <- function(x,y){
  f <- function(arg){return(arg*exp(arg)-1)}
  repeat{
    if(f(x)*f(y) > 0){
      x <- max(x, x - .05)
      y <- y + .05}
    else {break}
  }
  fx <- f(x)
  fy <- f(y)
  c0 <- (x*fy - y*fx)/(fy - fx)
  l <- 1
  repeat{
    c1 <- (x*fy - y*fx)/(fy - fx)
    if(f(c1)*fx < 0){
      y <- c1
      fy <- f(y)
      if (f(c0)*f(c1) > 0){fx <- fx/2}}
    else{
      x <- c1
      fx <- f(x)
      if (f(c0)*f(c1) > 0){fy <- fy/2}}
    if (l >= 1000 || abs(f(c1)) <= 1.0e-7){break}
    c0 <- c1
    l <- l + 1
  }
  print(c1)
}
illinois.method(0,1)