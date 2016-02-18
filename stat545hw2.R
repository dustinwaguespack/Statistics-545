#############################################################
#Dustin Waguespack                                          #
#Stat 545                                                   #
#2/25/16                                                    #
#this program corresponds to assignment 2.1 #2              #
#this program computes the 90 and 95 percentiles for        #
#a chi-squared distribution with 10, 15 and 20 degrees      #  
# of freedom using both Newton-Raphson and Illinois method  #
#############################################################
#deg = degrees of freedom
#quan = quantile expressed as a decimal
chi.quantile.il <- function(deg,quan){
  f <- function(value,degree,quan){return(pchisq(value,degree)-quan)}
  x <- deg*((1-(2/(9*deg)) + qnorm(quan)*sqrt(2/(9*deg)))^3)
  y <- x + 1
  repeat{
    fx <- f(x,deg,quan)
    fy <- f(y,deg,quan)
    if(fx*fy > 0){
      x <- max(0, x - .05)
      y <- y + .05
    }
    else {break}
  }
  c0 <- (x*fx - y*fy)/(fy - fx)
  l <-1
  repeat{
    c1 <- (x*fy - y*fx)/(fy - fx)
    if(f(c1,deg,quan)*fx < 0){
      y <- c1
      fy <- f(y,deg,quan)
      if(f(c0,deg,quan)*f(c1,deg,quan) > 0){fx <- fx/2}
    }
    else{
      x <- c1
      fx <- f(x,deg,quan)
      if(f(c0,deg,quan)*f(c1,deg,quan) > 0){fy <- fy/2}
    }
    c0 <- c1
    if(1 > 100 || abs(f(c1,deg,quan)) <= 1.0e-7){break}
    l <-l +1
  }
  print(c1)
}
chi.quantile.nr <- function(deg,quan){
  f <- function(x){return(pchisq(x,deg)-quan)}
  fp <- function(x){return(dchisq(x,deg))}
  x <- deg*((1-(2/(9*deg)) + qnorm(quan)*sqrt(2/(9*deg)))^3)
  l <- 1
  for (i in 1:6){
    repeat{
      x <- x - (f(x)/fp(x))
      if(abs(f(x)) <= 1.0e-7 || l > 100){break}
      l <- l + 1
      }
    }
  print(x)
}
chi.quantile.il(10,.90)
chi.quantile.nr(10,.90)
chi.quantile.il(15,.90)
chi.quantile.nr(15,.90)
chi.quantile.il(20,.90)
chi.quantile.nr(20,.90)
chi.quantile.il(10,.95)
chi.quantile.nr(10,.95)
chi.quantile.il(15,.95)
chi.quantile.nr(15,.95)
chi.quantile.il(20,.95)
chi.quantile.nr(20,.95)


