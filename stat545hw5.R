#this program corresponds to assignment 2.3
#computes the MLEs for xi and lambda using the 
#Newton-Raphson method

xi <- .75
lambda <- .3995

lnf <- function(xi,lambda){
  return(3062*log(xi+(1-xi)*exp(-lambda))+1013*((log(1-xi)-lambda))+1628*log(lambda))
}

fx <- function(xi,lambda){
  return(((-3062*(1-exp(-lambda)))/((1-xi)*exp(-lambda)+xi))-(1013/(1-xi)))
}
fl <- function(xi,lambda){
  return(((-3062*(1-xi)*exp(-lambda))/((1-xi)*exp(-lambda)+xi))+((1/lambda)*1628)-1013)
}



#a,b,and c are the second partial derivatives of the likelihood function with respect to xi and lambda

a <- (-3062*(1-exp(-lambda))^2)/(((1-xi)*exp(-lambda)+xi)^2)-((1013)/(1-xi)^2)
b <- (3062*exp(-lambda)/((1-xi)*exp(-lambda)+xi))+((3062*(1-xi)*(1-exp(-lambda))*exp(-lambda))/(((1-xi)*exp(-lambda)+xi)^2))
c <- (3062*(1-xi)*exp(-lambda)/((1-xi)*exp(-lambda)+xi))-((3062*((1-xi)^2)*exp(-2*lambda))/(((1-xi)*exp(-lambda)+xi)^2))-((1/(lambda^2))*1628)

i <- 1
repeat{
  jacobian <- matrix(c(a,b,b,c),2,2)
  new.param <- c(xi,lambda) - solve(jacobian,c(fx(xi,lambda),fl(xi,lambda)))
  if (abs(new.param[1]-.6150567) <= .0000001 && abs(new.param[2]-1.0378391) <= .0000001 || i >= 80){break}
  xi <- new.param[1]
  lambda <- new.param[2]
  a <- (-3062*(1-exp(-lambda))^2)/(((1-xi)*exp(-lambda)+xi)^2)-((1013)/(1-xi)^2)
  b <- (3062*exp(-lambda)/((1-xi)*exp(-lambda)+xi))+((3062*(1-xi)*(1-exp(-lambda))*exp(-lambda))/(((1-xi)*exp(-lambda)+xi)^2))
  c <- (3062*(1-xi)*exp(-lambda)/((1-xi)*exp(-lambda)+xi))-((3062*((1-xi)^2)*exp(-2*lambda))/(((1-xi)*exp(-lambda)+xi)^2))-((1/(lambda^2))*1628)
  i <- i + 1
  print(c(xi,lambda))
  print(i)
}







