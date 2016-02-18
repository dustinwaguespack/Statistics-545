#####################################################################
#Dustin Waguespack                                                  #
#Stat 545                                                           #
#2/25/16                                                            #
#this program corresponds to assignment 2.1 #3                      #  
#####################################################################
laplace.censored.sample <- function(n,r){
  laplace <- function(mu,sig){
    u <- runif(1,0,1)
    if(u>=(1/2)){
      return(-sig*log(2-2*u)+mu)
    }
    else{
      return(sig*log(2*u)+mu)
    }
  }
  laplace.sample <- c()
  i <- 1
  while(i <= n){
    laplace.sample[i] <- laplace(3,1)
    i <- i + 1
  }
  laplace.sample <- sort(laplace.sample)
  laplace.sample[1:r] <- laplace.sample[r+1]
  return(laplace.sample)
}
laplace.mles <- function(ord,n,r){
  mle.mu <-function(ord,n,r){
    if(r >= (n/2))
    {return(ord[r+1]-((mle.sig(ord,n,r))*log(n/(2*(n-r)))))}
    if(r <= ((n/2)-1) && n%%2 != 0)
    {return(ord[(n+1)/2])}
    if(r <= ((n/2)-1) && n%%2 == 0)
    {return((ord[n/2]+ord[(n+1)/2])/2)}
  }
  mle.sig <- function(ord,n,r){
    if(r >= (n/2))
      {return((1/(n-r))*(sum(ord[2:n])-(n-r-1)*ord[r+1]))}
    if(r <= (n/2)-1 && n%%2 != 0)
      {return((1/(n-r))*(sum(ord[(((n+1)/2)+1):n])-sum(ord[(r+2):((n-1)/2)])-(r+1)*ord[r+1]))}
    if(r <= (n/2)-1 && n%%2 == 0)
      {return((1/(n-r))*(sum(ord[((n/2)+1):n])-sum(ord[(r+2):(n/2)])-(r+1)*ord[r+1]))}
  }
  return(c(mle.mu(ord,n,r),mle.sig(ord,n,r)))
}


sample1 <- laplace.censored.sample(30,10)



