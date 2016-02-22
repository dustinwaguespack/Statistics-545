#####################################################################
#Dustin Waguespack                                                  #
#Stat 545                                                           #
#2/25/16                                                            #
#this program corresponds to assignment 2.1 #3                      #
#ord = the ordered sample                                           #
#n = the size of the sample                                         #
#r =  the number of the smallest order satsitics that are censored  #                                           #
#####################################################################
laplace.mles.cens <- function(ord,n,r){
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
  print(c(mle.mu(ord,n,r),mle.sig(ord,n,r)))
}
x = c(62,66,78,79,80,84,84,85,85,86,86,87,88,88,89,+
        89,91,91,91,91,92,92,92,92,93,94,94,94,95,95,+
        95,96,96,96,96,96,97,97,97,97,97,97,98,98,98,+
        98,98,98,98,99,99,99,99,99,100,100,100,100,100,101,+
        101,101,101,102,102,102,102,102,102,102,103,103,103,104,104,+
        104,104,104,104,104,105,105,106,107,107,109,110,111,111,111,+
        111,114,115,117,122,132,132,137,137,138)
laplace.mles.cens(x,100,30)



