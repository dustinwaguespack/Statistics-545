binomial.cis = function(k, n, cl){
  al <- 1-cl
  crt <- qnorm(1-al/2); zsq <- crt**2
  ph <- k/n; qh = 1-ph;
  #Wald CIs
  wlow = ph - crt*sqrt(ph*qh/n)
  wupp = ph + crt*sqrt(ph*qh/n)
  #Score CIs
  dr <- 1+zsq/n
  cent <- (ph+.5*zsq/n)/dr
  me <- crt*sqrt(ph*(1-ph)+.25*zsq/n)/sqrt(n)/dr
  slow <- cent-me
  supp <- cent+me
  return(list(wlow=wlow,wupp=wupp,slow=slow,supp=supp))
}
coverage.bin = function(n, p, cl){
  al = 1-cl
  zal = qnorm(1-al/2)
  covw = 0; covs = 0; covx = 0
  for(k in 0:n){
    out = binomial.cis(k, n, cl)
    if(out$wlow <= p & p <= out$wupp){covw = covw + dbinom(k,n,p)}
    if(out$slow <= p & p <= out$supp){covs = covs + dbinom(k,n,p)}
  }
  return(c(covw,covs))
}
j <- 1
wald <- c()
score <- c()
index <- c(17,20,25,30,35,37,42,44,49,10,12,13,15,18,23,28,33,40)
for (i in index){
  wald[j] <- coverage.bin(i,.5,.95)[1]
  score[j] <- coverage.bin(i,.5,.95)[2]
  j <- j + 1
}

coverage.frame <- data.frame(wald,score)
rownames(coverage.frame) <- index

write.csv(coverage.frame, file="CovProbTable.csv")
print(coverage.frame)