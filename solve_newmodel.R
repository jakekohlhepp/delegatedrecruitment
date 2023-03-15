## Idea: solve FOCs
library('stats')
library('rootSolve')

sig_h<-5
sig_l<-1
mu<-9
c<-0.1

## find upper and lower bounds for each type
sigl_bounds<-uniroot.all(function(x){return(x-c/pnorm(x,mean=mu,sd=sig_l,lower.tail=FALSE))},c(0,20) )
sigl_bounds<-sort(sigl_bounds)
sigh_bounds<-uniroot.all(function(x){return(x-c/pnorm(x,mean=mu,sd=sig_h,lower.tail=FALSE))},c(0,20) )
sigh_bounds<-sort(sigh_bounds)
## function to integrate normal pdf 
int_normal<-function(x,s){
  res<-integrate(pnorm,lower=x,upper=Inf,mean=mu,sd=s,lower.tail=FALSE)
  if (res$message!="OK") stop(res$message)
  return(res$value)
}

## FOC for beta above mean.
foc_above<-function(x){
  return( -(1-pnorm(x,sd=sig_h,mean=mu)*pnorm(x,sd=sig_l,mean=mu))+
            dnorm(x,sd=sig_h,mean=mu)*(-c + int_normal(x,sig_l)))
}

foc_below<-function(x){
  return( -(1-pnorm(x,sd=sig_h,mean=mu)*pnorm(x,sd=sig_l,mean=mu))+
            dnorm(x,sd=sig_l,mean=mu)*(-c + int_normal(x,sig_h)))
}

foc_total<-function(x){
  return(ifelse(x<mu,foc_below(x),foc_above(x)))
}
plot(seq(from=0, to=mu, by=0.1), sapply(seq(from=0, to=mu, by=0.1),foc_below))

plot(seq(from=mu, to=10, by=0.1), sapply(seq(from=mu, to=10, by=0.1),foc_above))

plot(seq(from=0, to=10, by=0.1),sapply(seq(from=0, to=10, by=0.1),foc_total))



##3 profit

profit_above<-function(x){
  return(-c + int_normal(x,sig_h) + pnorm(x,sd=sig_h, mean=mu)*(-c + int_normal(x,sig_l)))
}

profit_below<-function(x){
  return(-c + int_normal(x,sig_l) + pnorm(x,sd=sig_l, mean=mu)*(-c + int_normal(x,sig_h)))
}

profit_total<-function(x){
  return(ifelse(x<mu,profit_below(x),profit_above(x)))
}



plot(seq(from=0, to=10, by=0.1),sapply(seq(from=0, to=10, by=0.1),profit_total))


## alt foc


foc_above<-function(x){
  return( -pnorm(x,sd=sig_h,mean=mu)*dnorm(x,sd=sig_l,mean=mu)*x + dnorm(x,sd=sig_h,mean=mu)*(-c -x*pnorm(x,sd=sig_l,mean=mu)+int_normal(x,sig_l)))
}

foc_above<-function(x){
  return( -pnorm(x,sd=sig_l,mean=mu)*dnorm(x,sd=sig_h,mean=mu)*x + dnorm(x,sd=sig_l,mean=mu)*(-c -x*pnorm(x,sd=sig_h,mean=mu)+int_normal(x,sig_h)))
}

foc_total<-function(x){
  return(ifelse(x<mu,foc_below(x),foc_above(x)))
}
plot(seq(from=max(c(sigh_bounds[1],sigl_bounds[1] )), to=min(c(sigh_bounds[2],sigl_bounds[2] )), by=0.1),sapply(seq(from=max(c(sigh_bounds[1],sigl_bounds[1] )), to=min(c(sigh_bounds[2],sigl_bounds[2] )), by=0.1),foc_total))


### alt profit

profit_above<-function(x){
  return(-c + int_normal(x,sig_h)+x*pnorm(x,sd=sig_h,mean=mu, lower.tail = FALSE) + pnorm(x,sd=sig_h,mean=mu)*(-c++x*pnorm(x,sd=sig_l,mean=mu, lower.tail = FALSE)+int_normal(x,sig_l))
    
  )
}

profit_above<-function(x){
  return(-c + int_normal(x,sig_l)+x*pnorm(x,sd=sig_l,mean=mu, lower.tail = FALSE) + pnorm(x,sd=sig_l,mean=mu)*(-c++x*pnorm(x,sd=sig_h,mean=mu, lower.tail = FALSE)+int_normal(x,sig_h))
         
  )
}

profit_l<-function(x){
  return(-c + int_normal(x,sig_l)+x*pnorm(x,sd=sig_l,mean=mu, lower.tail = FALSE))
}

profit_h<-function(x){
  return(-c + int_normal(x,sig_h)+x*pnorm(x,sd=sig_h,mean=mu, lower.tail = FALSE))
}

profit_total<-function(x){
  if (x<max(c(sigh_bounds[1],sigl_bounds[1] ))) res<-profit_l(x)
  if (x>=max(c(sigh_bounds[1],sigl_bounds[1] )) & x<mu)  res<-profit_below(x)
  if (x>=mu & x<min(c(sigh_bounds[2],sigl_bounds[2] ))) res<-profit_above(x)
  if (x>min(c(sigh_bounds[2],sigl_bounds[2] ))  ) res<-profit_h(x)
  return(res)
}

plot(seq(from=min(c(sigh_bounds[1],sigl_bounds[1] )), to=max(c(sigh_bounds[2],sigl_bounds[2] )), by=0.01),sapply(seq(from=min(c(sigh_bounds[1],sigl_bounds[1] )), to=max(c(sigh_bounds[2],sigl_bounds[2] )), by=0.01),profit_total))


