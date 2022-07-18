##### Estimating alpha and beta from qunatiles and mean

quantiles_to_fit <- c(0.025, 0.975)

# output the comparison measures.
f.beta <- function(alpha, beta, x, lower=0, upper=1) {
  
  p <- pbeta(x[2:3], alpha, beta)
  # calculate the estimated mean. 
  mean_sample <- alpha/(alpha+beta)
  # return both
  return(c(mean_sample,p))
}

# Sums of squares - for comparing estimated to real. 
delta <- function(fit, actual) sum((fit-actual)^2)

#calculate for the theta and probs what is needed
objective <- function(theta, x, prob, ...) {
  
  ab <- exp(theta) # Parameters are the *logs* of alpha and beta
  #work out what the logit of the quanitle is for these parameters
  fit <- f.beta(ab[1], ab[2], x, ...)
  # return the sum of squares. 
  return (delta(fit, c(x[1],prob)))
}


start <-log(c(10,10))  
# for each of the samples. 
  x <- c(mean, lower, upper)
  
  sol <- optim(f=objective,p=start,
               method="L-BFGS-B", 
               x=x,
               prob=c(quantiles_to_fit),
               lower=c(0.01, 0.01), upper=c(10,10)
  )
  parms <- exp(sol$par)           # Estimates of alpha and beta

  a<- list(alpha = parms[1], 
           beta = parms[2])

