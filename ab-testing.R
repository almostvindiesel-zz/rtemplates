setwd("/Users/mars/code/rtemplates")

# ----------------------------------------------------------------------------------------------------
# Calculate whether the mean of an experiment is stastically greater than the mean of the control
#
# Adapted from https://www.r-bloggers.com/ab-testing-in-r-%E2%80%93-part-1/

install.packages("pwr")
library(pwr)


# Enter the Conversion Rate followed by the sameple
control    = c(.04, 2500) # pink
experiment = c(.05, 2500) # black
alpha = .05
sides_to_test = 1
critical_z = qt(1-alpha/sides_to_test, experiment[2])

abtest <- function(ad1, ad2) {

  #Calculate Standard Deviation
  cntl_se = sqrt( ad1[1] * (1-ad1[1]) / ad1[2] )
  expm_se = sqrt( ad2[1] * (1-ad2[1]) / ad2[2] )

  #Calculate Lower and Upper Bounds
  cntl_bounds = c((ad1[1] - critical_z * cntl_se) , (ad1[1] + critical_z * cntl_se) )
  expm_bounds = c((ad2[1] - critical_z * expm_se) , (ad2[1] + critical_z * expm_se) )
  
  cat("Lower Bound | Upper Bound:\n")
  cat(" control  ", round(cntl_bounds,3), "\n") 
  cat(" expment  ", round(expm_bounds,3), "\n")
  cat("p-value   ", round(1-pnorm( (experiment[1] - control[1]) / expm_se ),3), "\n")
  
  pwr <- pwr.t2n.test(n1 = ad1[2], n2 = ad2[2], d = ad2[1]-ad1[1], sig.level = 1-alpha)
  cat("alpha(α)  ", alpha, "\n")
  cat("power(1-β)", round(unlist(pwr['power']),2), "\n")
  #pwr.2p2n.test(h = , n1 = ad1[2], n2 = ad2[2], sig.level = , power = )
  #pwr.t2n.test(n1 = ad1[2], n2 = ad2[2], d = , sig.level =, power = )
  
  if (expm_bounds[1] > control[1]) { 
    cat("---> Reject the Null Hypothesis :)") 
  } else { 
    cat("---> Fail to reject the null hypothesis :(") 
  }
}

abtest(control, experiment)


# ----------------------------------------------------------------------------------------------------
# Determine Sample Size Needed Based on Expected Outcome
# 
# https://www.r-bloggers.com/a-clear-picture-of-power-and-significance-in-ab-tests/

# Power: The probability of rejecting the null hypothesis when it is false. 
# You want to design your experiment to have a power near 1. (1 – Beta is power)
# Prob of concluding difference when there is a difference
# Type I: Rejected null when it shouldn’t have been 	(false positive?)
# Alpha is the probability of Type I error in any hypothesis test–incorrectly claiming statistical significance.

# Significance: The probability of failing to reject the null hypothesis when it is true. 
# You want to design your experiment so you have significance or p-values near zer???
# Type II: retained null when it should have been retained (false negative)
# Beta is the probability of Type II error in any hypothesis test–incorrectly 
# concluding no statistical significance.  (1 – Beta is power)


install.packages("ggplot2")
install.packages("gtools")

library(gtools)
library(ggplot2)

# q>p, compute the probability of a 
# p-rate process measuring as q-rate 
# or better in n steps
pSignificanceError <- function(p,q,n) {
  pbinom(ceiling(q*n)-1,prob=p,size=n,lower.tail=FALSE)
}

# q>p, compute the proability of a
# q-rate process measuring as p-rate 
# or lower in n steps
pPowerError <- function(p,q,n) {
  pbinom(floor(p*n),prob=q,size=n,lower.tail=TRUE)
}

designExperiment <- function(pA,pB,pError,pAUpper=pB,pBLower=pA) {
  aSoln <- binsearch(
    function(k) {
      pSignificanceError(pA,pAUpper,k) - pError},
    range=c(100,1000000))
  nA <- max(aSoln$where)
  print(paste('nA',nA))
  
  bSoln <- binsearch(
    function(k) {
      pPowerError(pBLower,pB,k) - pError},
    range=c(100,1000000))
  nB <- max(bSoln$where)
  print(paste('nB',nB))
  
  
  low = floor(min(pA*nA,pB*nB))
  high = ceiling(max(pA*nA,pB*nB))
  width = high-low
  countRange <- (low-width):(high+width)
  
  dA <- data.frame(count=countRange)
  dA$group <- paste('A: sample size=',nA,sep='')
  dA$density <- dbinom(dA$count,prob=pA,size=nA)
  dA$rate <- dA$count/nA
  dA$error <- dA$rate>=pAUpper
  dB <- data.frame(count=countRange)
  dB$group <- paste('B: sample size=',nB,sep='')
  dB$density <- dbinom(dB$count,prob=pB,size=nB)
  dB$rate <- dB$count/nB
  dB$error <- dB$rate<=pBLower
  d <- rbind(dA,dB)
  
  plot = ggplot(data=d,aes(x=rate,y=density)) +
    geom_line() +
    geom_ribbon(data=subset(d,error),
                aes(ymin=0,ymax=density),fill='red') + 
    facet_wrap(~group,ncol=1,scales='free_y') +
    geom_vline(xintercept=pAUpper,linetype=2) +
    geom_vline(xintercept=pBLower,linetype=2)
  list(nA=nA,nB=nB,plot=plot)
}


r1 <- designExperiment(pA=0.050,pB=0.055,pError=0.05)
print(r1$plot)

r2 <- designExperiment(pA=0.050,pB=0.055,pError=0.05,pAUpper=(pA+pB)/2,pBLower=(pA+pB)/2)
print(r2$plot)

