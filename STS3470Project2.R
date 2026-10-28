# Task 1: Kelly Author, Caitlin Reviewer
rm(list=ls())

# STS3470 Project 2: Caitlin Bedway and Kelly Donovan
library(dplyr)

wald_int <- function(x, n, alpha){
  phat <- x/n 
  moe <- qnorm(.975)*sqrt(phat*(1-phat)/n) 
  lower <- phat-moe 
  upper <- phat+moe
  confint <- c(lower, upper)
  return(confint)
}

wald_int(126, 570, 0.05) # 0.1869873  0.2551180



# Task 2: Kelly Author, Caitlin Reviewer
set.seed(1008)
truep <- 1/6
n <- 100
out <- rbinom(n, 1, truep)

# Use the sum() function to calculate the value of x which will indicate count 
# how many of your dice rolls were 6’s. 
x <- sum(out) # 18/100 rolls were 'sixes'

wald_int(x = 18, n = 100, alpha = 0.05) # LB: 0.105   UB: 0.255
# ANS: Yes, the true population proportion (1/6, 0.166667), is within 
# the interval.

out_x <- replicate(1000, (sum(rbinom(n, 1, truep))))

# for-loop
CI_Wald <- matrix(NA, nrow=1000, ncol=2)  
CIcount <- NULL 

for(i in 1:1000){    
  CI_Wald[i,] <- wald_int(x = out_x[i], n = 100, alpha = 0.05)  
  CIcount[i] <- if_else((CI_Wald[i,1] <= 1/6 & CI_Wald[i,2] >= 1/6),
                        1, 0)
} 
sum(CIcount)/1000 

# The Wald confidence interval achieves approximately 93.8% confidence. 
# This does not technically meet 95% confidence, but it is very close.
# After running through 1000 simulations with sample size 100 (generated
# with 95% confidence each time) the proportion of times that our truep, 
# 1/6, fell into our confidence interval is 0.938. 



# Task 3: Kelly Author, Caitlin Reviewer

set.seed(1008)
truep <- 0.94
n <- 30
out <- rbinom(n, 1, truep)

# Use the sum() function to calculate the value of x which will indicate 
# count how many of your dice rolls were 6’s. 

x <- sum(out) # 

wald_int(x = 26, n = 30, alpha = 0.05) # LB: 0.745   UB: 0.988
# Yes, the true population proportion (0.94), is within 
# the interval.

out_x <- replicate(1000, (sum(rbinom(n, 1, truep))))

# for-loop

CI_Wald <- matrix(NA, nrow=1000, ncol=2)  
CIcount <- NULL 

for(i in 1:1000){    
  CI_Wald[i,] <- wald_int(x = out_x[i], n = 30, alpha = 0.05)  
  CIcount[i] <- if_else((CI_Wald[i,1] <= 0.94 & CI_Wald[i,2] >= 0.94),
                        1, 0)
} 
sum(CIcount)/1000 

# The Wald confidence interval achieves approximately 82.3% confidence. 
# This does not meet our 95% confidence goal, and is not close.
# After running through 1000 simulations with sample size 100 (generated
# with 95% confidence each time) the proportion of times that our truep,
# 0.94, fell into our confidence interval is 0.0.823. 



# Task 4: Caitlin Author, Kelly Reviewer
agresti_int <- function(x, n, alpha) {
  set.seed(1008)
  x <- x #or NULL
  n <- n
  alpha <- alpha 
  zstar <- qnorm(1-alpha/2)
  phat_ac <- (x + (zstar^2)/2)/(n+(zstar^2))
  moe_ac <- zstar*sqrt((phat_ac*(1-phat_ac))/(n+(zstar^2)))
  lower <- phat_ac - moe_ac
  upper <- phat_ac + moe_ac
  return(c(lower=lower, upper=upper))
}

#Red skittles proportions
agresti_int(126, 570, 0.05) # LB: 0.189   UB: 0.257
# ANS: Yes, the true population proportion (1/6, 0.166667), is within 
# the interval.

# How does this interval compare to the interval you generated in Task #1 
# to the new Agresti-Coull interval?
# ANS: The intervals are very similar to each other.



# Task 5: Caitlin Author, Kelly Reviewer
set.seed(1008)
truep <- 1/6
n <- 100
out <- rbinom(n, 1, truep)

# Use the sum() function to calculate the value of x which will indicate 
# count how many of your dice rolls were 6’s. 
x <- sum(out) # 18/100 rolls were 'sixes'

agresti_int(x = 18, n = 100, alpha = 0.05) # LB: 0.116   UB: 0.268
# ANS: Yes, the true population proportion (1/6, 0.166667), is within 
# the interval.

out_x <- replicate(1000, (sum(rbinom(n, 1, truep))))

# forloop
CI_AC <- matrix(NA, nrow=1000, ncol=2)  
CI_AC_count <- NULL 

for(i in 1:1000){    
  CI_AC[i,] <- agresti_int(x = out_x[i], n = 100, alpha = 0.05)  
  CI_AC_count[i] <- if_else((CI_AC[i,1] <= 1/6 & CI_AC[i,2] >= 1/6),
                            1, 0)
} 
sum(CI_AC_count)/1000 

# The Agresti-Coull confidence interval achieves approximately 94.8% confidence. 
# This does not technically meet 95% confidence, but it is very close.
# After running through 1000 simulations with sample size 100 (generated
# with 95% confidence each time) the proportion of times that our truep, 
# 1/6, fell into our confidence interval is 0.948. 

set.seed(1008)
truep <- 0.94
n <- 30
out <- rbinom(n, 1, truep)

# Use the sum() function to calculate the value of x which will indicate 
# count how many of your dice rolls were 6’s. 

x <- sum(out) # 26/30
x

agresti_int(x = 26, n = 30, alpha = 0.05) # LB: 0.697   UB: 0.953
# Yes, the true population proportion (0.94), is within 
# the interval.

out_x <- replicate(1000, (sum(rbinom(n, 1, truep))))

# forloop
CI_AC <- matrix(NA, nrow=1000, ncol=2)  
CI_AC_count <- NULL 

for(i in 1:1000){    
  CI_AC[i,] <- agresti_int(x = out_x[i], n = 30, alpha = 0.05)  
  CI_AC_count[i] <- if_else((CI_AC[i,1] <= 0.94 & CI_AC[i,2] >= 0.94),
                            1, 0)
} 
sum(CI_AC_count)/1000 

# The Agresti confidence interval achieves approximately 97.2% confidence. 
# This meets and goes above our 95% confidence goal.
# After running through 1000 simulations with sample size 100 (generated
# with 95% confidence each time) the proportion of times that our truep,
# 0.94, fell into our confidence interval is 0.972. 

