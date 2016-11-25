##' ---
##' title: The Birthday Paradox
##' author: Todd K. Hartman
##' date: November 2016
##' ---

##' Housekeeping
## Load packages via Pacman
pacman::p_load(rmarkdown)

## Set the working directory
# setwd("Enter Your Location Here")

## Change printing options
options(scipen = 999, digits = 3)

##' Some background information
## Pr(at least 2 people have same bday) = 1 - P(nobody has same bday)
## Or, we can solve an easier problem (using the equality rule): 
## Pr(nobody has same bday) = 1 - Pr(at least 2 people have same bday)
##
## Pr(A) = (# of elements in A) / (# of elements in sample space)
## Pr(nobody has same bday) = (# unique pairs) / (# of total pairs)
## Pr(nobody has same bday) = 365Permutationk / 365^k
## Pr(nobody has same bday) = 365! / 365^k(365 - k)!
## Pr(at least 2 people have same bday) = 1 - (365! / 365^k(365 - k)!)

##' Option 1: Create a function to calculate the probability of two people sharing the same birthday
birthday <- function(k) {
    logdenom <- k * log(365) + lfactorial(365 - k) # log denominator
    lognumer <- lfactorial(365) # log numerator
    ## P(at least two have the same bday) = 1 - P(nobody has the same bday)
    pr <- 1 - exp(lognumer - logdenom) # transform back
    return(pr)
}

k <- 1:60
bday <- birthday(k) # call the function
names(bday) <- k # add labels
plot(k, bday, xlab = "Number of people", xlim = c(0, 60),
     ylab = "Probability two people have same birthday")
abline(h = 0.5) # horizontal 0.5 line
abline(v = 23, lty = "dotted") # horizontal 0.5 line

bday[1:60]

##' Option 2: Run a Monte Carlo simulation 
k <- 23 # number of people in the same place
sims <- 1000 # number of simulations
pr <- rep(NA, sims) # container for the estimates
event <- 0 # counter
for (i in 1:sims) {
    days <- sample(1:365, k, replace = TRUE)
    days.unique <- unique(days) # unique birthdays
    ## if there are duplicates, the number of unique birthdays
    ## will be less than the number of birthdays, which is `k' 
    if (length(days.unique) < k) {
    event <- event + 1
    }
}

## Proportion of trials where at least two bdays are the same
answer <- event / sims
answer
