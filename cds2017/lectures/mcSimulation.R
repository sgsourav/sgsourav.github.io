# =======================================================
# Monte Carlo Simulations in R
# =======================================================

# -------------------------------------------------------
# Example 1 : Integrating under the Normal Curve
# Find Integral from 5 to 10 under N(3, 5) curve

runs <- 1000000
sample <- rnorm(runs, mean = 3, sd = 5)
sum(sample > 5 & sample < 10) / runs


# -------------------------------------------------------
# Example 2 : Simulating the Binomial Distribution
# Find Probability of 4 or more Heads in 10 Tosses

trial <- function() { 
  sum(sample(c(0,1), 10, replace = T)) > 4
}
runs <- 1000000
sum( replicate(runs, trial()) ) / runs


# -------------------------------------------------------
# Example 3 : Approximating Pi through Sampling
# Find the value of Pi using the area of circle

runs <- 1000000
x <- runif(runs)
y <- runif(runs)
( sum(x^2 + y^2 < 1) / runs) * 4


# -------------------------------------------------------
# Example 4 : Simulating the Wheel of Fortune
# Find Probability of "Loss" after 10 chances

game <- function(){
  sum( sample(c(1,1,-2,2), 10, replace = T) ) < 0
}
runs <- 100000
sum( replicate(runs, game()) ) / runs


# -------------------------------------------------------
# Example 5 : Simulating the Coupon Collector's Puzzle
# Find Expected number of Coupons to collect all

couponSpace <- c(1,1,1,1,1,1,2,2,2,3,3,4)
coupon <- function(){ sample(couponSpace, 1, replace = T) }

collectAll <- function() {
  collected <- c()
  while ( length(unique(collected)) < 4 ) {
    collected <- c(collected, coupon())
  }
  length(collected)
}

runs <- 100000
trials <- replicate(runs, collectAll())
mean(trials)
hist(trials)
