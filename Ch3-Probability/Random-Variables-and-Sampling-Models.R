# define random variable x to be 1 if blue, 0 otherwise
beads <- rep(c("red", "blue"), times = c(2, 3))
x <- ifelse(sample(beads, 1) == "blue", 1, 0)

# demonstrate that the random variable is different every time
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)


#Monte Carlo simulation: Chance of casino losing money on roulette
#We build a sampling model for the random variable  S  that represents the casino's total winnings. 

# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S
#We use the sampling model to run a Monte Carlo simulation and use the results to estimate the probability of the casino losing money.

n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
    X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 spins
    sum(X)    # determine total profit
})

mean(S < 0)    # probability of the casino losing money
#We can plot a histogram of the observed values of S as well as the normal density curve based on the mean and standard deviation of S.

library(tidyverse)
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
    ggplot(aes(S, ..density..)) +
    geom_histogram(color = "black", binwidth = 10) +
    ylab("Probability") +
    geom_line(data = normal_density, mapping = aes(s, f), color = "blue")


B<-10^7
X<-sample(c(-1,1),B,replace = TRUE, prob = c(9/19,10/19))
X
X<-sample(c(-1,1),B,replace = TRUE, prob = c(0.6,0.4))
mean(X)
sd(X)
sqrt(.24)*2



#DataCamp
#E1
# The variables `green`, `black`, and `red` contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2/38


#E2
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

sample(c(17,0,0),1,replace = TRUE, prob = c(2/38,18/24,18/38))
n <- 1000    # number of roulette players
B <- 1000000    # number of Monte Carlo experiments
set.seed(1)
S <- replicate(B, {
    c<-sample(c(17,-1,-1),1,replace = TRUE, prob = c(2/38,18/24,18/38))
    sum(c)    # determine total profit
})
mean(S)




# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Calculate the expected outcome if you win $17 if the ball lands on green and you lose $1 if the ball doesn't land on green
p_green * 17 + p_not_green * -1


# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define the number of bets using the variable 'n'
n <- 1000

# Create a vector called 'X' that contains the outcomes of 1000 samples
X <- sample(c(17,-1), size = n, replace=TRUE, prob=c(p_green, p_not_green))

# Assign the sum of all 1000 outcomes to the variable 'S'
S <- sum(X)

# Print the value of 'S' to the console
S


##E5
# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define the number of bets using the variable 'n'
n<-1000

# Create a vector called 'X' that contains the outcomes of 1000 samples
X<-sample(c(17,-1),n,replace = TRUE, prob = c(p_green,p_not_green))


# Assign the sum of all 1000 outcomes to the variable 'S'
S<-sum(X)

# Print the value of 'S' to the console
S


##E7
# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 1000

# Compute the standard error of the sum of 1,000 outcomes
sqrt(n)*abs(-1-17)*sqrt(.4*.6)


#E1
# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 100

# Calculate 'avg', the expected outcome of 100 spins if you win $17 when the ball lands on green and you lose $1 when the ball doesn't land on green
avg <- n * (17*p_green + -1*p_not_green)

# Compute 'se', the standard error of the sum of 100 outcomes
se <- sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)

# Using the expected value 'avg' and standard error 'se', compute the probability that you win money betting on green 100 times.
pnorm(mean = avg,sd = se,)


##E2
# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 100

# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 1000000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `S` that replicates the sample code for `B` iterations and sums the outcomes.
S <- replicate(B, {
    c<-sample(c(17,-1),n,replace = TRUE, prob = c(2/38,36/38))
    sum(c)    # determine total profit
})



# Compute the average value for 'S'
mean(S>1)

# Calculate the standard deviation of 'S'
sd(S)

##E5
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Define the number of bets using the variable 'n'
n <- 10000

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 - p_green

# Create a vector called `X` that contains the outcomes of `n` bets
X<-sample(c(17,-1),n,replace = TRUE, prob = c(2/38,36/38))

# Define a variable `Y` that contains the mean outcome per bet. Print this mean to the console.
Y<-mean(X)
Y



#E9
## Make sure you fully follow instructions, including printing values to the console and correctly running the `replicate` loop. If not, you may encounter "Session Expired" errors.

# The variable `n` specifies the number of independent bets on green
n <- 10000

# The variable `B` specifies the number of times we want the simulation to run
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random number generation
set.seed(1)

# Generate a vector `S` that contains the the average outcomes of 10,000 bets modeled 10,000 times
S <- replicate(B, {
    c<-sample(c(17,-1),n,replace = TRUE, prob = c(2/38,36/38))
    mean(c)    # determine total profit
})




# Compute the average of `S`
mean(S)

# Compute the standard deviation of `S`
sd(S)


n<-10000
B<-10000
S <- replicate(B, {
    X<-sample(c(1,0),44,replace = TRUE, prob = c(1/5,4/5))
    sum(X)    # determine total profit
})
sum(S==1)/B
sum(sample(c(1,0),44,replace = TRUE, prob = c(1/5,4/5)))


(abs(-.25-1) * sqrt(1/5*4/5)) * (1/sqrt(44))

pnorm(8,0,3.316625)


set.seed(21, sample.kind = "Rounding")
n<-10000
B<-10000
S <- replicate(B, {
    X<-sample(c(1,0),44,replace = TRUE, prob = c(1/4,3/4))
    mean(X)    # determine total profit
})
S
mean(S)
sum(S>8)/B

(3/4*0)+(1*1/4)*44

p <- seq(0.25, 0.95, 0.05)
f <- function(a) a*44
