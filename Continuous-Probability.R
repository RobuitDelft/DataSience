library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

#Using real data
F <- function(a) mean(x <= a)
1 - F(70)    # probability of male taller than 70 inches


#Approximation
1 - pnorm(70.5, mean(x), sd(x))


# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
#Relatief veel waarden vallen op een integer. 
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))


library(tidyverse)
x <- seq(-4, 4, length = 100)
x
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()


dnorm(x)


#Monte Carlo simulations using normally distributed variables

#Code: Generating normally distributed random numbers
# define x as male heights from dslabs data
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# generate simulated height data using normal distribution - both datasets should have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)

# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)

#Code: Monte Carlo simulation of tallest person over 7 feet
B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the tallest height
})
mean(tallest >= 7*12)    # proportion of times that tallest person exceeded 7 feet (84 inches)



x <- seq(-5, 5, length.out = 100)
data.frame(x, f = dnorm(x,sd=.5)) %>%
  ggplot(aes(x,f)) +
  geom_line()



##Datacamp
# Assign a variable 'female_avg' as the average female height.
female_avg <- 64

# Assign a variable 'female_sd' as the standard deviation for female heights.
female_sd <- 3

# Using variables 'female_avg' and 'female_sd', calculate the probability that a randomly selected female is shorter than 5 feet. Print this value to the console.
pnorm(5, mean(x), sd(x))



# Assign a variable 'male_avg' as the average male height.
male_avg <- 69

# Assign a variable 'male_sd' as the standard deviation for male heights.
male_sd <- 3

# Determine the height of a man in the 99th percentile of the distribution.
qnorm(0.99,mean=male_avg,sd=male_sd)



# The variable `B` specifies the number of times we want the simulation to run.
B <- 1000

# Use the `set.seed` function to make sure your answer matches the expected result after random number generation.
set.seed(1)

# Create an object called `highestIQ` that contains the highest IQ score from each random distribution of 10,000 people.
smartest <- replicate(B, {
  simulated_data <- rnorm(10000, 100, 15)    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the tallest height
})

# Make a histogram of the highest IQ scores.
hist(smartest,
     main="Highest IQ from 1000 generated data sets",
     xlab="IQ",
     xlim=c(140,190))

 
## Questions 1 and 2: ACT scores, part 1
act_avg <-  20.9
act_sd <- 5.7
set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000,act_avg,act_sd)
mean(act_scores)
sd(act_scores)
F <- function(a) mean(act_scores <= a)
scores_tab<-table(act_scores)
sum(act_scores>=36)
mean(act_scores<=10)
hist(act_scores)

#Q2
x<-seq(1,36)
m<-20.9
s<-5.7
data.frame(x, f = dnorm(x,mean = m,sd=s)) %>%
  ggplot(aes(x, f)) +
  geom_line()


dnorm(x)

#Q3
act_scores
z <- scale(act_scores)
mean(z > 2)

#Q3c
act_avg <-  20.9
act_sd <- 5.7
qnorm(0.975,mean=act_avg,sd=act_sd)

#Q4
F <- function(a) mean(act_scores <= a)
sq<-seq(1,36)
sapply(sq,F)
sum(res<=.95)

#q4B
qnorm(0.95,mean=act_avg,sd=act_sd)

#Q4c
p <- seq(0.01, 0.99, 0.01)
sample_quantiles<-quantile(act_scores, p)


#Q4d
p <- seq(0.01, 0.99, 0.01)

F <- function(a) qnorm(a,mean=act_avg,sd=act_sd)

theoretical_quantiles<-sapply(p, F)
plot(theoretical_quantiles, sample_quantiles)
abline(0,1)
=====