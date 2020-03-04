

paste("rob","van","laarhoven")
paste(letters[1:5],as.character(1:5))
paste(c("a","b","c","d"),c("a","b","c","d"))

# all combinations of two lists
x<-c("a","b","c","d")
expand.grid(letters[1:5],as.character(1:5),x)


## Deck of Cards
doc <- function() {
  suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
  numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
  deck <- expand.grid(number = numbers, suit = suits)
  deck <- paste(deck$number, deck$suit)
}

suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

# probability of drawing a king
kings <- paste("King", suits) # for ways to get a king
mean(deck %in% kings)

# PR(King|King)

library(gtools)
permutations(5,3)
combinations(5,3)    # order does not matter


##
##Code: Probability of drawing a second king given that one king is drawn
##
hands <- permutations(52,2, v = deck)
first_card <- hands[,1]
second_card <- hands[,2]
sum(first_card %in% kings)

sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

##
##Code: Probability of a natural 21 in blackjack
##
aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v=deck) # all possible hands

# probability of a natural 21 given that the ace is listed first in `combinations`
hands[,1] %in% aces & hands[,2] %in% facecard
mean(hands[,1] %in% aces & hands[,2] %in% facecard)
mean(hands[,1] %in% facecard & hands[,2] %in% facecard)
mean(hands[,1] %in% aces & hands[,2] %in% aces)
# probability of a natural 21 checking for both ace first and ace second

mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))


##Code: Monte Carlo simulation of natural 21 in blackjack
##Note that your exact values will differ because the process is random and the seed is not set.

# code for one hand of blackjack
hand <- sample(deck, 2)
hand

# code for B=10,000 hands of blackjack
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)


results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)


##Code: The birthday problem
# checking for duplicated bdays in one 50 person group
n <- 50

samplebdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays

any(duplicated(samplebdays))    # check if any birthdays are duplicated

# Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)    # calculates proportion of groups with duplicated bdays
x<-duplicated(samplebdays)
x
for (year in 1:50){
  print(paste(samplebdays[year],x[year]))
}

##
## Sapply
##

# function to calculate probability of shared bdays across n people

# Monte Carlo implemantation
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

n <- seq(1, 60)
prob<-sapply(n, compute_prob)
plot(n,prob)

#Exact 
exact_prob <- function(n) {
  prob_unique <- seq(365,365-n+1)/365
  1 - prod(prob_unique)
}

eprob<-sapply(n, exact_prob)
plot(n,prob)
lines(n,eprob,col="Red")



##
## How many Monte Carlo experiments are enough?
##
B <- 10^seq(1, 5, len = 100)    # defines vector of many B values
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l")    # plot a line graph of estimates 

##
## Datacamp
##
p_no6 <- 5/6
p_no6*p_no6*p_no6*p_no6*p_no6*p_no6repl
p_no6
# Calculate the probability of not seeing a 6 on six rolls using `p_no6`. Print your result to the console: do not assign it to a variable.
p_no6<-prod(replicate(6,5/6))

1-p_no6

rolls <- sample(6,100000000,replace=TRUE)
not6 <- rolls[rolls < 6]
## percent not 6 on single roll
length(not6) / length(rolls)


set.seed(121)
rolls <- sample(6, 100, replace = TRUE)
t <- table(rolls)
t
no_6 <- t[names(t)!="6"]
no_6
sum(no_6)
((5/6) * 100)


#E5
# This line of example code simulates four independent random games where the Celtics either lose or win. Copy this example code to use within the `replicate` function.
simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
simulated_games
any(simulated_games %in% "win")
# The variable 'B' specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)
results <- replicate(B, {
  simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  any(simulated_games %in% "win")
})
mean(results)



simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
any(simulated_games %in% "win")

