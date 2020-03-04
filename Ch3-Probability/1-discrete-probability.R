beads <- rep(c("red", "blue"), times = c(2,3))    # create an urn with 2 red, 3 blue
beads    # view beads object
sample(beads, 1)    # sample 1 bead at random

B <- 1000000    # number of times to draw 1 bead
events <- replicate(B, sample(beads, 1))    # draw 1 bead, B times
tab <- table(events)    # make a table of outcome counts
tab    # view count table
prop.table(tab)    # view table of outcome proportions


events <- sample(beads,replace = TRUE,B)  
tab<-table(events)
tab    # view count table
prop.table(tab)    # view table of outcome proportions


sample(beads,2)
sample(beads,2)
sample(beads,2)
sample(beads,6)

# Simulate Pre 3.6 seed implementation
set.seed(1232)
set.seed(1, sample.kind="Rounding")    # will make R 3.6 generate a seed as in R 3.5


#Using the mean Function for Probability

beads=="blue"  #TRUE==1 FALSE==0
mean(beads=="blue")

mean(beads=="red")

#independence
x<-sample(beads,5)
x
x

#DAtaCamp

cyan <- 3
magenta <- 5
yellow <- 7
p<-3/15
p
