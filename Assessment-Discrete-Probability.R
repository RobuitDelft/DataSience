# Question 2: Restaurant management
entrees<-paste("entree",as.character(seq(1,6)))
drinks<-paste("drink",as.character(seq(1,3)))
sides<-paste("side",as.character(seq(1,6)))

combis<-expand.grid(entrees,drinks,sides,sides)

expand.grid(sides,sides)
library(gtools)
nrow(expand.grid(entree = 1:6, drink = 1:3, side = lapply(seq_len(nrow(combinations(6,3))), function(i) combinations(6,3)[i,])))
nrow

lapply(seq_len(nrow(combinations(6,3))), function(i) combinations(6,3)[i,])
combies<-combinations(6,3,set=TRUE,repeats.allowed=FALSE)
typeof(entrees)
expand.grid(entrees,drinks,combinations(6,3,set=TRUE,repeats.allowed=FALSE))
x
sort(x)
?permutations
?nrow
combinations(6,3,set=TRUE,repeats.allowed=FALSE)


comp_entrees <- function (entnum) {
  nrow(expand.grid(entree = lapply(seq_len(nrow(combinations(entnum,1))), function(i) combinations(entnum,1)[i,]), drink = 1:3, side = lapply(seq_len(nrow(combinations(6,2))), function(i) combinations(6,2)[i,])))
}

p<-seq(1,12)
sapply(p, comp_entrees)

comp_sides <- function (numm) {
  nrow(expand.grid(entree = 1:6, drink = 1:3, side = lapply(seq_len(nrow(combinations(numm,2))), function(i) combinations(numm,2)[i,])))
}

p<-seq(2,12)
sapply(p, comp_sides)


#Questions 3 and 4: Esophageal cancer and alcohol/tobacco use, part 1
library(tidyverse)
head(esoph)
esoph
all_cases<-sum(esoph$ncases)
all_controls<-sum(esoph$ncontrols)

maxalc<-esophesoph
esoph[maxalc]
data(esoph)
newdata <- esoph[ which(esoph$alcgp=="120+"), ]
newdata
sum(newdata$ncases)/(sum(newdata$ncontrols)+sum(newdata$ncases))


x<-esoph %>% filter (esoph$alcgp=="0-39g/day") %>% summarize(ncases =sum(ncases,na.rm=TRUE),ncontrols= sum(ncontrols,na.rm =TRUE)) 
x
x$ncases/(x$ncases+x$ncontrols)


newdata <- esoph[ which(esoph$alcgp=="0-39g/day"), ]
newdata
sum(newdata$ncases)/(sum(newdata$ncontrols)+sum(newdata$ncases))


newdata <- esoph[ which(esoph$tobgp %in% c("10-19","20-29","30+")), ]
nrow(newdata)
sum(newdata$ncon)/sum(esoph$ncases)

sum(newdata$ncases)/sum(esoph$ncontrols)

sum(newdata$ncases)/sum(esoph$ncases)


newdata <- esoph[ which(esoph$tobgp %in% c("30+")), ]
newdata <- esoph[ which(esoph$alcgp=="120+"), ]
newdata <- esoph[ which(esoph$tobgp=="30+" | esoph$alcgp=="120+"), ]

sum(newdata$ncontrols)/sum(esoph$ncontrols)

sum(newdata$ncontrols)

sum(newdata$ncases)/(sum(esoph$ncases))
sum(newdata$ncontrols)/(sum(esoph$ncontrols))

(sum(newdata$ncases)/(sum(esoph$ncases)))/(sum(newdata$ncontrols)/(sum(esoph$ncontrols)))


sum(esoph$ncontrols)
sum(newdata$ncontrols)


sum(newdata$ncontrols)/sum(esoph$ncontrols)
sum(newdata$ncontrols)

sum(newdata$ncases)
