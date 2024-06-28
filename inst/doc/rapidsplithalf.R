## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      fig.path = "./../man/figures/README-")

## ----intro--------------------------------------------------------------------
library(rapidsplithalf)
data(foodAAT)
data(raceIAT)

## ----singlediff---------------------------------------------------------------
rel<-
rapidsplit(data=foodAAT,
           subjvar="subjectid", # Subject identifier
           diffvars="is_pull", # The variable indicating the two conditions that need to be subtracted from each other
           stratvars="stimid", # splits are stratified by stimulus ID such that each half features a near-equal number of trials for each stimulus
           aggvar="RT", # Defining the variable to be aggregated
           aggfunc="means", # Method to aggregate each condition: simple means
           splits=5500)

print(rel)

## ----plotter,fig.asp=1--------------------------------------------------------
plot(rel)

## ----plotter2,fig.asp=1-------------------------------------------------------
plot(rel,type="all")

## ----doublediff---------------------------------------------------------------
rel2<-
rapidsplit(data=foodAAT,
           subjvar="subjectid",
           # We specify 2 diffvars for a double-difference score
           diffvars=c("is_pull","is_target"),
           stratvars="stimid",
           aggvar="RT",
           # We specify the median here
           aggfunc="medians",
           splits=5500)

print(rel2)

## ----iatrel,fig.asp=1---------------------------------------------------------

iatrel<-
  rapidsplit(data=raceIAT,
             subjvar="session_id",
             diffvars="congruent",
             
             # The subscorevar argument specifies that we want to compute
             # multiple scores for each participant, based on subsets of
             # their data, and then average the scores together 
             # as is done in the IAT D-score.
             subscorevar="blocktype",
             aggvar="latency",
             splits=1000,
             
             # the errorhandling argument controls how error trials
             # are replaced with the block mean plus a penalty,
             # as is done in the IAT D-score.
             errorhandling=list(type="fixedpenalty",
                                errorvar="error",
                                fixedpenalty=600,
                                blockvar="block_number"),
             
             # The standardize argument specifies that we want to
             # divide the person's score by the standard deviation of their
             # RTs, as in the IAT D-score.
             standardize=TRUE)

print(iatrel)

plot(iatrel,show.labels=FALSE)

