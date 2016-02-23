#read input files
library(readr)
outcome<-read_csv(file="data/outcome-of-care-measures.csv")
hospital<-read_csv(file="data/hospital-data.csv")

#make histogram
outcome[,11]<-as.numeric(outcome[,11])

best=function(state,outcome){
  library(readr)
  library(plyr)
  library(magrittr)
  ## Read outcome data
  my_outcome<-read_csv(file="data/outcome-of-care-measures.csv")
  ## Check that state and outcome are valid
  outcome_possibilities<-data.frame(Description=c("heart attack","heart failure","pneumonia"),"Column"=c(11,17,23),stringsAsFactors=F)
  my_column<-outcome_possibilities[match(outcome,outcome_possibilities[,1]),2]
  if(!(state %in% unique(my_outcome$State))){
    stop("invalid state")
  }
  if(!(outcome %in% unique(outcome_possibilities$Description))){
    stop("invalid outcome")
  }
  ## Return hospital name in that state with lowest 30-day death rate
  my_outcome<-my_outcome[!is.na(my_outcome[,my_column]),]
  my_outcome<-my_outcome[-which(my_outcome[,my_column]=="Not Available"),]
  my_outcome[,my_column]<-as.numeric(my_outcome[,my_column])
  outcome.state<-my_outcome[my_outcome$State==state,]
  ordered.outcome.state<-outcome.state[order(outcome.state[,my_column],outcome.state[,2]),]
  
  return(ordered.outcome.state[1,2])
}

test=function(){
  best("TX","heart attack")
  best("TX","heart failure")
  best("MD","heart attack")
  best("MD","pneumonia")
  best("BB","heart attack")
  best("NY","hert attack")
}