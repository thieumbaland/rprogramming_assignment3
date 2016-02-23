rankhospital=function(state,outcome,num="best"){
  library(readr)
  library(plyr)
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
  
  if(!is.character(num) & num>nrow(ordered.outcome.state)){
    returnval<-NA
  }else{
    myrow<-ifelse(num=="best",1,ifelse(num=="worst",nrow(ordered.outcome.state),num))
    returnval<-ordered.outcome.state[myrow,2]
  }
  
  return(returnval)
}

test=function(){
  rankhospital("TX","heart failure",4)
  rankhospital("MD","heart attack","worst")
  rankhospital("MN","heart attack",5000)
}