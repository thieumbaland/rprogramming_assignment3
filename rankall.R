rankall=function(outcome,num="best"){
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
  
  sorted_states<-sort(my_outcome$State)
  df<-{}
  for(i in 1:length(unique(sorted_states))){
    small.outcome<-my_outcome[my_outcome$State==unique(sorted_states)[i],]
    ordered.small.outcome<-small.outcome[order(small.outcome[,my_column],small.outcome[,2]),]
    
    if(!is.character(num) & num>nrow(ordered.small.outcome)){
      rowval<-NA
    }else{
      myrow<-ifelse(num=="best",1,ifelse(num=="worst",nrow(ordered.small.outcome),num))
      rowval<-ordered.small.outcome[myrow,2]
    }
    df<-rbind(df,c(rowval,unique(sorted_states)[i]))
  }
  colnames(df)<-c("hospital","state")
  
  return(as.data.frame(df))
}

test=function(){
  head(rankall("heart attack",20),10)
  tail(rankall("pneumonia","worst"),3)
  tail(rankall("heart failure"),10)
}