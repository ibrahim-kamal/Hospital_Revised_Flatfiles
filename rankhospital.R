rankhospital <- function(st, out, num = "best") {
  
  out <- stringr::str_replace(string = stringr::str_to_title(out),pattern = " ",replacement = ".")
  ## Read outcome data
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  nameoutcome      <- paste('Hospital.30.Day.Death..Mortality..Rates.from.',out,sep = '')
  outcomenames     <- names(outcome)
  Exoutcome        <- outcomenames[outcomenames == nameoutcome] 
  if(length(Exoutcome) != 1){
    return(paste('Error in best("',st,',", "',out,'") : invalid outcome',sep = ''))
  }
  Statenames     <- outcome['State']
  Exstate        <- Statenames[Statenames == st] 
  if(length(Exstate) <= 0){
    return(paste('Error in best("',st,',", "',out,'") : invalid state',sep = ''))
  }
  ## Return hospital name in that state with lowest 30-day death
  My_sub <- subset(outcome,outcome$State==st & outcome[nameoutcome]!= "Not Available",select = c("Hospital.Name",nameoutcome))
  
  return_data <-  My_sub[order(as.numeric(My_sub[[nameoutcome]])),]
  if(num == "best"){
    num <- 1
  }
  else if(num == "worst"){
    num <- length(return_data$Hospital.Name)
  }
  return_data <- head(return_data,num)
  evaluate    <- (return_data[[2]])[num]
  outcomevalue <- return_data[[2]]
  newNum =  num - length(outcomevalue[outcomevalue < evaluate])
  hosname <- subset(outcome,outcome$State==st & outcome[nameoutcome] == evaluate , select = "Hospital.Name")
  ## rate
  return_data<- sort(hosname$Hospital.Name)
  return(return_data[newNum])
}