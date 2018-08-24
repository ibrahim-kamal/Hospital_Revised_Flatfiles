# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.
# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals “b”, “c”,
#                                                                                      and “f” are tied for best, then hospital “b” should be returned).
# The function should use the following template


best <- function(st,out){
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
  sub <- subset(outcome,outcome$State==st & outcome[nameoutcome]!= "Not Available",select = nameoutcome)
  lowest <- min(as.numeric(sub[[nameoutcome]]))
  hosname <- subset(outcome,outcome$State==st & outcome[nameoutcome] == lowest , select = "Hospital.Name")
  ## rate
  return_data<- sort(hosname$Hospital.Name)
  return(return_data[1])
}



