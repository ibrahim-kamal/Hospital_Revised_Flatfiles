rankall <- function(out, num = "best") {
  numRow <- num
  if(num == "best"){
    numRow <- 1
  }
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
  ## For each state, find the hospital of the given rank
  outcome <- subset(outcome,select = c("Hospital.Name",nameoutcome,"State"))
  My_sub <- outcome[[nameoutcome]]
  My_sub[My_sub == "Not Available"] = NA
  outcome[[nameoutcome]] <- My_sub
  My_sub <- outcome
  if(num != "worst"){
  return_data <- My_sub[order(My_sub[['State']],as.numeric(My_sub[[nameoutcome]]),My_sub[['Hospital.Name']]),]
  }
  else{
    return_data <- My_sub[order(My_sub[['State']],as.numeric(My_sub[[nameoutcome]]),My_sub[['Hospital.Name']],na.last = FALSE),]
  }
  #return (return_data)
  
  y <- names(table(return_data$State))
  if(num == "worst"){
    Row <- (return_data[return_data['State'] == y[1],])
    numRow = (length(Row[[nameoutcome]]))
  }
  x <- (return_data[return_data['State'] == y[1],])[numRow,]
  for (i in (2:length(y))) {
    if(num == "worst"){
      rm(Row)
      Row <- (return_data[return_data['State'] == y[i],])
      numRow = (length(Row[[nameoutcome]]))
    }
    x[i,] <- (return_data[return_data['State'] == y[i],])[numRow,]
    x[i,'State'] <- y[i]
  }
  re_data = data.frame(hospital = x$Hospital.Name,state = x$State)
  return(re_data)
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}