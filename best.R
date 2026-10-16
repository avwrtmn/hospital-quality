best <- function(state,outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  cols <- c("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
            "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
            "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  if (!state %in% data$State) stop("invalid state")
  if (!outcome %in% names(cols)) stop("invalid outcome")
  
  state_data <- data[data$State == state, ]
  rates <- suppressWarnings(as.numeric(state_data[[cols[[outcome]]]]))
  valid <- !is.na(rates)
  rates <- rates[valid]
  hospitals <- state_data$Hospital.Name[valid]
  
  sort(hospitals[rates == min(rates)])[1]
}
source("best.R")
best("TX", "heart attack")

best("ZZ", "heart attack")
best("TX", "wrong outcome")
best("NY", "heart attack")
