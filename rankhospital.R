
rankhospital <- function(state, outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  cols <- c("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
            "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
            "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  if (!state %in% data$State) stop("invalid state")
  if (!outcome %in% names(cols)) stop("invalid outcome")
  
  state_data <- data[data$State == state, ]
  rates <- suppressWarnings(as.numeric(state_data[[cols[[outcome]]]]))
  valid <- !is.na(rates)
  df <- data.frame(hospital = state_data$Hospital.Name[valid],
                   rate = rates[valid],
                   stringsAsFactors = FALSE)
  df <- df[order(df$rate, df$hospital), ]
  
  if (identical(num, "best")) {
    num <- 1
  } else if (identical(num, "worst")) {
    num <- nrow(df)
  } else if (!is.numeric(num)) {
    stop("invalid num")
  }
  
  if (num > nrow(df)) return(NA_character_)
  df$hospital[num]
}

source("rankhospital.R")
rankhospital("TX", "heart failure", 4)

rankhospital("TX", "heart failure", "best")
rankhospital("TX", "heart failure", "worst")
rankhospital("TX", "heart failure", 10000)
