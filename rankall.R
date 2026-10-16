rankall <- function(outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  cols <- c("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
            "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
            "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  if (!outcome %in% names(cols)) stop("invalid outcome")
  data$rate <- suppressWarnings(as.numeric(data[[cols[[outcome]]]]))
  data <- data[!is.na(data$rate), ]
  
  states <- sort(unique(data$State))
  hospital <- character(length(states))
  
  for (i in seq_along(states)) {
    st <- data[data$State == states[i], ]
    st <- st[order(st$rate, st$Hospital.Name), ]
    rank <- if (identical(num, "best")) 1
    else if (identical(num, "worst")) nrow(st)
    else num
    hospital[i] <- if (rank > nrow(st)) NA_character_ else
      st$Hospital.Name[rank]
  }
  
  data.frame(hospital = hospital, state = states,
             row.names = states, stringsAsFactors = FALSE)
}
source("rankall.R")
head(rankall("heart attack", 20), 10)
head(rankall("heart attack", "best"), 10)
head(rankall("heart attack", 20), 10)
tail(rankall("heart failure", "worst"), 5)

