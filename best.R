best <- function(state,outcome1){
        outcome <- read.csv('ProgAssignment3-data/outcome-of-care-measures.csv', colClasses = 'character')
        if (!(is.element(state,unlist(outcome)))){stop('invalid state')}
        
        if (!(outcome1 %in% c("heart attack",'heart failure',"pneumonia"))){stop('invalid outcome')}
        
        if (outcome1 == 'heart attack'){
                outcome1 = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
        }
        else if (outcome1 == 'heart failure'){
                outcome1 = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
        }
        else{
                outcome1 = 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
        }
        
        temp <- subset(outcome,State == state,select = c(Hospital.Name,get(outcome1)))
        temp[, (outcome1)] <- as.numeric(temp[, (outcome1)])
        answer <- temp[which(temp[, outcome1] == min(temp[outcome1], na.rm = TRUE)), 'Hospital.Name']
        answer <- sort(answer, decreasing = FALSE)
        answer[1]
        
}

