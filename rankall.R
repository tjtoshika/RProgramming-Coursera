rankall <- function(outcome1, num = 'best'){
        outcome <- read.csv('ProgAssignment3-data/outcome-of-care-measures.csv', colClasses = 'character')
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
        
        states <- unique(outcome$State)
        states <- sort(states)
        temp1 <- data.frame()
        for (i in 1:length(states)){
                temp <- subset(outcome,State == states[i],select = c(Hospital.Name,State,get(outcome1)))
                temp[, (outcome1)] <- as.numeric(temp[, (outcome1)])
                temp <- temp[order(temp$Hospital.Name), ]
                temp <- temp[order(temp[outcome1]), ]
                temp <- temp[complete.cases(temp), ]
                
                if (num == 'best'){
                        answer <- data.frame(temp[1,])
                }
                else if (num == 'worst'){
                        answer <- data.frame(tail(temp,n=1))
                }
                else{
                        answer <- data.frame(temp[num,])
                }
                if (is.na(answer)){
                        answer$State <- states[i]
                }
                temp1 <- rbind(temp1,answer)
        }
        names(temp1) <- c('hospital', 'state','nn')
        keeps <- c('hospital','state')
        temp1[keeps]
}