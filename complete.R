complete <- function(directory, id = 1:332){
  files_list <- list.files(directory, full.names = TRUE) #creates a list of files
  nobs <- numeric()
  for (i in id){
    new_data_frame <- read.csv(files_list[i])[complete.cases(read.csv(files_list[i])), ]
    nrow_id <- nrow(new_data_frame)
    nobs <- append(nobs, nrow_id)
  }
  
  id_1 = numeric()
  for (i in seq_along(id)){
    id_1[i] <- id[i]
  }
  df <- data.frame(id_1, nobs)
  names(df) <- c('id','nobs')
  df
}

