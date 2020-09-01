corr <- function(directory, threshold = 0){
  files_list <- list.files(directory, full.names = TRUE) #creates a list of files
  data_before_threshold <- complete(directory)
  data_after_threshold <- data_before_threshold[which(data_before_threshold$nobs > threshold), "id"]
  x <- numeric()
  for (i in seq_along(data_after_threshold)){
    data <- read.csv(files_list[data_after_threshold[i]])
    ans <- cor(data$sulfate, data$nitrate, use = 'complete.obs')
    x <- append(x, ans)
  }
  x
}