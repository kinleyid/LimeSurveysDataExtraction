
extract.ESS.metrics <- function(full.path) {
  # Extracts the following metrics:
  # Depression score
  # Anxiety score
  # Stress score
  data <- read.table(full.path,
                     header = T,
                     sep = ',',
                     quote = '"')
  return(data.frame(ESS.score = rowSums(data[, grep('likely', names(data))], na.rm = T)))
}