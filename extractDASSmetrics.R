
extract.DASS.metrics <- function(full.path) {
  # Extracts the following metrics:
  # Depression score
  # Anxiety score
  # Stress score
  data <- read.table(full.path,
                     header = T,
                     sep = ',',
                     quote = '"')
  depression.questions <- c(3, 5, 10, 13, 16, 17, 21, 24, 26, 31, 34, 37, 38, 42)
  anxiety.questions <- c(2, 4, 7, 9, 15, 19, 20, 23, 25, 28, 30, 36, 40, 41)
  stress.questions <- c(1, 6, 8, 11, 12, 14, 18, 22, 27, 29, 32, 33, 35, 39)
  
  all.sets <- list(depression.questions,
                   anxiety.questions,
                   stress.questions)
  set.names <- c('depression',
                 'anxiety',
                 'stress')
  
  output.data <- data.frame(matrix(nrow = nrow(data), ncol = 3))
  colnames(output.data) <- set.names
  for (curr.set.idx in 1:length(all.sets)) {
    curr.sums <- array(0, dim = nrow(data))
    curr.totals <- array(0, dim = nrow(data))
    for (q.n in all.sets[[curr.set.idx]]) {
      curr.resps <- data[, grep(sprintf('SQ%03d', q.n), names(data), value=T)]
      curr.totals[!is.na(curr.resps)] <- curr.totals[!is.na(curr.resps)] + 3
      curr.resps[is.na(curr.resps)] <- 0
      curr.sums <- curr.sums + curr.resps
    }
    # curr.sums <- curr.sums / curr.totals # Normalize by missing data
    output.data[, set.names[curr.set.idx]] <- curr.sums
  }
  
  return(output.data)
  
}