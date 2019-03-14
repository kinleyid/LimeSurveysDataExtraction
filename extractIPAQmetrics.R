
extract.IPAQ.metrics <- function(full.path) {
  # Extracts the continuous score
  data <- read.table(full.path,
                     header = T,
                     sep = ',',
                     quote = '"')
  names(data) <- tolower(names(data))
  formats <- paste(c('work',
                     'trans',
                     'yard',
                     'house',
                     'leis'), '%s%s', sep = '')
  middle.words <- list(
    c('walking', 'moderate', 'vigorous'),
    c('motor', 'cycle'),
    c('moderate', 'vigorous'),
    c('moderate'),
    c('walking', 'moderate', 'vigorous')
  )
  coeffs <- list(
    c(3.3, 4, 8),
    c(3.3, 6),
    c(4, 5.5),
    3,
    c(3.3, 4, 8)
  )
  total.scores <- array(0, dim = dim(data)[1])
  for (idx.1 in 1:length(formats)) {
    curr.fmt <- formats[idx.1]
    curr.m.ws <- middle.words[[idx.1]]
    curr.coefs <- coeffs[[idx.1]]
    for (idx.2 in 1:length(curr.coefs)) {
      curr.days <- data[, grep(sprintf(curr.fmt, curr.m.ws[idx.2], 'days'), names(data))]
      curr.days[is.na(curr.days)] <- 0
      curr.mins <- data[, grep(sprintf(curr.fmt, curr.m.ws[idx.2], 'minutes.*minutes'), names(data))]
      curr.mins[is.na(curr.mins)] <- 0
      curr.hrs <- data[, grep(sprintf(curr.fmt, curr.m.ws[idx.2], 'minutes.*hours'), names(data))]
      curr.hrs[is.na(curr.hrs)] <- 0
      total.scores <- total.scores + curr.coefs[idx.2] * curr.days * (60*curr.hrs + curr.mins)
    }
  }
  
  return(data.frame(IPAQ.MET = total.scores))
  
}