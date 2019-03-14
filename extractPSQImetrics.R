
extract.PSQI.metrics <- function(full.path) {
  # Extracts sleep quality indicator
  data <- read.table(full.path,
                     header = T,
                     sep = ',',
                     quote = '"')
  n.partic <- dim(data)[1]
  # component 1
  comp.1 <- as.numeric(gsub(".*([0-9]+).*", "\\1", get.question(9, data)))
  # component 2
  sleep.onset.mins <- get.question(2, data)
  score.2 <- array(dim = n.partic)
  score.2[sleep.onset.mins <= 15] <- 0
  score.2[sleep.onset.mins > 15 & sleep.onset.mins <= 30] <- 1
  score.2[sleep.onset.mins > 30 & sleep.onset.mins <= 60] <- 2
  score.2[sleep.onset.mins > 60] <- 3
  score.5a <- as.numeric(gsub(".*([0-9]+).*", "\\1", data[, grep('Q5.*30', names(data))]))
  c2.sum <- score.2 + score.5a
  comp.2 <- array(dim = n.partic)
  comp.2[c2.sum == 0] <- 0
  comp.2[c2.sum %in% c(1, 2)] <- 1
  comp.2[c2.sum %in% c(3, 4)] <- 2
  comp.2[c2.sum > 4] <- 3
  # component 3
  hrs.sleep <- data[, grep('Q4a', names(data))]
  comp.3 <- array(dim = n.partic)
  comp.3[hrs.sleep > 7] <- 0
  comp.3[hrs.sleep > 6 & hrs.sleep <= 7] <- 1
  comp.3[hrs.sleep > 5 & hrs.sleep <= 6] <- 2
  comp.3[hrs.sleep <= 5] <- 3
  # component 4
  hrs.bed <- data[, grep('Q4b', names(data))]
  ppn.sleep <- hrs.sleep / hrs.bed
  comp.4 <- array(dim = n.partic)
  comp.4[ppn.sleep > 0.85] <- 0
  comp.4[ppn.sleep > 0.75 & ppn.sleep <= 0.85] <- 1
  comp.4[ppn.sleep > 0.65 & ppn.sleep <= 0.75] <- 2
  comp.4[ppn.sleep <= 0.65] <- 3
  # component 5
  Q5.idx <- grep('Q5', names(data))
  Q5.a.idx <- grep('Q5.*30', names(data))
  Q5.b.j.idx <- Q5.idx[! Q5.idx %in% Q5.a.idx]
  Q5.b.j <- data[, Q5.b.j.idx]
  Q5.b.j[] <- lapply(Q5.b.j, function(x){as.numeric(gsub(".*([0-9]+).*", "\\1", x))})
  Q5.score <- rowSums(Q5.b.j, na.rm = T)
  comp.5 <- array(dim = n.partic)
  comp.5[Q5.score == 0] <- 0
  comp.5[Q5.score %in% 1:9] <- 1
  comp.5[Q5.score %in% 10:18] <- 2
  comp.5[Q5.score %in% 19:27] <- 3
  # component 6
  comp.6 <- as.numeric(gsub(".*([0-9]+).*", "\\1", data[, grep('medicine', names(data))]))
  # component 7
  score.7.8 <- as.numeric(gsub(".*([0-9]+).*", "\\1", data[, grep('driving', names(data))])) +
    as.numeric(gsub(".*([0-9]+).*", "\\1", data[, grep('enthusiasm', names(data))]))
  comp.7 <- array(dim = n.partic)
  comp.7[score.7.8 == 0] <- 0
  comp.7[score.7.8 %in% c(1, 2)] <- 1
  comp.7[score.7.8 %in% c(3, 4)] <- 2
  comp.7[score.7.8 %in% c(5, 6)] <- 3
  
  global.PSQI <- data.frame(global.PSQI =
    comp.1 +
    comp.2 +
    comp.3 +
    comp.4 +
    comp.5 +
    comp.6 +
    comp.7)
  return(global.PSQI)
}

get.question <- function(n, data) {
  return(data[, grep(sprintf('Q%d', n), names(data))])
}