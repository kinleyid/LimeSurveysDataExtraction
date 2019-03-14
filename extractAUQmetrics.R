
na.to.zero <- function(x) {
  if (is.na(x)) {
    x <- 0
  }
  return(x)
}

cleaner.func <- function(m) {
  x <- array(NA, dim(m))
  x[] <- vapply(m, na.to.zero, numeric(1))
  x <- colSums(x)
  x[colSums(is.na(m)) == dim(m)[1]] <- NA
  return(x)
}

get.days.from.months.years <- function(data, string) {
  return(
    cleaner.func(rbind(365*data[, grep(sprintf('%s.*years', string), tolower(names(data)))],
                       30*data[, grep(sprintf('%s.*months', string), tolower(names(data)))],
                       data[, grep(sprintf('%s.*days', string), tolower(names(data)))]))
  )
}

get.days <- function(string) {
  x <- string
  string <- tolower(string)
  if (string == 'every day') {
    x <- 9
  } else if (string == '5 to 6 days a week') {
    x <- 8
  } else if (string == '3 to 4 days a week') {
    x <- 7
  } else if (string %in% c('two days a week', 'twice a week')) {
    x <- 6
  } else if (string %in% c('one day a week', 'once a week')) {
    x <- 5
  } else if (string == '2 to 3 days a month') {
    x <- 4
  } else if (string %in% c('one day a month', 'once a month')) {
    x <- 3
  } else if (string == '3 to 11 days in the past year') {
    x <- 2
  } else if (string == '1 or 2 days in the past year') {
    x <- 1
  } else if (string == 'never') {
    x <- 0
  } else if (string == '') {
    x <- NA
  }
  return(x)
}

get.drink.n.more <- function(string) {
  if (string == '36 drinks or more') {
    x <- 10
  } else if (string == '24 to 35 drinks') {
    x <- 9
  } else if (string == '18 to 23 drinks') {
    x <- 8
  } else if (string == '12 to 17 drinks') {
    x <- 7
  } else if (string == '8 to 11 drinks') {
    x <- 6
  } else if (string == '5 to 7 drinks') {
    x <- 5
  } else if (string == '4 drinks') {
    x <- 4
  } else if (string == '3 drinks') {
    x <- 3
  } else if (string == '2 drinks') {
    x <- 2
  } else if (string == '1 drink') {
    x <- 1
  } else if (string == '') {
    x <- NA
  }
  return(x)
}

get.drink.n.less <- function(string) {
  if (string == '25 or more drinks') {
    x <- 10
  } else if (string == '19 to 24 drinks') {
    x <- 9
  } else if (string == '16 to 18 drinks') {
    x <- 8
  } else if (string == '12 to 15 drinks') {
    x <- 7
  } else if (string == '9 to 11 drinks') {
    x <- 6
  } else if (string == '7 to 8 drinks') {
    x <- 5
  } else if (string == '5 to 6 drinks') {
    x <- 4
  } else if (string == '3 to 4 drinks') {
    x <- 3
  } else if (string == '2 drinks') {
    x <- 2
  } else if (string == '1 drink') {
    x <- 1
  } else if (string == '') {
    x <- NA
  }
  return(x)
}

extract.AUQ.metrics <- function(full.path) {
  data <- read.table(full.path,
                     header = T,
                     sep = ',',
                     quote = '"')
  output.data <- data.frame()[1:nrow(data), ]
  output.data$gender <- data[, grep('gender', tolower(names(data)))]
  output.data$age <- data[, grep('how.*old.*you.*years', tolower(names(data)))]
  output.data$years.schooling <- data[, grep('years.*schooling', tolower(names(data)))]
  never.consumed <- data[, grep('ever.*consumed.*alcohol', tolower(names(data)))] == 'No'
  output.data$first.any.alc.age <- data[, grep('first.*sip.*very.*small', tolower(names(data)))]
  output.data$first.drink.age <- data[, grep('first.*standard.*not', tolower(names(data)))]
  output.data$first.felt.drunk.age <- data[, grep('first.*drunk', tolower(names(data)))]
  output.data$first.binge.age <- data[, grep('first.*five.*more', tolower(names(data)))]
  output.data$max.drinks.ever <- data[, grep('maximum.*drinks.*ever', tolower(names(data)))]
  output.data$max.drinks.ever[never.consumed] <- 0
  output.data$days.since.any <- cleaner.func(rbind(365*data[, grep('since.*even.*sip.*years', tolower(names(data)))],
                          30*data[, grep('since.*even.*sip.*months', tolower(names(data)))],
                          data[, grep('since.*even.*sip.*days', tolower(names(data)))]))
  output.data$days.since.drink <- get.days.from.months.years(data, 'since.*standard')
  output.data$days.since.binge <- get.days.from.months.years(data, 'since.*five.*more')
  output.data$days.since.drunk <- get.days.from.months.years(data, 'since.*drunk')
  
  # Questions about the last 12 months
  
  last.year.no.drinks <- data[, grep('12.*months.*how.*often.*any.*drink', tolower(names(data)))] ==
    'I did not drink any alcohol in the past year, but I did drink in the past'
  output.data$last.year.drink.freq <- sapply(data[, grep('12.*months.*often.*any.*drink', tolower(names(data)))], get.days)
  output.data$last.year.drink.freq[never.consumed] <- 0
  output.data$last.year.drink.freq[last.year.no.drinks] <- 0
  output.data$last.year.typical.n.drinks <- sapply(data[, grep('12.*months.*how.*many.*typical', tolower(names(data)))], get.drink.n.less)
  output.data$last.year.typical.n.drinks[never.consumed] <- 0
  output.data$last.year.typical.n.drinks[last.year.no.drinks] <- 0
  output.data$last.year.most.drinks.24.hrs <- sapply(data[, grep('12.*months.*largest.*24', tolower(names(data)))], get.drink.n.more)
  output.data$last.year.most.drinks.24.hrs[never.consumed] <- 0
  output.data$last.year.most.drinks.24.hrs[last.year.no.drinks] <- 0
  output.data$last.year.most.drinks.freq <- sapply(data[, grep('12.*months.*often.*largest', tolower(names(data)))], get.days)
  output.data$last.year.most.drinks.freq[never.consumed] <- 0
  output.data$last.year.most.drinks.freq[last.year.no.drinks] <- 0
  output.data$last.year.binge.freq <- sapply(data[, grep('12.*months.*5.*more', tolower(names(data)))], get.days)
  output.data$last.year.binge.freq[never.consumed] <- 0
  output.data$last.year.binge.freq[last.year.no.drinks] <- 0
  output.data$lifetime.most.drinks.24.hrs <- sapply(data[, grep('12.*months.*24', tolower(names(data)))], get.drink.n.more)
  output.data$lifetime.most.drinks.24.hrs[never.consumed] <- 0
  output.data$lifetime.most.drinks.24.hrs[last.year.no.drinks] <- 0
  
  # Questions about binge drinking intensity over lifespan
  
  for (idx in grep('at.*age.*[0-9].*often', tolower(names(data)))) {
    lower.age <- as.numeric(gsub("^.*age.*([0-9][0-9]).*$", "\\1", names(data)[idx]))
    name <- sprintf('%s.binge', gsub("^.*(age.*[0-9][0-9]).*$", "\\1", names(data)[idx]))
    curr.vec <- sapply(data[, idx], get.days)
    curr.vec[never.consumed] <- 0
    curr.vec[output.data$age < lower.age] <- NA
    output.data[, name] <- curr.vec
  }
  return(output.data)
}