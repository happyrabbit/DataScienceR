######################################## impute missing value
impute0 <- function(dat) {
  for (i in 1:ncol(dat)) {
    idx <- which(is.na(dat[, i]))
    if (length(idx))
      dat[, i][idx] <- 0
  }
  return(dat)
}
##---------------------------------
impute_mean <- function(dat) {
  for (i in 1:ncol(dat)) {
    idx <- which(is.na(dat[, i]))
    if (length(idx))
      dat[, i][idx] <- mean(na.omit(dat[, i]))
  }
  return(dat)
}
##---------------------------------
impute_inf0 <- function(dat) {
  for (i in 1:ncol(dat)) {
    idx <- which(dat[, i] == Inf)
    if (length(idx))
      dat[, i][idx] <- 0
  }
  return(dat)
}
##---------------------------------
impute99 <- function(dat) {
  for (i in 1:ncol(dat)) {
    idx <- which(dat[, i] == 99)
    if (length(idx))
      dat[, i][idx] <- 0
  }
  return(dat)
}
