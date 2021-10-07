library(tidyverse)
library(lubridate)

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

x <- 1:100

rescale01(x)

rescale01_alt <- function(x, na.rm = FALSE) {
  rng <- range(x, na.rm = na.rm, finite = TRUE)
  print(rng)
  print(x)
  (x - rng[1]) / (rng[2] - rng[1])
}

rescale01_alt(c(NA, 1:5), na.rm = FALSE)

rescale01_alt(c(NA, 1:5), na.rm = TRUE)

rescale01_alt_2 <- function(x, na.rm = FALSE, finite = FALSE) {
  rng <- range(x, na.rm = na.rm, finite = finite)
  print(rng)
  print(x)
  (x - rng[1]) / (rng[2] - rng[1])
}

rescale01_alt_2(c(NA, 1:5), na.rm = FALSE, finite = FALSE)

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  print(rng)
  print(x)
  y <- (x - rng[1]) / (rng[2] - rng[1])
  y[y == -Inf] <- 0
  y[y == Inf] <- 1
  y
}

rescale01(c(Inf, -Inf, 0:5, NA))

mean(is.na(x))

x / sum(x, na.rm = TRUE)

sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)

mean(is.na(x))
