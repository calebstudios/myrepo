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

View(mtcars)

mtcars %>% 
  split(.$cyl) %>% # from base R
  map(~ lm(mpg ~ wt, data = .)) %>% 
  map(summary) %>% 
  map_dbl("r.squared")

search()

#install.packages("repurrrsive")
library(repurrrsive)

(3:5) ^ 2
sqrt(c(9, 16, 25))

map(c(9, 16, 25), sqrt)

map(got_chars[1:4], "name")

map(got_chars[5:8], 3)

got_chars %>% 
  map("name")

got_chars %>% 
  map(3)

map_chr(got_chars[9:12], "name")

map_chr(got_chars[13:16], 3)

got_chars[[3]][c("name", "culture", "gender", "born")]

x <- map(got_chars, `[`, c("name", "culture", "gender", "born"))
str(x[16:17])

search()
#install.packages("magrittr")
library(magrittr)
library(tidyverse)

x <- map(got_chars, magrittr::extract, c("name", "culture", "gender", "born"))
str(x[18:19])
