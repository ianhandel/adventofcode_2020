🎄🎄🎄 day 13 🎄🎄🎄
================

``` r
library(tidyverse)
library(here)
library(pracma) # for primes

x <- read_lines(here("day_13", "day13.txt"))

# extract my timestamp
time <- parse_integer(x[[1]])

# extract bus ID's
buses <- x[[2]] %>% 
  str_split(",") %>%
  `[[`(1) %>% 
  parse_integer(na = "x")
```

### Part 1

``` r
map_int(buses, ~.x - (time %% .x )) %>%
  {buses[which(. == min(., na.rm = TRUE))] *  min(., na.rm = TRUE)}
```

    ## [1] 2382

### Part 2

Stuck for ages here. Then hint to use lowest common multipliers to
repeat patterns.

Then just work through adding buses and leaping ahead by lcm of found
patterns.

Phew.

``` r
orders <- seq_along(buses) %>%
  `-`(1) %>% 
  discard(is.na(buses))

orders
```

    ## [1]  0  7 17 19 36 40 48 54 61

``` r
buses <- buses %>% 
  discard(is.na(buses))

buses
```

    ## [1]  17  41 983  29  19  23 397  37  13

``` r
# I think because of Chinese Remainder Theorem need to be prime
stopifnot(buses %in% primes(max(buses)))

# lowest common multiplier of primes!
lcm <- function(i){
  if(i == 1){
    return(1)
  }else{
    return(prod(buses[1:i-1]))
  }
}

# find time that satisifies a set of buses searching in lcm jumps
find_time <- function(tstart, i) {
  t <- tstart
  while ((t + orders[i]) %% buses[i] != 0){
    t <- t + lcm(i)
  }
  t
}

# over increasing number of buses
reduce(seq_along(buses), find_time, .init = 1) %>% 
  format(digits = 20)
```

    ## [1] "906332393333683"
