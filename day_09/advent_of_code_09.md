🎄🎄🎄 day 09 🎄🎄🎄
================

``` r
library(tidyverse)
library(here)

x <- read_lines(here("day_09", "day09.txt")) %>% 
  parse_number()
```

### Part 1

``` r
check_one <- function(x, index, n){
  to_check <- x[(index - 1):(index - n - 1)]
  sums <- outer(to_check, to_check, `+`)
  diag(sums) <- NA
  x[index] %in% sums
}

check_all <- function(x, n){
  x[-(1:n)][!map_lgl((1 + n):length(x), check_one, n = n, x = x)]
}

key <- check_all(x, n = 25)

key
```

    ## [1] 217430975

### Part 2

``` r
# check for one start position

check_sum <- function(start, target){
  for(ii in (start + 1):length(x)){
    if(sum(x[start:ii]) == target){
      return(tibble(start = start, end = ii))
    }
  }
  return(tibble(start = NA, end = NA))
}

# check for all and then get sum of min and max
map_df(1:(length(x)-1), ~check_sum(.x, key)) %>% 
  filter(!is.na(start)) %>% 
  summarise(answer = min(x[start:end] + max(x[start:end])))
```

    ## # A tibble: 1 x 1
    ##     answer
    ##      <dbl>
    ## 1 28509180
