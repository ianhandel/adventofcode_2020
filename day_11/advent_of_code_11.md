🎄🎄🎄 day 11 🎄🎄🎄
================

``` r
library(tidyverse)
library(here)
library(simecol)

room <- read_lines(here("day_11", "day11.txt")) %>%
  map(~ str_split(.x, "", simplify = TRUE)) %>%
  stringi::stri_list2matrix(byrow = TRUE)

room[1:6, 1:6]
```

    ##      [,1] [,2] [,3] [,4] [,5] [,6]
    ## [1,] "L"  "L"  "."  "L"  "L"  "." 
    ## [2,] "L"  "L"  "L"  "L"  "L"  "L" 
    ## [3,] "."  "L"  "L"  "L"  "L"  "." 
    ## [4,] "L"  "L"  "L"  "L"  "L"  "." 
    ## [5,] "L"  "L"  "L"  "L"  "L"  "L" 
    ## [6,] "."  "L"  "L"  "L"  "L"  "."

``` r
dims <- dim(room)

# Coding for simecol as needs numeric
# 0 = floor
# 1 = empty
# 2 = full

room <- matrix(room %>%
  str_replace_all(c(
    "\\." = "0",
    "L" = "1",
    "#" = "2"
  )) %>% parse_double(), dims) 
```

If a seat is empty (L) and there are no occupied seats adjacent to it,
the seat becomes occupied.

If a seat is occupied (\#) and four or more seats adjacent to it are
also occupied, the seat becomes empty.

## Part 1

Rather than the rubbish solution I’d code, let’s try using simecol…

``` r
sims <- 200
seating <- new("gridModel",
    main = function(time, init, parms) {
        x   <- init
        n   <- nrow(x)
        m   <- ncol(x)
        nb  <- eightneighbours(x[] == 2)
        ## seat rule
        x <- case_when(x == 0 ~ 0,
                       x == 1 & nb == 0 ~ 2,
                       x == 2 & nb >= 4 ~ 1,
                       TRUE ~ x)
        dim(x) <- c(n,m)
    x
    },
    times = c(from=1, to=sims, by=1),
    init = room,
    solver = "iteration"
)

sim(seating)@out[sims] %>% table()
```

    ## .
    ##    0    1    2 
    ## 1482 5064 2281
