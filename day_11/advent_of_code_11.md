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

## Part 2

``` r
# make search vectors
search <- 
  expand.grid(dc = -1:1,
            dr = -1:1) %>%
  filter(!(dc ==0 & dr == 0)) %>% 
  mutate(direction = 1:8)

check_seat <- function(r, c){
  if (c <= 0 | c > ncol(room)) return(0) # outside room
  if (r <= 0 | r > nrow(room)) return(0) # outside room
  return(room[r, c])
}


# code 1 or 3 for empty
# code 2 or 4 for full
# first bit now second bit next

for(ii in 1:100){

  room2 <- room

for (row in 1:nrow(room)) {
  for (col in 1:ncol(room)) {
    nfull <- 0
    for (direction in 1:8) {
      for (step in 1:max(nrow(room), ncol(room))) {
        status <- check_seat(
          c = col + step * search$dc[direction == search$direction],
          r = row + step * search$dr[direction == search$direction]
        )
        if (status %in% 1:2) {
          break()
        }
      }
      if (status == 2) nfull <- nfull + 1
    }
    if (room[row, col] == 2 & nfull >= 5) room2[row, col] <- 1
    if (room[row, col] == 1 & nfull == 0) room2[row, col] <- 2
  }
}

room <- room2

}

table(room)
```

    ## room
    ##    0    1    2 
    ## 1482 5260 2085
