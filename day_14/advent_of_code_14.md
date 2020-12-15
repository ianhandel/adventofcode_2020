🎄🎄🎄 day 14 🎄🎄🎄
================

``` r
library(tidyverse)
library(here)
library(binaryLogic)

x <- tibble(raw = read_lines(here("day_14", "day14.txt"))) %>%
  extract(raw,
    c("inst", "mem", "input"),
    regex = "^(mask|mem)\\[*(\\d*)\\]* = ([X|\\d]+)",
    convert = TRUE
  )
```

### Part 1

``` r
memory <- numeric(max(x$mem, na.rm = TRUE))

apply_mask <- function(mask, value) {
  do_logic <- function(v, m) {
    case_when(
      m == "X" ~ v,
      m == "0" ~ FALSE,
      m == "1" ~ TRUE
    )
  }
  mask <- str_split(mask, "")[[1]]
  value <- as.binary(value, signed = FALSE) %>% {
    c(rep(FALSE, 36 - length(.)), .)
  }
  map2_lgl(value, mask, do_logic) %>%
    `*`(2^(35:0)) %>%
    sum()
}

for (ii in 1:nrow(x)) {
  if (x$inst[ii] == "mask") {
    mask <- x$input[ii]
  }
  if (x$inst[ii] == "mem") {
    memory[x$mem[ii]] <- apply_mask(mask, x$input[ii])
  }
}

sum(memory) %>%
  format(digits = 20)
```

    ## [1] "9967721333886"

### Part 2

Unfinished

``` r
float <- function(mask, mem){
  mask %>%
    map(~if(is.na(.x)){0:1}else{.x}) %>% do.call(expand.grid, .)
  
}

x %>% 
  mutate(mask = if_else(inst == "mask", input, NA_character_)) %>%
  fill(mask) %>% 
  filter(inst == "mem") %>% 
  select(-inst) %>% 
  mutate(mask = map(mask, ~as.integer(str_split(.x, "")[[1]]))) %>%
  mutate(newmem = map2(mask, mem, float))
```

    ## # A tibble: 445 x 4
    ##      mem input     mask       newmem             
    ##    <int> <chr>     <list>     <list>             
    ##  1 41579 225076    <int [36]> <df[,36] [16 × 36]>
    ##  2 14806 26208185  <int [36]> <df[,36] [16 × 36]>
    ##  3 47659 176531392 <int [36]> <df[,36] [16 × 36]>
    ##  4 27723 186971157 <int [36]> <df[,36] [16 × 36]>
    ##  5 35129 3483636   <int [36]> <df[,36] [16 × 36]>
    ##  6 27142 4246      <int [36]> <df[,36] [16 × 36]>
    ##  7 16685 392461    <int [36]> <df[,36] [64 × 36]>
    ##  8 65343 13662482  <int [36]> <df[,36] [64 × 36]>
    ##  9 53292 736       <int [36]> <df[,36] [64 × 36]>
    ## 10  6830 382342975 <int [36]> <df[,36] [64 × 36]>
    ## # … with 435 more rows
