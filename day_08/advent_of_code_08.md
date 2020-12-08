🎄🎄🎄 day 08 🎄🎄🎄
================

``` r
library(tidyverse)
library(here)
code <- tibble(line = read_lines(here("day_08", "day08.txt"))) %>% 
  separate(line, c("inst", "arg"), " ", convert = TRUE) %>% 
  mutate(executed = FALSE) %>% 
  mutate(row = 1:n())
```

### Part 1

``` r
run <- function(code) {
  
  code <- code %>% 
    arrange(row)
  
  acc <- 0
  ii <- 1

  while (!code$executed[ii] & ii <= nrow(code)) {
    code$executed[ii] <- TRUE

    if (code$inst[ii] == "acc") {
      acc <- acc + code$arg[ii]
    }

    if (code$inst[ii] == "jmp") {
      ii <- ii + code$arg[ii]
    } else {
      ii <- ii + 1
    }
  }
  list(acc = acc, infinite = ii < nrow(code) )
}


run(code)
```

    ## $acc
    ## [1] 1200
    ## 
    ## $infinite
    ## [1] TRUE

### Part 2

Using a list column to store code with one line changed.

Then run each reporting accumulator and if infinite.

``` r
code %>% 
  crossing(tibble(changed = 0:nrow(.))) %>% 

  mutate(inst = case_when(inst == "nop" & row == changed ~ "jmp",
                          inst == "jmp" & row == changed ~ "nop",
                          TRUE ~ inst)) %>%
  
  group_nest(changed) %>% 
  mutate(run = map(data, run)) %>% 
  mutate(acc = map_dbl(run, "acc")) %>%
  mutate(infinite = map_lgl(run, "infinite")) %>% 
  filter(!infinite)
```

    ## # A tibble: 1 x 5
    ##   changed               data run                acc infinite
    ##     <int> <list<tbl_df[,4]>> <list>           <dbl> <lgl>   
    ## 1     328          [643 × 4] <named list [2]>  1023 FALSE
