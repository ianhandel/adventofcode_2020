🎄🎄🎄 day 10 🎄🎄🎄
================

``` r
library(tidyverse)
library(here)
x <- read_lines(here("day_10", "day10.txt")) %>% 
  parse_integer()
```

### Part 1

``` r
x <- x %>%
  c(0) %>% 
  sort() %>% 
  c(max(.) + 3) %>% 
  diff()

x %>% 
  table() %>% 
  prod()
```

    ## [1] 1690

### Part 2

``` r
x %>%
  
  # use run length encoding to get
  # runs of ones (cant drop 3 jolt adaptors)
  
  rle() %>% 
  unclass() %>% 
  as_tibble() %>% 
  filter(values == 1) %>%
  
  # how many ways can you drop some
  # adaptors from runs of ones
  # NB need total drop <= 3
  # this bit by inspection rather
  # than clever way
  
  mutate(opts = case_when(lengths == 1 ~ 1,
                          lengths == 2 ~ 2,
                          lengths == 3 ~ 4,
                          lengths == 4 ~ 7)) %>% 
  
  # total ways is product of ways at each run of ones
  
  summarise(answer = prod(opts)) %>%
  pull(answer) %>% 
  format(digits = 20)
```

    ## [1] "5289227976704"
