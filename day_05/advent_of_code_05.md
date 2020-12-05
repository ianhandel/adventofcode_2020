🎄🎄🎄 day 05 🎄🎄🎄
================

``` r
library(tidyverse)
library(here)

seats <- read_csv(here("day_05", "day_05.txt"), col_names = "seat") %>% 
  extract(seat, into = c("row", "col"), regex = "(.{7})(.{3})")

seats
```

    ## # A tibble: 817 x 2
    ##    row     col  
    ##    <chr>   <chr>
    ##  1 FBFFFFB RLL  
    ##  2 FFBFFFF RLR  
    ##  3 BFBBFFB RLL  
    ##  4 FBBBBFF LLR  
    ##  5 BFFFBFF LRL  
    ##  6 FBBFBFF LRR  
    ##  7 FBBFBFB LLL  
    ##  8 BFFBFFF LRL  
    ##  9 FBFFBBB LRR  
    ## 10 BFFFFFB RRR  
    ## # … with 807 more rows

### Part 1

``` r
seats <- seats %>% 
  
  # code as binary character (must be a neater way)
  
  mutate(row = str_replace_all(row, "B", "1"),
         row = str_replace_all(row, "F", "0"),
         col = str_replace_all(col, "R", "1"),
         col = str_replace_all(col, "L", "0")) %>% 
  
  # string to number (2L is binary)
  
  mutate(across(everything(), strtoi, 2L)) %>% 
  
  mutate(id = row * 8 + col)


seats %>% 
  summarise(max(id))
```

    ## # A tibble: 1 x 1
    ##   `max(id)`
    ##       <dbl>
    ## 1       901

### Part 2

Look for an id difference of 2 then back one

``` r
seats %>% 
  arrange(id) %>% 
  mutate(diff = id - lag(id)) %>% 
  filter(diff == 2) %>% 
  pull(id) %>% 
  `-`(1)
```

    ## [1] 661
