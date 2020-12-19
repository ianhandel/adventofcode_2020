🎄🎄🎄 day 17 🎄🎄🎄
================

``` r
library(tidyverse)
library(here)
```

``` r
x <- read_lines(here("day_17", "test.txt")) %>% 
  map(~ str_split(.x, "", simplify = TRUE)) %>% 
  stringi::stri_list2matrix(byrow = TRUE)

x <- which(x == "#", arr.ind = TRUE) %>%
  as_tibble() %>% 
  rename(x = row, y = col) %>% 
  mutate(z = 1)

x
```

    ## # A tibble: 5 x 3
    ##       x     y     z
    ##   <int> <int> <dbl>
    ## 1     3     1     1
    ## 2     1     2     1
    ## 3     3     2     1
    ## 4     2     3     1
    ## 5     3     3     1
