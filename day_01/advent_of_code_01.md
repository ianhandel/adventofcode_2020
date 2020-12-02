🎄🎄🎄day 01 🎄🎄🎄
================

``` r
library(tidyverse)

expense <- read_csv("aoc_01.txt", col_names = "fee1")
```

``` r
expense %>%
  crossing(fee2 = .$fee1) %>%
  filter(fee1 + fee2 == 2020) %>%
  mutate(product = fee1 * fee2)
```

    ## # A tibble: 2 x 3
    ##    fee1  fee2 product
    ##   <dbl> <dbl>   <dbl>
    ## 1   409  1611  658899
    ## 2  1611   409  658899

``` r
expense %>%
  crossing(fee2 = .$fee1) %>%
  crossing(fee3 = .$fee1) %>%
  filter(fee1 + fee2 + fee3 == 2020) %>%
  mutate(product = fee1 * fee2 * fee3)
```

    ## # A tibble: 6 x 4
    ##    fee1  fee2  fee3   product
    ##   <dbl> <dbl> <dbl>     <dbl>
    ## 1   250   485  1285 155806250
    ## 2   250  1285   485 155806250
    ## 3   485   250  1285 155806250
    ## 4   485  1285   250 155806250
    ## 5  1285   250   485 155806250
    ## 6  1285   485   250 155806250
