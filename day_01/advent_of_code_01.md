🎄🎄🎄day 01 🎄🎄🎄
================

``` r
library(tidyverse)
library(here)
library(bench)

expense <- read_csv(here("day_01", "aoc_01.txt"), col_names = "fee1")
```

### Part 1

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

### Part 2

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

### More thoughts

Try a faster way???

Work out what you need to add to each value in fee vector and then
intersect this with fee vector.

Yup - faster.

Originally tried using %in% to find intersect - much slower\!

But how to do this with part 2???

``` r
bench::mark(
  original = {
    expense %>%
      crossing(fee2 = .$fee1) %>%
      filter(fee1 + fee2 == 2020) %>%
      pull(fee1) %>%
      prod()
  },
  faster = {
    intersect(expense$fee1, 2020 - expense$fee1) %>%
      prod()
  }
)
```

    ## # A tibble: 2 x 6
    ##   expression      min   median `itr/sec` mem_alloc `gc/sec`
    ##   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
    ## 1 original     5.59ms   6.79ms      132.    2.03MB     4.47
    ## 2 faster      30.38µs  36.15µs    21640.   11.03KB     4.33
