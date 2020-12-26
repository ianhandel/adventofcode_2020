🎄🎄🎄 day 25 🎄🎄🎄
================

``` r
library(tidyverse)
library(here)
library(VeryLargeIntegers)
```

puzzle input 8458505 16050997

test 5764801 17807724

Thanks to Cameron for \``subnum^loops %% x` identity, which I didn’t end
up using.

### Part 1

``` r
encode <- function(subnum, loops) {
  value <- 1
  for (ii in 1:loops) {
    value <- (value * subnum) %% 20201227
  }
  value
}

# must be a better way as slow and big
bench::mark(
  encodes <- accumulate(1:20000000, ~ (.x * 7) %% 20201227)[-1],
  iterations = 1
)
```

    ## # A tibble: 1 x 6
    ##   expression                                                min median `itr/sec`
    ##   <bch:expr>                                              <bch> <bch:>     <dbl>
    ## 1 encodes <- accumulate(1:2e+07, ~(.x * 7)%%20201227)[-1]    2m     2m   0.00834
    ## # … with 2 more variables: mem_alloc <bch:byt>, `gc/sec` <dbl>

``` r
# find loops and apply to other public key
loops <- which(encodes == 8458505, arr.ind = TRUE)
encode(subnum = 16050997, loops = loops)
```

    ## [1] 448851
