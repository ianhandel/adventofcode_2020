🎄🎄🎄 day 01 🎄🎄🎄
================

from @drob - why did I read his answer first??

``` r
library(tidyverse)

passwords <- read_csv("aoc_02.txt", col_names = "policy") %>%
  extract(policy,
    c("min", "max", "letter", "password"),
    "(\\d+)-(\\d+) (.): *(.*)",
    convert = TRUE
  )
```

``` r
passwords %>%
  mutate(count = map2_int(password, letter, str_count)) %>%
  filter(count >= min & count <= max) %>%
  nrow()
```

    ## [1] 666

``` r
passwords %>%
  mutate(count = (str_sub(password, min, min) == letter) +
    (str_sub(password, max, max) == letter)) %>%
  filter(count == 1)
```

    ## # A tibble: 670 x 5
    ##      min   max letter password            count
    ##    <int> <int> <chr>  <chr>               <int>
    ##  1     3    11 c      ccchcccccclxnkcmc       1
    ##  2     3    10 h      xcvxkdqshh              1
    ##  3     5     9 j      ddqwznjhjcjn            1
    ##  4     8     9 d      fddddddmd               1
    ##  5     6     8 t      qtlwttsqg               1
    ##  6     7    15 m      lxzxrdbmmtvwhgm         1
    ##  7     6    10 h      hhnhhhhxhkh             1
    ##  8     6     8 z      zhgztgjzzfzqzzvnbmv     1
    ##  9     6     7 n      nnnqgdnn                1
    ## 10     7     8 k      kkgkkbskkk              1
    ## # … with 660 more rows
