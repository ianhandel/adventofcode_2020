🎄🎄🎄 day 15 🎄🎄🎄
================

``` r
library(tidyverse)

x <- c(20,0,1,11,6,3)
```

### Part 1

Ugly and super ugly as indexes off 1

``` r
n <- 2020
times <- numeric(n + 1)
last <- numeric(n + 1)
previous <- numeric(n + 1)

last[x + 1] <- seq_along(x)
times[x + 1] <- 1

say <- last(x)

for(r in (length(x) + 1):n){

  last_said <- say
  
  if(times[last_said + 1] == 1){
    say <- 0
  }else{
    say <- r - 1 - previous[last_said + 1]
  }
  
  times[say + 1] <- times[say + 1] + 1
  previous[say + 1] <- last[say + 1]
  last[say + 1] <- r
  
}


say
```

    ## [1] 421
