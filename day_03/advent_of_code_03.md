🎄🎄🎄 day 03 🎄🎄🎄
================

``` r
library(tidyverse)
library(here)

forest <- read_lines(here("day_03", "day_03.txt")) %>%
  
  # Break string into characters - probably a better way
  
  map(~ str_split(.x, "", simplify = TRUE)) %>% 
  
  # I wish I'd found this function sooner...
  
  stringi::stri_list2matrix(byrow = TRUE)
```

``` r
dim(forest)
```

    ## [1] 323  31

``` r
forest[1:10, 1:10]
```

    ##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    ##  [1,] "."  "."  "."  "#"  "."  "."  "."  "."  "."  "."  
    ##  [2,] "."  "."  "."  "#"  "."  "."  "#"  "."  "."  "#"  
    ##  [3,] "."  "."  "."  "."  "#"  "."  "#"  "."  "."  "."  
    ##  [4,] "."  "."  "#"  "#"  "."  "."  "."  "."  "."  "#"  
    ##  [5,] "."  "."  "."  "#"  "."  "."  "."  "."  "."  "."  
    ##  [6,] "."  "."  "#"  "."  "#"  "."  "."  "."  "#"  "."  
    ##  [7,] "."  "."  "."  "."  "#"  "#"  "."  "#"  "#"  "#"  
    ##  [8,] "."  "#"  "."  "."  "#"  "#"  "."  "."  "."  "#"  
    ##  [9,] "."  "."  "."  "."  "."  "."  "."  "."  "."  "."  
    ## [10,] "."  "."  "."  "."  "."  "."  "."  "."  "."  "."

### Part 1

``` r
# count trees in forest (recycles forest)

count_trees <- function(forest, start_x, start_y, step_x, step_y){

  # furthest steps - limits of forest one way or another
  
  steps <- max(nrow(forest) %/% step_y, ncol(forest) %/% step_y)

  # vectors of x and y steps
  
  x_locations <- seq(from = start_x, length.out = steps, by = step_x)
  y_locations <- seq(from = start_y, length.out = steps, by = step_y)
  
  # wrap round at edges of forest
  # uses modulo division to wrap back into forest
  
  indices <- cbind((y_locations[1:steps]-1) %% nrow(forest) + 1, (x_locations[1:steps]-1) %% ncol(forest) + 1)
  
  # count trees - using index a matrix by a matrix - see ?`[`
  
  sum(forest[indices] == "#")
}



count_trees(forest, 1, 1, 3, 1)
```

    ## [1] 191

### Part 2

``` r
slopes <- tribble(~step_x, ~step_y,
                  1, 1,
                  3, 1,
                  5, 1,
                  7, 1,
                  1, 2)


slopes %>% 
  
  # just uses the count_trees function for all test cases
  
  mutate(trees = map2_int(step_x, step_y, count_trees, start_x = 1, start_y = 1, forest = forest)) %>% 
  pull(trees) %>% 
  prod()
```

    ## [1] 1478615040
