🎄🎄🎄 day 06 🎄🎄🎄
================

Load in data and make into dataframe of responses split into characters.

  - id = group id
  - data = dataframe with…
      - ans = original response
      - split = character vector of response

E.g.

    > forms[1, ]
    # A tibble: 1 x 2
         id               data
      <int> <list<tbl_df[,2]>>
    1     0            [2 × 2]
    
    > forms[1, ]$data
    # A tibble: 2 x 2
      ans              split     
      <chr>            <list>    
    1 jmqnkzlsfedaptx  <chr [15]>
    2 usjfkadqwmeyilph <chr [16]>

``` r
library(tidyverse)
library(here)

forms <- read_csv(here("day_06", "day_06.txt"),
                 col_names = "ans",
                 skip_empty_rows = FALSE) %>%
  
  # make an id row by counting blank rows
  
  mutate(id = cumsum(is.na(ans))) %>% 
  filter(!is.na(ans)) %>% 
  
  # separate response strings into a vector and nest into dataframes
  
  mutate(split = map(ans, ~unlist(str_split(.x, "")))) %>% 
  
  group_nest(id)
```

Function to apply a set operation to all responses for a group then
count for each group and return grand total.

``` r
apply_set_operation <- function(df, fun){
  df %>% 
  mutate(x = map(data, ~as.vector(reduce(.x$split, fun)))) %>% 
  mutate(length = map_int(x, length)) %>% 
  summarise(total = sum(length))
}
```

### Part 1

``` r
apply_set_operation(forms, union)
```

    ## # A tibble: 1 x 1
    ##   total
    ##   <int>
    ## 1  6437

### Part 2

``` r
apply_set_operation(forms, intersect)
```

    ## # A tibble: 1 x 1
    ##   total
    ##   <int>
    ## 1  3229
