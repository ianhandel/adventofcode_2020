🎄🎄🎄 day 07 🎄🎄🎄
================

``` r
library(tidyverse)
library(here)
library(data.tree)
```

``` r
bags <- read_lines(here("day_07", "day07.txt")) %>% 
  as_tibble() %>% 
  mutate(value = str_replace_all(value, "bags", "bag")) %>% 
  extract(value, c("outer", "inner"), regex = "(\\w+ \\w+ \\w+) contain (.+)\\.") %>% 
  separate_rows(inner, sep = ", ") %>%
  separate(inner, c("number", "inner"), " ", extra = "merge") %>% 
  filter(number != "no")

bags
```

    ## # A tibble: 1,458 x 3
    ##    outer              number inner             
    ##    <chr>              <chr>  <chr>             
    ##  1 muted lavender bag 5      dull brown bag    
    ##  2 muted lavender bag 4      pale maroon bag   
    ##  3 muted lavender bag 2      drab orange bag   
    ##  4 plaid aqua bag     1      posh violet bag   
    ##  5 plaid aqua bag     5      pale yellow bag   
    ##  6 plaid aqua bag     4      bright salmon bag 
    ##  7 wavy lime bag      3      vibrant indigo bag
    ##  8 wavy lime bag      1      posh gray bag     
    ##  9 pale coral bag     5      mirrored olive bag
    ## 10 pale coral bag     2      posh salmon bag   
    ## # … with 1,448 more rows

### Part 1

``` r
find_outers <- function(inners){
  
  # find bags that can wrap the inners
  # - add in inners with c(inners)
  # - remove duplicates
  
  containing_bags <- bags %>% 
    inner_join(tibble(inner = inners), by = "inner") %>% 
    pull(outer) %>% 
    c(inners) %>% 
    unique()
  
  # if no new onws stop otherwise recurse to find more wrapping bags
  
  if(length(containing_bags) == length(inners)){
    return(inners)
  }else{
    find_outers(containing_bags)
  }
}


# Need to subtract one as method includes oringinal!

find_outers("shiny gold bag") %>% 
  length() %>% 
  `-`(1)
```

    ## [1] 248
