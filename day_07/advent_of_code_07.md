🎄🎄🎄 day 07 🎄🎄🎄
================

``` r
library(tidyverse)
library(here)
```

``` r
bags <- read_lines(here("day_07", "day07.txt")) %>% 
  as_tibble() %>% 
  mutate(value = str_replace_all(value, "bags", "bag")) %>% 
  extract(value, c("outer", "inner"), regex = "(\\w+ \\w+ \\w+) contain (.+)\\.") %>% 
  separate_rows(inner, sep = ", ") %>%
  separate(inner, c("number", "inner"), " ", extra = "merge") %>% 
  filter(number != "no") %>% 
  mutate(number = parse_integer(number))

bags
```

    ## # A tibble: 1,458 x 3
    ##    outer              number inner             
    ##    <chr>               <int> <chr>             
    ##  1 muted lavender bag      5 dull brown bag    
    ##  2 muted lavender bag      4 pale maroon bag   
    ##  3 muted lavender bag      2 drab orange bag   
    ##  4 plaid aqua bag          1 posh violet bag   
    ##  5 plaid aqua bag          5 pale yellow bag   
    ##  6 plaid aqua bag          4 bright salmon bag 
    ##  7 wavy lime bag           3 vibrant indigo bag
    ##  8 wavy lime bag           1 posh gray bag     
    ##  9 pale coral bag          5 mirrored olive bag
    ## 10 pale coral bag          2 posh salmon bag   
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
  
  # if no new ones stop otherwise recurse to find more wrapping bags
  
  if(length(containing_bags) == length(inners)){
    return(inners)
  }else{
    find_outers(containing_bags)
  }
}


# Need to subtract one as method includes oringinal shiny gold bag!

find_outers("shiny gold bag") %>% 
  length() %>% 
  `-`(1)
```

    ## [1] 248

### Part 2

This is really ungly and dataframe based as I only think in rectangular
data. Probably much better with lists/tree structure. Same true of
above.

``` r
# get a dataframe of inner bags - multiply outers by inner numbers

find_inners <- function(bag_list){
  bag_list %>% 
    left_join(bags, by = c("inner" = "outer")) %>% 
    mutate(number = number.x * number.y) %>% 
    select(-outer, -number.x, -number.y) %>% 
    rename(outer = inner, inner = inner.y) %>% 
    filter(!is.na(number))
}


# start with outer bag
outer_bags <- bags %>% 
  filter(outer == "shiny gold bag")

bag_list <- outer_bags

# add inners until no more to add
while(nrow(inner_bags <- find_inners(outer_bags)) > 0)
  {
  # build final list
  bag_list <- bag_list %>% 
    bind_rows(inner_bags)
  
  # new outer is old inner list!
  outer_bags <- inner_bags
  }

#  result
sum(bag_list$number)
```

    ## [1] 57281
