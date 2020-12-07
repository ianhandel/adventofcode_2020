🎄🎄🎄 day 07 🎄🎄🎄
================

``` r
library(tidyverse)
library(here)
library(data.tree)
```

``` r
bags <- read_lines(here("day_07", "test.txt")) %>% 
  as_tibble() %>% 
  extract(value, c("outer", "inner"), regex = "(\\w+ \\w+ \\w+) contain (.+)\\.") %>% 
  separate_rows(inner, sep = ", ") %>%
  separate(inner, c("number", "inner"), " ", extra = "merge") %>% 
  filter(number != "no")
```

Try using data.tree

``` r
bags$pathString <- paste("world", 
                            bags$outer, 
                            bags$inner, 
                            sep = "/")

rules <- as.Node(bags)
print(rules, "number", limit = 20)
```

    ##                       levelName number
    ## 1  world                              
    ## 2   ¦--light red bags                 
    ## 3   ¦   ¦--bright white bag          1
    ## 4   ¦   °--muted yellow bags         2
    ## 5   ¦--dark orange bags               
    ## 6   ¦   ¦--bright white bags         3
    ## 7   ¦   °--muted yellow bags         4
    ## 8   ¦--bright white bags              
    ## 9   ¦   °--shiny gold bag            1
    ## 10  ¦--muted yellow bags              
    ## 11  ¦   ¦--shiny gold bags           2
    ## 12  ¦   °--faded blue bags           9
    ## 13  ¦--shiny gold bags                
    ## 14  ¦   ¦--dark olive bag            1
    ## 15  ¦   °--vibrant plum bags         2
    ## 16  ¦--dark olive bags                
    ## 17  ¦   ¦--faded blue bags           3
    ## 18  ¦   °--dotted black bags         4
    ## 19  °--vibrant plum bags              
    ## 20      °--... 2 nodes w/ 0 sub
