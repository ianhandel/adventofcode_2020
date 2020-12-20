🎄🎄🎄 day 19 🎄🎄🎄
================

``` r
library(tidyverse)
library(here)

# splitting code from @drob day 16
x <- read_lines(here("day_19", "day19.txt")) %>% 
  as_tibble() %>% 
  mutate(group = c("rules", "message")[cumsum(value == "") + 1]) %>% 
  filter(value != "") %>% 
  nest(cols = value) %>% 
  spread(group, cols) %>% 
  map(1)
```

### Part 1

``` r
rules <- x$rules %>%
  separate(value, c("id", "rule"), sep = ": ") %>%
  mutate(rule = map_chr(rule, ~ str_remove_all(.x, "\"")))

find_rule <- function(rule_id) {
  if (str_trim(rule_id) %in% c("a", "b", "|", "(", ")")) {
    return(rule_id)
  }
  new <- rules %>%
    filter(id == rule_id) %>%
    pull(rule)
  str_glue("({new})")
}

rule <- find_rule(0)

while (str_detect(rule, "[0-9]")) {
  rule <- str_replace_all(rule,
    "\\d+",
    replacement = find_rule
  )
}

final_rule <- str_remove_all(rule, " ") 
final_rule <- str_glue("^{final_rule}$")

x$message %>%
  mutate(OK = str_detect(value, final_rule)) %>% 
  count(OK)
```

    ## # A tibble: 2 x 2
    ##   OK        n
    ##   <lgl> <int>
    ## 1 FALSE   245
    ## 2 TRUE    198

### Part 2

Working on this!

``` r
# 8: 42 | 42 8
# 11: 42 31 | 42 11 31

rules <- rules %>%
  mutate(rule = case_when(id == "8" ~ "42 | 42 42 | 42 42 42 | 42 42 42 42 | 42 42 42 42 42",
                          id == "11" ~ "42 31 | 42 42 31 31 | 42 42 42 31 31 31 | 42 42 42 42 31 31 31 31",
                          TRUE ~ rule))

find_rule <- function(rule_id) {
  if (str_trim(rule_id) %in% c("a", "b", "|", "(", ")")) {
    return(rule_id)
  }
  new <- rules %>%
    filter(id == rule_id) %>%
    pull(rule)
  str_glue("({new})")
}

rule <- find_rule(0)

while (str_detect(rule, "[0-9]")) {
  rule <- str_replace_all(rule,
    "\\d+",
    replacement = find_rule
  )
}

final_rule <- str_remove_all(rule, " ") 
final_rule <- str_glue("^{final_rule}$")
final_rule <- str_replace_all(final_rule, "\\(a\\)", "a")
final_rule <- str_replace_all(final_rule, "\\(b\\)", "b")

x$message %>%
  mutate(OK = str_detect(value, final_rule)) %>% 
  count(OK)
```

    ## # A tibble: 2 x 2
    ##   OK        n
    ##   <lgl> <int>
    ## 1 FALSE    71
    ## 2 TRUE    372
