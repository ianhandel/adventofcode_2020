🎄🎄🎄 day 16 🎄🎄🎄
================

``` r
library(tidyverse)
library(here)
```

``` r
x <- read_lines(here("day_16", "day16.txt"))

rules <- x %>%
  keep(str_detect, " or ") %>% 
  as_tibble() %>% 
  extract(value, c("name", "rule"), regex = "^([a-z ]+):(.+)", convert = TRUE) %>% 
  separate_rows(rule, sep = " or ") %>% 
  separate(rule, c("from", "to"), "-", convert = TRUE)

ticket <- x %>% 
  pluck(., purrr::detect_index(., str_detect, "your") + 1) %>% 
  as_tibble() %>% 
  separate_rows(value, convert = TRUE) %>% 
  mutate(value_number = 1:n())

nearby <- x %>% 
  `[`(., (purrr::detect_index(., str_detect, "nearby") + 1) : length(.)) %>%
  as_tibble() %>% 
  mutate(ticket = 1:n()) %>%
  separate_rows(value, convert = TRUE) %>% 
  group_by(ticket) %>% 
  mutate(value_number = 1:n()) %>% 
  ungroup()
```

### Part 1

``` r
nearby <- nearby %>% 
  crossing(rules) %>% 
  mutate(valid = value >= from & value <= to) %>% 
  group_by(ticket, value_number, value) %>% 
  summarise(valid = any(valid)) %>% 
  ungroup()

nearby %>% 
  filter(!valid) %>% 
  summarise(sum(value))
```

    ## # A tibble: 1 x 1
    ##   `sum(value)`
    ##          <int>
    ## 1        26026

### Part 2
