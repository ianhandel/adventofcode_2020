🎄🎄🎄 day 04 🎄🎄🎄
================

``` r
library(tidyverse)
library(here)
```

``` r
pass <- read_csv(here("day_04", "day_4.txt"),
                 col_names = "raw",
                 skip_empty_rows = FALSE) %>%
  
  # make an id row by counting blank rows
  
  mutate(id = cumsum(is.na(raw))) %>% 
  filter(!is.na(raw)) %>% 
  
  # split pairs and make into single rows per pair
  
  mutate(raw = str_split(raw, " ")) %>% 
  unnest(raw) %>% 
  
  # separate key:value pairs
  
  separate(raw, into = c("key", "value"), sep = ":")
  
  

pass
```

    ## # A tibble: 2,082 x 3
    ##    key   value     id
    ##    <chr> <chr>  <int>
    ##  1 byr   1971       0
    ##  2 eyr   2039       0
    ##  3 hgt   172in      0
    ##  4 pid   170cm      0
    ##  5 hcl   17106b     0
    ##  6 iyr   2012       0
    ##  7 ecl   gry        0
    ##  8 cid   339        0
    ##  9 hgt   161cm      1
    ## 10 eyr   2027       1
    ## # … with 2,072 more rows

### Part 1

Need to have…

  - byr (Birth Year)
  - iyr (Issue Year)
  - eyr (Expiration Year)
  - hgt (Height)
  - hcl (Hair Color)
  - ecl (Eye Color)
  - pid (Passport ID)
  - cid (Country ID) - optional

<!-- end list -->

``` r
keys_needed <- c("byr", "iyr", "eyr", "hgt",
                 "hcl", "ecl", "pid")

pass %>% 
  group_by(id) %>%
  select(id, key) %>% 
  
  # sometimes keys repeat
  
  distinct() %>% 
  
  # count valid keys!
  
  summarise(count = sum(key %in% keys_needed)) %>% 
  filter(count == length(keys_needed))
```

    ## # A tibble: 242 x 2
    ##       id count
    ##    <int> <int>
    ##  1     0     7
    ##  2     1     7
    ##  3     2     7
    ##  4     4     7
    ##  5     5     7
    ##  6     6     7
    ##  7     7     7
    ##  8     8     7
    ##  9     9     7
    ## 10    11     7
    ## # … with 232 more rows
