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

valid <- pass %>% 
  group_by(id) %>%
  select(id, key) %>% 
  
  # sometimes keys repeat
  
  distinct() %>% 
  
  # count valid keys!
  
  summarise(valid = sum(key %in% keys_needed) == length(keys_needed)) %>% 
  filter(valid)

valid
```

    ## # A tibble: 242 x 2
    ##       id valid
    ##    <int> <lgl>
    ##  1     0 TRUE 
    ##  2     1 TRUE 
    ##  3     2 TRUE 
    ##  4     4 TRUE 
    ##  5     5 TRUE 
    ##  6     6 TRUE 
    ##  7     7 TRUE 
    ##  8     8 TRUE 
    ##  9     9 TRUE 
    ## 10    11 TRUE 
    ## # … with 232 more rows

### Part 2

Now also require…

  - byr (Birth Year) - four digits; at least 1920 and at most 2002.
  - iyr (Issue Year) - four digits; at least 2010 and at most 2020.
  - eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
  - hgt (Height) - a number followed by either cm or in:
      - If cm, the number must be at least 150 and at most 193.
      - If in, the number must be at least 59 and at most 76.
  - hcl (Hair Color) - a \# followed by exactly six characters 0-9 or
    a-f.
  - ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
  - pid (Passport ID) - a nine-digit number, including leading zeroes.
  - cid (Country ID) - ignored, missing or not.

<!-- end list -->

``` r
pass %>% 
  
  
  # filter as above
  
  inner_join(valid, by = "id") %>% 


  # make columns as easier to manage
  
  pivot_wider(names_from = key, values_from = value) %>% 
  
  # rules one by one
  
  mutate(byr_valid = if_else(str_detect(byr, "^\\d{4}$") &
                         between(parse_integer(byr), 1920, 2002),
                       TRUE,
                       FALSE),
         
         iyr_valid = if_else(str_detect(iyr, "^\\d{4}$") &
                         between(parse_integer(iyr), 2010, 2020),
                       TRUE,
                       FALSE),
         
         eyr_valid = if_else(str_detect(eyr, "^\\d{4}$") &
                         between(parse_integer(eyr), 2020, 2030),
                       TRUE,
                       FALSE),
         
         # yeah yeah - should've written a function for above bit
         
         hgt_valid = case_when(!str_detect(hgt, "^\\d+(in|cm)") ~ FALSE,
                               str_detect(hgt, "cm") & !between(parse_number(hgt), 150, 193) ~ FALSE,
                               str_detect(hgt, "in") & !between(parse_number(hgt),  59, 76)  ~ FALSE,
                         TRUE ~ TRUE),
         
         hcl_valid = str_detect(hcl, "^#([0-9]|[a-f]){6}$"),
         
         ecl_valid = str_detect(ecl, "^(amb|blu|brn|gry|grn|hzl|oth)"),
         
         pid_valid = str_detect(pid, "^\\d{9}$")) %>%
  
  filter(across(contains("_valid")))
```

    ## # A tibble: 186 x 17
    ##       id valid byr   eyr   hgt   pid   hcl   iyr   ecl   cid   byr_valid
    ##    <int> <lgl> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <lgl>    
    ##  1     1 TRUE  1977  2027  161cm 9104… #a97… 2011  grn   <NA>  TRUE     
    ##  2     2 TRUE  1941  2029  186cm 1089… #cfa… 2012  gry   257   TRUE     
    ##  3     4 TRUE  1966  2020  151cm 9477… #ceb… 2012  grn   105   TRUE     
    ##  4     5 TRUE  1966  2027  155cm 8536… #888… 2012  hzl   <NA>  TRUE     
    ##  5     8 TRUE  1923  2025  191cm 5741… #a97… 2019  gry   <NA>  TRUE     
    ##  6    11 TRUE  1955  2029  188cm 7401… #a97… 2020  oth   309   TRUE     
    ##  7    12 TRUE  1974  2026  151cm 3941… #cfa… 2016  grn   <NA>  TRUE     
    ##  8    13 TRUE  1956  2029  188cm 2265… #efc… 2014  blu   272   TRUE     
    ##  9    16 TRUE  1997  2029  193cm 1417… #cfa… 2019  hzl   83    TRUE     
    ## 10    17 TRUE  1985  2022  165cm 6815… #6b5… 2019  brn   <NA>  TRUE     
    ## # … with 176 more rows, and 6 more variables: iyr_valid <lgl>, eyr_valid <lgl>,
    ## #   hgt_valid <lgl>, hcl_valid <lgl>, ecl_valid <lgl>, pid_valid <lgl>
