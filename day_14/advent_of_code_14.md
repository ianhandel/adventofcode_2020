🎄🎄🎄 day 14 🎄🎄🎄
================

``` r
library(tidyverse)
library(here)
library(binaryLogic)

x <- tibble(raw = read_lines(here("day_14", "day14.txt"))) %>%
  extract(raw,
    c("inst", "mem", "input"),
    regex = "^(mask|mem)\\[*(\\d*)\\]* = ([X|\\d]+)",
    convert = TRUE
  )
```

### Part 1

``` r
# make sparse memmory dataframe
memory <- x %>%
  filter(inst == "mem") %>%
  select(mem) %>%
  mutate(value = NA) %>%
  distinct()

apply_mask <- function(mask, value) {
  do_logic <- function(v, m) {
    case_when(
      m == "X" ~ v,
      m == "0" ~ FALSE,
      m == "1" ~ TRUE
    )
  }

  mask <- str_split(mask, "")[[1]]

  value <- as.binary(value, signed = FALSE) %>% {
    c(rep(FALSE, 36 - length(.)), .)
  }

  map2_lgl(value, mask, do_logic) %>%
    `*`(2^(35:0)) %>%
    sum()
}

for (ii in 1:nrow(x)) {
  if (x$inst[ii] == "mask") {
    mask <- x$input[ii]
  }
  if (x$inst[ii] == "mem") {
    memory$value[memory$mem == x$mem[ii]] <- apply_mask(mask, x$input[ii])
  }
}

sum(memory$value) %>%
  format(digits = 20)
```

    ## [1] "9967721333886"
