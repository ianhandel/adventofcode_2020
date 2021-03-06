🎄🎄🎄 day 12 🎄🎄🎄
================

``` r
library(tidyverse)
library(here)

x <- tibble(raw = read_lines(here("day_12", "day12.txt"))) %>%
  extract(raw, c("command", "value"), "(^[A-Z])(\\d+)", convert = TRUE)
```

### Part 1

``` r
# lookup table for effect of commands

steer_lkp <- tribble(
  ~"command", ~"d_x", ~"d_y", ~"d_angle", ~"d_move",
  "N", 0, 1, 0, 0,
  "S", 0, -1, 0, 0,
  "E", 1, 0, 0, 0,
  "W", -1, 0, 0, 0,
  "R", 0, 0, 1, 0,
  "L", 0, 0, -1, 0,
  "F", 0, 0, 0, 1,
)

# execute a command

steer <- function(ship, command) {
  ship$x <- ship$x + command$value * (command$d_x + command$d_move * sinpi(ship$facing / 180))
  ship$y <- ship$y + command$value * (command$d_y + command$d_move * cospi(ship$facing / 180))
  ship$facing <- (ship$facing + command$d_angle * command$value) %% 360
  ship
}

# tidy up commands

commands <- x %>%
  left_join(steer_lkp, by = "command") %>%
  select(-command) %>%
  transpose()

# initial state
ship <- list(x = 0, y = 0, facing = (90))

# sail course
for (ii in seq_along(commands)) {
  ship <- steer(ship, command = commands[[ii]])
}

# manhattan distance
abs(ship$x) + abs(ship$y)
```

    ## [1] 445

### Part 2

Very mechanistic solution.

Must rewrite more elegantly\!\!\!

``` r
# execute a command

execute <- function(state, command) {
  if (command$command == "N") {
    state$wy <- state$wy + command$value
  }
  if (command$command == "S") {
    state$wy <- state$wy - command$value
  }
  if (command$command == "E") {
    state$wx <- state$wx + command$value
  }
  if (command$command == "W") {
    state$wx <- state$wx - command$value
  }

  if (command$command == "F") {
    state$x <- state$x + state$wx * command$value
    state$y <- state$y + state$wy * command$value
  }

  if (command$command == "R" & command$value == 90) {
    temp <- state$wx
    state$wx <- state$wy
    state$wy <- -temp
  }

  if (command$command == "R" & command$value == 180) {
    state$wx <- -state$wx
    state$wy <- -state$wy
  }

  if (command$command == "R" & command$value == 270) {
    temp <- state$wx
    state$wx <- -state$wy
    state$wy <- temp
  }

  if (command$command == "L" & command$value == 270) {
    temp <- state$wx
    state$wx <- state$wy
    state$wy <- -temp
  }

  if (command$command == "L" & command$value == 180) {
    state$wx <- -state$wx
    state$wy <- -state$wy
  }

  if (command$command == "L" & command$value == 90) {
    temp <- state$wx
    state$wx <- -state$wy
    state$wy <- temp
  }

  state
}

# tidy up commands

commands <- x %>%
  transpose()

# initial state
state <- list(x = 0, y = 0, wx = 10, wy = 1)

# sail course
for (ii in seq_along(commands)) {
  state <- execute(state, command = commands[[ii]])
}

# manhattan distance
abs(state$x) + abs(state$y)
```

    ## [1] 42495
