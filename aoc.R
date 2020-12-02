aoc <- function(day){
  
  day <- stringr::str_pad(day, width = 2, pad = "0", side = "left")
  
  text <- stringr::str_glue("---\ntitle: \"🎄🎄🎄 day {day}  🎄🎄🎄\"\noutput: github_document\n---\n```{{r , include = FALSE}}\nknitr::opts_chunk$set(warning = FALSE, message = FALSE)\n```")
  
  folder <- stringr::str_glue("day_{day}")
  if(fs::dir_exists(folder)) stop("folder exits")
  
  fs::dir_create(stringr::str_glue("day_{day}"))
  
  readr::write_lines(text, path = fs::path(folder, stringr::str_glue("advent_of_code_{day}.Rmd")))
}
