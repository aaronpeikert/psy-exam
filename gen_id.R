item <- "Das ist ein Test für ein Item!"

hash <- function(string){
  hash <- digest::digest(string, serialize = FALSE, algo = "md5") # hash with md5
  stringr::str_sub(hash, 1, 8) # use the first 8 characters
}

prepare <- function(item){
  if(length(item) != 1)stop("Only one item at the time!")
  #browser()
  item <- stringr::str_to_lower(item)
  words <- stringi::stri_extract_all_words(item)
  first <- words[[1]]#[1:5] it was an idea to only use the first words
  # but items beginnings are to similar
  first <- stringr::str_extract_all(first, "\\p{L}") # keep only letters
  first <- first[!map_lgl(first, ~length(.)==0)] # remove zero length entries
  first <- purrr::map_chr(first, stringr::str_flatten) # paste lowest level
  first <- stringr::str_flatten(first) #paste higher level
  first
}

gen_id <- function(item){
  item <- prepare(item)
  hash(item)
}

library(tidyverse)
codebook <- read_csv("codebook.csv")
codebook <- mutate(codebook, itemid = map_chr(item, gen_id))
write_csv(codebook, here::here("codebook.csv"), na = "")
