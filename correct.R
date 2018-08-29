#----load-packages----
library(tidyverse)

#----define-general-functions----
postcheck <- function(item, out, postcheck){
  postcheck <- parse_character(flatten_chr(str_split(postcheck, ",")))
  if(!all(is.na(postcheck))&(!all(out %in% postcheck)))warning(paste0("Postcheck failed for item: ", item))
}
precheck <- function(item, precheck){
  precheck <- parse_character(flatten_chr(str_split(precheck, ",")))
  if(!all(is.na(precheck))&(!all(pull(data, item) %in% precheck)))warning(paste0("Precheck failed for item: ", item))
}
#----read-data----
if(!file.exists("codebook.xlsx"))stop(paste0("There is no codebook.xlsx in ", getwd()))
if(!file.exists("data.xlsx"))stop(paste0("There is no data.xlsx in ", getwd()))

data <- readxl::read_xlsx("data.xlsx", col_types = "text")
codebook <- readxl::read_xlsx("codebook.xlsx", col_types = "text")

#----check-files----
if(names(data)[1] != "id")stop("The first column in data.xlsx should be 'id'")
if(!all(names(codebook) %in% c("item", "answer", "type", "points", "precheck", "postcheck", "status")))stop("You have not supplied the right columns in codebook.xlsx")
if(!all(names(data) %in% pull(codebook, "item")))stop("Itemnames in data.xlsx don't match itemnames in codebook.xlsx, therefore there are some items in the data, which are not in the codebook.")
if(!all(pull(codebook, "item") %in% names(data)))stop("Itemnames in codebook.xlsx don't match itemnames in data.xlsx")

#----define-code-functions----
code_meta <- function(item, answer, points, precheck, postcheck, status){
  out <- pull(data, item)
  return(out)
}

code_single <- function(item, answer, points, precheck, postcheck, status){
  precheck(item, precheck)
  out <- ifelse(pull(data, item) == answer, points, 0)
  out[is.na(out)] <- 0
  if(anyNA(out))browser()
  postcheck(item, out, postcheck)
  return(out)
}

#----extract-numbers----

not_numericish <- function(x){
  numericish_ <- function(x){
    if(length(x) != 1)stop("This function checks only single elemts")
    if(is.na(x))return(TRUE)
    if(is.na(suppressWarnings(as.numeric(x))))return(TRUE)
    else return(FALSE)
  }
  out <- map_lgl(x, numericish_)
  return(out)
}

numbers <- data
numbers[as.matrix(mutate_all(data, not_numericish))] <- NA
numbers <- as.data.frame(numbers, stringsAsFactors = FALSE)

#----apply-code-functions----
code <- function(...){
  args <- list(...)
  func <- paste0("code_", args[["type"]])
  args[["type"]] <- NULL
  out <- do.call(func, args)
  return(out)
}

points <- pmap(codebook, code)
names(points) <- pull(codebook, "item")
points <- as.data.frame(points, stringsAsFactors = FALSE)

#----overwrite-numbers----
points[!is.na(numbers)] <- numbers[!is.na(numbers)]

#----handle-item-status---
if(!all(na.omit(pull(codebook, "status")) %in% c("included", "excluded", "optional")))warning("Some items have a unnknown status.")
codebook <- mutate(codebook, status = ifelse(is.na(status), "included", status))
excluded <- pull(codebook, "item")[map_lgl(pull(codebook, "status") == "excluded", isTRUE)]
points <- select(points, -one_of(excluded))
codebook <- filter(codebook, !(item %in% excluded))

#----calc-maxpoints----
maxpoints <- pull(
  filter(codebook, (status == "included"), !(type == "meta")),
  "points")
# meaning; only include items that are included but not meta items
maxpoints <- sum(as.numeric(maxpoints))
