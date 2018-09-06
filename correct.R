#----load-packages----
library(tidyverse)

#----define-general-functions----
postcheck <- function(item, out, postcheck){
  postcheck <- parse_character(flatten_chr(str_split(postcheck, ",")))
  if(!all(is.na(postcheck))&(!all(out %in% postcheck))){
    warning(paste0("Postcheck failed for item: ", item))
    #browser()
  }
}
precheck <- function(item, precheck){
  precheck <- parse_character(flatten_chr(str_split(precheck, ",")))
  if(!all(is.na(precheck))&(!all(pull(data, item) %in% precheck))){
    warning(paste0("Precheck failed for item: ", item))
    #browser()
  }
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

#----calc-percentage----
raw_points <- mutate_all(select(points, one_of(pull(
  filter(codebook, !(type == "meta")), "item"))), parse_double)

final <- mutate(select(points, id), percent = pmap_dbl(raw_points, lift(sum))/maxpoints)

#----grade----
calc_cutpoints <- function(percent, average_function = median){
  if(!all(between(percent, 0, 1)))stop("Some values lies beyond the plausible range!")
  average <- do.call(average_function, list(percent))
  grades <- c(flatten_dbl(map(1:3, ~. + c(0, .3, .7))), 4)
  border <- c(.5, average-.1)
  border <- border[which.min(border)]
  toshare <- (1-border)
  breaks <- c(0, border, .125*toshare + border,seq(.25, .75, by = .25/3)*toshare + border, .95, 1)
  grades <- c(grades, 5) %>% rev()
  out <- list(grades = grades, breaks = breaks)
  return(out)
}

print_cutpoints <- function(cutpoints){
  breaks <- cutpoints$breaks[-length(cutpoints$breaks)]
  grades <- cutpoints$grades
  out <- data.frame(breaks = breaks, grades = grades)
  return(out)
}

grade <- function(percent, cutpoints = calc_cutpoints(percent)){
  breaks <- cutpoints$breaks
  grades <- cutpoints$grades
  out <- cut(percent, breaks = breaks, labels = grades, right = FALSE)
  out <- as.numeric(as.character(out))
  return(out)
}

final <- mutate(final, grade = grade(percent))

#----write-out----
write_csv(final, "grades.csv")
