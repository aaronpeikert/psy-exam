#----load-packages----
library(tidyverse)

#----define-general-functions----
postcheck <- function(item, out, postcheck){
  postcheck <- flatten_chr(str_split(postcheck, ","))
  if(!all(is.na(postcheck))&(!all(out %in% postcheck)))warning(paste0("Postcheck failed for item: ", item))
}
precheck <- function(item, precheck){
  precheck <- flatten_chr(str_split(precheck, ","))
  if(!all(is.na(precheck))&(!all(pull(data, item) %in% precheck)))warning(paste0("Precheck failed for item: ", item))
}
#----read-data----
if(!file.exists("codebook.xlsx"))stop(paste0("There is no codebook.xlsx in ", getwd()))
if(!file.exists("data.xlsx"))stop(paste0("There is no data.xlsx in ", getwd()))

data <- readxl::read_xlsx("data.xlsx", col_types = "text")
codebook <- readxl::read_xlsx("codebook.xlsx", col_types = "text")

#----check-files----
if(names(data)[1] != "id")stop("The first column in data.xlsx should be 'id'")
if(!all(names(codebook) %in% c("item", "answer", "type", "points", "precheck", "postcheck")))stop("You have not supplied the right columns in codebook.xlsx")
if(!all(names(data) %in% pull(codebook, "item")))stop("Itemnames in data.xlsx don't match itemnames in codebook.xlsx, therefore there are some items in the data, which are not in the codebook.")
if(!all(pull(codebook, "item") %in% names(data)))stop("Itemnames in codebook.xlsx don't match itemnames in data.xlsx")

#----define-code-functions----
code_meta <- function(item, answer, points, precheck, postcheck){
  out <- pull(data, item)
  return(out)
}

code_single <- function(item, answer, points, precheck, postcheck){
  precheck(item, precheck)
  out <- ifelse(pull(data, item) == answer, points, 0)
  postcheck(item, out, postcheck)
  return(out)
}

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
points <- as.tibble(points)
