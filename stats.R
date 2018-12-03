source(here::here("correct.R"))

#----selectivity----
selectivity <- function(data, ...){
  data <- data[tidyselect::vars_select(names(data), ...)]
  data <- data[!map_lgl(data, ~all(is.na(.)))]
  data <- data[!is.na(apply(data, 1, possibly(sd, NA), na.rm = TRUE)), ]
  sum <- rowSums(data, na.rm = TRUE)
  suppressWarnings(map(data, cor.test, sum)) %>%
    map_df(broom::glance) %>%
    select(conf.low, estimate, conf.high) %>%
    cbind(item = names(data), .)
}

#----difficulty----

difficulty <- function(data, ...){
  data <- data[tidyselect::vars_select(names(data), ...)]
  data <- data[!map_lgl(data, ~all(is.na(.)))]
  data.frame(item = names(data),
             difficulty = map_dbl(data, mean, na.rm = TRUE),
             cases = map_dbl(data, ~sum(!is.na(.))))
}