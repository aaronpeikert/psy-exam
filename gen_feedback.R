library(knitr)
data <- readxl::read_xlsx("data.xlsx", col_types = "text")
fs::dir_create(here::here("feedback"))
ids <- data$id
filenames <- here::here("feedback", paste0(ids, ".pdf"))
purrr::map2(filenames, ids,
            ~rmarkdown::render(input = here::here("feedback.Rmd"),
                               output_format = "pdf_document",
                               output_file = .x,
                               params = list(id = .y)))
