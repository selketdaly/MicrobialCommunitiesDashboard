library("readxl")
library("dplyr")
get_data <- function(filepath) {
  my_data <- read_excel(filepath)
  return (my_data)
}

