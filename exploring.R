library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(purrr)

# Читаем исходные данные ---------------------------------------------


df_data <- 
  map_df(
    list.files(path = "data", pattern = "_mode.\\.csv", full.names = T), 
    function(filename) {
      # dt <- data.table::fread(filename)
      dt <- read_csv(filename)
      dt$filenum <- str_sub(filename, 19, 21)
      dt$mode <- str_sub(filename, 23, 27)
      return(dt)
    }) %>% 
  # as.data.frame() %>% 
  # as_tibble() %>% 
  mutate_at(vars("filenum", "mode"), as.factor)

colnames(df_data)

df_tidy <- 
  df_data %>% 
  gather("variable", "value", -mode, -filenum, -timestamp, factor_key = T)
