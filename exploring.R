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

# Оцениваем периодичность pCut::Actual_speed стандартными методами --------


ts_test <- 
  df_tidy %>% 
  filter(
    filenum == "000", 
    variable == "pCut::CTRL_Position_controller::Actual_speed"
  ) %>% 
  select(value) %>% 
  unlist(use.names = F) %>% 
  ts(start = 0.008, frequency = 250)

ts_test %>% 
  spectrum(spans = 5)

ts_test %>% 
  stl(s.window = "per") %>% 
  plot()

ts_test %>% 
  stats::filter(rep(1/652, 652)) %>% 
  plot()

# Отбираем VAX_speed с наименьшим sd --------------------------------------


df_filtred <- 
  df_tidy %>% 
  group_by(mode, filenum, variable) %>% 
  filter(
    variable == "pSpintor::VAX_speed"
  ) %>% 
  summarise(sd = sd(value)) %>% 
  filter(sd < 0.00001)

df_filtred %>% 
  summary()
