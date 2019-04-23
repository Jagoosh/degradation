library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(purrr)
library(broom)

# Читаем исходные данные --------------------------------------------------


df_data <- 
  map_df(
    list.files(path = "data", pattern = "_mode.\\.csv", full.names = T), 
    function(filename) {
      # dt <- data.table::fread(filename)
      dt <- read_csv(
        filename, 
        col_types = c(
          timestamp = col_double(),
          `pCut::Motor_Torque` = col_double(),
          `pCut::CTRL_Position_controller::Lag_error` = col_double(),
          `pCut::CTRL_Position_controller::Actual_position` = col_integer(),
          `pCut::CTRL_Position_controller::Actual_speed` = col_double(),
          `pSvolFilm::CTRL_Position_controller::Actual_position` = col_integer(),
          `pSvolFilm::CTRL_Position_controller::Actual_speed` = col_double(),
          `pSvolFilm::CTRL_Position_controller::Lag_error` = col_double(),
          `pSpintor::VAX_speed` = col_double()
        )
      )
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

# Оцениваем период и фазу колебаний pCut::Actual_speed --------------------


get_period <- 
  function(timestamp, variable) {
    variable <- unlist(variable, use.names = F) %>% 
      ts(start = min(timestamp), frequency = median(timestamp - lag(timestamp), na.rm = T))
    value <- 
      tibble(
        pos = order(variable)[1:99] %>% sort(),
        step = pos - lag(pos, default = first(pos)),
        peak = cumsum(step > 10)
      ) %>%
      group_by(peak) %>%
      summarise(pos = median(pos)) %>%
      transmute(period = round(pos - lag(pos), 0)) %>%
      unlist(use.names = F) %>%
      mean(na.rm = T) %>% 
      round(0) %>% 
      as.integer()
    return(value)
  }

get_phase <- 
  function(timestamp, variable) {
    variable <- unlist(variable, use.names = F) %>% 
      ts(start = min(timestamp), frequency = median(timestamp - lag(timestamp), na.rm = T))
    tmp <- 
      tibble(
        pos = order(variable)[1:99] %>% sort(),
        step = pos - lag(pos, default = first(pos)),
        peak = cumsum(step > 10)
      ) %>%
      group_by(peak) %>%
      summarise(pos = median(pos))
    return(min(tmp$pos) %>% round(0) %>% as.integer())
  }

df_oscillation <- 
  df_tidy %>% 
  filter(
    filenum %in% df_filtred$filenum, 
    variable == "pCut::CTRL_Position_controller::Actual_speed"
  ) %>% 
  group_by(filenum) %>% 
  summarise(
    ts_start = min(timestamp), 
    ts_period = median(timestamp - lag(timestamp), na.rm = T), 
    phase = get_phase(timestamp, value),
    period = get_period(timestamp, value)
  )

# Определение параметров колебаний через lm() -----------------------------


colnames(df_tidy)

df_nest <- 
  df_tidy %>% 
  group_by(filenum, mode, variable) %>% 
  nest()

df_model <- 
  df_nest %>% 
  mutate(
    model = data %>% 
      map(function(df) {lm(value ~ timestamp, data = df)}), 
    augment = model %>% 
      map(augment)
  )

df_augment <- 
  df_model %>% 
  select(filenum, mode, variable, augment) %>% 
  unnest()
