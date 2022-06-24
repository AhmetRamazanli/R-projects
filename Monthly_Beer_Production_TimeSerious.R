# Import libraries & dataset ----
library(tidyverse)
library(data.table)
library(lubridate)
library(skimr)
library(timetk)
library(highcharter)
library(h2o)
library(tidymodels)
library(modeltime)
library(rstudioapi)
library(zoo)
path <- dirname(getSourceEditorContext()$path)
setwd(path)

data <- fread("monthly-beer-production.csv")
colnames(data) <- c('Date','Count')

data %>% glimpse()

data$Date <- data$Date %>% as.yearmon()
data$Date <- data$Date %>% as.Date()

# timetk package ----
data %>% 
  plot_time_series(
    Date, Count, 
     .color_var = lubridate::month(Date),
     .color_lab = "Month",
    .interactive = T,
    .plotly_slider = T,
    .smooth = F)

# Seasonality plots
data %>%
  plot_seasonal_diagnostics(
    Date, Count, .interactive = T)

all_time_arg <- data %>% tk_augment_timeseries_signature()

all_time_arg %>% skim()

df <- all_time_arg %>%
  select(-contains("hour"),
         -contains("day"),
         -contains("week"),
         -minute,-second,-am.pm) %>% 
  mutate_if(is.ordered, as.character) %>% 
  mutate_if(is.character,as_factor)
# ------------------------------------ H2O ------------------------------------
h2o.init()    

train_h2o <- df %>% filter(year < 1988) %>% as.h2o()
test_h2o <- df %>% filter(year >= 1988) %>% as.h2o()

y <- "Count"
x <- df %>% select(-Count) %>% names()

model_h2o <- h2o.automl(
  x = x, y = y, 
  training_frame = train_h2o, 
  validation_frame = test_h2o,
  leaderboard_frame = test_h2o,
  stopping_metric = "RMSE",
  seed = 123, nfolds = 10,
  exclude_algos = "GLM",
  max_runtime_secs = 150) 

model_h2o@leaderboard %>% as.data.frame() 
h2o_leader <- model_h2o@leader

pred_h2o <- h2o_leader %>% h2o.predict(test_h2o)

h2o_leader %>% 
  h2o.rmse(train = T,
           valid = T,
           xval = T)

error_tbl <- df %>% 
  filter(lubridate::year(Date) >= 1988) %>% 
  add_column(pred = pred_h2o %>% as_tibble() %>% pull(predict)) %>%
  rename(actual = Count) %>% 
  select(Date,actual,pred)

highchart() %>% 
  hc_xAxis(categories = error_tbl$Date) %>% 
  hc_add_series(data=error_tbl$actual, type='line', color='red', name='Actual') %>% 
  hc_add_series(data=error_tbl$pred, type='line', color='green', name='Predicted') %>% 
  hc_title(text='Predict')

# Forecast average monthly beer production. New data (next year) ----
new_data <- seq(as.Date("1995/09/01"), as.Date("1996/08/01"), "months") %>%
  as_tibble() %>% 
  add_column(Count=0) %>% 
  rename(Date=value) %>% 
  tk_augment_timeseries_signature() %>%
  select(-contains("hour"),
         -contains("day"),
         -contains("week"),
         -minute,-second,-am.pm) %>% 
  mutate_if(is.ordered, as.character) %>% 
  mutate_if(is.character,as_factor)

# Forcaste ----
new_h2o <- new_data %>% as.h2o()

new_predictions <- h2o_leader %>% #new predictionların sayı 12 dir(1 illik forecast görə)
  h2o.predict(new_h2o) %>% 
  as_tibble() %>%
  add_column(Date=new_data$Date) %>% 
  select(Date,predict) %>% 
  rename(Count=predict)

nrow(data) # datamdakı row sayı.
nrow(new_predictions)

	
data %>% 
  bind_rows(new_predictions) %>% #bind_rows ilə datama new_predictions datanısı rowwise birləşdirirəm
  mutate(colors=c(rep('Actual',476),rep('Predicted',12))) %>% #mutate funksiyası ilə colors column yaradırıq
  hchart("line", hcaes(Date, Count, group = colors)) %>% #group=colors - colors columundakı Actual və Predicted yazılarına görə qrafikdə datanı fərqli rənglərdə qruplaşdırır
  hc_title(text='Forecast') %>% 
  hc_colors(colors = c('red','green'))

# -------------------------------- Tidy models --------------------------------
train <- data %>% filter(Date < "1988-01-01")
test <- data %>% filter(Date >= "1988-01-01")
#	Forecast beer production for next year with both h2o and Arima.
# 1.Auto ARIMA
model_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(Count ~ Date, train)

# calibration
calibration <- modeltime_table(
  model_fit_arima) %>%
  modeltime_calibrate(test)

# Forecast ----
calibration %>% 
  #filter(.model_id == 3) %>% 
  modeltime_forecast(actual_data = data) %>%
  plot_modeltime_forecast(.interactive = T,
                          .plotly_slider = T)

# Accuracy ----
calibration %>% modeltime_accuracy() %>% 
  table_modeltime_accuracy(.interactive = F)

# Forecast Forward ----
calibration %>%
  modeltime_refit(data) %>%
  modeltime_forecast(h = "1 year", 
                     actual_data = data) %>%
  select(-contains("conf")) %>% 
  plot_modeltime_forecast(.interactive = T,
                          .plotly_slider = T,
                          .legend_show = F)



#RMSE for h20 = 9.98,
#RMSE for Arima = 12.78