library(tidyverse)
library(DataExplorer)
library(skimr)

data_folder <- "../data"
train <- 'train.csv'
test <- 'test.csv'
sample_submission <- 'sample_submission.csv'

train_data <- read_csv(file.path(data_folder, train))
test_data <- read_csv(file.path(data_folder, test))
sample_submission_data <- read_csv(file.path(data_folder, sample_submission))

# In this competition, you will be predicting the number of 
# open_channels present, based on electrophysiological signal data.
#  
# IMPORTANT: While the time series appears continuous, the data 
# is from discrete batches of 50 seconds long 10 kHz samples 
# (500,000 rows per batch). In other words, the data from 
# 0.0001 - 50.0000 is a different batch than 50.0001 - 100.0000, 
# and thus discontinuous between 50.0000 and 50.0001.
#  
# You can find detailed information about the data from the paper 
# Deep-Channel uses deep neural networks to detect single-molecule 
# events from patch-clamp data.


train_data %>% View()



# looking at signal vs open_channels
SAMPLE_SIZE <- 1000
train_data %>%
  sample_n(SAMPLE_SIZE) %>% 
  ggplot(aes(x = signal, open_channels)) +
    geom_jitter()

train_data %>%
#  sample_n(SAMPLE_SIZE) %>%
  ggplot(aes(x = time, open_channels, alpha = 0.01)) +
  geom_line() +
#  geom_smooth() +
  scale_y_continuous(breaks = round(seq(1, 11, 1), 0))



DataExplorer::plot_histogram(train_data %>% sample_n(SAMPLE_SIZE))

skimr::skim(train_data)
# ── Data Summary ────────────────────────
# Values    
# Name                       train_data
# Number of rows             5000000   
# Number of columns          3         
# _______________________              
# Column type frequency:               
#   numeric                  3         
# ________________________             
# Group variables            None      
# 
# ── Variable type: numeric ─────────────────────────────────────────────────
# skim_variable n_missing complete_rate   mean     sd      p0    p25    p50
# 1 time                  0             1 250.   144.    0.0001 125.   250.  
# 2 signal                0             1   1.39   3.34 -5.80    -1.59   1.12
# 3 open_channels         0             1   2.73   2.67  0        1      2   
# p75  p100 hist 
# 1 375.   500   ▇▇▇▇▇

# 2   3.69  13.2 ▅▇▇▂▁

# 3   4     10   ▇▃▂▂▁


skimr::skim(test_data)
# ── Data Summary ────────────────────────
# Values   
# Name                       test_data
# Number of rows             2000000  
# Number of columns          2        
# _______________________             
# Column type frequency:              
#   numeric                  2        
# ________________________            
# Group variables            None     
# 
# ── Variable type: numeric ─────────────────────────────────────────────────
# skim_variable n_missing complete_rate     mean    sd     p0    p25
# 1 time                  0             1 600.     57.7  500.   550.  
# 2 signal                0             1  -0.0426  2.51  -5.51  -2.47
# p50    p75  p100 hist 

# 1 600.    650.   700   ▇▇▇▇▇

# 2  -0.355   1.78  10.8 ▆▇▆▁▁


# distribution of open channels
train_data$open_channels %>% 
  qplot() +
  stat_bin(bins = 10) +
  scale_x_continuous(breaks = round(seq(0, 11, 1), 0)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Distribution of Open Channels",
       x = "Open Channel Count") + 
  theme_minimal() +
  theme(plot.title.position = "plot")


# TO DO
## 1) Add batches to data... Done
## 2) Look in to incorporating de-noising 
## 3) Look into feature building
## 4) Add 


preprocesser <- function(fresh_read_in_df) {
  #'
  #'@param fresh_read_in_df: the data frame right after read-in
  #'
  
  fresh_read_in_df %>%
    mutate(batch = (time - 0.0001) %/% 50)
}


train_clean <- train_data %>%
  preprocesser(.)
 








