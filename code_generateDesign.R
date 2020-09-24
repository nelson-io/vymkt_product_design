#Code to generate factorial orthogonal designs
# example
library(conjoint)
library(tidyverse)
library(rio)
library(fastDummies)

c <- expand.grid(
  price <- c("80", "120", "160", "200"),
  size <- c("6 pulgadas", "8 pulgadas", "10 pulgadas"),
  backlight <- c("sí", "no"),
  water_resistant <- c("sí", "no"),
  internal_mem <- c('4GB','8GB','16GB','32GB'))

#change the column names to these
names(c) <- c("price", "size", "backlight", "water_resistant", "internal_memory")


set.seed(0)
design <- caFactorialDesign(data=c, type="fractional")
code <- caEncodedDesign(design)
encodedorthodesign <- data.frame(design, code)
print(encodedorthodesign)

# print designs

for(i in 1:nrow(design)){
  print(paste('Precio (USD):', design$price[i],'-', 
              'Tamaño:', design$size[i],',',
              'Backlight:', design$backlight[i],',',
              'Sumergible:', design$water_resistant[i],',',
              'Memoria interna:', design$internal_memory[i]))
}



### import survey data

path <- 'https://docs.google.com/spreadsheets/d/1xcZ50RHmDu-X2GYyOh7OUkJLhqBkNmJ9chydSBOcag4/export?gid=1222909420&format=csv'

survey_data <- read_csv(path) %>% 
  select(-Timestamp)


# Make dummies

dummified <- dummy_cols(code %>% mutate_all(as.factor), remove_first_dummy = T, remove_selected_columns = T)

# add scores for every participant

scores_dfs <-  map(1:nrow(survey_data), ~dummified %>% cbind(value = survey_data %>% slice(.x) %>% t() %>% as.vector()))

scores_total <- map_df(scores_dfs, ~.x)

model <- lm(value~.,scores_total)

model_summary <- summary(model)


model_summary$coefficients

# relative importance

range_calculation <- function(coef_numbers){
  range <- abs(max(0,model$coefficients[coef_numbers]) - min(0,model$coefficients[coef_numbers]))
  return(range)
}

relative_importance_df <- data.frame()
attributes <- c('Price', 'size', 'backlight', 'water_resistant','internal_memory')
coeff_positions <- list(2:4,5:6,7,8,9:11)

for(i in 1:length(scores_dfs)){
  
  model <- lm(value~., scores_dfs[[i]])
  ranges <- map_dfc(coeff_positions, ~ range_calculation(.x)) %>% set_names(str_c(attributes,'_range'))
  ranges_sum <- sum(ranges)
  relative_importance <- map_dfc(ranges, ~.x/ranges_sum) %>% 
    mutate(id = i) %>% 
    select(id, everything())
  
  relative_importance_df <- rbind(relative_importance_df, relative_importance)
  
}







