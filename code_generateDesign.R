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





