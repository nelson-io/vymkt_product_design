#Code to generate factorial orthogonal designs
# example
library(conjoint)
library(tidyverse)
library(rio)
library(fastDummies)
library(ggthemes)
library(gridExtra)
library(factoextra)
library(NbClust)

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


# get coeffs
get_coeffs <- function(df){
  model <- lm(value~.,df)
  coeffs <- model$coefficients %>% 
           t() %>% 
            as.data.frame() 
  
  return(coeffs)
}

partial_values <- map_dfr(scores_dfs, get_coeffs)


# relative importance

range_calculation <- function(coef_numbers){
  range <- abs(max(0,model$coefficients[coef_numbers]) - min(0,model$coefficients[coef_numbers]))
  return(range)
}

relative_importance_df <- data.frame()
attributes <- c('price', 'size', 'backlight', 'water_resistant','internal_memory')
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

relative_importance_df


summary(relative_importance_df %>% select(-id))

plist <- list()
for(i in 1:length(scores_dfs)){
  
  model <- lm(value~., scores_dfs[[i]])
  plot_df <- data.frame(worth = c(0,model$coefficients[2:4]) %>% as.vector(), 
                        label = c('$80', '$120', '$160', '$200'))
  
  plist[[i]] <- ggplot(plot_df)+
    geom_line(aes(x = factor(label, levels = c('$80', '$120', '$160', '$200')), y = worth, group = 1),
              color = 'steelblue', size = 1.5)+
    theme_bw()+
    xlab(NULL)
    
  
}


do.call("grid.arrange", c(plist, ncol=4)) #Siendo el nivel base el de menor precio, se observa que en general el aumento de precio se encuentra asociado a una perdida de utilidad


#Evaluamos la importancia relativa de los atributos para el general de los encuestados para identificar el atributo más importante


model <- lm(value~., scores_total)
ranges <- map_dfc(coeff_positions, ~ range_calculation(.x)) %>% set_names(str_c(attributes,'_range'))
ranges_sum <- sum(ranges)
relative_importance <- map_dfc(ranges, ~.x/ranges_sum) 

#Después del precio, el atributo más importante es la memoria interna
# para pasar de 4 a 8 GB de ROM, los encuestados están dispouestos a gastar

price_rel <- 0-(-1.9210)
diff_price <- 200-80

.3205/price_rel * diff_price #20.02 USD

# de 8 a 16 GB de ROM

(.6986-.3205) /price_rel * diff_price #23.62 USD

# de 16 a 32 GB de ROM

(1.3419 - .6986)/price_rel * diff_price #40.19 USD

#5 

# select data and determine optimum kmeans clusters
#scale  data

data_scaled <- partial_values %>% 
  select(-1) %>% 
  scale() %>% 
  data.frame()

fviz_nbclust(data_scaled,nstart = 50, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# apply kmeans


kmeans_clustering <- kmeans(data_scaled,centers = 5,nstart = 50)

#data_scaled$cluster <- kmeans_clustering$cluster


#PCA to show data

res.pca <- prcomp(data_scaled, scale = TRUE)
fviz_eig(res.pca)


groups <- as.factor(kmeans_clustering$cluster)
fviz_pca_ind(res.pca,
             col.ind = groups, # color by groups
             palette = c("black",  "#AA3939","#AA6C39","#226666","#2D882D"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)

p <- fviz_pca_var(res.pca)
fviz_add(p,data_scaled,color ="blue", geom="arrow")

fviz_pca_biplot(res.pca, label="var", habillage=kmeans_clustering$cluster)+
  ggforce::geom_mark_ellipse(aes(fill = Groups,
                                 color = Groups),expand = 1e-2)




#analizamos segmentos 1:4

#generamos perfiles de productos

# elegimos aleatoriamente el producto de X y el de Y
mkt_prods <- c %>% 
  caEncodedDesign()
  
dummified_2 <-  dummy_cols(.data = mkt_prods %>% mutate_all(as.factor), remove_first_dummy = T, remove_selected_columns = T) %>% 
  sample_n(2)

# estimamos utilidad de cada producto para cada agente y asignamos el que maximice

product_selected <- c()

for(i in 1:length(scores_dfs)){
  data <- scores_dfs[[i]]
  model <- lm(value~.,data)
  pred <- predict(model, dummified_2)
  
  product_selected <- product_selected %>% append(ifelse(pred[1]>pred[2], 'x', 'y'))
  
  
}

product_selected %>% table() %>% prop.table()

