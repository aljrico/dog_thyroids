library(tidyverse)
library(data.table)
library(lubridate)
library(readxl)



df <- 
  read_excel('thyroids_values.xlsx') %>% 
  rename(
    id = `...1`,
    name = `...2`,
    race = `...3`,
  ) %>% 
  select(-`...4`) %>% 
  mutate(
    id = id %>% zoo::na.locf(),
    name = name %>% zoo::na.locf(),
    race = race %>% zoo::na.locf()
  ) %>% 
  mutate(f = is.na(`right NC`) + is.na(`Left NC`) + is.na(`right WC`) + is.na(`Left WC`)) %>% 
  filter(f < 4) %>% 
  data.table()



a <- df$`right NC`

get_avg <- function(column){
  a <- column
  avg <- c()
  
  for(i in seq_along(a)){
    if(is.na(a[[i]])){
      avg[[i]] <- NA 
    } else{
      splitted <- a[[i]] %>% str_replace_all('\\+-', '\\+') %>% str_replace_all('\\+', '\\+-') %>%  str_split(pattern = '\\+-') %>% .[[1]]
      avg[[i]] <-  splitted %>% .[[1]]
    }
  } 
  return(avg)
}
get_sd <- function(column){
  a <- column
  sd <- c()
  
  for(i in seq_along(a)){
    if(is.na(a[[i]])){
      sd[[i]] <- NA 
    } else{
      splitted <- a[[i]] %>% str_replace_all('\\+-', '\\+') %>% str_replace_all('\\+', '\\+-') %>%  str_split(pattern = '\\+-') %>% .[[1]]
      if(length(splitted) > 1){
        sd[[i]] <-  splitted %>% .[[2]]
      }else{
        sd[[i]] <- NA
      }
    }
  } 
  return(sd)
}


df$right_NC_avg = df$`right NC` %>% get_avg()
df$right_NC_sd = df$`right NC` %>% get_sd()

df$right_WC_avg = df$`right WC` %>% get_avg()
df$right_WC_sd = df$`right WC` %>% get_sd()

df$left_NC_avg = df$`Left NC` %>% get_avg()
df$left_NC_sd = df$`Left NC` %>% get_sd()

df$left_WC_avg = df$`Left WC` %>% get_avg()
df$left_WC_sd = df$`Left WC` %>% get_sd()



df %>% 
  ggplot(aes(x = right_NC_avg %>% as.numeric(), fill = name, colour = name)) +
  geom_density(alpha = 0.8) +
  facet_wrap(.~name, scales = 'free')
