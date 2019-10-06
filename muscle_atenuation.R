source('clean_thyroids.R')

get_avg <- function(column){
  a <- column
  avg <- c()
  
  for(i in seq_along(a)){
    if(is.na(a[[i]])){
      avg[[i]] <- NA 
    } else{
      splitted <- a[[i]] %>% str_replace_all('\\+-', '\\+') %>% str_replace_all('\\+', '\\+-') %>%  str_split(pattern = '\\+-') %>% .[[1]]
      avg[[i]] <-  splitted %>% .[[1]] %>% as.numeric()
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
        sd[[i]] <-  splitted %>% .[[2]] %>% as.numeric()
      }else{
        sd[[i]] <- NA
      }
    }
  } 
  return(sd)
}




df <- read_excel('datos_3.xlsx') %>% 
  rename(id = `Nº (P)`) %>% 
  data.frame()

muscle_values <- colnames(df)[colnames(df) %>% str_detect('musculo')] %>% na.omit() %>% array()

precontrast_values <- muscle_values[muscle_values %>% str_detect('Precontraste')]
postcontrast_values <- muscle_values[muscle_values %>% str_detect('Postcontraste')]

muscle_df <- 
  df[, c('id', 'braquicefalico', muscle_values)] %>% 
  melt(id.vars = c('id', 'braquicefalico')) %>% 
  mutate(contrast = ifelse(variable %>% str_detect('Precontraste'), 'Pre-contrast', 'Post-contrast')) %>% 
  mutate(side = ifelse(variable %>% str_detect('derecho'), 'Right Longus Capitis', 'Left Longus Capitis')) %>% 
  select(-variable) %>% 
  na.omit() %>% 
  data.table()


muscle_df %>% 
  mutate(value = get_avg(value)) %>% 
  ggplot(aes(x = braquicefalico, y = value, fill = braquicefalico)) +
  geom_boxplot(size = 0.5, outlier.shape = NA) +
  facet_wrap(contrast~side, scales = 'free') +
  stat_compare_means(method = 'wilcox.test') +
  scale_x_discrete(labels = c('No-Braqui', 'Braqui')) +
  scale_fill_got_d(option = 'Margaery', name = '', guide = FALSE) +
  xlab('') +
  theme_bw() +
  ylab('Atenuation Value') +
  labs(
    title = "Comparing average Atenuation Values between \n brachycephalic and non-brachycephalic subjects"
  )
