library(tidyverse)
library(data.table)
library(lubridate)
library(readxl)

library(gameofthrones)
library(harrypotter)

library(ggpubr)

get_clean_thyroids <- function(unify_left_right = TRUE){
  
  df <- 
    read_excel('thyroids_values.xlsx') %>% 
    rename(
      id = `...1`,
      name = `...2`,
      race = `...3`,
    ) %>% 
    select(-`...4`) %>% 
    mutate(id = id %>% str_remove_all('.*\\)') %>% str_trim() %>% as.numeric()) %>% 
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
  
  
  df$right_NC_avg = df$`right NC` %>% get_avg()
  df$right_NC_sd = df$`right NC` %>% get_sd()
  
  df$right_WC_avg = df$`right WC` %>% get_avg()
  df$right_WC_sd = df$`right WC` %>% get_sd()
  
  df$left_NC_avg = df$`Left NC` %>% get_avg()
  df$left_NC_sd = df$`Left NC` %>% get_sd()
  
  df$left_WC_avg = df$`Left WC` %>% get_avg()
  df$left_WC_sd = df$`Left WC` %>% get_sd()
  
  df <- 
    df %>% 
    select(-`right NC`, -`Left NC`, -`right WC`, -`Left WC`) %>% 
    data.table()
  
  if(unify_left_right){
    df <-
      df %>% 
      melt(id.vars = c('id', 'name', 'race', 'f')) %>% 
      filter(! (variable %>% str_detect('_sd'))) %>% 
      mutate(variable = variable %>% str_remove_all('left_') %>% str_remove_all('right_') %>% str_remove_all('_avg')) %>% 
      rename(contrast = variable) %>% 
      data.table()
  }
  
  return(df)
}
thyroids_unique_value <- function(unified_left_right = TRUE){
  
  if(unified_left_right){
    clean_thyroids %>% 
      select(id, contrast, value) %>% 
      group_by(id, contrast) %>% 
      summarise(value = value %>% mean(na.rm = TRUE))
  }else{
    clean_thyroids %>% 
      group_by(id) %>% 
      summarise(
        right_NC = mean(right_NC_avg, na.rm = TRUE),
        left_NC = mean(left_NC_avg, na.rm = TRUE),
        right_WC = mean(right_WC_avg, na.rm = TRUE),
        left_WC = mean(left_WC_avg, na.rm = TRUE)
      ) %>% 
      inner_join(
        read_excel('datos_1.xlsx') %>% 
          rename(id = `Nº (P)`) %>% 
          select(id, atenuacion_baja, braquicefalico) %>% 
          na.omit()
      ) %>% 
      as_tibble()
  }

}
braqui_atenuation_test <- function(unified_left_right = TRUE){
  
  if(unified_left_right){
    clean_thyroids %>% 
      group_by(id, contrast) %>% 
      summarise(value = mean(value, na.rm = TRUE)) %>% 
      inner_join(
        read_excel('datos_1.xlsx') %>% 
          rename(id = `Nº (P)`) %>% 
          select(id, atenuacion_baja, braquicefalico) %>% 
          na.omit()
      ) %>% 
      as_tibble() %>% 
      ggplot(aes(x = braquicefalico, y = value, fill = braquicefalico)) +
      geom_boxplot(size = 0.5, outlier.shape = NA) +
      facet_wrap(contrast~., scales = 'free') +
      stat_compare_means(method = 't.test') +
      scale_x_discrete(labels = c('No Braqui', 'Braqui')) +
      scale_fill_got_d(option = 'Margaery', labels = c('No Braquicefálico', 'Braquicefálico'), name = '', guide = FALSE) +
      theme_pubr() +
      xlab('') +
      ylab('Atenuation Value')
  }else{
    
    clean_thyroids %>% 
      group_by(id) %>% 
      summarise(
        right_NC = mean(right_NC_avg, na.rm = TRUE),
        left_NC = mean(left_NC_avg, na.rm = TRUE),
        right_WC = mean(right_WC_avg, na.rm = TRUE),
        left_WC = mean(left_WC_avg, na.rm = TRUE)
      ) %>% 
      inner_join(
        read_excel('datos_1.xlsx') %>% 
          rename(id = `Nº (P)`) %>% 
          select(id, atenuacion_baja, braquicefalico) %>% 
          na.omit()
      ) %>% 
      as_tibble() %>% 
      melt(id.vars = c('id', 'atenuacion_baja', 'braquicefalico')) %>% 
      ggplot(aes(x = braquicefalico, y = value, fill = braquicefalico)) +
      geom_boxplot(size = 0.5, outlier.shape = NA) +
      facet_wrap(variable~., scales = 'free') +
      stat_compare_means(method = 't.test') +
      scale_x_discrete(labels = c('No Braqui', 'Braqui')) +
      scale_fill_got_d(option = 'Margaery', labels = c('No Braquicefálico', 'Braquicefálico'), name = '', guide = FALSE) +
      theme_pubr() +
      xlab('') +
      ylab('Atenuation Value')
  }
 
}
sick_thyroids_test <- function(unified_left_right = TRUE){
  if(unified_left_right){
    clean_thyroids %>% 
      group_by(id, contrast) %>% 
      summarise(value = mean(value, na.rm = TRUE)) %>% 
      inner_join(
        read_excel('datos_1.xlsx') %>% 
          rename(id = `Nº (P)`) %>% 
          select(id, atenuacion_baja, control) %>% 
          na.omit()
      ) %>% 
      as_tibble() %>% 
      ggplot(aes(x = control, y = value, fill = control)) +
      geom_boxplot(size = 0.5, outlier.shape = NA) +
      facet_wrap(contrast~., scales = 'free') +
      stat_compare_means(method = 't.test') +
      scale_x_discrete() +
      scale_fill_got_d(option = 'Margaery', name = '', guide = FALSE) +
      theme_pubr() +
      xlab('') +
      ylab('Atenuation Value')
  }else{
    
    
    clean_thyroids %>% 
      group_by(id) %>% 
      summarise(
        right_NC = mean(right_NC_avg, na.rm = TRUE),
        left_NC = mean(left_NC_avg, na.rm = TRUE),
        right_WC = mean(right_WC_avg, na.rm = TRUE),
        left_WC = mean(left_WC_avg, na.rm = TRUE)
      ) %>% 
      inner_join(
        read_excel('datos_1.xlsx') %>% 
          rename(id = `Nº (P)`) %>% 
          select(id, atenuacion_baja, control) %>% 
          na.omit()
      ) %>% 
      as_tibble() %>% 
      melt(id.vars = c('id', 'atenuacion_baja', 'control')) %>% 
      ggplot(aes(x = control, y = value, fill = control)) +
      geom_boxplot(size = 0.5, outlier.shape = NA) +
      facet_wrap(variable~., scales = 'free') +
      stat_compare_means(method = 't.test') +
      scale_x_discrete() +
      scale_fill_got_d(option = 'Margaery', name = '', guide = FALSE) +
      theme_pubr() +
      xlab('') +
      ylab('Atenuation Value')
  }

}

sample_size <- function(){
  sum <- clean_thyroids %>% 
    group_by(id) %>% 
    summarise(
      right_NC = mean(right_NC_avg, na.rm = TRUE),
      left_NC = mean(left_NC_avg, na.rm = TRUE),
      right_WC = mean(right_WC_avg, na.rm = TRUE),
      left_WC = mean(left_WC_avg, na.rm = TRUE)
    ) %>% 
    inner_join(
      read_excel('datos_1.xlsx') %>% 
        rename(id = `Nº (P)`) %>% 
        select(id, atenuacion_baja, control) %>% 
        na.omit()
    ) %>% 
    as_tibble() %>% 
    melt(id.vars = c('id', 'atenuacion_baja', 'control')) %>% 
    group_by(variable) %>% 
    mutate(all_sd = sd(value, na.rm = TRUE)) %>% 
    group_by(control, variable) %>% 
    summarise(
      avg = mean(value, na.rm = TRUE),
      sd  = sd(value, na.rm = TRUE),
      all_sd = mean(all_sd, na.rm = TRUE)
    )
  
  
  # Positive means non-control higher than control
  effect_size <-
    sum %>% 
    group_by(variable) %>% 
    mutate(avg2 = lead(avg)) %>% 
    mutate(effect_size = 100 * (avg2 - avg) / avg)%>% 
    select(variable, effect_size) %>% 
    na.omit()
  
  estimated_deviation <- 
    sum %>% 
    group_by(variable) %>% 
    summarise(sd = mean(all_sd))
  
  
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2876926/
  error_constant <- 1.65
  power_constant <- 0.8416
  
  effect_size %>% 
    inner_join(
      estimated_deviation
    ) %>% 
    mutate(
      sample_size = (2 * (error_constant + power_constant)^2 * (sd)^2) / (effect_size ^ 2)
    )
  
  
  
}


clean_thyroids <- get_clean_thyroids(unify_left_right = TRUE)
blood_thyroids_corr <- function(unified_left_right = TRUE){
  
  if(unified_left_right){
    dat <- 
      read_excel('datos_1.xlsx', sheet = 'Full2') %>% 
      rename(T4 = `T4 (ug/dL)`,
             TSH = `TSH (ng/mL)`) %>% 
      select(Nom, T4, TSH) %>% 
      mutate(
        T4 = T4 %>% str_remove_all('\\<') %>% as.numeric(),
        TSH = TSH %>% str_remove_all('\\<') %>% as.numeric()
      ) %>% 
      na.omit() %>% 
      mutate(name = Nom %>% tolower()) %>% 
      select(-Nom) %>% 
      inner_join(
        clean_thyroids %>% 
          group_by(name, contrast) %>% 
          summarise(value = mean(value, na.rm = TRUE)) %>% 
          ungroup() %>% 
          mutate(name = name %>% tolower())
      ) %>% 
      data.table()
    
    
    setDT(dat)
    
    sum_t4 <- dat[,summary(lm(T4~value))$r.squared,by=contrast] %>% rename(r_squared = V1)
    sum_tsh <- dat[,summary(lm(TSH~value))$r.squared,by=contrast] %>% rename(r_squared = V1)
    
    t4_plot <- dat %>% 
      ggplot(aes( x = value, y = T4, colour = contrast)) +
      geom_point() +
      geom_smooth(method = 'lm', se = FALSE) +
      facet_wrap(contrast~., scales = 'free')
    
    tsh_plot <- dat %>% 
      ggplot(aes( x = value, y = TSH, colour = contrast)) +
      geom_point() +
      geom_smooth(method = 'lm', se = FALSE) +
      facet_wrap(contrast~., scales = 'free')
    
  }else{
    
    dat <- 
      read_excel('datos_1.xlsx', sheet = 'Full2') %>% 
      rename(T4 = `T4 (ug/dL)`,
             TSH = `TSH (ng/mL)`) %>% 
      select(Nom, T4, TSH) %>% 
      mutate(
        T4 = T4 %>% str_remove_all('\\<') %>% as.numeric(),
        TSH = TSH %>% str_remove_all('\\<') %>% as.numeric()
      ) %>% 
      na.omit() %>% 
      mutate(name = Nom %>% tolower()) %>% 
      select(-Nom) %>% 
      inner_join(
        clean_thyroids %>% 
          group_by(name) %>% 
          summarise(
            right_NC = mean(right_NC_avg, na.rm = TRUE),
            left_NC = mean(left_NC_avg, na.rm = TRUE),
            right_WC = mean(right_WC_avg, na.rm = TRUE),
            left_WC = mean(left_WC_avg, na.rm = TRUE)
          ) %>% 
          mutate(name = name %>% tolower())
      ) %>% 
      melt(id.vars = c('T4', 'TSH', 'name')) 
    
    
    setDT(dat)
    
    sum_t4 <- dat[,summary(lm(T4~value))$r.squared,by=variable] %>% rename(r_squared = V1)
    sum_tsh <- dat[,summary(lm(TSH~value))$r.squared,by=variable] %>% rename(r_squared = V1)
    
    t4_plot <- dat %>% 
      ggplot(aes( x = value, y = T4, colour = variable)) +
      geom_point() +
      geom_smooth(method = 'lm', se = FALSE) +
      facet_wrap(variable~., scales = 'free')
    
    tsh_plot <- dat %>% 
      ggplot(aes( x = value, y = TSH, colour = variable)) +
      geom_point() +
      geom_smooth(method = 'lm', se = FALSE) +
      facet_wrap(variable~., scales = 'free')
  }
  
  return(list(t4_plot = t4_plot, tsh_plot = tsh_plot))
  
}



blood_plots <- blood_thyroids_corr()

blood_plots$t4_plot
blood_plots$tsh_plot
