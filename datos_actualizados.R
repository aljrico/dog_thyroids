library(tidyverse)
library(data.table)
library(readxl)
library(ggpubr)

clean_edat <- function(df) {
  df %>%
    mutate(Edat = ifelse(Edat %>% str_detect("a"), as.numeric(Edat %>% str_remove_all("a")), ifelse(Edat %>% str_detect("m"), as.numeric(Edat %>% str_remove_all("m")) / 12, as.numeric(Edat))))
}
clean_datos <- function(df) {
  colnames(df)[[1]] <- "id"
  colnames(df)[[2]] <- "name"
  colnames(df)[[3]] <- "race"

  df <- df %>% clean_edat()
  df
}

get_clean_thyroids <- function(filename, unify_left_right = TRUE) {
  df <-
    read_excel(filename) %>%
    select(-`...4`)

  colnames(df)[[1]] <- "id"
  colnames(df)[[2]] <- "name"
  colnames(df)[[3]] <- "race"

  df <-
    df %>%
    mutate(id = id %>% str_remove_all(".*\\)") %>% str_trim() %>% as.numeric()) %>%
    mutate(
      id = id %>% zoo::na.locf(),
      name = name %>% zoo::na.locf(),
      race = race %>% zoo::na.locf()
    ) %>%
    mutate(f = is.na(`right NC`) + is.na(`Left NC`) + is.na(`right WC`) + is.na(`Left WC`)) %>%
    filter(f < 4) %>%
    data.table()


  a <- df$`right NC`

  get_avg <- function(column) {
    a <- column
    avg <- c()

    for (i in seq_along(a)) {
      if (is.na(a[[i]])) {
        avg[[i]] <- NA
      } else {
        splitted <- a[[i]] %>%
          str_replace_all("\\+-", "\\+") %>%
          str_replace_all("\\+", "\\+-") %>%
          str_split(pattern = "\\+-") %>%
          .[[1]]
        avg[[i]] <- splitted %>%
          .[[1]] %>%
          as.numeric()
      }
    }
    return(avg)
  }
  get_sd <- function(column) {
    a <- column
    sd <- c()

    for (i in seq_along(a)) {
      if (is.na(a[[i]])) {
        sd[[i]] <- NA
      } else {
        splitted <- a[[i]] %>%
          str_replace_all("\\+-", "\\+") %>%
          str_replace_all("\\+", "\\+-") %>%
          str_split(pattern = "\\+-") %>%
          .[[1]]
        if (length(splitted) > 1) {
          sd[[i]] <- splitted %>%
            .[[2]] %>%
            as.numeric()
        } else {
          sd[[i]] <- NA
        }
      }
    }
    return(sd)
  }


  df$right_NC_avg <- df$`right NC` %>% get_avg()
  df$right_NC_sd <- df$`right NC` %>% get_sd()

  df$right_WC_avg <- df$`right WC` %>% get_avg()
  df$right_WC_sd <- df$`right WC` %>% get_sd()

  df$left_NC_avg <- df$`Left NC` %>% get_avg()
  df$left_NC_sd <- df$`Left NC` %>% get_sd()

  df$left_WC_avg <- df$`Left WC` %>% get_avg()
  df$left_WC_sd <- df$`Left WC` %>% get_sd()

  df <-
    df %>%
    select(-`right NC`, -`Left NC`, -`right WC`, -`Left WC`) %>%
    data.table()

  if (unify_left_right) {
    df <-
      df %>%
      melt(id.vars = c("id", "name", "race", "f")) %>%
      filter(!(variable %>% str_detect("_sd"))) %>%
      mutate(variable = variable %>% str_remove_all("left_") %>% str_remove_all("right_") %>% str_remove_all("_avg")) %>%
      rename(contrast = variable) %>%
      data.table()
  }

  return(df)
}
thyroids_unique_value <- function(filename, unified_left_right = TRUE) {
  clean_thyroids <- get_clean_thyroids(filename = filename, unify_left_right = unified_left_right)

  if (unified_left_right) {
    clean_thyroids %>%
      select(id, contrast, value) %>%
      group_by(id, contrast) %>%
      summarise(value = value %>% mean(na.rm = TRUE))
  } else {
    clean_thyroids %>%
      group_by(id) %>%
      summarise(
        right_NC = mean(right_NC_avg, na.rm = TRUE),
        left_NC = mean(left_NC_avg, na.rm = TRUE),
        right_WC = mean(right_WC_avg, na.rm = TRUE),
        left_WC = mean(left_WC_avg, na.rm = TRUE)
      ) %>%
      inner_join(
        read_excel(filename) %>%
          rename(id = `Nº (P)`) %>%
          select(id, braquicefalico) %>%
          na.omit()
      ) %>%
      as_tibble()
  }
}

read_excel("datos actualizatos.xlsx") %>%
  clean_edat() %>%
  ggplot(aes(x = braquicefálicos, y = Edat, fill = braquicefálicos)) +
  geom_boxplot(size = 0.5, outlier.shape = NA) +
  stat_compare_means(method = "t.test") +
  scale_fill_discrete(labels = c("Braquicefálico", "No Braquicefálico"), name = "") +
  theme_pubr() +
  xlab("") +
  scale_x_discrete(labels = c("Braquicefálico", "Braquicefálico"))

read_excel("datos actualizatos.xlsx") %>%
  clean_edat() %>%
  mutate(Control = ifelse(is.na(Control), "N", "Y")) %>%
c
  scale_x_discrete(labels = c("Control", "Variant"))

clean_thyroids <-
  get_clean_thyroids(filename = "thyroids_values_2.xlsx") %>%
  group_by(id, contrast) %>%
  summarise(athenuation_value = mean(value, na.rm = TRUE))



# Athenuation Value by group control/variant
read_excel("datos actualizatos.xlsx") %>%
  clean_datos() %>%
  inner_join(clean_thyroids) %>%
  mutate(Control = ifelse(is.na(Control), "N", "Y")) %>%
  ggplot(aes(x = Control, y = athenuation_value, colour = Control)) +
  geom_boxplot(size = 1, outlier.shape = NA) +
  facet_wrap(contrast ~ ., scales = "free") +
  geom_jitter(size = 2, width = 0.15) +
  stat_compare_means(method = "t.test") +
  scale_colour_brewer(labels = c("Control", "Variant"), palette = "Dark2", guide = FALSE) +
  theme_pubr() +
  xlab("") +
  scale_x_discrete(labels = c("Control", "Variant"))


# Athenuation value by braqui
read_excel("datos actualizatos.xlsx") %>%
  clean_datos() %>%
  inner_join(clean_thyroids) %>%
  mutate(Control = ifelse(is.na(Control), "N", "Y")) %>%
  mutate(braqui = ifelse(is.na(braquicefálicos), "N", "Y")) %>%
  filter(Control == "Y") %>%
  ggplot(aes(x = braqui, y = athenuation_value, colour = braqui)) +
  geom_boxplot(size = 1, outlier.shape = NA) +
  facet_wrap(contrast ~ ., scales = "free") +
  geom_jitter(size = 2, width = 0.15) +
  stat_compare_means(method = "t.test") +
  scale_colour_brewer(labels = c("No Braqui", "Braqui"), palette = "Dark2", guide = FALSE) +
  theme_pubr() +
  xlab("") +
  scale_x_discrete(labels = c("No Braqui", "Braqui"))


# Athenuation Value ~ Braqui
read_excel("datos actualizatos.xlsx") %>%
  mutate(`braquicefálicos` = ifelse(`Nom pacient` == 'aretha', 'Y', `braquicefálicos`)) %>% 
  clean_datos() %>%
  inner_join(clean_thyroids) %>%
  mutate(Control = ifelse(is.na(Control), "N", "Y")) %>%
  mutate(braqui = ifelse(is.na(braquicefálicos), "N", "Y")) %>% 
  # filter(Control == "Y") %>%
  ggplot(aes(x = braqui, y = athenuation_value, colour = braqui)) +
  geom_boxplot(size = 1, outlier.shape = NA) +
  facet_wrap(contrast ~ ., scales = "free") +
  geom_point(size = 2, width = 0.15) +
  stat_compare_means(method = "t.test") +
  scale_colour_brewer(labels = c("non-brachycephalic dogs", "brachycephalic dogs"), palette = "Dark2", guide = FALSE) +
  theme_pubr() +
  xlab("") +
  ylab("attenuation values") +
  scale_x_discrete(labels = c("non-brachycephalic", "brachycephalic"))

# Athenuation Value ~ Braqui | HD
read_excel("datos actualizatos.xlsx") %>%
  mutate(`braquicefálicos` = ifelse(`Nom pacient` == 'aretha', 'Y', `braquicefálicos`)) %>% 
  clean_datos() %>%
  inner_join(clean_thyroids) %>%
  mutate(Control = ifelse(is.na(Control), "N", "Y")) %>%
  mutate(braqui = ifelse(is.na(braquicefálicos), "N", "Y")) %>% 
  # filter(Control == "Y") %>%
  ggplot(aes(x = braqui, y = athenuation_value, colour = braqui)) +
  geom_boxplot(size = 1.4, outlier.shape = NA) +
  facet_wrap(contrast ~ ., scales = "free") +
  # geom_point(size = 2, width = 0.15) +
  stat_compare_means(method = "t.test") +
  scale_colour_manual(labels = c("non-brachycephalic dogs", "brachycephalic dogs"), values = c('#171717', '#6d6d6d'), guide = FALSE) +
  theme_pubr() +
  xlab("") +
  ylab("attenuation values") +
  scale_x_discrete(labels = c("non-brachycephalic", "brachycephalic"))

# Athenuation Value ~ Medical Complaint
read_excel("datos actualizatos.xlsx") %>%
  rename(complaint = `Presenting complain (lo que interesa para el estudio)`) %>% 
  mutate(`braquicefálicos` = ifelse(`Nom pacient` == 'aretha', 'Y', `braquicefálicos`)) %>% 
  clean_datos() %>%
  inner_join(clean_thyroids) %>%
  mutate(Control = ifelse(is.na(Control), "N", "Y")) %>%
  mutate(braqui = ifelse(is.na(braquicefálicos), "N", "Y")) %>%
  # filter(Control == "N") %>%
  group_by(contrast, complaint) %>% 
  mutate(order_value = mean(athenuation_value)) %>% 
  ggplot(aes(x = reorder(complaint, (order_value)), y = athenuation_value, colour = complaint)) +
  geom_boxplot(size = 1, outlier.shape = NA) +
  facet_grid(~ contrast, scales = "free") +
  geom_jitter(size = 2, width = 0.15) +
  stat_compare_means(method = "t.test") +
  coord_flip() +
  scale_colour_brewer(palette = "Dark2", guide = FALSE) +
  theme_pubr() +
  xlab("") +
  ylab("attenuation values")

read_excel("datos actualizatos.xlsx") %>%
  rename(complaint = `Presenting complain (lo que interesa para el estudio)`) %>% 
  mutate(`braquicefálicos` = ifelse(`Nom pacient` == 'aretha', 'Y', `braquicefálicos`)) %>% 
  clean_datos() %>%
  inner_join(clean_thyroids) %>%
  mutate(Control = ifelse(is.na(Control), "N", "Y")) %>%
  mutate(braqui = ifelse(is.na(braquicefálicos), "N", "Y")) %>% 
  select(athenuation_value, contrast, complaint)


ath_data <-
  read_excel("datos actualizatos.xlsx") %>%
  clean_datos() %>%
  inner_join(clean_thyroids) %>%
  mutate(Control = ifelse(is.na(Control), "N", "Y")) %>%
  mutate(braqui = ifelse(is.na(braquicefálicos), "N", "Y")) %>%
  filter(Control == "N") %>%
  select(braqui, contrast, athenuation_value) %>%
  na.omit() %>%
  group_by(braqui, contrast) %>%
  summarise(
    quant_min = quantile(athenuation_value, 0.1),
    quant_max = quantile(athenuation_value, 0.9)
  )


read_excel("datos actualizatos.xlsx") %>%
  clean_datos() %>%
  inner_join(clean_thyroids) %>%
  mutate(Control = ifelse(is.na(Control), "N", "Y")) %>%
  mutate(braqui = ifelse(is.na(braquicefálicos), "N", "Y")) %>%
  # filter(Control == 'Y') %>%
  select(Control, contrast, athenuation_value) %>%
  na.omit() %>%
  group_by(Control, contrast) %>%
  summarise(
    quant_min = quantile(athenuation_value, 0.1),
    quant_max = quantile(athenuation_value, 0.9)
  )


read_excel("datos actualizatos.xlsx") %>%
  clean_datos() %>%
  inner_join(clean_thyroids) %>%
  mutate(Control = ifelse(is.na(Control), "N", "Y")) %>%
  mutate(braqui = ifelse(is.na(braquicefálicos), "N", "Y")) %>%
  group_by(braqui, contrast) %>%
  summarise(
    athenuation_mean = mean(athenuation_value, na.rm = TRUE),
    athenuation_sd = sd(athenuation_value, na.rm = TRUE)
  )

data_to_evaluate <- 
  read_excel("datos actualizatos.xlsx") %>%
  clean_datos() %>%
  inner_join(clean_thyroids) %>%
  mutate(Control = ifelse(is.na(Control), "N", "Y")) %>%
  mutate(braqui = ifelse(is.na(braquicefálicos), "N", "Y")) %>%
  mutate(Medication = ifelse(medicacions == "no med", 'N', 'Y')) %>% 
  mutate(sex = if_else(sexo %>% str_detect('m'), 'M', 'F')) %>% 
  mutate(castrated = if_else(sexo %>% str_detect('nc'), 'N', 'Y')) %>% 
  # group_by(contrast, braqui) %>%
  select(Control, contrast, athenuation_value, Edat, peso, T4, TSH, Medication, sex, castrated) %>%
  mutate(peso = peso %>% stringr::str_remove_all('kg') %>% stringr::str_remove_all('ks') %>% as.numeric()) %>% 
  mutate(T4 = T4 %>% stringr::str_remove_all('<') %>% as.numeric()) %>%
  mutate(TSH = as.numeric(TSH))

data_to_evaluate %>% 
  select(Control, contrast, athenuation_value, Edat, peso, T4, TSH) %>% 
  pivot_longer(cols = -c(contrast, Control)) %>% 
  group_by(contrast, name) %>% 
  do(broom::tidy(t.test(value ~ Control, data = .))) %>% 
  select(contrast, name, p.value) %>% 
  pivot_wider(id_cols = c(name, contrast), values_from = p.value)

data_to_evaluate %>% 
  select(Control, contrast, athenuation_value, Edat, peso, T4, TSH, Medication) %>% 
  pivot_longer(cols = -c(contrast, Control, Medication)) %>% 
  group_by(contrast, name) %>% 
  do(broom::tidy(t.test(value ~ Medication, data = .))) %>% 
  select(contrast, name, p.value) %>% 
  pivot_wider(id_cols = c(name, contrast), values_from = p.value)

data_to_evaluate %>% 
  select(sex, contrast, athenuation_value, Control) %>%
  group_by(contrast) %>% 
  do(broom::tidy(t.test(athenuation_value ~ sex, data = .))) %>% 
  select(contrast, p.value)
  
  

data_to_evaluate %>% 
  select(Control, contrast, athenuation_value, Edat, peso, T4, TSH) %>% 
  rename(`Weight` = peso, `Age` = Edat) %>% 
  pivot_longer(-c(Control, contrast, athenuation_value)) %>% 
  ggplot(aes(x = value, y = athenuation_value)) +
  geom_point(size = 1.6) +
  facet_grid(contrast~name, scales = 'free') +
  theme_bw() +
  xlab('') +
  ylab('Athenuation Value')


read_excel("datos actualizatos.xlsx") %>%
  mutate(`braquicefálicos` = ifelse(`Nom pacient` == 'aretha', 'Y', `braquicefálicos`)) %>% 
  clean_datos() %>%
  inner_join(clean_thyroids) %>%
  mutate(Control = ifelse(is.na(Control), "N", "Y")) %>%
  mutate(braqui = ifelse(is.na(braquicefálicos), "N", "Y")) %>% 
  filter(contrast == "NC", Control == "N", braqui == "N")
  
  
  data_to_evaluate %>% 
    filter(Control == 'N') %>% 
    select(athenuation_value, T4, TSH) %>% 
    pivot_longer(-athenuation_value) %>% 
    ggplot(aes(x = athenuation_value, y = value)) +
    geom_point(size = 2) +
    facet_wrap(name~., scale = 'free') +
    theme_bw()
  
  data_to_evaluate %>% 
    filter(Control == 'Y') %>% 
    select(athenuation_value, contrast) %>% 
    # pivot_longer(-athenuation_value) %>% 
    group_by(contrast) %>% 
    summarise(athenuation_value = mean(athenuation_value, na.rm = TRUE))
    ggplot(aes(x = athenuation_value, y = value)) +
    geom_point(size = 2) +
    facet_wrap(name~., scale = 'free') +
    theme_bw()
  
  data_to_evaluate %>% 
    filter(Control == 'N') %>% 
    select(athenuation_value, T4, TSH, contrast) %>% 
    pivot_longer(-c(athenuation_value, contrast)) %>% 
    group_by(name, contrast) %>% 
    do(broom::tidy(lm(value ~ athenuation_value, data = .)))
  
  data_to_evaluate %>% 
    filter(Control == 'Y') %>% 
    select(athenuation_value, T4, TSH, contrast) %>% 
    pivot_longer(-c(athenuation_value, contrast)) %>% 
    group_by(name, contrast) %>% 
    do(broom::tidy(lm(value ~ athenuation_value, data = .)))
  
# Control group stats
  read_excel("datos actualizatos.xlsx") %>%
    clean_datos() %>%
    inner_join(clean_thyroids) %>%
    mutate(Control = ifelse(is.na(Control), "N", "Y")) %>% 
    filter(Control == "Y") %>% 
    select(id, name, race, Control, contrast, athenuation_value) %>% 
    group_by(id, contrast) %>% 
    summarise(athenuation_value = mean(athenuation_value, na.rm = TRUE)) %>% 
    group_by(contrast) %>% 
    summarise(
      average = mean(athenuation_value, na.rm = TRUE),
      standard_deviation = sd(athenuation_value, na.rm = TRUE)
    )

  # parameters for power analysis
  effect = 0.2
  alpha = 0.05
  # perform power analysis
  pwr::pwr.t2n.test(n1 = 600, n2 = 700, d = effect, sig.level = 0.05)
  