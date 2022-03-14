# calculate ES for syntactic bootstrapping group
library(tidyverse)
library(here)
library(janitor)
library(gsubfn)



INPATH <- here("data/raw/syntactic_bootstrapping_raw_data.csv")
OUTPATH <- here("data/processed/syntactic_bootstrapping_tidy_data.csv")

# ES function - adopted from compute_es (within-one case)
get_es <- function(df){
  if (!is.na(df$x_1) & !is.na(df$x_2) & !is.na(df$sd_1)) {
    d_calc <- (df$x_1 - df$x_2) / df$sd_1
    es_method  <- "group_means_one"
  } else if (!is.na(df$t)) {
    d_calc <- df$t / sqrt(df$n_1)
    es_method  <- "t_one"
  } else  if (!is.na(df$d)) {
    d_calc <- df$d
    es_method  <- "d_one"
  }
  if(!is.na(d_calc)) {
    d_var_calc <- (1 / df$n_1) + (d_calc ^ 2 / (2 * df$n_1))# this models what is done in metafor package, escalc(measure="SMCR"() (Viechtbauer, 2010)
  }

  data.frame(d_calc = d_calc,
             d_var_calc = d_var_calc,
             es_method = es_method)
}


# read in raw data
ma_data <- read_csv(INPATH)

# add effect sizes
ma_data_with_es <- ma_data %>%
  filter(!is.na(t)| (!is.na(x_1) & !is.na(x_2) & !is.na(sd_1)) | (!is.na(d))) %>%
  nest(data = c(n_1, x_1,x_2,x_2_raw,sd_1,sd_2,sd_2_raw,t,d)) %>%
  mutate(es = map(data, get_es)) %>%
  unnest(cols = c(es,data)) %>%
  mutate(d_calc = ifelse(d_calc == Inf, NA_real_, d_calc),
         d_var_calc = ifelse(d_var_calc == Inf, NA_real_, d_var_calc), 
         mean_age_months = mean_age/30.44, 
         row_id = 1:n())


write_csv(ma_data_with_es, OUTPATH)




