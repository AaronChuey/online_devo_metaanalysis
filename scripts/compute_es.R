#This script calculates effect sizes for entries in metalab
#metalab.stanford.edu
#for questions please contact team members on metalab website

complete <- function(...) {
  args = list(...)
  !any(unlist(map(args, ~(is.null(.x) || is.na(.x)))))
}

# on_failure(`%in%`) <- function(call, env) {
#   paste0(deparse(call$x), " is not in ", deparse(call$table))
# }

compute_es <- function(participant_design, x_1 = NA, x_2 = NA, x_dif = NA,
                       SD_1 = NA, SD_2 = NA, SD_dif = NA, n_1 = NA, n_2 = NA,
                       t = NA, f = NA, d = NA, d_var = NA, corr = NA,
                       corr_imputed = NA, r = NA, study_ID = NA, expt_num = NA,
                       special_cases_measures = NA, contrast_sampa = NA) {
  
  assert_that(participant_design %in% c("between", "within_two", "within_one", "precomputed"))
  
  # we introduce variables calles d_calc and d_var_calc to distiguish them from the fields d and d_var, 
  # which are fields where effect sizes were already available from the source of the data
  d_calc <- NA
  d_var_calc <- NA
  es_method <- "missing"
  
  # start of decision tree where effect sizes are calculated differently based on participant design
  # depending on which data is available, effect sizes are calculated differently
  # switches on participant_design = "between", "within_two", or "within_one"
  
  if (participant_design == "precomputed") {
    d_calc <- d
    d_var_calc <- d_var
  }
  
  ####### BETWEEN ########
  if (participant_design == "between") {
    es_method  <- "between"
    #effect size calculation
    if (complete(x_1, x_2, SD_1, SD_2)) {
      pooled_SD <- sqrt(((n_1 - 1) * SD_1 ^ 2 + (n_2 - 1) * SD_2 ^ 2) / (n_1 + n_2 - 2)) # Lipsey & Wilson, 3.14
      d_calc <- (x_1 - x_2) / pooled_SD # Lipsey & Wilson (2001)
    } else if (complete(t)) {
      d_calc <- t * sqrt((n_1 + n_2) / (n_1 * n_2)) # Lipsey & Wilson (2001)
    } else if (complete(f)) {
      d_calc <- sqrt(f * (n_1 + n_2) / (n_1 * n_2)) # Lipsey & Wilson (2001)
    }
    if (complete(n_1, n_2, d_calc)) {
      #now that effect size are calculated, effect size variance is calculated
      d_var_calc <- ((n_1 + n_2) / (n_1 * n_2)) + (d_calc ^ 2 / (2 * (n_1 + n_2)))
    } else if (complete(r)) {
      #if r instead of d is reported, transform for standardization
      d_calc <- 2 * r / sqrt(1 - r ^ 2)
      d_var_calc <- 4 * r_var / ((1 - r ^ 2) ^ 3)
    } else if (complete(d, d_var)) {
      #if d and d_var were already reported, use those values
      d_calc <- d
      d_var_calc <- d_var
    }
    
    ####### WITHIN_TWO ########
  } else if (participant_design == "within_two") {
    if (is.na(corr)) {
      #if correlation between two measures is not reported, use an imputed correlation value
      corr <- corr_imputed
    }
    #effect size calculation
    if (complete(x_1, x_2, SD_1, SD_2)) { 
      pooled_SD <- sqrt((SD_1 ^ 2 + SD_2 ^ 2) / 2) # Lipsey & Wilson (2001)
      d_calc <- (x_1 - x_2) / pooled_SD # Lipsey & Wilson (2001)
      es_method  <- "group_means_two"
    } else if (complete(x_1, x_2, SD_dif)) {
      within_SD <- SD_dif / sqrt(2 * (1 - corr)) # Lipsey & Wilson (2001); Morris & DeShon (2002)
      d_calc <- (x_1 - x_2) / within_SD # Lipsey & Wilson (2001)
      es_method  <- "group_means_two"
    } else if (complete(x_dif, SD_1, SD_2)) {
      pooled_SD <- sqrt((SD_1 ^ 2 + SD_2 ^ 2) / 2) # Lipsey & Wilson (2001)
      d_calc <- x_dif / pooled_SD # Lipsey & Wilson (2001)
      es_method  <- "subj_diff_two"
    } else if (complete(x_dif, SD_dif)) {
      wc <- sqrt(2 * (1 - corr))
      d_calc <- (x_dif / SD_dif) * wc #Morris & DeShon (2002)
      es_method  <- "subj_diff_two"
    } else if (complete(t)) {
      wc <- sqrt(2 * (1 - corr))
      d_calc <- (t / sqrt(n_1)) * wc #Dunlap et al., 1996, p.171
      es_method  <- "t_two"
    } else if (complete(f)) {
      wc <- sqrt(2 * (1 - corr))
      d_calc <- sqrt(f / n_1) * wc
      es_method  <- "f_two"
    }
    if (complete(n_1, d_calc)) {
      #now that effect size are calculated, effect size variance is calculated
      #d_var_calc <- ((1 / n_1) + (d_calc ^ 2 / (2 * n_1))) * 2 * (1 - corr) #we used this until 4/7/17
      d_var_calc <- (2 * (1 - corr)/ n_1) + (d_calc ^ 2 / (2 * n_1)) # Lipsey & Wilson (2001)
    } else  if (complete(r)) {
      #if r instead of d is reported, transform for standardization
      d_calc <- 2 * r / sqrt(1 - r ^ 2)
      d_var_calc <- 4 * r_var / ((1 - r ^ 2) ^ 3)
      es_method  <- "r_two"
    } else if (complete(d, d_var)) {
      #if d and d_var were already reported, use those values
      d_calc <- d
      d_var_calc <- d_var
      es_method  <- "d_two"
    }
    
    ####### WITHIN_ONE ########
  } else if (participant_design == "within_one") {
    if (complete(x_1, x_2, SD_1)) {
      d_calc <- (x_1 - x_2) / SD_1
      es_method  <- "group_means_one"
    } else if (complete(t)) {
      d_calc <- t / sqrt(n_1)
      es_method  <- "t_one"
    } else if (complete(f)) {
      d_calc <- sqrt(f / n_1)
      es_method  <- "f_one"
    }
    if (complete(n_1, d_calc)) {
      #d_var_calc <- (2/n_1) + (d_calc ^ 2 / (4 * n_1)) # we used this until 4/7/2017
      d_var_calc <- (1 / n_1) + (d_calc ^ 2 / (2 * n_1)) # this models what is done in metafor package, escalc(measure="SMCR"() (Viechtbauer, 2010)
      
    } else if (complete(r, n_1)){ 
      # this deals with pointing and vocabulary 
      # Get variance of transformed r (z; fisher's tranformation)
      SE_z = 1 / sqrt(n_1 - 3) # from Howell (2010; "Statistical methods for Psychology", pg 275)
      var_z = SE_z ^ 2
      
      # Transform z variance to r variance
      var_r = tanh(var_z)  # from wikipedia (https://en.wikipedia.org/wiki/Fisher_transformation) for convert z -> r, consistent with Howell
      
      # Transform r to d
      d_calc = 2 * r / (sqrt(1 - r ^ 2)) # from (Hunter and Schmidt, pg. 279)
      d_var_calc = (4 * var_r)/(1 - r ^ 2) ^ 3 # from https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf (pg. 4)
      
      es_method  <- "r_one"
      
    } else if (complete(r)) {
      #if r instead of d is reported, transform for standardization
      d_calc <- 2 * r / sqrt(1 - r ^ 2)
      d_var_calc <- 4 * r_var / ((1 - r ^ 2) ^ 3)
      
      es_method  <- "r_one"
      
    } else  if (complete(d, d_var)) {
      #if d and d_var were already reported, use those values
      d_calc <- d
      d_var_calc <- d_var
      es_method  <- "d_one"
    }
  }
  
  return(data.frame("d_calc" = d_calc, "d_var_calc" = d_var_calc,
                    "es_method" = es_method))
  
}
