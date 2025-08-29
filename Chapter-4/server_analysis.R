
# Load packages
library("tidyverse")
library("lmtp")
library("future")
library("listenv")
library("SuperLearner")
library("glmnet")

# Avoid exponential notation in numbers
options("scipen" = 100, "digits" = 4)

# Clear workspace
rm(list = ls())

# Set global seed
set.seed(999)

# Set last age, truncation and folds, and select SuperLearner models
last_age <- 65
trim <- 0.975
folds <- 10
SL_folds <- 10
SL_libs <- c("SL.glm", "SL.ranger", "SL.xgboost", "SL.glmnet")

# Load data and choose from imputed data sets
load(".../data_final_de.RData")

data_wide <- data_wide %>% mutate(across(starts_with("T1"), ~factor(., levels = c("Renter", "Homeowner"))))

data_wide <- data_wide %>% mutate(across(starts_with("T2"), ~factor(., levels = c("Renter", "Mortgagor", "Outright owner"))))

rm(list = setdiff(ls(), c("data_wide", "last_age", "trim", "folds", "SL_folds", "SL_libs")))

# Create containers for results
results_renters <- listenv()
results_renters_private <- listenv()
results_owners <- listenv()
results_owners_outright <- listenv()
results_owners_mortgage <- listenv()

# Set up parallel computing with max 30 cores
workers <- as.numeric(availableCores())
workers <- pmin(30, workers)

plan(multisession, workers = workers)

for (i in c(51:last_age)) {
  
  outcome <- paste0("Y_", 51:i)
  
  censoring <- paste0("C_", 50:(i - 1))
  
  baseline <- c("B1", "B2", "B3", "B4")
  
  vars <- data_wide %>% select(starts_with(c("L"))) %>% names()
  
  time_vary = list()
  
  for (tp in c(50:(i - 1))) {
    time_vary_new <- c(vars[str_detect(vars, str_pad(tp, 2, pad = "0"))])
    
    time_vary[[length(time_vary) + 1]] <- time_vary_new}
  
  ##### Renters #####
  treatment <- paste0("T1_", 50:(i - 1))
  
  shifted <- data_wide %>% 
    mutate(across(starts_with("T1_"), ~if_else(. %in% c("Renter", "Homeowner"), "Renter", NA))) %>%
    mutate(across(starts_with("T1_"), ~factor(., levels = c("Renter", "Homeowner")))) %>%
    mutate(across(starts_with("C_"), ~replace(1, 1, 1))) %>%
    mutate(hid = as.character(hid))
  
  results_renters[[i]] %<-% {lmtp_tmle(data_wide,
                                       trt = treatment,
                                       outcome = outcome,
                                       baseline = baseline,
                                       time_vary = time_vary,
                                       cens = censoring,
                                       shifted = shifted,
                                       outcome_type = ifelse(i == 51, "binomial", "survival"),
                                       learners_outcome = SL_libs,
                                       learners_trt = SL_libs,
                                       folds = folds,
                                       id = "hid",
                                       weights = data_wide$final_weight,
                                       control = lmtp_control(
                                         .trim = trim,
                                         .learners_outcome_folds = SL_folds,
                                         .learners_trt_folds = SL_folds))} %seed% TRUE
  
  ##### Owners #####
  treatment <- paste0("T1_", 50:(i - 1))
  
  shifted <- data_wide %>% 
    mutate(across(starts_with("T1_"), ~if_else(. %in% c("Renter", "Homeowner"), "Homeowner", NA))) %>%
    mutate(across(starts_with("T1_"), ~factor(., levels = c("Renter", "Homeowner")))) %>%
    mutate(across(starts_with("C_"), ~replace(1, 1, 1))) %>%
    mutate(hid = as.character(hid))
  
  results_owners[[i]] %<-% {lmtp_tmle(data_wide,
                                      trt = treatment,
                                      outcome = outcome,
                                      baseline = baseline,
                                      time_vary = time_vary,
                                      cens = censoring,
                                      shifted = shifted,
                                      outcome_type = ifelse(i == 51, "binomial", "survival"),
                                      learners_outcome = SL_libs,
                                      learners_trt = SL_libs,
                                      folds = folds,
                                      id = "hid",
                                      weights = data_wide$final_weight,
                                      control = lmtp_control(
                                        .trim = trim,
                                        .learners_outcome_folds = SL_folds,
                                        .learners_trt_folds = SL_folds))} %seed% TRUE
  
  ##### Owners outright #####
  treatment <- paste0("T2_", 50:(i - 1))
  
  shifted <- data_wide %>% 
    mutate(across(starts_with("T2_"), ~if_else(. %in% c("Renter", "Mortgagor", "Outright owner"), "Outright owner", NA))) %>%
    mutate(across(starts_with("T2_"), ~factor(., levels = c("Renter", "Mortgagor", "Outright owner")))) %>%
    mutate(across(starts_with("C_"), ~replace(1, 1, 1))) %>%
    mutate(hid = as.character(hid))
  
  results_owners_outright[[i]] %<-% {lmtp_tmle(data_wide,
                                               trt = treatment,
                                               outcome = outcome,
                                               baseline = baseline,
                                               time_vary = time_vary,
                                               cens = censoring,
                                               shifted = shifted,
                                               outcome_type = ifelse(i == 51, "binomial", "survival"),
                                               learners_outcome = SL_libs,
                                               learners_trt = SL_libs,
                                               folds = folds,
                                               id = "hid",
                                               weights = data_wide$final_weight,
                                               control = lmtp_control(
                                                 .trim = trim,
                                                 .learners_outcome_folds = SL_folds,
                                                 .learners_trt_folds = SL_folds))} %seed% TRUE
  
  ##### Owners mortgage #####
  treatment <- paste0("T2_", 50:(i - 1))
  
  shifted <- data_wide %>% 
    mutate(across(starts_with("T2_"), ~if_else(. %in% c("Renter", "Mortgagor", "Outright owner"), "Mortgagor", NA))) %>%
    mutate(across(starts_with("T2_"), ~factor(., levels = c("Renter", "Mortgagor", "Outright owner")))) %>%
    mutate(across(starts_with("C_"), ~replace(1, 1, 1))) %>%
    mutate(hid = as.character(hid))
  
  results_owners_mortgage[[i]] %<-% {lmtp_tmle(data_wide,
                                               trt = treatment,
                                               outcome = outcome,
                                               baseline = baseline,
                                               time_vary = time_vary,
                                               cens = censoring,
                                               shifted = shifted,
                                               outcome_type = ifelse(i == 51, "binomial", "survival"),
                                               learners_outcome = SL_libs,
                                               learners_trt = SL_libs,
                                               folds = folds,
                                               id = "hid",
                                               weights = data_wide$final_weight,
                                               control = lmtp_control(
                                                 .trim = trim,
                                                 .learners_outcome_folds = SL_folds,
                                                 .learners_trt_folds = SL_folds))} %seed% TRUE
  
  ##### Renters in private housing #####
  treatment <- paste0("T3_", 50:(i - 1))
  
  data_wide <- data_wide %>%
    mutate(
      T3_50 = ifelse(T1_50 == "Renter" & T3_50 == "1", "Social renter", as.character(T1_50)),
      T3_51 = ifelse(T1_51 == "Renter" & T3_51 == "1", "Social renter", as.character(T1_51)),
      T3_52 = ifelse(T1_52 == "Renter" & T3_52 == "1", "Social renter", as.character(T1_52)),
      T3_53 = ifelse(T1_53 == "Renter" & T3_53 == "1", "Social renter", as.character(T1_53)),
      T3_54 = ifelse(T1_54 == "Renter" & T3_54 == "1", "Social renter", as.character(T1_54)),
      T3_55 = ifelse(T1_55 == "Renter" & T3_55 == "1", "Social renter", as.character(T1_55)),
      T3_56 = ifelse(T1_56 == "Renter" & T3_56 == "1", "Social renter", as.character(T1_56)),
      T3_57 = ifelse(T1_57 == "Renter" & T3_57 == "1", "Social renter", as.character(T1_57)),
      T3_58 = ifelse(T1_58 == "Renter" & T3_58 == "1", "Social renter", as.character(T1_58)),
      T3_59 = ifelse(T1_59 == "Renter" & T3_59 == "1", "Social renter", as.character(T1_59)),
      T3_60 = ifelse(T1_60 == "Renter" & T3_60 == "1", "Social renter", as.character(T1_60)),
      T3_61 = ifelse(T1_61 == "Renter" & T3_61 == "1", "Social renter", as.character(T1_61)),
      T3_62 = ifelse(T1_62 == "Renter" & T3_62 == "1", "Social renter", as.character(T1_62)),
      T3_63 = ifelse(T1_63 == "Renter" & T3_63 == "1", "Social renter", as.character(T1_63)),
      T3_64 = ifelse(T1_64 == "Renter" & T3_64 == "1", "Social renter", as.character(T1_64)),
      T3_65 = ifelse(T1_65 == "Renter" & T3_65 == "1", "Social renter", as.character(T1_65))) %>%
    mutate(across(starts_with("T3_"), ~factor(., levels = c("Renter", "Social renter", "Homeowner"))))
  
  shifted <- data_wide %>% 
    mutate(across(starts_with("T3_"), ~if_else(. %in% c("Renter", "Social renter", "Homeowner"), "Renter", NA))) %>%
    mutate(across(starts_with("T3_"), ~factor(., levels = c("Renter", "Social renter", "Homeowner")))) %>%
    mutate(across(starts_with("C_"), ~replace(1, 1, 1))) %>%
    mutate(hid = as.character(hid))
  
  results_renters_private[[i]] %<-% {lmtp_tmle(data_wide,
                                               trt = treatment,
                                               outcome = outcome,
                                               baseline = baseline,
                                               time_vary = time_vary,
                                               cens = censoring,
                                               shifted = shifted,
                                               outcome_type = ifelse(i == 51, "binomial", "survival"),
                                               learners_outcome = SL_libs,
                                               learners_trt = SL_libs,
                                               folds = folds,
                                               id = "hid",
                                               weights = data_wide$final_weight,
                                               control = lmtp_control(
                                                 .trim = trim,
                                                 .learners_outcome_folds = SL_folds,
                                                 .learners_trt_folds = SL_folds))} %seed% TRUE
  
  rm(time_vary_new, tp, shifted, treatment)
  
  # clear memory
  gc()
  
}

# Gather results
results_renters <- as.list(results_renters)
results_renters_private <- as.list(results_renters_private)
results_owners <- as.list(results_owners)
results_owners_outright <- as.list(results_owners_outright)
results_owners_mortgage <- as.list(results_owners_mortgage)

rm(treatment, outcome, censoring, time_vary, baseline, workers, i)

# clear memory
gc()


# Clean results
contrasts_renters_owners <- data.frame()
contrasts_renters_private_owners <- data.frame()
contrasts_renters_owners_outright <- data.frame()
contrasts_renters_owners_mortgage <- data.frame()
contrasts_owners_outright_owners_mortgage <- data.frame()

results <- data.frame()

for (i in c(51:last_age)) {
  
  cntr <- lmtp_contrast(results_owners[[i]], ref = results_renters[[i]], type = c("additive"))
  cntr$vals$age <- i
  contrasts_renters_owners <- rbind(contrasts_renters_owners, cntr$vals)
  
  cntr <- lmtp_contrast(results_owners[[i]], ref = results_renters_private[[i]], type = c("additive"))
  cntr$vals$age <- i
  contrasts_renters_private_owners <- rbind(contrasts_renters_private_owners, cntr$vals)
  
  cntr <- lmtp_contrast(results_owners_outright[[i]], ref = results_renters[[i]], type = c("additive"))
  cntr$vals$age <- i
  contrasts_renters_owners_outright <- rbind(contrasts_renters_owners_outright, cntr$vals)
  
  cntr <- lmtp_contrast(results_owners_mortgage[[i]], ref = results_renters[[i]], type = c("additive"))
  cntr$vals$age <- i
  contrasts_renters_owners_mortgage <- rbind(contrasts_renters_owners_mortgage, cntr$vals)
  
  cntr <- lmtp_contrast(results_owners_mortgage[[i]], ref = results_owners_outright[[i]], type = c("additive"))
  cntr$vals$age <- i
  contrasts_owners_outright_owners_mortgage <- rbind(contrasts_owners_outright_owners_mortgage, cntr$vals)
  
  rslt <- tidy(results_renters[[i]])
  rslt$age <- i
  rslt$group <- "Renters"
  results <- rbind(results, rslt)
  
  rslt <- tidy(results_renters_private[[i]])
  rslt$age <- i
  rslt$group <- "Renters"
  results <- rbind(results, rslt)
  
  rslt <- tidy(results_owners[[i]])
  rslt$age <- i
  rslt$group <- "Owners"
  results <- rbind(results, rslt)
  
  rslt <- tidy(results_owners_outright[[i]])
  rslt$age <- i
  rslt$group <- "Owners outright"
  results <- rbind(results, rslt)
  
  rslt <- tidy(results_owners_mortgage[[i]])
  rslt$age <- i
  rslt$group <- "Owners mortgage"
  results <- rbind(results, rslt)
  
  rm(cntr, rslt)
  
}

# Covert probability estimate at age 51 into survival
contrasts_renters_owners$age <- c(51:last_age)
contrasts_renters_owners$theta <- ifelse(contrasts_renters_owners$age == 51, (-1) * contrasts_renters_owners$theta, contrasts_renters_owners$theta)
contrasts_renters_owners$conf.low <- ifelse(contrasts_renters_owners$age == 51, (-1) * contrasts_renters_owners$conf.low, contrasts_renters_owners$conf.low)
contrasts_renters_owners$conf.high <- ifelse(contrasts_renters_owners$age == 51, (-1) * contrasts_renters_owners$conf.high, contrasts_renters_owners$conf.high)

contrasts_renters_private_owners$age <- c(51:last_age)
contrasts_renters_private_owners$theta <- ifelse(contrasts_renters_private_owners$age == 51, (-1) * contrasts_renters_private_owners$theta, contrasts_renters_private_owners$theta)
contrasts_renters_private_owners$conf.low <- ifelse(contrasts_renters_private_owners$age == 51, (-1) * contrasts_renters_private_owners$conf.low, contrasts_renters_private_owners$conf.low)
contrasts_renters_private_owners$conf.high <- ifelse(contrasts_renters_private_owners$age == 51, (-1) * contrasts_renters_private_owners$conf.high, contrasts_renters_private_owners$conf.high)

contrasts_renters_owners_outright$age <- c(51:last_age)
contrasts_renters_owners_outright$theta <- ifelse(contrasts_renters_owners_outright$age == 51, (-1) * contrasts_renters_owners_outright$theta, contrasts_renters_owners_outright$theta)
contrasts_renters_owners_outright$conf.low <- ifelse(contrasts_renters_owners_outright$age == 51, (-1) * contrasts_renters_owners_outright$conf.low, contrasts_renters_owners_outright$conf.low)
contrasts_renters_owners_outright$conf.high <- ifelse(contrasts_renters_owners_outright$age == 51, (-1) * contrasts_renters_owners_outright$conf.high, contrasts_renters_owners_outright$conf.high)

contrasts_renters_owners_mortgage$age <- c(51:last_age)
contrasts_renters_owners_mortgage$theta <- ifelse(contrasts_renters_owners_mortgage$age == 51, (-1) * contrasts_renters_owners_mortgage$theta, contrasts_renters_owners_mortgage$theta)
contrasts_renters_owners_mortgage$conf.low <- ifelse(contrasts_renters_owners_mortgage$age == 51, (-1) * contrasts_renters_owners_mortgage$conf.low, contrasts_renters_owners_mortgage$conf.low)
contrasts_renters_owners_mortgage$conf.high <- ifelse(contrasts_renters_owners_mortgage$age == 51, (-1) * contrasts_renters_owners_mortgage$conf.high, contrasts_renters_owners_mortgage$conf.high)

contrasts_owners_outright_owners_mortgage$age <- c(51:last_age)
contrasts_owners_outright_owners_mortgage$theta <- ifelse(contrasts_owners_outright_owners_mortgage$age == 51, (-1) * contrasts_owners_outright_owners_mortgage$theta, contrasts_owners_outright_owners_mortgage$theta)
contrasts_owners_outright_owners_mortgage$conf.low <- ifelse(contrasts_owners_outright_owners_mortgage$age == 51, (-1) * contrasts_owners_outright_owners_mortgage$conf.low, contrasts_owners_outright_owners_mortgage$conf.low)
contrasts_owners_outright_owners_mortgage$conf.high <- ifelse(contrasts_owners_outright_owners_mortgage$age == 51, (-1) * contrasts_owners_outright_owners_mortgage$conf.high, contrasts_owners_outright_owners_mortgage$conf.high)

results$estimate <- ifelse(results$age == 51, 1 - results$estimate, results$estimate)
results$conf.low <- ifelse(results$age == 51, 1 - results$conf.low, results$conf.low)
results$conf.high <- ifelse(results$age == 51, 1 - results$conf.high, results$conf.high)

rm(i)

#Save results
save.image(".../output_de_imp1.RData")
