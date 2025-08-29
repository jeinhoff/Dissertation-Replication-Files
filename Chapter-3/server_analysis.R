
library("tidyverse")
library("foreach")
library("doParallel")
library("nnet")
library("SuperLearner")

options("scipen" = 100, "digits" = 4)

rm(list = ls())

print("em_marg")

# load functions
source(".../decomp_functions.R")

# load data
load(".../data_final.Rdata")

# define outcome
data_final$outcome <- ifelse(data_final$job_status %in% c("Full-time employed", "Part-time employed"), 1, 0)

# define treatment model
formula_treatment <- formula("position_kldb_2 ~ age + birth_year + state + urban_residence + highest_education_detailed + family_status + has_children + hh_size + migration_background + partner_job_status")

# select variables for outcome model
preds_outcome <- c("position_kldb_2", "age", "birth_year", "state", "urban_residence", "highest_education_detailed", "family_status", "has_children", "hh_size", "migration_background", "partner_job_status")

# select models
sl_models <- c("SL.glm", "SL.ranger", "SL.xgboost")

# set seed
set.seed(1)

# set number of bootstrap iterations
bs_iterations <- 500

# set up and start parallel processing
cluster <- makeCluster(8)

registerDoParallel(cluster)

# run decomposition iterations in parallel
outputs <- foreach(i = 1:bs_iterations, .combine = "rbind", .packages = c("tidyverse", "nnet", "SuperLearner")) %dopar% {
  
  # randomly draw individuals
  sample <- data_final %>% group_by(sex, highest_education, position_kldb_2) %>% slice_sample(prop = 1, replace = T) %>% ungroup()
  sample$person_id <- c(1:nrow(sample))
  
  # run decomposition and store results
  estimates <- decompose_marg_kldb(sample, formula_treatment, preds_outcome, sl_models) #alt: decompose_cond_kldb
  
  # save number of bootstrap iteration
  estimates$bs_iteration <- i
  
  # return bootstrap results
  return(estimates)
  
  rm(list = setdiff(ls(), c("decompose_cond_kldb", "formula_treatment", "preds_outcome", "sl_models", "cluster", "bs_iterations", "i")))
  
  # clear memory
  gc()
  
}

# close parallel processing
stopCluster(cluster)

# clear memory
gc()

# compile results
results <- get_results(outputs)
results$intervention <- "Marginal"
results$outcome <- "Employment"
results$group <- "Total population"

# save results
save.image(".../output_em_cond.RData")
