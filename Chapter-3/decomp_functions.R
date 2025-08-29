
decompose_marg_kldb_param <- function(data, formula_treatment, preds_outcome, sl_models) {
  
  n_occupations <- length(unique(sample$position_kldb_2))
  
  # determine counter-factual composition via intervention rule
  composition_cf <- multinom("position_kldb_2 ~ 1", data = sample, family = "binomial", weights = survey_weight, MaxNWts = 10000000)
  
  composition_cf <- as.data.frame(predict(composition_cf, type = "probs"))
  
  colnames(composition_cf) <- paste0("counterfact_share_occ_", colnames(composition_cf))
  
  sample <- cbind(sample, composition_cf)
  
  rm(list = setdiff(ls(), c("sample", "formula_treatment", "preds_outcome", "sl_models", "n_occupations")))
  
  # determine composition within sex
  composition_men <- multinom("position_kldb_2 ~ 1", data = sample %>% filter(sex == "Male"), family = "binomial", weights = survey_weight, MaxNWts = 10000000)
  composition_women <- multinom("position_kldb_2 ~ 1", data = sample %>% filter(sex == "Female"), family = "binomial", weights = survey_weight, MaxNWts = 10000000)
  
  composition_men <- as.data.frame(predict(composition_men, type = "probs")) %>% slice(1)
  composition_women <- as.data.frame(predict(composition_women, type = "probs")) %>% slice(1)
  
  colnames(composition_men) <- paste0("share_occ_men_", colnames(composition_men))
  colnames(composition_women) <- paste0("share_occ_women_", colnames(composition_women))
  
  sample <- sample %>% cross_join(composition_men) %>% cross_join(composition_women)
  
  rm(list = setdiff(ls(), c("sample", "formula_treatment", "preds_outcome", "sl_models", "n_occupations")))
  
  # data by gender
  data_male <- sample %>% filter(sex == "Male") %>% ungroup() %>% mutate(position_kldb_2 = factor(position_kldb_2, levels = sort(unique(sample$position_kldb_2))))
  data_female <- sample %>% filter(sex == "Female") %>% ungroup() %>% mutate(position_kldb_2 = factor(position_kldb_2, levels = sort(unique(sample$position_kldb_2))))

  # train
  model_male <- glm(formula(paste("outcome ~", paste(preds_outcome, collapse = " + "))), data = data_male, family = binomial(), weights = survey_weight)
  model_female <- glm(formula(paste("outcome ~", paste(preds_outcome, collapse = " + "))), data = data_female, family = binomial(), weights = survey_weight)

  # predictions
  for (i in sort(unique(sample$position_kldb_2))) {
    
    data_male$position_kldb_2 <- factor(i, levels = sort(unique(sample$position_kldb_2)))

    data_female$position_kldb_2 <- factor(i, levels = sort(unique(sample$position_kldb_2)))

    data_male$prediction <- as.data.frame(predict(model_male, newdata = data_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_female$prediction <- as.data.frame(predict(model_female, newdata = data_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_male <- data_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)

    data_female <- data_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)

  }
  
  rm(i)
  
  data_combined <- bind_rows(data_male, data_female)
  
  # SELECTION
  levels_selection_male_1 <- data_combined %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_selection_male_2 <- data_combined %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("share_occ_men_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "share_occ") %>% select(-name)
  levels_selection_male <- cbind(levels_selection_male_1, levels_selection_male_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_selection_male_1, levels_selection_male_2)
  
  levels_selection_female_1 <- data_combined %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_selection_female_2 <- data_combined %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("share_occ_women_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "share_occ") %>% select(-name)
  levels_selection_female <- cbind(levels_selection_female_1, levels_selection_female_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_selection_female_1, levels_selection_female_2)
  
  # TOTAL
  levels_post_male_1 <- data_combined %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_post_male_2 <- data_combined %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("counterfact_share_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "counterfact_share_occ") %>% select(-name)
  levels_post_male <- cbind(levels_post_male_1, levels_post_male_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * counterfact_share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_post_male_1, levels_post_male_2)
  
  levels_post_female_1 <- data_combined %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_post_female_2 <- data_combined %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("counterfact_share_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "counterfact_share_occ") %>% select(-name)
  levels_post_female <- cbind(levels_post_female_1, levels_post_female_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * counterfact_share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_post_female_1, levels_post_female_2)
  
  # ESTIMANDS
  
  # OBSERVED
  # observed levels
  levels_pre_female <- sample %>% filter(sex == "Female") %>% ungroup() %>% summarize(exp = weighted.mean(outcome, w = survey_weight) * 100) %>% as.numeric()
  levels_pre_male <- sample %>% filter(sex == "Male") %>% ungroup() %>% summarize(exp = weighted.mean(outcome, w = survey_weight) * 100) %>% as.numeric()
  
  # observed disparity
  gap_pre <- levels_pre_female - levels_pre_male
  
  # COMPONENTS
  # selection component
  selection <- (levels_pre_female - levels_pre_male) - (levels_selection_female - levels_selection_male)
  
  selection_pp <- (selection / gap_pre) * 100
  
  # composition component
  composition <- (levels_selection_female - levels_selection_male) - (levels_post_female - levels_post_male)
  
  composition_pp <- (composition / gap_pre) * 100
  
  # total segregation component
  total <- gap_pre - (levels_post_female - levels_post_male)
  
  total_pp <- (total / gap_pre) * 100
  
  # COUNTERFACTUAL LEVELS
  # levels after differential composition eliminated
  levels_compositon_female <- levels_pre_female - (levels_selection_female - levels_post_female)
  levels_compositon_male <- levels_pre_male - (levels_selection_male - levels_post_male)
  
  # COUNTERFACTUAL DISPARITIES
  # disparity after differential composition eliminated
  gap_post_composition <- levels_compositon_female - levels_compositon_male
  
  # disparity after differential selection eliminated
  gap_post_selection <- levels_selection_female - levels_selection_male
  
  # disparity after segregation eliminated
  gap_post_total <- levels_post_female - levels_post_male
  
  
  estimates_pre_disparity <- data.frame(estimand = "Disparity (observed)", "est" = round(gap_pre, 2))
  estimates_pre_level_male <- data.frame(estimand = "Men (observed)", "est" = round(levels_pre_male, 2))
  estimates_pre_level_female <- data.frame(estimand = "Women (observed)", "est" = round(levels_pre_female, 2))
  
  estimates_composition <- data.frame(estimand = "Due to composition", "est" = round(composition, 2))
  estimates_composition_pp <- data.frame(estimand = "Due to composition (perc.)", "est" = round(composition_pp, 2))
  estimates_composition_disparity <- data.frame(estimand = "Disparity after composition elimiated", "est" = round(gap_post_composition, 2))
  estimates_composition_level_male <- data.frame(estimand = "Men after composition elimiated", "est" = round(levels_compositon_male, 2))
  estimates_composition_level_female <- data.frame(estimand = "Women after composition elimiated", "est" = round(levels_compositon_female, 2))
  
  estimates_selection <- data.frame(estimand = "Due to selection", "est" = round(selection, 2))
  estimates_selection_pp <- data.frame(estimand = "Due to selection (perc.)", "est" = round(selection_pp, 2))
  estimates_selection_disparity <- data.frame(estimand = "Disparity after selection elimiated", "est" = round(gap_post_selection, 2))
  estimates_selection_level_male <- data.frame(estimand = "Men after selection elimiated", "est" = round(levels_selection_male, 2))
  estimates_selection_level_female <- data.frame(estimand = "Women after selection elimiated", "est" = round(levels_selection_female, 2))
  
  estimates_total <- data.frame(estimand = "Due to segregation (composition + selection)", "est" = round(total, 2))
  estimates_total_pp <- data.frame(estimand = "Due to segregation (composition + selection) (perc.)", "est" = round(total_pp, 2))
  estimates_total_disparity <- data.frame(estimand = "Disparity after segregation elimiated", "est" = round(gap_post_total, 2))
  estimates_total_level_male <- data.frame(estimand = "Men after segregation elimiated", "est" = round(levels_post_male, 2))
  estimates_total_level_female <- data.frame(estimand = "Women after segregation elimiated", "est" = round(levels_post_female, 2))
  
  estimates <- rbind(estimates_pre_disparity, estimates_pre_level_male, estimates_pre_level_female,
                     estimates_composition, estimates_composition_pp, estimates_composition_disparity, estimates_composition_level_male, estimates_composition_level_female,
                     estimates_selection, estimates_selection_pp, estimates_selection_disparity, estimates_selection_level_male, estimates_selection_level_female,
                     estimates_total, estimates_total_pp, estimates_total_disparity, estimates_total_level_male, estimates_total_level_female)
  
  rm(list = setdiff(ls(), c("estimates")))
  
  gc()
  
  return(estimates)
  
}
























decompose_cond_kldb_param <- function(data, formula_treatment, preds_outcome, sl_models) {
  
  n_occupations <- length(unique(sample$position_kldb_2))
  
  # determine counter-factual composition via intervention rule
  composition_cf <- multinom("position_kldb_2 ~ highest_education_detailed", data = sample, family = "binomial", weights = survey_weight, MaxNWts = 10000000)
  
  composition_cf <- as.data.frame(predict(composition_cf, type = "probs"))
  
  colnames(composition_cf) <- paste0("counterfact_share_occ_", colnames(composition_cf))
  
  sample <- cbind(sample, composition_cf)
  
  rm(list = setdiff(ls(), c("sample", "formula_treatment", "preds_outcome", "sl_models", "n_occupations")))
  
  # determine composition within sex
  composition_men <- multinom("position_kldb_2 ~ highest_education_detailed", data = sample %>% filter(sex == "Male"), family = "binomial", weights = survey_weight, MaxNWts = 10000000)
  composition_women <- multinom("position_kldb_2 ~ highest_education_detailed", data = sample %>% filter(sex == "Female"), family = "binomial", weights = survey_weight, MaxNWts = 10000000)
  
  composition_men <- as.data.frame(predict(composition_men, type = "probs")) %>% slice(1)
  composition_women <- as.data.frame(predict(composition_women, type = "probs")) %>% slice(1)
  
  colnames(composition_men) <- paste0("share_occ_men_", colnames(composition_men))
  colnames(composition_women) <- paste0("share_occ_women_", colnames(composition_women))
  
  sample <- sample %>% cross_join(composition_men) %>% cross_join(composition_women)
  
  rm(list = setdiff(ls(), c("sample", "formula_treatment", "preds_outcome", "sl_models", "n_occupations")))
  
  # data by gender
  data_male <- sample %>% filter(sex == "Male") %>% ungroup() %>% mutate(position_kldb_2 = factor(position_kldb_2, levels = sort(unique(sample$position_kldb_2))))
  data_female <- sample %>% filter(sex == "Female") %>% ungroup() %>% mutate(position_kldb_2 = factor(position_kldb_2, levels = sort(unique(sample$position_kldb_2))))
  
  # train
  model_male <- glm(formula(paste("outcome ~", paste(preds_outcome, collapse = " + "))), data = data_male, family = binomial(), weights = survey_weight)
  model_female <- glm(formula(paste("outcome ~", paste(preds_outcome, collapse = " + "))), data = data_female, family = binomial(), weights = survey_weight)
  
  # predictions
  for (i in sort(unique(sample$position_kldb_2))) {
    
    data_male$position_kldb_2 <- factor(i, levels = sort(unique(sample$position_kldb_2)))
    
    data_female$position_kldb_2 <- factor(i, levels = sort(unique(sample$position_kldb_2)))
    
    data_male$prediction <- as.data.frame(predict(model_male, newdata = data_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_female$prediction <- as.data.frame(predict(model_female, newdata = data_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_male <- data_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
    data_female <- data_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
  }
  
  rm(i)
  
  data_combined <- bind_rows(data_male, data_female)
  
  # SELECTION
  levels_selection_male_1 <- data_combined %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_selection_male_2 <- data_combined %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("share_occ_men_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "share_occ") %>% select(-name)
  levels_selection_male <- cbind(levels_selection_male_1, levels_selection_male_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_selection_male_1, levels_selection_male_2)
  
  levels_selection_female_1 <- data_combined %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_selection_female_2 <- data_combined %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("share_occ_women_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "share_occ") %>% select(-name)
  levels_selection_female <- cbind(levels_selection_female_1, levels_selection_female_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_selection_female_1, levels_selection_female_2)
  
  # TOTAL
  levels_post_male_1 <- data_combined %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_post_male_2 <- data_combined %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("counterfact_share_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "counterfact_share_occ") %>% select(-name)
  levels_post_male <- cbind(levels_post_male_1, levels_post_male_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * counterfact_share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_post_male_1, levels_post_male_2)
  
  levels_post_female_1 <- data_combined %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_post_female_2 <- data_combined %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("counterfact_share_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "counterfact_share_occ") %>% select(-name)
  levels_post_female <- cbind(levels_post_female_1, levels_post_female_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * counterfact_share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_post_female_1, levels_post_female_2)
  
  # ESTIMANDS
  
  # OBSERVED
  # observed levels
  levels_pre_female <- sample %>% filter(sex == "Female") %>% ungroup() %>% summarize(exp = weighted.mean(outcome, w = survey_weight) * 100) %>% as.numeric()
  levels_pre_male <- sample %>% filter(sex == "Male") %>% ungroup() %>% summarize(exp = weighted.mean(outcome, w = survey_weight) * 100) %>% as.numeric()
  
  # observed disparity
  gap_pre <- levels_pre_female - levels_pre_male
  
  # COMPONENTS
  # selection component
  selection <- (levels_pre_female - levels_pre_male) - (levels_selection_female - levels_selection_male)
  
  selection_pp <- (selection / gap_pre) * 100
  
  # composition component
  composition <- (levels_selection_female - levels_selection_male) - (levels_post_female - levels_post_male)
  
  composition_pp <- (composition / gap_pre) * 100
  
  # total segregation component
  total <- gap_pre - (levels_post_female - levels_post_male)
  
  total_pp <- (total / gap_pre) * 100
  
  # COUNTERFACTUAL LEVELS
  # levels after differential composition eliminated
  levels_compositon_female <- levels_pre_female - (levels_selection_female - levels_post_female)
  levels_compositon_male <- levels_pre_male - (levels_selection_male - levels_post_male)
  
  # COUNTERFACTUAL DISPARITIES
  # disparity after differential composition eliminated
  gap_post_composition <- levels_compositon_female - levels_compositon_male
  
  # disparity after differential selection eliminated
  gap_post_selection <- levels_selection_female - levels_selection_male
  
  # disparity after segregation eliminated
  gap_post_total <- levels_post_female - levels_post_male
  
  
  estimates_pre_disparity <- data.frame(estimand = "Disparity (observed)", "est" = round(gap_pre, 2))
  estimates_pre_level_male <- data.frame(estimand = "Men (observed)", "est" = round(levels_pre_male, 2))
  estimates_pre_level_female <- data.frame(estimand = "Women (observed)", "est" = round(levels_pre_female, 2))
  
  estimates_composition <- data.frame(estimand = "Due to composition", "est" = round(composition, 2))
  estimates_composition_pp <- data.frame(estimand = "Due to composition (perc.)", "est" = round(composition_pp, 2))
  estimates_composition_disparity <- data.frame(estimand = "Disparity after composition elimiated", "est" = round(gap_post_composition, 2))
  estimates_composition_level_male <- data.frame(estimand = "Men after composition elimiated", "est" = round(levels_compositon_male, 2))
  estimates_composition_level_female <- data.frame(estimand = "Women after composition elimiated", "est" = round(levels_compositon_female, 2))
  
  estimates_selection <- data.frame(estimand = "Due to selection", "est" = round(selection, 2))
  estimates_selection_pp <- data.frame(estimand = "Due to selection (perc.)", "est" = round(selection_pp, 2))
  estimates_selection_disparity <- data.frame(estimand = "Disparity after selection elimiated", "est" = round(gap_post_selection, 2))
  estimates_selection_level_male <- data.frame(estimand = "Men after selection elimiated", "est" = round(levels_selection_male, 2))
  estimates_selection_level_female <- data.frame(estimand = "Women after selection elimiated", "est" = round(levels_selection_female, 2))
  
  estimates_total <- data.frame(estimand = "Due to segregation (composition + selection)", "est" = round(total, 2))
  estimates_total_pp <- data.frame(estimand = "Due to segregation (composition + selection) (perc.)", "est" = round(total_pp, 2))
  estimates_total_disparity <- data.frame(estimand = "Disparity after segregation elimiated", "est" = round(gap_post_total, 2))
  estimates_total_level_male <- data.frame(estimand = "Men after segregation elimiated", "est" = round(levels_post_male, 2))
  estimates_total_level_female <- data.frame(estimand = "Women after segregation elimiated", "est" = round(levels_post_female, 2))
  
  estimates <- rbind(estimates_pre_disparity, estimates_pre_level_male, estimates_pre_level_female,
                     estimates_composition, estimates_composition_pp, estimates_composition_disparity, estimates_composition_level_male, estimates_composition_level_female,
                     estimates_selection, estimates_selection_pp, estimates_selection_disparity, estimates_selection_level_male, estimates_selection_level_female,
                     estimates_total, estimates_total_pp, estimates_total_disparity, estimates_total_level_male, estimates_total_level_female)
  
  rm(list = setdiff(ls(), c("estimates")))
  
  gc()
  
  return(estimates)
  
}
































decompose_marg_kldb <- function(data, formula_treatment, preds_outcome, sl_models) {
  
  n_occupations <- length(unique(sample$position_kldb_2))
  
  # determine counter-factual composition via intervention rule
  composition_cf <- multinom("position_kldb_2 ~ 1", data = sample, family = "binomial", weights = survey_weight, MaxNWts = 10000000)
  
  composition_cf <- as.data.frame(predict(composition_cf, type = "probs"))
  
  colnames(composition_cf) <- paste0("counterfact_share_occ_", colnames(composition_cf))
  
  sample <- cbind(sample, composition_cf)
  
  rm(list = setdiff(ls(), c("sample", "formula_treatment", "preds_outcome", "sl_models", "n_occupations")))
  
  # determine composition within sex
  composition_men <- multinom("position_kldb_2 ~ 1", data = sample %>% filter(sex == "Male"), family = "binomial", weights = survey_weight, MaxNWts = 10000000)
  composition_women <- multinom("position_kldb_2 ~ 1", data = sample %>% filter(sex == "Female"), family = "binomial", weights = survey_weight, MaxNWts = 10000000)
  
  composition_men <- as.data.frame(predict(composition_men, type = "probs")) %>% slice(1)
  composition_women <- as.data.frame(predict(composition_women, type = "probs")) %>% slice(1)
  
  colnames(composition_men) <- paste0("share_occ_men_", colnames(composition_men))
  colnames(composition_women) <- paste0("share_occ_women_", colnames(composition_women))
  
  sample <- sample %>% cross_join(composition_men) %>% cross_join(composition_women)
  
  rm(list = setdiff(ls(), c("sample", "formula_treatment", "preds_outcome", "sl_models", "n_occupations")))
  
  # create cross-fitting splits
  split <- split(sample, f = rep_len(1:2, nrow(sample)))
  
  data_split_1 = rbind(split$`1`)
  data_split_1_heldout = split$`2`
  
  data_split_2 = rbind(split$`2`)
  data_split_2_heldout = split$`1`
  
  rm(split)
  
  # treatment model
  data_split_1_male <- data_split_1 %>% filter(sex == "Male") %>% ungroup() %>% mutate(position_kldb_2 = factor(position_kldb_2, levels = sort(unique(sample$position_kldb_2))))
  data_split_2_male <- data_split_2 %>% filter(sex == "Male") %>% ungroup() %>% mutate(position_kldb_2 = factor(position_kldb_2, levels = sort(unique(sample$position_kldb_2))))
  
  data_split_1_female <- data_split_1 %>% filter(sex == "Female") %>% ungroup() %>% mutate(position_kldb_2 = factor(position_kldb_2, levels = sort(unique(sample$position_kldb_2))))
  data_split_2_female <- data_split_2 %>% filter(sex == "Female") %>% ungroup() %>% mutate(position_kldb_2 = factor(position_kldb_2, levels = sort(unique(sample$position_kldb_2))))
  
  data_split_1_heldout_male <- data_split_1_heldout %>% filter(sex == "Male") %>% ungroup() %>% mutate(position_kldb_2 = factor(position_kldb_2, levels = sort(unique(sample$position_kldb_2))))
  data_split_2_heldout_male <- data_split_2_heldout %>% filter(sex == "Male") %>% ungroup() %>% mutate(position_kldb_2 = factor(position_kldb_2, levels = sort(unique(sample$position_kldb_2))))
  
  data_split_1_heldout_female <- data_split_1_heldout %>% filter(sex == "Female") %>% ungroup() %>% mutate(position_kldb_2 = factor(position_kldb_2, levels = sort(unique(sample$position_kldb_2))))
  data_split_2_heldout_female <- data_split_2_heldout %>% filter(sex == "Female") %>% ungroup() %>% mutate(position_kldb_2 = factor(position_kldb_2, levels = sort(unique(sample$position_kldb_2))))
  
  # train
  
  # men
  model_male_split_1 <- multinom(formula_treatment, data = data_split_1_male, family = "binomial", weights = survey_weight, MaxNWts = 10000000)
  model_male_split_2 <- multinom(formula_treatment, data = data_split_2_male, family = "binomial", weights = survey_weight, MaxNWts = 10000000)
  
  # women
  model_female_split_1 <- multinom(formula_treatment, data = data_split_1_female, family = "binomial", weights = survey_weight, MaxNWts = 10000000)
  model_female_split_2 <- multinom(formula_treatment, data = data_split_2_female, family = "binomial", weights = survey_weight, MaxNWts = 10000000)
  
  # predict using model trained on other split
  # men
  data_split_1_heldout_male <- cbind(data_split_1_heldout_male, as.data.frame(predict(model_male_split_1, newdata = data_split_1_heldout_male, type = "probs")))
  data_split_1_heldout_male <- data_split_1_heldout_male %>% rename_at(vars(starts_with("(")), funs(paste0('prop_occ_', .)))
  
  data_split_2_heldout_male <- cbind(data_split_2_heldout_male, as.data.frame(predict(model_male_split_2, newdata = data_split_2_heldout_male, type = "probs")))
  data_split_2_heldout_male <- data_split_2_heldout_male %>% rename_at(vars(starts_with("(")), funs(paste0('prop_occ_', .)))
  
  # women
  data_split_1_heldout_female <- cbind(data_split_1_heldout_female, as.data.frame(predict(model_female_split_1, newdata = data_split_1_heldout_female, type = "probs")))
  data_split_1_heldout_female <- data_split_1_heldout_female %>% rename_at(vars(starts_with("(")), funs(paste0('prop_occ_', .)))
  
  data_split_2_heldout_female <- cbind(data_split_2_heldout_female, as.data.frame(predict(model_female_split_2, newdata = data_split_2_heldout_female, type = "probs")))
  data_split_2_heldout_female <- data_split_2_heldout_female %>% rename_at(vars(starts_with("(")), funs(paste0('prop_occ_', .)))
  
  # combine
  data_split_1_heldout <- rbind(data_split_1_heldout_male, data_split_1_heldout_female)
  data_split_2_heldout <- rbind(data_split_2_heldout_male, data_split_2_heldout_female)
  
  data_split_1_heldout <- data_split_1_heldout %>% as.data.frame() %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% as.data.frame() %>% ungroup()
  
  rm(list = setdiff(ls(), c("sample", "formula_treatment", "preds_outcome", "sl_models", "n_occupations", "data_split_1_male", "data_split_2_male", "data_split_1_female", "data_split_2_female", "data_split_1_heldout", "data_split_1_heldout_male", "data_split_1_heldout_female", "data_split_2_heldout", "data_split_2_heldout_male", "data_split_2_heldout_female")))
  
  # train
  # men
  model_male_split_1 <- SuperLearner(Y = data_split_1_male$outcome, X = data_split_1_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_1_male$survey_weight, cvControl = list(V = 10))
  model_male_split_2 <- SuperLearner(Y = data_split_2_male$outcome, X = data_split_2_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_2_male$survey_weight, cvControl = list(V = 10))
  
  # women
  model_female_split_1 <- SuperLearner(Y = data_split_1_female$outcome, X = data_split_1_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_1_female$survey_weight, cvControl = list(V = 10))
  model_female_split_2 <- SuperLearner(Y = data_split_2_female$outcome, X = data_split_2_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_2_female$survey_weight, cvControl = list(V = 10))
  
  # predictions
  for (i in sort(unique(sample$position_kldb_2))) {
    
    data_split_1_heldout_male$position_kldb_2 <- factor(i, levels = sort(unique(sample$position_kldb_2)))
    data_split_2_heldout_male$position_kldb_2 <- factor(i, levels = sort(unique(sample$position_kldb_2)))
    
    data_split_1_heldout_female$position_kldb_2 <- factor(i, levels = sort(unique(sample$position_kldb_2)))
    data_split_2_heldout_female$position_kldb_2 <- factor(i, levels = sort(unique(sample$position_kldb_2)))
    
    data_split_1_heldout_male$prediction <- as.data.frame(predict(model_male_split_1, newdata = data_split_1_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_2_heldout_male$prediction <- as.data.frame(predict(model_male_split_2, newdata = data_split_2_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_split_1_heldout_female$prediction <- as.data.frame(predict(model_female_split_1, newdata = data_split_1_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_2_heldout_female$prediction <- as.data.frame(predict(model_female_split_2, newdata = data_split_2_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_split_1_heldout_male <- data_split_1_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_2_heldout_male <- data_split_2_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
    data_split_1_heldout_female <- data_split_1_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_2_heldout_female <- data_split_2_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
  }
  
  rm(model_male_split_1, model_male_split_2, model_female_split_1, model_female_split_2)
  
  rm(i)
  
  # combine
  preds_split_1 <- bind_rows(data_split_1_heldout_male, data_split_1_heldout_female)
  preds_split_2 <- bind_rows(data_split_2_heldout_male, data_split_2_heldout_female)
  
  preds_split_1 <- preds_split_1 %>% select(person_id, age, contains("exp_outcome_occ_"))
  preds_split_2 <- preds_split_2 %>% select(person_id, age, contains("exp_outcome_occ_"))
  
  data_split_1_heldout <- data_split_1_heldout %>% left_join(preds_split_1, by = c("person_id")) %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% left_join(preds_split_2, by = c("person_id")) %>% ungroup()
  
  # counterfactual probability of being assigned to the factual occupation
  cf_prob_fc_occ <- sample %>% ungroup() %>%
    select(person_id, position_kldb_2, contains("counterfact_share_occ")) %>%
    pivot_longer(cols = c(3:n_occupations), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 23, 500)) %>% 
    filter(position_kldb_2 == occupation) %>%
    rename("cf_prob_fc_occ" = "value") %>%
    select(person_id, cf_prob_fc_occ) %>%
    group_by(person_id) %>% slice(1) %>% ungroup()
  
  # inverse of the factual probability of the factual occupation
  inv_fc_prob_fc_occ_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(person_id, position_kldb_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(3:n_occupations), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 500)) %>% 
    filter(position_kldb_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(person_id, inv_fc_prob_fc_occ) %>%
    group_by(person_id) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(person_id, position_kldb_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(3:n_occupations), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 500)) %>% 
    filter(position_kldb_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(person_id, inv_fc_prob_fc_occ) %>%
    group_by(person_id) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  # outcome predictions
  exp_outcome_fc_occ_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(person_id, position_kldb_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(3:n_occupations), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 500)) %>% 
    filter(position_kldb_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(person_id, exp_outcome_fc_occ) %>%
    group_by(person_id) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(person_id, position_kldb_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(3:n_occupations), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 500)) %>% 
    filter(position_kldb_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(person_id, exp_outcome_fc_occ) %>%
    group_by(person_id) %>% slice(1) %>% ungroup()
  
  # create combined weight
  weight_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(person_id, survey_weight) %>% 
    left_join(cf_prob_fc_occ, by = c("person_id")) %>%
    left_join(inv_fc_prob_fc_occ_split_1, by = c("person_id")) %>%
    mutate(combined_weight = survey_weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(person_id, combined_weight) %>%
    group_by(person_id) %>% slice(1) %>% ungroup()
  
  weight_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(person_id, survey_weight) %>% 
    left_join(cf_prob_fc_occ, by = c("person_id")) %>%
    left_join(inv_fc_prob_fc_occ_split_2, by = c("person_id")) %>%
    mutate(combined_weight = survey_weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(person_id, combined_weight) %>%
    group_by(person_id) %>% slice(1) %>% ungroup()
  
  # join
  data_split_1_heldout <- data_split_1_heldout %>% left_join(exp_outcome_fc_occ_split_1, by = c("person_id")) %>% left_join(weight_split_1, by = c("person_id")) %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% left_join(exp_outcome_fc_occ_split_2, by = c("person_id")) %>% left_join(weight_split_2, by = c("person_id")) %>% ungroup()
  
  # calculate bias correction
  bias_correction_m_split_1 <- data_split_1_heldout %>% filter(sex == "Male") %>% ungroup() %>%
    summarise(sex = sex, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight, na.rm = T)) %>% unique()
  
  bias_correction_m_split_2 <- data_split_2_heldout %>% filter(sex == "Male") %>% ungroup() %>%
    summarise(sex = sex, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight, na.rm = T)) %>% unique()
  
  bias_correction_f_split_1 <- data_split_1_heldout %>% filter(sex == "Female") %>% ungroup() %>%
    summarise(sex = sex, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight, na.rm = T)) %>% unique()
  
  bias_correction_f_split_2 <- data_split_2_heldout %>% filter(sex == "Female") %>% ungroup() %>%
    summarise(sex = sex, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight, na.rm = T)) %>% unique()
  
  bias_correction_split_1 <- rbind(bias_correction_m_split_1, bias_correction_f_split_1)
  bias_correction_split_2 <- rbind(bias_correction_m_split_2, bias_correction_f_split_2)
  
  # correct bias in outcome predictions
  data_split_1_heldout <- data_split_1_heldout %>% 
    left_join(bias_correction_split_1, by = "sex") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction))
  
  data_split_2_heldout <- data_split_2_heldout %>% 
    left_join(bias_correction_split_2, by = "sex") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction))
  
  rm(list = setdiff(ls(), c("sample", "data_split_1_heldout", "data_split_2_heldout")))
  
  # SELECTION
  
  # split 1
  levels_selection_male_split_1_1 <- data_split_1_heldout %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_selection_male_split_1_2 <- data_split_1_heldout %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("share_occ_men_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "share_occ") %>% select(-name)
  levels_selection_male_split_1 <- cbind(levels_selection_male_split_1_1, levels_selection_male_split_1_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_selection_male_split_1_1, levels_selection_male_split_1_2)
  
  levels_selection_female_split_1_1 <- data_split_1_heldout %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_selection_female_split_1_2 <- data_split_1_heldout %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("share_occ_women_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "share_occ") %>% select(-name)
  levels_selection_female_split_1 <- cbind(levels_selection_female_split_1_1, levels_selection_female_split_1_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_selection_female_split_1_1, levels_selection_female_split_1_2)
  
  # split 2
  levels_selection_male_split_2_1 <- data_split_2_heldout %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_selection_male_split_2_2 <- data_split_2_heldout %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("share_occ_men_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "share_occ") %>% select(-name)
  levels_selection_male_split_2 <- cbind(levels_selection_male_split_2_1, levels_selection_male_split_2_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_selection_male_split_1_1, levels_selection_male_split_1_2)
  
  levels_selection_female_split_2_1 <- data_split_2_heldout %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_selection_female_split_2_2 <- data_split_2_heldout %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("share_occ_women_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "share_occ") %>% select(-name)
  levels_selection_female_split_2 <- cbind(levels_selection_female_split_2_1, levels_selection_female_split_2_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_selection_female_split_2_1, levels_selection_female_split_2_2)
  
  # average cross-fits
  levels_selection_male <- (levels_selection_male_split_1 + levels_selection_male_split_2) / 2
  levels_selection_female <- (levels_selection_female_split_1 + levels_selection_female_split_2) / 2
  
  # TOTAL
  
  # split 1
  levels_post_male_split_1_1 <- data_split_1_heldout %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_post_male_split_1_2 <- data_split_1_heldout %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("counterfact_share_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "counterfact_share_occ") %>% select(-name)
  levels_post_male_split_1 <- cbind(levels_post_male_split_1_1, levels_post_male_split_1_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * counterfact_share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_post_male_split_1_1, levels_post_male_split_1_2)
  
  levels_post_female_split_1_1 <- data_split_1_heldout %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_post_female_split_1_2 <- data_split_1_heldout %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("counterfact_share_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "counterfact_share_occ") %>% select(-name)
  levels_post_female_split_1 <- cbind(levels_post_female_split_1_1, levels_post_female_split_1_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * counterfact_share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_post_female_split_1_1, levels_post_female_split_1_2)
  
  # split 2
  levels_post_male_split_2_1 <- data_split_2_heldout %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_post_male_split_2_2 <- data_split_2_heldout %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("counterfact_share_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "counterfact_share_occ") %>% select(-name)
  levels_post_male_split_2 <- cbind(levels_post_male_split_2_1, levels_post_male_split_2_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * counterfact_share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_post_male_split_2_1, levels_post_male_split_2_2)
  
  levels_post_female_split_2_1 <- data_split_2_heldout %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_post_female_split_2_2 <- data_split_2_heldout %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("counterfact_share_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "counterfact_share_occ") %>% select(-name)
  levels_post_female_split_2 <- cbind(levels_post_female_split_2_1, levels_post_female_split_2_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * counterfact_share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_post_female_split_2_1, levels_post_female_split_2_2)
  
  # average cross-fits
  levels_post_female <- (levels_post_female_split_1 + levels_post_female_split_2) / 2
  levels_post_male <- (levels_post_male_split_1 + levels_post_male_split_2) / 2
  
  # ESTIMANDS
  
  # OBSERVED
  # observed levels
  levels_pre_female <- sample %>% filter(sex == "Female") %>% ungroup() %>% summarize(exp = weighted.mean(outcome, w = survey_weight) * 100) %>% as.numeric()
  levels_pre_male <- sample %>% filter(sex == "Male") %>% ungroup() %>% summarize(exp = weighted.mean(outcome, w = survey_weight) * 100) %>% as.numeric()
  
  # observed disparity
  gap_pre <- levels_pre_female - levels_pre_male
  
  # COMPONENTS
  # selection component
  selection <- (levels_pre_female - levels_pre_male) - (levels_selection_female - levels_selection_male)
  
  selection_pp <- (selection / gap_pre) * 100
  
  # composition component
  composition <- (levels_selection_female - levels_selection_male) - (levels_post_female - levels_post_male)
  
  composition_pp <- (composition / gap_pre) * 100
  
  # total segregation component
  total <- gap_pre - (levels_post_female - levels_post_male)
  
  total_pp <- (total / gap_pre) * 100
  
  # COUNTERFACTUAL LEVELS
  # levels after differential composition elimiated
  levels_compositon_female <- levels_pre_female - (levels_selection_female - levels_post_female)
  levels_compositon_male <- levels_pre_male - (levels_selection_male - levels_post_male)
  
  # COUNTERFACTUAL DISPARITIES
  # disparity after differential composition elimiated
  gap_post_composition <- levels_compositon_female - levels_compositon_male
  
  # disparity after differential selection elimiated
  gap_post_selection <- levels_selection_female - levels_selection_male
  
  # disparity after segregation elimiated
  gap_post_total <- levels_post_female - levels_post_male
  
  
  estimates_pre_disparity <- data.frame(estimand = "Disparity (observed)", "est" = round(gap_pre, 2))
  estimates_pre_level_male <- data.frame(estimand = "Men (observed)", "est" = round(levels_pre_male, 2))
  estimates_pre_level_female <- data.frame(estimand = "Women (observed)", "est" = round(levels_pre_female, 2))
  
  estimates_composition <- data.frame(estimand = "Due to composition", "est" = round(composition, 2))
  estimates_composition_pp <- data.frame(estimand = "Due to composition (perc.)", "est" = round(composition_pp, 2))
  estimates_composition_disparity <- data.frame(estimand = "Disparity after composition elimiated", "est" = round(gap_post_composition, 2))
  estimates_composition_level_male <- data.frame(estimand = "Men after composition elimiated", "est" = round(levels_compositon_male, 2))
  estimates_composition_level_female <- data.frame(estimand = "Women after composition elimiated", "est" = round(levels_compositon_female, 2))
  
  estimates_selection <- data.frame(estimand = "Due to selection", "est" = round(selection, 2))
  estimates_selection_pp <- data.frame(estimand = "Due to selection (perc.)", "est" = round(selection_pp, 2))
  estimates_selection_disparity <- data.frame(estimand = "Disparity after selection elimiated", "est" = round(gap_post_selection, 2))
  estimates_selection_level_male <- data.frame(estimand = "Men after selection elimiated", "est" = round(levels_selection_male, 2))
  estimates_selection_level_female <- data.frame(estimand = "Women after selection elimiated", "est" = round(levels_selection_female, 2))
  
  estimates_total <- data.frame(estimand = "Due to segregation (composition + selection)", "est" = round(total, 2))
  estimates_total_pp <- data.frame(estimand = "Due to segregation (composition + selection) (perc.)", "est" = round(total_pp, 2))
  estimates_total_disparity <- data.frame(estimand = "Disparity after segregation elimiated", "est" = round(gap_post_total, 2))
  estimates_total_level_male <- data.frame(estimand = "Men after segregation elimiated", "est" = round(levels_post_male, 2))
  estimates_total_level_female <- data.frame(estimand = "Women after segregation elimiated", "est" = round(levels_post_female, 2))
  
  estimates <- rbind(estimates_pre_disparity, estimates_pre_level_male, estimates_pre_level_female,
                     estimates_composition, estimates_composition_pp, estimates_composition_disparity, estimates_composition_level_male, estimates_composition_level_female,
                     estimates_selection, estimates_selection_pp, estimates_selection_disparity, estimates_selection_level_male, estimates_selection_level_female,
                     estimates_total, estimates_total_pp, estimates_total_disparity, estimates_total_level_male, estimates_total_level_female)
  
  rm(list = setdiff(ls(), c("estimates")))
  
  gc()
  
  return(estimates)
  
}












decompose_cond_kldb <- function(data, formula_treatment, preds_outcome, sl_models) {
  
  n_occupations <- length(unique(sample$position_kldb_2))
  
  # determine counter-factual composition via intervention rule
  composition_cf <- multinom("position_kldb_2 ~ highest_education_detailed", data = sample, family = "binomial", weights = survey_weight, MaxNWts = 10000000)
  
  composition_cf <- as.data.frame(predict(composition_cf, type = "probs"))
  
  colnames(composition_cf) <- paste0("counterfact_share_occ_", colnames(composition_cf))
  
  sample <- cbind(sample, composition_cf)
  
  rm(list = setdiff(ls(), c("sample", "formula_treatment", "preds_outcome", "sl_models", "n_occupations")))
  
  # determine composition within sex
  composition_men <- multinom("position_kldb_2 ~ highest_education_detailed", data = sample %>% filter(sex == "Male"), family = "binomial", weights = survey_weight, MaxNWts = 10000000)
  composition_women <- multinom("position_kldb_2 ~ highest_education_detailed", data = sample %>% filter(sex == "Female"), family = "binomial", weights = survey_weight, MaxNWts = 10000000)
  
  composition_men <- as.data.frame(predict(composition_men, type = "probs")) %>% slice(1)
  composition_women <- as.data.frame(predict(composition_women, type = "probs")) %>% slice(1)
  
  colnames(composition_men) <- paste0("share_occ_men_", colnames(composition_men))
  colnames(composition_women) <- paste0("share_occ_women_", colnames(composition_women))
  
  sample <- sample %>% cross_join(composition_men) %>% cross_join(composition_women)
  
  rm(list = setdiff(ls(), c("sample", "formula_treatment", "preds_outcome", "sl_models", "n_occupations")))
  
  # create cross-fitting splits
  split <- split(sample, f = rep_len(1:2, nrow(sample)))
  
  data_split_1 = rbind(split$`1`)
  data_split_1_heldout = split$`2`
  
  data_split_2 = rbind(split$`2`)
  data_split_2_heldout = split$`1`
  
  rm(split)
  
  # treatment model
  data_split_1_male <- data_split_1 %>% filter(sex == "Male") %>% ungroup() %>% mutate(position_kldb_2 = factor(position_kldb_2, levels = sort(unique(sample$position_kldb_2))))
  data_split_2_male <- data_split_2 %>% filter(sex == "Male") %>% ungroup() %>% mutate(position_kldb_2 = factor(position_kldb_2, levels = sort(unique(sample$position_kldb_2))))
  
  data_split_1_female <- data_split_1 %>% filter(sex == "Female") %>% ungroup() %>% mutate(position_kldb_2 = factor(position_kldb_2, levels = sort(unique(sample$position_kldb_2))))
  data_split_2_female <- data_split_2 %>% filter(sex == "Female") %>% ungroup() %>% mutate(position_kldb_2 = factor(position_kldb_2, levels = sort(unique(sample$position_kldb_2))))
  
  data_split_1_heldout_male <- data_split_1_heldout %>% filter(sex == "Male") %>% ungroup() %>% mutate(position_kldb_2 = factor(position_kldb_2, levels = sort(unique(sample$position_kldb_2))))
  data_split_2_heldout_male <- data_split_2_heldout %>% filter(sex == "Male") %>% ungroup() %>% mutate(position_kldb_2 = factor(position_kldb_2, levels = sort(unique(sample$position_kldb_2))))
  
  data_split_1_heldout_female <- data_split_1_heldout %>% filter(sex == "Female") %>% ungroup() %>% mutate(position_kldb_2 = factor(position_kldb_2, levels = sort(unique(sample$position_kldb_2))))
  data_split_2_heldout_female <- data_split_2_heldout %>% filter(sex == "Female") %>% ungroup() %>% mutate(position_kldb_2 = factor(position_kldb_2, levels = sort(unique(sample$position_kldb_2))))
  
  # train
  
  # men
  model_male_split_1 <- multinom(formula_treatment, data = data_split_1_male, family = "binomial", weights = survey_weight, MaxNWts = 10000000)
  model_male_split_2 <- multinom(formula_treatment, data = data_split_2_male, family = "binomial", weights = survey_weight, MaxNWts = 10000000)
  
  # women
  model_female_split_1 <- multinom(formula_treatment, data = data_split_1_female, family = "binomial", weights = survey_weight, MaxNWts = 10000000)
  model_female_split_2 <- multinom(formula_treatment, data = data_split_2_female, family = "binomial", weights = survey_weight, MaxNWts = 10000000)
  
  # predict using model trained on other split
  # men
  data_split_1_heldout_male <- cbind(data_split_1_heldout_male, as.data.frame(predict(model_male_split_1, newdata = data_split_1_heldout_male, type = "probs")))
  data_split_1_heldout_male <- data_split_1_heldout_male %>% rename_at(vars(starts_with("(")), funs(paste0('prop_occ_', .)))
  
  data_split_2_heldout_male <- cbind(data_split_2_heldout_male, as.data.frame(predict(model_male_split_2, newdata = data_split_2_heldout_male, type = "probs")))
  data_split_2_heldout_male <- data_split_2_heldout_male %>% rename_at(vars(starts_with("(")), funs(paste0('prop_occ_', .)))
  
  # women
  data_split_1_heldout_female <- cbind(data_split_1_heldout_female, as.data.frame(predict(model_female_split_1, newdata = data_split_1_heldout_female, type = "probs")))
  data_split_1_heldout_female <- data_split_1_heldout_female %>% rename_at(vars(starts_with("(")), funs(paste0('prop_occ_', .)))
  
  data_split_2_heldout_female <- cbind(data_split_2_heldout_female, as.data.frame(predict(model_female_split_2, newdata = data_split_2_heldout_female, type = "probs")))
  data_split_2_heldout_female <- data_split_2_heldout_female %>% rename_at(vars(starts_with("(")), funs(paste0('prop_occ_', .)))
  
  # combine
  data_split_1_heldout <- rbind(data_split_1_heldout_male, data_split_1_heldout_female)
  data_split_2_heldout <- rbind(data_split_2_heldout_male, data_split_2_heldout_female)
  
  data_split_1_heldout <- data_split_1_heldout %>% as.data.frame() %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% as.data.frame() %>% ungroup()
  
  rm(list = setdiff(ls(), c("sample", "formula_treatment", "preds_outcome", "sl_models", "n_occupations", "data_split_1_male", "data_split_2_male", "data_split_1_female", "data_split_2_female", "data_split_1_heldout", "data_split_1_heldout_male", "data_split_1_heldout_female", "data_split_2_heldout", "data_split_2_heldout_male", "data_split_2_heldout_female")))
  
  # train
  # men
  model_male_split_1 <- SuperLearner(Y = data_split_1_male$outcome, X = data_split_1_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_1_male$survey_weight, cvControl = list(V = 10))
  model_male_split_2 <- SuperLearner(Y = data_split_2_male$outcome, X = data_split_2_male %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_2_male$survey_weight, cvControl = list(V = 10))
  
  # women
  model_female_split_1 <- SuperLearner(Y = data_split_1_female$outcome, X = data_split_1_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_1_female$survey_weight, cvControl = list(V = 10))
  model_female_split_2 <- SuperLearner(Y = data_split_2_female$outcome, X = data_split_2_female %>% select(all_of(preds_outcome)), SL.library = sl_models, family = binomial(), obsWeights = data_split_2_female$survey_weight, cvControl = list(V = 10))
  
  # predictions
  for (i in sort(unique(sample$position_kldb_2))) {
    
    data_split_1_heldout_male$position_kldb_2 <- factor(i, levels = sort(unique(sample$position_kldb_2)))
    data_split_2_heldout_male$position_kldb_2 <- factor(i, levels = sort(unique(sample$position_kldb_2)))
    
    data_split_1_heldout_female$position_kldb_2 <- factor(i, levels = sort(unique(sample$position_kldb_2)))
    data_split_2_heldout_female$position_kldb_2 <- factor(i, levels = sort(unique(sample$position_kldb_2)))
    
    data_split_1_heldout_male$prediction <- as.data.frame(predict(model_male_split_1, newdata = data_split_1_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_2_heldout_male$prediction <- as.data.frame(predict(model_male_split_2, newdata = data_split_2_heldout_male %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_split_1_heldout_female$prediction <- as.data.frame(predict(model_female_split_1, newdata = data_split_1_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    data_split_2_heldout_female$prediction <- as.data.frame(predict(model_female_split_2, newdata = data_split_2_heldout_female %>% select(all_of(preds_outcome)), type = "response"))$pred
    
    data_split_1_heldout_male <- data_split_1_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_2_heldout_male <- data_split_2_heldout_male %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
    data_split_1_heldout_female <- data_split_1_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    data_split_2_heldout_female <- data_split_2_heldout_female %>% rename_with(~paste0("exp_outcome_occ_", i), .cols = prediction)
    
  }
  
  rm(model_male_split_1, model_male_split_2, model_female_split_1, model_female_split_2)
  
  rm(i)
  
  # combine
  preds_split_1 <- rbind(data_split_1_heldout_male, data_split_1_heldout_female)
  preds_split_2 <- rbind(data_split_2_heldout_male, data_split_2_heldout_female)
  
  preds_split_1 <- preds_split_1 %>% select(person_id, age, contains("exp_outcome_occ_"))
  preds_split_2 <- preds_split_2 %>% select(person_id, age, contains("exp_outcome_occ_"))
  
  data_split_1_heldout <- data_split_1_heldout %>% left_join(preds_split_1, by = c("person_id")) %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% left_join(preds_split_2, by = c("person_id")) %>% ungroup()
  
  # counterfactual probability of being assigned to the factual occupation
  cf_prob_fc_occ <- sample %>% ungroup() %>%
    select(person_id, position_kldb_2, contains("counterfact_share_occ")) %>%
    pivot_longer(cols = c(3:n_occupations), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 23, 500)) %>% 
    filter(position_kldb_2 == occupation) %>%
    rename("cf_prob_fc_occ" = "value") %>%
    select(person_id, cf_prob_fc_occ) %>%
    group_by(person_id) %>% slice(1) %>% ungroup()
  
  # inverse of the factual probability of the factual occupation
  inv_fc_prob_fc_occ_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(person_id, position_kldb_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(3:n_occupations), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 500)) %>% 
    filter(position_kldb_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(person_id, inv_fc_prob_fc_occ) %>%
    group_by(person_id) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  inv_fc_prob_fc_occ_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(person_id, position_kldb_2, contains("prop_occ")) %>%
    pivot_longer(cols = c(3:n_occupations), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 10, 500)) %>% 
    filter(position_kldb_2 == occupation) %>%
    mutate(value = 1 / value) %>%
    rename("inv_fc_prob_fc_occ" = "value") %>%
    select(person_id, inv_fc_prob_fc_occ) %>%
    group_by(person_id) %>% slice(1) %>% ungroup() %>%
    mutate(inv_fc_prob_fc_occ = case_when(inv_fc_prob_fc_occ > quantile(inv_fc_prob_fc_occ, 0.999) ~ quantile(inv_fc_prob_fc_occ, 0.999),
                                          inv_fc_prob_fc_occ < quantile(inv_fc_prob_fc_occ, 0.001) ~ quantile(inv_fc_prob_fc_occ, 0.001),
                                          T ~ inv_fc_prob_fc_occ))
  
  # outcome predictions
  exp_outcome_fc_occ_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(person_id, position_kldb_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(3:n_occupations), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 500)) %>% 
    filter(position_kldb_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(person_id, exp_outcome_fc_occ) %>%
    group_by(person_id) %>% slice(1) %>% ungroup()
  
  exp_outcome_fc_occ_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(person_id, position_kldb_2, contains("exp_outcome_occ")) %>%
    pivot_longer(cols = c(3:n_occupations), names_to = "occupation") %>% 
    mutate(occupation = substr(occupation, 17, 500)) %>% 
    filter(position_kldb_2 == occupation) %>%
    rename("exp_outcome_fc_occ" = "value") %>%
    select(person_id, exp_outcome_fc_occ) %>%
    group_by(person_id) %>% slice(1) %>% ungroup()
  
  # create combined weight
  weight_split_1 <- data_split_1_heldout %>% ungroup() %>%
    select(person_id, survey_weight) %>% 
    left_join(cf_prob_fc_occ, by = c("person_id")) %>%
    left_join(inv_fc_prob_fc_occ_split_1, by = c("person_id")) %>%
    mutate(combined_weight = survey_weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(person_id, combined_weight) %>%
    group_by(person_id) %>% slice(1) %>% ungroup()
  
  weight_split_2 <- data_split_2_heldout %>% ungroup() %>%
    select(person_id, survey_weight) %>% 
    left_join(cf_prob_fc_occ, by = c("person_id")) %>%
    left_join(inv_fc_prob_fc_occ_split_2, by = c("person_id")) %>%
    mutate(combined_weight = survey_weight * cf_prob_fc_occ * inv_fc_prob_fc_occ) %>%
    select(person_id, combined_weight) %>%
    group_by(person_id) %>% slice(1) %>% ungroup()
  
  # join
  data_split_1_heldout <- data_split_1_heldout %>% left_join(exp_outcome_fc_occ_split_1, by = c("person_id")) %>% left_join(weight_split_1, by = c("person_id")) %>% ungroup()
  data_split_2_heldout <- data_split_2_heldout %>% left_join(exp_outcome_fc_occ_split_2, by = c("person_id")) %>% left_join(weight_split_2, by = c("person_id")) %>% ungroup()
  
  # calculate bias correction
  bias_correction_m_split_1 <- data_split_1_heldout %>% filter(sex == "Male") %>% ungroup() %>%
    summarise(sex = sex, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight, na.rm = T)) %>% unique()
  
  bias_correction_m_split_2 <- data_split_2_heldout %>% filter(sex == "Male") %>% ungroup() %>%
    summarise(sex = sex, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight, na.rm = T)) %>% unique()
  
  bias_correction_f_split_1 <- data_split_1_heldout %>% filter(sex == "Female") %>% ungroup() %>%
    summarise(sex = sex, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight, na.rm = T)) %>% unique()
  
  bias_correction_f_split_2 <- data_split_2_heldout %>% filter(sex == "Female") %>% ungroup() %>%
    summarise(sex = sex, bias_correction = weighted.mean(outcome - exp_outcome_fc_occ, w = combined_weight, na.rm = T)) %>% unique()
  
  bias_correction_split_1 <- rbind(bias_correction_m_split_1, bias_correction_f_split_1)
  bias_correction_split_2 <- rbind(bias_correction_m_split_2, bias_correction_f_split_2)
  
  # correct bias in outcome predictions
  data_split_1_heldout <- data_split_1_heldout %>% 
    left_join(bias_correction_split_1, by = "sex") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction))
  
  data_split_2_heldout <- data_split_2_heldout %>% 
    left_join(bias_correction_split_2, by = "sex") %>% ungroup() %>%
    mutate_at(vars(contains("exp_outcome_occ_")), ~(. - bias_correction))
  
  rm(list = setdiff(ls(), c("sample", "data_split_1_heldout", "data_split_2_heldout")))
  
  # SELECTION
  
  # split 1
  levels_selection_male_split_1_1 <- data_split_1_heldout %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_selection_male_split_1_2 <- data_split_1_heldout %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("share_occ_men_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "share_occ") %>% select(-name)
  levels_selection_male_split_1 <- cbind(levels_selection_male_split_1_1, levels_selection_male_split_1_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_selection_male_split_1_1, levels_selection_male_split_1_2)
  
  levels_selection_female_split_1_1 <- data_split_1_heldout %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_selection_female_split_1_2 <- data_split_1_heldout %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("share_occ_women_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "share_occ") %>% select(-name)
  levels_selection_female_split_1 <- cbind(levels_selection_female_split_1_1, levels_selection_female_split_1_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_selection_female_split_1_1, levels_selection_female_split_1_2)
  
  # split 2
  levels_selection_male_split_2_1 <- data_split_2_heldout %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_selection_male_split_2_2 <- data_split_2_heldout %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("share_occ_men_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "share_occ") %>% select(-name)
  levels_selection_male_split_2 <- cbind(levels_selection_male_split_2_1, levels_selection_male_split_2_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_selection_male_split_1_1, levels_selection_male_split_1_2)
  
  levels_selection_female_split_2_1 <- data_split_2_heldout %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_selection_female_split_2_2 <- data_split_2_heldout %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("share_occ_women_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "share_occ") %>% select(-name)
  levels_selection_female_split_2 <- cbind(levels_selection_female_split_2_1, levels_selection_female_split_2_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_selection_female_split_2_1, levels_selection_female_split_2_2)
  
  # average cross-fits
  levels_selection_male <- (levels_selection_male_split_1 + levels_selection_male_split_2) / 2
  levels_selection_female <- (levels_selection_female_split_1 + levels_selection_female_split_2) / 2
  
  # TOTAL
  
  # split 1
  levels_post_male_split_1_1 <- data_split_1_heldout %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_post_male_split_1_2 <- data_split_1_heldout %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("counterfact_share_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "counterfact_share_occ") %>% select(-name)
  levels_post_male_split_1 <- cbind(levels_post_male_split_1_1, levels_post_male_split_1_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * counterfact_share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_post_male_split_1_1, levels_post_male_split_1_2)
  
  levels_post_female_split_1_1 <- data_split_1_heldout %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_post_female_split_1_2 <- data_split_1_heldout %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("counterfact_share_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "counterfact_share_occ") %>% select(-name)
  levels_post_female_split_1 <- cbind(levels_post_female_split_1_1, levels_post_female_split_1_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * counterfact_share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_post_female_split_1_1, levels_post_female_split_1_2)
  
  # split 2
  levels_post_male_split_2_1 <- data_split_2_heldout %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_post_male_split_2_2 <- data_split_2_heldout %>% filter(sex == "Male") %>% select(person_id, survey_weight, contains("counterfact_share_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "counterfact_share_occ") %>% select(-name)
  levels_post_male_split_2 <- cbind(levels_post_male_split_2_1, levels_post_male_split_2_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * counterfact_share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_post_male_split_2_1, levels_post_male_split_2_2)
  
  levels_post_female_split_2_1 <- data_split_2_heldout %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("exp_outcome_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "exp_outcome_occ") %>% select(-name)
  levels_post_female_split_2_2 <- data_split_2_heldout %>% filter(sex == "Female") %>% select(person_id, survey_weight, contains("counterfact_share_occ_")) %>% pivot_longer(!c(person_id, survey_weight), values_to = "counterfact_share_occ") %>% select(-name)
  levels_post_female_split_2 <- cbind(levels_post_female_split_2_1, levels_post_female_split_2_2) %>% select(1, 2, 3, 6) %>% mutate(exp = exp_outcome_occ * counterfact_share_occ) %>% group_by(person_id) %>% reframe(exp = sum(exp), survey_weight = survey_weight) %>% unique() %>% reframe(weighted.mean(exp, w = survey_weight) * 100) %>% as.numeric()
  
  rm(levels_post_female_split_2_1, levels_post_female_split_2_2)
  
  # average cross-fits
  levels_post_female <- (levels_post_female_split_1 + levels_post_female_split_2) / 2
  levels_post_male <- (levels_post_male_split_1 + levels_post_male_split_2) / 2
  
  # ESTIMANDS
  
  # OBSERVED
  # observed levels
  levels_pre_female <- sample %>% filter(sex == "Female") %>% ungroup() %>% summarize(exp = weighted.mean(outcome, w = survey_weight) * 100) %>% as.numeric()
  levels_pre_male <- sample %>% filter(sex == "Male") %>% ungroup() %>% summarize(exp = weighted.mean(outcome, w = survey_weight) * 100) %>% as.numeric()
  
  # observed disparity
  gap_pre <- levels_pre_female - levels_pre_male
  
  # COMPONENTS
  # selection component
  selection <- (levels_pre_female - levels_pre_male) - (levels_selection_female - levels_selection_male)
  
  selection_pp <- (selection / gap_pre) * 100
  
  # composition component
  composition <- (levels_selection_female - levels_selection_male) - (levels_post_female - levels_post_male)
  
  composition_pp <- (composition / gap_pre) * 100
  
  # total segregation component
  total <- gap_pre - (levels_post_female - levels_post_male)
  
  total_pp <- (total / gap_pre) * 100
  
  # COUNTERFACTUAL LEVELS
  # levels after differential composition elimiated
  levels_compositon_female <- levels_pre_female - (levels_selection_female - levels_post_female)
  levels_compositon_male <- levels_pre_male - (levels_selection_male - levels_post_male)
  
  # COUNTERFACTUAL DISPARITIES
  # disparity after differential composition elimiated
  gap_post_composition <- levels_compositon_female - levels_compositon_male
  
  # disparity after differential selection elimiated
  gap_post_selection <- levels_selection_female - levels_selection_male
  
  # disparity after segregation elimiated
  gap_post_total <- levels_post_female - levels_post_male
  
  
  estimates_pre_disparity <- data.frame(estimand = "Disparity (observed)", "est" = round(gap_pre, 2))
  estimates_pre_level_male <- data.frame(estimand = "Men (observed)", "est" = round(levels_pre_male, 2))
  estimates_pre_level_female <- data.frame(estimand = "Women (observed)", "est" = round(levels_pre_female, 2))
  
  estimates_composition <- data.frame(estimand = "Due to composition", "est" = round(composition, 2))
  estimates_composition_pp <- data.frame(estimand = "Due to composition (perc.)", "est" = round(composition_pp, 2))
  estimates_composition_disparity <- data.frame(estimand = "Disparity after composition elimiated", "est" = round(gap_post_composition, 2))
  estimates_composition_level_male <- data.frame(estimand = "Men after composition elimiated", "est" = round(levels_compositon_male, 2))
  estimates_composition_level_female <- data.frame(estimand = "Women after composition elimiated", "est" = round(levels_compositon_female, 2))
  
  estimates_selection <- data.frame(estimand = "Due to selection", "est" = round(selection, 2))
  estimates_selection_pp <- data.frame(estimand = "Due to selection (perc.)", "est" = round(selection_pp, 2))
  estimates_selection_disparity <- data.frame(estimand = "Disparity after selection elimiated", "est" = round(gap_post_selection, 2))
  estimates_selection_level_male <- data.frame(estimand = "Men after selection elimiated", "est" = round(levels_selection_male, 2))
  estimates_selection_level_female <- data.frame(estimand = "Women after selection elimiated", "est" = round(levels_selection_female, 2))
  
  estimates_total <- data.frame(estimand = "Due to segregation (composition + selection)", "est" = round(total, 2))
  estimates_total_pp <- data.frame(estimand = "Due to segregation (composition + selection) (perc.)", "est" = round(total_pp, 2))
  estimates_total_disparity <- data.frame(estimand = "Disparity after segregation elimiated", "est" = round(gap_post_total, 2))
  estimates_total_level_male <- data.frame(estimand = "Men after segregation elimiated", "est" = round(levels_post_male, 2))
  estimates_total_level_female <- data.frame(estimand = "Women after segregation elimiated", "est" = round(levels_post_female, 2))
  
  estimates <- rbind(estimates_pre_disparity, estimates_pre_level_male, estimates_pre_level_female,
                     estimates_composition, estimates_composition_pp, estimates_composition_disparity, estimates_composition_level_male, estimates_composition_level_female,
                     estimates_selection, estimates_selection_pp, estimates_selection_disparity, estimates_selection_level_male, estimates_selection_level_female,
                     estimates_total, estimates_total_pp, estimates_total_disparity, estimates_total_level_male, estimates_total_level_female)
  
  rm(list = setdiff(ls(), c("estimates")))
  
  gc()
  
  return(estimates)
  
}













get_results <- function(decomp_results) {
  
  results <- decomp_results %>%
    group_by(estimand) %>%
    summarise(
      se = sd(est),
      conf_lower_999 = est - 3.291 * se, conf_higher_999 = est + 3.291 * se,
      conf_lower_99 = est - 2.576 * se, conf_higher_99 = est + 2.576 * se,
      conf_lower_95 = est - 1.960 * se, conf_higher_95 = est + 1.960 * se,
      conf_lower_90 = est - 1.645 * se, conf_higher_90 = est + 1.645 * se,
      est = mean(est),
      sig = case_when(
        conf_lower_999 > 0 & conf_higher_999 > 0 | conf_lower_999 < 0 & conf_higher_999 < 0  ~ "***",
        conf_lower_99 > 0 & conf_higher_99 > 0 | conf_lower_99 < 0 & conf_higher_99 < 0  ~ "***",
        conf_lower_95 > 0 & conf_higher_95 > 0 | conf_lower_95 < 0 & conf_higher_95 < 0  ~ "**",
        conf_lower_90 > 0 & conf_higher_90 > 0 | conf_lower_90 < 0 & conf_higher_90 < 0  ~ "*",
        T ~ "")) %>%
    select(estimand, est, se, sig, conf_lower_999, conf_higher_999, conf_lower_99, conf_higher_99, conf_lower_95, conf_higher_95, conf_lower_90, conf_higher_90)
  
  return(results)
  
}



















