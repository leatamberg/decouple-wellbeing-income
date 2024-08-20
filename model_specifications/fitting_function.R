library(dplyr)
library(readr)
library(lme4)
library(lmerTest)
library(clubSandwich)
library(tidyr)




run_model_fit <- function(predictors = NULL,
                          outcome = "life_evaluation", 
                          control_year = TRUE, 
                          drop_na_variables = NULL, 
                          level_effects = "(1 | country_name) + (1 | country_year) + year",
                          demographic_controls = c("gender", "age", "age_squared", "relationship_status"), 
                          subsample_India = TRUE, 
                          optimizer = "nloptwrap", 
                          control = lmerControl(optimizer=optimizer),
                          REML = TRUE,
                          cluster_robust_vcov = TRUE,
                          vcov_correction = "CR1", 
                          vcov_target = list(1,2), 
                          logistic = FALSE,
                          suffix_output_files = "",
                          data_file_path = "../data_all.csv",
                          seed = 42){
  
  drop_na_variables <- c(predictors[!grepl(':', predictors)], drop_na_variables)

  data <- 
    readr::read_csv(data_file_path, 
                    col_types = "idicccddddcddciiiiiiddiiiiiiiddddddddddddddddddddddddcddddid") %>% 
    mutate(across(
      c(year,
        gender,
        relationship_status,
        food,
        housing,
        health,
        water,
        air,
        healthcare,
        security, 
        security_obj,
        social_support,
        respect,
        education,
        interesting_activity,
        recreation,
        occupation,
        freedom,
        religious), ~ factor(.)), religious = relevel(relevel(religious, "yes (formally)"), "no"), 
      relationship_status = relevel(relationship_status, "single")) %>% 
    dplyr::select(
                  year,
                  country_code,
                  country_name,
                  country_year,
                  all_of(outcome),
                  all_of(demographic_controls),
                  all_of(drop_na_variables)) %>% 
    drop_na()
  
  # subsampling of India to make it small enough for clubsandwich
  if(subsample_India){
    set.seed(seed)
    data_India <- data %>% 
      filter(country_name=="India") %>% 
      group_by(country_year) %>% 
      slice_sample(n = 4900)
    
    data <- bind_rows(data %>% filter(country_name != "India"), data_India)
  }
  
  # build the regression formula out of the outcome variable, the default predictors (i.e. demographic control variables), and the additional predictors.

  all_predict_terms <- as.list(c(level_effects, demographic_controls, predictors))

  rhs_formula <- do.call(paste, c(all_predict_terms, sep = " + "))
  
  formula <- reformulate(rhs_formula, response = outcome)
  
  
  # fit the model
  if(logistic){
    print("Warning: Logistic regression not yet implemented!")
  }else{
    set.seed(seed)
    model <- lmer(formula = formula, 
                  data = data,
                  REML = REML,
                  control = control)
    saveRDS(model, paste0("model", suffix_output_files, ".rds"))
    
    # estimate the cluster-robust variance-covariance matrix
    if(REML & cluster_robust_vcov){
      set.seed(seed)
      vcov <- clubSandwich::vcovCR(model, type = vcov_correction, target = vcov_target) 
      saveRDS(vcov, paste0("vcov", suffix_output_files, ".rds"))
    }
    
  }

}
