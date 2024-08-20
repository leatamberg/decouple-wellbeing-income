library(dplyr)
library(readr)
library(lme4)
library(lmerTest)
library(clubSandwich)
library(parameters)
library(tidyr)



data_needs <- 
  readr::read_csv("../data_all.csv", col_types = "idicccddddcddciiiiiiddiiiiiiiddddddddddddddddddddddddcddddid") %>% 
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
      religious
      ), ~ factor(.)), religious = relevel(relevel(religious, "yes (formally)"), "no"), relationship_status = relevel(relationship_status, "single")) %>% 
  dplyr::select(id,
                year,
                country_code,
                country_name,
                country_year,
                life_evaluation,
                gender,
                age,
                age_squared,
                relationship_status,
                food,
                housing,
                health,
                water,
                air,
                healthcare,
                security,
                social_support,
                respect,
                education,
                interesting_activity,
                recreation,
                occupation,
                freedom,
                personal_income,
                relative_income,
                gdp,
                growth
  ) %>% 
  drop_na()


data_India <- data_needs %>% 
  filter(country_name=="India") %>% 
  group_by(country_year) %>% 
  slice_sample(n = 4900)

data_needs <- bind_rows(data_needs %>% filter(country_name != "India"), data_India) 


outcome_vars = c("food",
                "housing",
                "health",
                "water",
                "air",
                "healthcare",
                "security",
                "social_support",
                "respect",
                "education",
                "interesting_activity",
                "recreation",
                "occupation",
                "freedom")

rhs_formula <- "(1 | country_name) + (1 | country_year) + year +  gender + age + age_squared +  relationship_status + scale(personal_income) + scale(relative_income) + scale(gdp) + scale(growth)" 

fit_models_glmer <- function(outcome_vars, rhs_formula, data, ...) {
  models <- lapply(outcome_vars, function(outcome_var) {
    formula <- reformulate(rhs_formula, response = outcome_var)
    model <- glmer(formula, data = data, ...)
    return(model)
  })
  names(models) <- outcome_vars
  return(models)
}


models_needs <- fit_models_glmer(outcome_vars, rhs_formula, data_needs, nAGQ = 0L, family="binomial", verbose = TRUE)

saveRDS(models_needs, "models_needs.rds")