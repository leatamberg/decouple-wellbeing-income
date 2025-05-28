source("../fitting_function.R")

run_model_fit(predictors = c("personal_income",
                             "relative_income",
                             "gdp",
                             "growth"),
              drop_na_variables = c("life_satisfaction"),
outcome = "life_evaluation",
level_effects = "(1 | country_name)")
