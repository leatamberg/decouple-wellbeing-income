source("../fitting_function.R")

run_model_fit(
    predictors = c("food",
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
                             "freedom",
                             "personal_income",
                             "relative_income",
                             "gdp",
                             "growth"),
    drop_na_variables = c("life_evaluation"),
    outcome = "life_satisfaction",
    level_effects = "(1 | country_name)")
