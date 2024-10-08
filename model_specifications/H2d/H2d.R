source("../fitting_function.R")

run_model_fit(predictors = c("food",
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
                             "personal_income:importance_rich",
                             "relative_income",
                             "relative_income:importance_rich",
                             "country_average_food",
                             "country_average_housing",
                             "country_average_health",
                             "country_average_water",
                             "country_average_air",
                             "country_average_healthcare",
                             "country_average_security",
                             "country_average_social_support",
                             "country_average_respect",
                             "country_average_education",
                             "country_average_interesting_activity",
                             "country_average_recreation",
                             "country_average_occupation",
                             "country_average_freedom",
                             "gdp",
                             "gdp:importance_rich",
                             "growth",
                             "growth:importance_rich",
                             "government_effectiveness",
                             "democracy",
                             "social_protection",
                             "importance_rich"))
