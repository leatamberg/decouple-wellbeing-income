source("../fitting_function.R")

run_model_fit(predictors = c("food", "housing", "health", "security_obj", "social_support", "education", "occupation", "gdp"), drop_na_variables = c("food",
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
                "gdp"))
