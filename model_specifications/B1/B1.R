source("../fitting_function.R")

run_model_fit(drop_na_variables = c("food",
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
