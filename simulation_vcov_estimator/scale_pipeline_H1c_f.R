library(dplyr)
library(readr)
library(lme4)
library(clubSandwich)
library(parameters)
library(tidyr)

options(error=function()traceback(2))

data_H1c_f_resample <- 
  readr::read_csv("./resamples/data_level{level}.csv", 
                  col_types = "idicccdddddcdciiiiiidiiiiiiidddddddddddddddddddddddcddddi") %>% 
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
      social_support,
      respect,
      education,
      interesting_activity,
      recreation,
      occupation,
      freedom,
      religious,
      satisfaction_standard_living), ~ factor(.)), religious = relevel(relevel(religious, "yes (formally)"), "no"), 
    relationship_status = relevel(relationship_status, "single")) %>% 
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
                gdp, #) %>% #,
                growth) %>% 
  drop_na()

model_H1c_f_resample <- lmer(life_evaluation ~ 
                    (1 | country_name) + 
                    (1 | country_year) +
                    year + 
                    gender +
                    age +
                    age_squared +
                    relationship_status +
                    food +
                    housing +
                    health +
                    water +
                    air +
                    healthcare +
                    security +
                    social_support +
                    respect +
                    education +
                    interesting_activity +
                    recreation +
                    occupation +
                    freedom +
                    personal_income +
                    relative_income +
                    gdp +
                    growth,
                  data = data_H1c_f_resample)

saveRDS(model_H1c_f_resample, "results.H1c_f/model_level{level}.rds")

saveRDS(vcov(model_H1c_f_resample), "results.H1c_f/vcov_level{level}.rds")

vcov <- clubSandwich::vcovCR(model_H1c_f_resample, type = "CR1")
saveRDS(vcov, "results.H1c_f/vcov_CR1_level{level}.rds")

vcov <- clubSandwich::vcovCR(model_H1c_f_resample, type = "CR2")
saveRDS(vcov, "results.H1c_f/vcov_CR2_level{level}.rds")

problems()
