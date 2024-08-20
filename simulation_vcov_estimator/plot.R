library(dplyr)
library(tidyverse)
library(tibble)
library(comprehenr)

x=.05*(1:11)

fields <- c("X.Intercept.",
            "gendermale",
            "age",
            "age_squared",
            "relationship_statuspartnered",
            "relationship_statusseparated",
            "relationship_statuswidowed",
            "food1",
            "housing1",
            "health1",
            "water1",
            "air1",
            "healthcare1",
            "security",
            "social_support1",
            "respect1",
            "education1",
            "interesting_activity1",
            "recreation1",
            "occupation1",
            "freedom1",
            "personal_income",
            "relative_income",
            "gdp",
            "growth")

new_fields <- c("Intercept",
                "Male",
                "Age",
                "Age (squared)",
                "Partnered",
                "Separated",
                "Widowed",
                "Food",
                "Housing",
                "Health",
                "Water",
                "Air",
                "Healthcare",
                "Security",
                "Social support",
                "Respect",
                "Education",
                "Interesting activity",
                "Recreation",
                "Occupation",
                "Freedom",
                "Log(personal income)",
                "Relative income",
                "Log(GDP)",
                "GDP growth")

### Load CR1 data

p1 <- read.csv("p_values/p_values_CR1.csv",row.names=1)

p1 <- tibble::rownames_to_column(data.frame(t(data.frame(t(p1)) %>%
                                                  select(-starts_with("year")) %>%
                                                  rename_at(fields, ~new_fields)))) %>% rename_at("rowname", ~"X")

p1$X <- factor(p1$X, levels = new_fields)

df1 <- Reduce(function(x,y) merge(x,y,all=TRUE),
              to_list(for (i in 1:11) c(data.frame(fnum=x[i],
                                                   f = paste(as.character(100*x[i]),"%"),
                                                   p1[c(1,i+1)] %>% rename_at(2,~'p1')))
              )
       )


# Load CR2 data

p2 <- read.csv("p_values/p_values_CR2.csv",row.names=1)

p2 <- tibble::rownames_to_column(data.frame(t(data.frame(t(p2)) %>%
                                              select(-starts_with("year")) %>%
                                              rename_at(fields, ~new_fields)))) %>% rename_at("rowname", ~"X")

p2$X <- factor(p2$X, levels = new_fields)

df2 <- Reduce(function(x,y) merge(x,y,all=TRUE),
              to_list(for (i in 1:11) c(data.frame(fnum=x[i],
                                                   f=paste(as.character(100*x[i]),"%"), p2[c(1,i+1)] %>% rename_at(2,~'p2')))
              )
       )

df <- full_join(df1, df2)


### Make the plot

ggplot(transform(df, f=as.character(f)),
       mapping = aes(x = fnum),
       legend.text=element_text(size=20),
       axis.title=element_text(size=20)) +
geom_line(aes(y = p1, colour = X)) +
geom_line(aes(y = p2, colour = X), linetype = "dashed") +
labs(x="Sample size (w.r.t. the whole dataset)", y="p-value", colour="Predictor") +
scale_y_log10() +
theme_light() +
theme(text=element_text(size=26), legend.position = "bottom") +
scale_x_continuous(labels=scales::percent) +
pdf(NULL)

ggsave("p_values/p_values_ggplot2.png", width=20, height=15)
