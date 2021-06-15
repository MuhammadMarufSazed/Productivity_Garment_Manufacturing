prod_impute = read.csv("Data\\garments_worker_productivity.csv")
production = read.csv("Data\\garments_worker_productivity.csv")

summary(production)
dim(production)

colnames(production)

levels(production$department)


library(dplyr)

dep_summary = production %>%
  group_by(department) %>%
  summarise(mean_smv = mean(smv), 
            mean_workers = mean(no_of_workers), 
            mean_productivity = mean(actual_productivity))

knitr::kable(dep_summary)


quarter_summary = production %>%
  group_by(quarter) %>%
  summarise(mean_smv = mean(smv), 
            mean_workers = mean(no_of_workers), 
            mean_productivity = mean(actual_productivity))

knitr::kable(quarter_summary)