rm(list = ls())

library(readr)
library(ggplot2)
library(tidyverse)

twins_path <- "data/Survivors vs Twins comparison files_absolute/abs_20231221/"

################################################################################
# Ages -------------------------------------------------------------------------
read_and_pivot <- function(path, value_name) {
  read_csv(paste0(twins_path, path)) %>%
    select(-c("...103")) %>% # Remove unnecessary column
    pivot_longer(
      cols = starts_with("prop_"), 
      values_to = value_name
    )
}

ageProp_Mean_long <- read_and_pivot("ageProp_Mean.csv", "Mean")
ageProp_UB_long <- read_and_pivot("ageProp_UB.csv", "UpperBound")
ageProp_LB_long <- read_and_pivot("ageProp_LB.csv", "LowerBound")

ageProp <- reduce(list(ageProp_Mean_long, 
                       ageProp_UB_long, 
                       ageProp_LB_long), 
                  inner_join, by = c("Group", "Outcome", "Property"))

ageProp$Property <- as.numeric(gsub("prop_", "", ageProp$Property))

rm(ageProp_Mean_long, ageProp_UB_long, ageProp_LB_long)

write_csv(ageProp, "data/Survivors vs Twins comparison files_absolute/cleaned/ageProp.csv")

# make a plot of ageProp that shows the mean and the upper and lower bounds
ageProp %>%
  filter(Outcome == "AnyCHC" & Group == "all") %>%
  ggplot(aes(x = Property, y = Mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = LowerBound, ymax = UpperBound), alpha = 0.2) +
  labs(title = "Age Proportion",
       x = "Age",
       y = "Proportion") +
  theme_minimal()

charCols <- names(ageProp)[sapply(ageProp, is.character)]
################################################################################

read <- function(path, value_name) {
  read_csv(paste0(twins_path, path))[,-6] %>%
    rename_with(~ value_name, .cols = "Value")
}

cond_mean <- read("cond_Mean.csv", "Mean")
cond_lb <- read("cond_LB.csv", "LowerBound")
cond_ub <- read("cond_UB.csv", "UpperBound")

cond <- reduce(list(cond_mean, cond_lb, cond_ub), 
               inner_join, by = c("Group", "Age", "Outcome", "Condition"))

rm(cond_mean, cond_lb, cond_ub)

################################################################################
read_and_pivot <- function(path, value_name) {
  read_csv(paste0(twins_path, path)) %>%
    select(-c("...64")) %>% # Remove unnecessary column
    pivot_longer(
      cols = starts_with("age_"), 
      names_to = "Age", 
      values_to = value_name
    )
}

cond40_Mean_long <- read_and_pivot("cond40_Mean.csv", "Mean")
cond40_UB_long <- read_and_pivot("cond40_UB.csv", "UpperBound")
cond40_LB_long <- read_and_pivot("cond40_LB.csv", "LowerBound")

cond40 <- reduce(list(cond40_Mean_long, cond40_UB_long, cond40_LB_long), 
               inner_join, by = c("Group", "Age", "Outcome", "Condition"))

rm(cond40_Mean_long, cond40_UB_long, cond40_LB_long)

################################################################################
read_and_pivot <- function(path, value_name) {
  read_csv(paste0(twins_path, path)) %>%
    select(-c("...107")) %>% # Remove unnecessary column
    pivot_longer(
      cols = starts_with("cumInc_"), 
      names_to = "cumInc", 
      values_to = value_name
    )
}

jointCumIncAge_Mean <- read_and_pivot("jointCumIncAge_Mean.csv", "Mean")
jointCumIncAge_UB <- read_and_pivot("jointCumIncAge_UB.csv", "UpperBound")
jointCumIncAge_LB <- read_and_pivot("jointCumIncAge_LB.csv", "LowerBound")

jointCumIncAge <- reduce(list(jointCumIncAge_Mean, jointCumIncAge_UB), 
                 inner_join, by = c('Group', "cumInc"))

rm(jointCumIncAge_Mean, jointCumIncAge_UB, jointCumIncAge_LB)

################################################################################
read_and_pivot <- function(path, value_name) {
  read_csv(paste0(twins_path, path)) %>%
    select(-c("...107")) %>% # Remove unnecessary column
    pivot_longer(
      cols = starts_with("cumInc_"), 
      names_to = "cumInc", 
      values_to = value_name
    )
}

jointCumInc_Mean <- read_and_pivot("jointCumInc_Mean.csv", "Mean")
jointCumInc_UB <- read_and_pivot("jointCumInc_UB.csv", "UpperBound")
jointCumInc_LB <- read_and_pivot("jointCumInc_LB.csv", "LowerBound")

################################################################################
read_and_pivot <- function(path, value_name) {
  read_csv(paste0(twins_path, path)) %>%
    select(-c("...103")) %>% # Remove unnecessary column
    pivot_longer(
      cols = starts_with("age_"), 
      names_to = "Age", 
      values_to = value_name
    )
}

cumCHCFree_Mean <- read_and_pivot("cumCHCFree_Mean.csv", "Mean")
cumCHCFree_UB <- read_and_pivot("cumCHCFree_UB.csv", "UpperBound")
cumCHCFree_LB <- read_and_pivot("cumCHCFree_LB.csv", "LowerBound")


cumCHCFree <- reduce(list(cumCHCFree_Mean, cumCHCFree_UB, cumCHCFree_LB), 
                     inner_join, by = c("Group", "Outcome", "Age"))

rm(cumCHCFree_Mean, cumCHCFree_UB, cumCHCFree_LB)
################################################################################
