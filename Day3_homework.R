#Day 3 
#T-test workflow
#Home work

library(tidyverse)
library(ggplot2)
library(ggpubr)

ecklonia <- read_csv("Intro_R_Workshop_UWC2018/data/ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)

ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()

# filter the data
ecklonia_sub <- ecklonia %>% 
  filter(variable == "stipe_mass")

# then create a new figure
ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "stipe mass (kg)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Checking Assumptions
ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(variable.norm = as.numeric(shapiro.test(value)[2]),
            variable_var = var(value))

#Running an Analysis

# traditional output
t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater")

# dataframe output

compare_means(value ~ site, data = ecklonia_sub, method = "t.test", var.equal = TRUE, alternative = "greater")

# The stipe mass (kg) of the kelp Ecklonia maxima was found to be significantly greater at Batsata Rock
# than at Boulders Beach (p = 0.03, t = 1.87, df = 24).

            
