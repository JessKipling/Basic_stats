#Day 3 
#T-test workflow
#Home work

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(fitdistrplus)
library(plotly)

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


# My Own Exercise ---------------------------------------------------------

Beaver <- datasets::beaver1


# Beaver dataset shows the long-term temperature dynamics of beaver Castor canadensis in North-central Wisconsin

#Checking assumptions

#Measurements are done at intervals - continuous
#Measurements are done on two different days, at different times, under different conditions - independent
Beaver%>% 
  group_by(day) %>% 
  summarise(variable.norm = as.numeric(shapiro.test(temp)[2]),
            variable_var = var(temp))


#it is noted that this dataset is inconsistent in number of samples per day
 #therefore not appropirate for a t-test of means due to varying sample sizes


# Generating a Random dataset rather

r_norm <- rnorm(n = 80, mean = 35, sd = 1)

hist(r_norm)

descdist(r_norm)

r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 15, sd = 3),
                            rnorm(n = 1000, mean = 10, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

#Are these data smaller or greater than the population mean?


