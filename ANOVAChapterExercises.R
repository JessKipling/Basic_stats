#End of Chapter Exercersis 7.4
#24 April 2018
#Jess Kipling


# 7.4.1 Exercise 1 --------------------------------------------------------

#load libraries

library(tidyverse)
library(ggplot2)
library(ggpubr)

# A bunch of data for pigs raised on different diets

# enter the mass at the end of the experiment

feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)


#Does feed type have an effect on the mass of pigs at the end of the experiment?
#H0: Feed type has no effect on the mass of pigs at the end of the experiment
#H1: Feed type has an effect on the mass of pigs at the end of the experiment


# make a dataframe
bacon <- as.tibble(data.frame(
  feed = c(
    rep("Feed 1", length(feed_1)),
    rep("Feed 2", length(feed_2)),
    rep("Feed 3", length(feed_3)),
    rep("Feed 4", length(feed_4))
  ),
  mass = c(feed_1, feed_2, feed_3, feed_4)
))

bacon.aov1 <- aov(mass ~ feed, data = bacon)
summary(bacon.aov1)

BaconTukey <- TukeyHSD(bacon.aov1)

plot(TukeyHSD(bacon.aov1))

#We do not accept the null hypothesis, instead we accept the alternative hypothesis that feed type has an effect
#on the mass of pigs at the end of the experiment


# Exercise 2 --------------------------------------------------------------

teeth <- datasets::ToothGrowth

#Does a difference in the form of Vitamin C administered influence tooth growth in Guinea Pigs?
#H0: A difference in the form of Vitamin C adminitered does not influence the tooth growth in Guinea Pigs
#H1: A difference in the form of Vitamin C adminitered influences the tooth growth in Guinea Pigs


# Then subset out only the sample sets to be compared

teeth_sub <- teeth %>% # Tell R which dataframe we are using
  select(len, supp)# Select only specific columns

teeth.aov1 <- aov(len ~ supp, data = teeth_sub)
summary(teeth.aov1)


#We accept the nul hypothesis that states that 
  #A difference in the form of Vitamin C adminitered does not influence the tooth growth in Guinea Pigs


# Exercise 3 --------------------------------------------------------------

library(Rmisc)
diet <- read.csv("Diet_data.csv", row.names = 1)

diet$weight.loss = diet$pre.weight - diet$weight6weeks

diet_sub <- diet %>% 
  select(weight.loss, Age, gender)

weightloss.summary <- diet_sub %>% 
  group_by(Age) %>% 
  summarise(mean_weightloss = mean(weight.loss),
            sd_ = sd(weight.loss)) %>% 
  ungroup()

WL.summary2 <- summarySE(data = diet_sub, measurevar = "weight.loss", groupvars = c("Age"))

WL.aov <- aov(weight.loss ~ Age + gender, data = diet)
summary(snake.aov)

#None of this worked
#Will come back to this exercise with a differnt dataset



  
