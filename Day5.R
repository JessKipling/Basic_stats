# Day 5
#Jess Kipling
# Friday, 20 April 2018


# Load Libraries ----------------------------------------------------------

library(tidyverse)



# Load the Data -----------------------------------------------------------

snakes <- read_csv("snakes.csv")
snakes$day = as.factor(snakes$day)

#this changes day into a factor of day

#look at the raw data

# Manipulate the Data -----------------------------------------------------

# calculate a bunch of summary statistics
# what is the mean and standard deviation

snakes.summary1 <- snakes %>% 
  group_by(day, snake) %>% 
  summarise(mean_openings = mean(openings),
            snakes_sd = sd(openings)) 
  
#grouping by day and snake, counts how any time each snake responds in a day but snake only occurs once in a day
#there is no mean calculation because there is only 1 snake per day (we need multiple reps per day of an individual)

#So instead, we can calculate the average number of openings per day for ALL of the snakes

snakes.summary2 <- snakes %>% 
  group_by(day) %>%  #note snakes is removed
  summarise(mean_openings = mean(openings),
            snakes_sd = sd(openings))


#visualising the data to analyse it 

ggplot(snakes.summary2, aes(x = day, y = mean_openings)) +
       geom_point(size = 5, shape = 4) 

# Formulate a hypothesis --------------------------------------------------

#H0: There is NO difference in the number of openings from day to day
#H1: There IS a difference in the number of openings from day to day



# Testing a hypothesis ----------------------------------------------------

library(Rmisc) #this package has a summary called SE 


snakes.summary3 <- summarySE(data = snakes, #data it operates on
                             measurevar = "openings", #measurement (openings) 
                             groupvars = c("day")) #variable

#Visualise the data

ggplot(data = snakes, aes(x = day, y = openings)) + 
  geom_segment(data = snakes.summary3, aes(x = day, xend = day, y = openings - ci, yend = openings + ci, colour = day), 
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) +
  geom_jitter(width = 0.05)

#this graph takes the info from the summary stats and layers it. 
#X end is the same as x because they share the same day
# the y end is set by the confidence intervals in summarySE
#y = is the mean minus the confidence interval, yend is the mean plus the confidence interval 
#the distribution of the raw data points are shown on here with the black dots


#But where is there a significant difference?
#Do we have a 95% probabilty of this?


# ANOVA for our Hypothesis ------------------------------------------------

#to test non-independence - we have to ensure that all of the snakes behave in the same kind of way
#To do this, we ask: Do snakes behave in the same way?
#Null: There is no difference between snakes in terms of how they respond to the opening and closing of boxes

snakes.aov <- aov(openings ~ day, data = snakes) #formula for ANOVa, then which data
#opening as a function of day

summary(snakes.aov) #pulls out the outcome of the analysis



# To test both Hypotheses -------------------------------------------------

snakes.all.aov <- aov(openings ~ day + snake, data = snakes)

summary(snakes.all.aov) 

#NB terms: DF, mean sum of squares(), F value (for olden days), p value( prob is less than 0.05)
#Residuals is much larger than sum of squares, so there is a large amount of variation still
#tells us that there are still some things that could be added to our analysis to further explain the data

#Shows that snakes are not having much of an effect on the data (p > 0.05)


# Testing the Assumptions Afterwards --------------------------------------


# First test: normality of data

snakes.residuals <- residuals(snakes.all.aov)
hist(snakes.residuals)

#we know our model is normally distributed and accounting for some of the variation

#We can now produce a plot for the predicted values and the known/error values

plot(fitted(snakes.all.aov), residuals(snakes.all.aov))

#Tukey Honest Signif Diff Test (HSD)
snakes.tukey <- TukeyHSD(snakes.all.aov, which = "day") 
plot(snakes.tukey)

#snakes.tukey views the stats behind this
#shows diff, lwr, upr, p values between the variables







