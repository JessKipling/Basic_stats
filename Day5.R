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

# library(Rmisc) # Unfortunately this overrides many dplyr functions 
#this package has a summary called SE 


snakes.summary3 <- Rmisc::summarySE(data = snakes, #data it operates on
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


#after the break
 #Visulatisation

ggplot(data = snakes, aes(x = as.numeric(day),
                          y = openings,
                          colour = snake)) +
  geom_line(size = 3)+
  geom_point(size = 4)



# Exercise  ---------------------------------------------------------------

#make 2 null hypothesis
#run a two factor anova with interaction between the two factors
#ANOva
#Tukey
#Line Graph
#Write a conclusion

Moths <- read_csv("Moths.csv") %>% 
  gather(key = "trap", value = "count", -Location)

#levevls
#number of moth per trap
#used 3 different trap types: sugar, chemical, scent

Moths.summary <- Moths %>% 
  group_by(Location) %>% 
  summarise(mean_count = mean(count),
            sd_count = sd(count)) 

Moths.summary
         
Moths.summary2 <- Rmisc::summarySE(data = Moths, measurevar = "count", groupvars = c("Location"))

BoxPlot <- ggplot(data = Moths, aes(x = Location, y = count)) +
  geom_segment(data = Moths.summary2, aes(x = Location, xend = Location, y = count - ci, yend = count + ci, colour = Location),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = Location), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.1)

# Hypothesis

#H0: There is no difference in number of moths caught in the various locations
#H1: There IS a difference in number of moths caught in the various locations

Moths.aov <- aov(count ~ Location, data = Moths)

summary(Moths.aov)

#We do not accept the null hypothesis as there is a difference in the number of moths caught in the various locations

#Location and Traps

#H0: There is no difference in number of moths caught in the various locations and various traps
#H1: There IS a difference in number of moths caught in the various locations and various traps

#We do not reject the null hypothesis as there is no difference in the number of moths caught in the various traps 
  #in the various locations

Moths.all.aov <- aov(count ~ Location + trap, data = Moths)

summary(Moths.all.aov)

# Checking assumptions...
# make a histogram of the residuals;
# they must be normal

Moths.res <- residuals(Moths.aov)
hist(Moths.res)

# make a plot of residuals and the fitted values;
# # they must be normal and homoscedastic

plot(fitted(Moths.all.aov), residuals(Moths.all.aov))

Moths.tukey <- TukeyHSD(Moths.aov, which = "Location")

plot(Moths.tukey)


BoxPlot2 <- ggplot(data = Moths, aes(x = Location, y = count)) +
  geom_boxplot(aes(fill = trap), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.1)

BoxPlot3 <- ggplot(data = Moths, aes(x = trap, y = count)) +
  geom_boxplot() + 
  geom_jitter(width = 0.1)

BoxPlot4 <- ggplot(data = Moths, aes(x = Location, y = count)) +
  geom_boxplot() + 
  geom_jitter(width = 0.1)

library(ggplot2)
library(ggpubr)

Final.Moths <- ggarrange(BoxPlot2, BoxPlot3, BoxPlot4)
Final.Moths

ggplot(data = Moths, aes(x = Location)) +
  geom_bar(aes(fill = trap,
               position = "dodge"))
#not really what I was aiming for



# Missing Information on Linear Regressions -------------------------------





# Correlations ------------------------------------------------------------

#what is the relationship between a variable with another variable
#not how does another variable account for the variance
#Morphometric properties of things... ie. a correlation
#kelp length is not independent of the weight
#but there is a relationship that you can qunatify


# Correlation Assumptions -------------------------------------------------

#data are not indepedent
#abscence of outliers
#linearity 
#type of correlation we are going to do

library(corrplot)
library(tidyverse)
library(ggpubr)

#Load Data

ecklonia <- read.csv("Intro_R_Workshop_UWC2018/data/ecklonia.csv")


# Formulate a hypothesis --------------------------------------------------

# H0: THere is no relationship between stipe length and stipe diameter
#for the kelp Ecklonia maxima
#H1: there is a relationship between stipe length and stipe diameter 
  #for the kelp Ecklonia maxima


# #Test a Hypothesis ------------------------------------------------------


cor.test(ecklonia$stipe_length, ecklonia$stipe_diameter)

# Visualise the Data

ggplot(data = ecklonia, aes(x = stipe_length, y = stipe_diameter)) +
  geom_point()


# Run HECKA tests at once -------------------------------------------------

#we just want the continuos variables

ecklonia_sub <- ecklonia %>% 
  select(stipe_length:epiphyte_length) #creating column names

ecklonia_cor <- cor(ecklonia_sub) #will give you just the pearsons cor value

ecklonia_cor


# Spearman rank test ------------------------------------------------------

# ordinal data

ecklonia$length <- as.numeric(cut((ecklonia$stipe_length + ecklonia$stipe_diameter), 3))
                              
#output is a word based 

#Then run a Spearman test

cor.test(ecklonia$length, ecklonia$primary_blade_length, method = "spearman")

#Rho value is similar to pearson R. 


# Kendall rank test -------------------------------------------------------

ecklonia_norm <- ecklonia_sub %>% 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))
ecklonia_norm

cor.test(ecklonia$stipe_length, ecklonia$stipe_diameter, method = "kendall")

#tau 


# Visualise the results ------------------------------------------------------
library(corrplot)
ecklonia_pearson <- cor(ecklonia_sub)

corrplot(ecklonia_pearson, method = "circle")

summary(ecklonia_pearson)

# Homework Exercise -------------------------------------------------------

#Heat map

library(reshape2)

melted_eck <- melt(ecklonia_pearson)
head(melted_eck)

ggplot(melted_eck, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "dodgerblue") +
  labs(x = "Variable_1", y = "Variable_2")




