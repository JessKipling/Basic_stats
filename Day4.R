#Day 4 
# Thursday 19 April 2018


# ANOVA - looking for differences in means between more than 2 samples
# Data have to be indepedent of eachother
# Data needs to be balanced
# Data must be balanced - same number of individuals assigned per group 
#ANOVA simultaeneously looks at all the differences between different pairs of samples


# T-test refresh ----------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(ggplot2)

# First grab the data
chicks <- as_tibble(ChickWeight)

# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21) #filtering out Diet column, 
#and only selecting rows labelled as diet 1 or diet 2, at time 21


#T-test
t.test(weight ~ Diet, data = chicks_sub) #comparing weight as a function of diet, and the name of our data is chics_sub

#We dont not reject the null hypothessis that there is not a significant difference between the two diets

#An ANOVA would be useful here to compare all of the diets


# 1-way ANOVA  ------------------------------------------------------------------

#we are only looking at the effect of diet alone, so this is a single factor analysis

#generating our research question
  #is there a difference in chicken mass attained after 21 days having been fed 4 different diets?
#null hypothesis: there is no difference in chicken mass at 21 days after of being fed 4 different diets

chicks_21 <- chicks %>% 
  filter(Time == 21)

chicks.aov.1 <- aov(weight ~ Diet, data = chicks_21) #assigning an output to the ANOVA

#we do not accept the null hypothesis, therefore we accept the alternative hypothesis that there is a significant
#difference in chicken mass attained after 21 days after being fed 4 different diets

#Devise a graphical display of this outcome

ggplot(chicks_21, aes(x = Time, y = weight, fill = Diet)) +
  geom_boxplot(notch = TRUE)
         
#Notches: If the notches of two boxes *do not* overlap, this suggests that the medians are signifianctly different
  #and that there is a different between the groups
  #sometimes, the distance it extendes in one direction is further than the interquartile range, 
  #the "ears" are added - the width of the box is 1.5x the interquartile range

#We want to know precisely where the significant difference lies
#a tukey higher significance test 
#we can perfrom a tuey on the output of the ANOVA


# Tukey HSD ---------------------------------------------------------------

Tukey <- TukeyHSD(chicks.aov1)

#P-adj shows the significance between the various diets. 
#diet 3-1 shows that there is a significant difference
#another way - the lower confidence interval should always be below zero, upper above zero
#so if both are above zero, it shows that there is a significance differene

#This gives us a hint about what to explore in your data

ggplot(chicks_21, aes(x = Diet, y = weight, fill = Diet)) +
  geom_boxplot(notch = TRUE, colour = "grey50")+
  geom_segment(aes(x = Diet, xend = Diet, y = weight, yend = weight+5),colour = "red", size = 0.8)

# Segements showing confidence interval

chicks_Tukey <- as.data.frame(TukeyHSD(aov(weight ~ Diet, data = chicks_21))$Diet)

chicks_Tukey$pairs <-row.names(chicks_Tukey)

chicks_Tukey

#simple answer by looking at the "Help" file...

#first way

plot(TukeyHSD(chicks.aov1, "Diet"))

#second way
plot(TukeyHSD(aov(weight ~ Diet, data = chicks_21)))



# Multiple Factor ANOVA ---------------------------------------------------

# what is the relationship of diet and time on the chickens

# H0: there is no change in chicken mass (kg) from 0 to day 21

#Visualise Data
chicks_0_21 <- ChickWeight %>% 
  filter(Time %in% c(0,2,21))

#Before Filtering
ggplot(data = ChickWeight, aes(x = Time, y = weight)) +
  geom_boxplot(notch = T, aes(fill = as.factor(Time)))

#With TIme only filtered
ggplot(data = chicks_0_21, aes(x = Time, y = weight)) +
  geom_boxplot(notch = T, aes(fill = as.factor(Time)))


#Run as ANOVA

summary(aov(weight ~ as.factor(Time), data = chicks_0_21))

# Perform a Tukey post-hoc test

TukeyHSD((aov(weight ~ as.factor(Time), data = chicks_0_21)))

# Visualise the Data

plot(TukeyHSD((aov(weight ~ as.factor(Time), data = chicks_0_21))))

#This is still a one factor ANOVA

summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(2)))) #what is the significance dfference of weight by
#diet, only on day 2

#We can already see there is already a significant difference

#We want to see the significant difference of each day, but we don't need to do it all at once. 
#This is where we used a multiple factor ANOVA

# ~ this is a Tilde
#T-test and ANOVA to y as a function of x
# BUT if we want to do multiple factors 
#summary just simplifies the output
#inside #aov() is the formula

#what is the effect of diet AND time on weight, in just day 0 and 21 - see below
#Do the chickens weight more at day 0 than at day 21, regardless of diet
#DO the chickens weigh more on a particular diet

summary(aov(weight ~ Diet + as.factor(Time), data = filter(chicks, Time %in% c(0, 21))))

#Results:
#shows diet, but then also shows as.factor(Time)
#shows what are your different resulst based on each factor
#degrees of freedom (n-1)
#Pr>F IS your P value
#residuals: the amount of variation that is not explained by the examined factors

#Now look at interactions BETWEEN factors
#Is there a significant relationship between diet and time

summary(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0,21))))

# Let's look at the Tukey results
TukeyChick <- TukeyHSD((aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0,21)))))

plot(TukeyChick)

#What is the interaction between factors? Shown with a *

# Create a line graph to help explain this concept
# First create mean values by Time and Diet
#one mean value, per diet, per day

chicks_mean <- ChickWeight %>% 
  group_by(Diet, Time) %>% 
  summarise(weight_mean = mean(weight, na.rm = T))



ggplot(data = chicks_mean, aes(x = Time, y = weight_mean, colour = Diet)) +
  geom_point(shape = 15, size = 5) +
  geom_line(size = 2) +
  labs( x = "Time (days)", y = "Mean Weight (kg)")

#But what if we don't ave normal data 


# Working with abnormal data ----------------------------------------------

#if data are not normally distributed (lots of outliers) now we need a non parametric test

wilcox.test() #and then one fills this in the same for t.test
#gives you the same output as a t-test if your data are non-parametic


# Krushkall- Wallis test --------------------------------------------------

library(pgirmess)
# And now for the Kruskall-Wallis test

kruskalmc(weight ~ Diet, data = chicks_0_21)





