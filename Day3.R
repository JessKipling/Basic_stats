#Day3
#17 April 2018



# Introductory Information ------------------------------------------------


#simple data
#binomial - yes or no, black or white, absence or presence, success or unsucessful 

#larger number of samples we can draw from a population, the closer the approximation will be to the accurate 
  #represenation of the population
  #importance of sample size 
  #in short, make sure your statistics are appropriate for the data you have

#negative binomial distribution
#the number of times before the outcome will be a fail

#geometric distribution
  #the likelihood of antyhing other than your success occuring

#Poisson Distribution: fixed interval of time in which you countthe occurences of something, for example,
  #data above would result in a poisson distribution

#interesting question: no things lacking defintive orientation have a natural orientation

#continuous normal distribution
  #uniform distribution has a min an a max value but no certain value has a greater chance of 
  #arising randomly

#Chi-squared distribution
  #pie-chart equivalence of dissapproval

#expotential distribution
  #looks at the amount of time elapses between the events
  #sort of opposite of poisson 
  #results in a right skewed distribution

#Paranormal distribution



# Generating a Cullen and Frey Graph --------------------------------------

library(logspline)
library(fitdistrplus)

#generate a random normal data

r_norm <- rnorm(n = 1000, mean = 13, sd = 1) #samplw size, known mean, sd
#this generated a long vector o f numbers

hist(r_norm)

descdist(r_norm)

# uniform data

y <- runif(100)
par(mfrow = c(2, 2))
plot(x = c(1:100), y = y)
hist(y)
descdist(y, discrete = FALSE)
runif(r_norm)


# t-tests -----------------------------------------------------------------

library(tidyverse)
library(plotly)

#ttest used for two samples, ANOVA used for 3

#what is an assumption? 
#with t tests you assume data are normally distributed

# Random normal data
set.seed(666)
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

# Create histogram
h <- ggplot(data = r_dat, aes(x = dat, fill = sample)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  labs(x = "value")
\

# Check Assumptions -------------------------------------------------------

# NOrmality
# FOr this we may use the Shapiro-Wilk test
#If p value is less than 0.05 this means the data aren't normal
#testing the null hypothesis that the data are not significantly from normal (null hypothesis)
shapiro.test(r_dat$dat)

# But this is testnig all of the data together
# We must be a bit more clever about how we make this test
#we have to tell R to run the data based o nthe goups

r_dat %>% 
  group_by (sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2])) 


#in order to play nicely with tidy data we only want p data
#the data are non-normal when p < 0.05


# CHeck Homoscedasticity --------------------------------------------------

#There are many ways to do this
#which is the similarity of variance between data
#for now we will simpyl say that this asummption is met when
  #the varianceo of samples are not more that 2 -4 times greater
  #than another

#Checking the variance for everything
  #this is incorrect to do
var(r_dat$dat)

#or do it in tidy
r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]),
            r_norm_var = var(dat))

#are the data independent of one another
#or are they grouped

#you cannot compare categories and categories in a t test
#your data have to be values
#functions are available in the book to do extra info/work on this


# A One Sampled T-test ----------------------------------------------------

r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")

#visulisations?

hist(r_dat$dat)

#test normailty of the distribution
shapiro.test(r_one$dat)

#run the test

t.test(r_one$dat, mu = 20) #we need the population mean

# Run a test we know will produce a signnifcant result
t.test(r_one$dat, mu = 30)


# Pick a Side -------------------------------------------------------------

#are these data smaller /less than the population mean

t.test(r_one$dat, mu = 20, alternative = "less" ) #alternative asks if its smaller or less than the other tail

#Greater
t.test(r_one$dat, mu = 20, alternative = "greater" )

# But what about for the larger populationmean?
#Are the samples less than the population of 30
t.test(r_one$dat, mu = 30, alternative = "less")

#What about greater than?
t.test(r_one$dat, mu = 30, alternative = "greater")


# Two Sample T-Test -------------------------------------------------------

#what happens when you subtract a mean from one sample from another and divde by a group std dev 
  #this is your t table

#create another dataframe

r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

#Run a default/basic test

t.test(dat ~ sample, data = r_two)
#we can run this but its not quite correct
#by default, R assumes that any data you give it does not have equal variable
#but if your variances are similar you need to tell R this
t.test(dat ~ sample, data = r_two, var.equal = TRUE)


histtwo <- hist(r_two$dat) 
histone <- hist(r_one$dat)

#Pick a side
#compare spits it out as the actual numbers - we don't have time for this today

#we will look at how two do a one sided, two sampled t test

#is A less than B
t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "less")
#Is A greater than B?
t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "greater")



# Workflow Exercises ------------------------------------------------------

#A t test workflow is good to do in own time

#loading data
ecklonia <- read_csv("Intro_R_Workshop_UWC2018/data/ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)

#data is converted from wide format into a long format 
  #this is more useful for the rest of the workflow

#Visualising the data

ggbox1 <- ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()

#notice that the different measurements are on different scales
#this is making comparing the data challenging
#however, we can still make *some* observations
  #e.g. that the measurement values at Batsata Rock seem to be greater than at boulders

#Refine our observation into a hypothesis
  #by what measurement are the kelps greater at one site than at the other?

#Stipe mass appears to be similar


# Creating a boxplot ------------------------------------------------------

# filter the data
ecklonia_sub <- ecklonia %>% 
  filter(variable == "stipe_length")

# then create a new figure
ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "stipe mass (kg)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#this generates the question: are the stipe masses at Bat. greater than at Boulders?
#comparison of two sample sets so T-test will be used
#a one sided T-test to assess if one is greater than the other
#does the data meet the assumptions for the test?
  #continuous...yes
  #independent...yes
  #normally distributed...?
  #Homoscedastic...?

# Checking for Normal Distrib. and Homosced. ------------------------------

ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(stipe_mass_var = two_assum(value)[1])
          

