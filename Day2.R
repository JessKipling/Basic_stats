# Day2 of Biostats
# 13 April 2018 (Friday)
# day in which we discuss data visulisation and distributions

#n ote: we get extra points for trying extra visulisations we don't do in class - so play around

# Load Libraries ----------------------------------------------------------
library(tidyverse)

# how do we calculate mean, median, variance and SD

# the mean
# rnorm wants 3 variables: n, mean, SD
rdat <- data.frame(dat = rnorm(n = 601, mean = 372, sd = 50),
                   sample = "A")

# Quick visulisation
rdat

# can make histograms, density plots
ghistogram <- ggplot(data = rdat, aes(x = dat), bins = 30) +
  geom_histogram()

gdensity <- ggplot(data = rdat, aes(x = dat)) +
  geom_density()

gdensity
ghistogram


# The Mean ----------------------------------------------------------------
# 3 steps:
# Sum of all the points
# divided by
# the number of the points
# lots of data boiled down to little bit of data using summarise 
rdat %>% 
  summarise(r_sum = sum(dat),
            r_n = n(),
            r_mean = r_sum/r_n,
            r_mean_func = mean(dat))


# The Median --------------------------------------------------------------

# Subset data at a position that is n+1/2
#slice selects a specific row of data (tell it what you want to slice by)
#$ is used to select certain parts within a dataframe

#brute force with base R
rdat$dat[(length(rdat$dat)+1)/2]

#tidyverse automagic way
rdat %>% 
  summarise(r_median = median(dat))

# trying to calc mean,median and mode on our own
#how do you order data

rdat_ordered <- order(rdat)

rdat_ordered


# Median in Tidyverse -----------------------------------------------------
#arranged in tidyverse to find the median

rdat %>% 
  arrange(dat) %>% 
  slice(n()/2)
#when you have an even number of data remember that its between the two middle values


# Finding Variance --------------------------------------------------------

# we use standard variation so we need to know how to get variance
# Sum of each value 
  # minus the mean
  # squared
#Divided by
  # the count of samples minus 1

#mutate to make a new column which is the values minus the mean
#mutate adds a new column onto the existing dataframe
rdat %>% 
  mutate(r_error = dat - mean(dat),
         r_error_square = r_error*r_error) %>% 
  summarise(r_squared_sum = sum(r_error_square),
            r_var = r_squared_sum/(n()-1),
            r_var_funct = var(dat))


rdat %>% 
  summarise(r_var = var(dat),
            r_sd = sqrt(r_var),
            r_sd_funct = sd(dat))




# Exercise 1 --------------------------------------------------------------
#constructing the same summary statistics as summary() returns

summary(ChickWeight$weight) # What we aim to get in our manual breakdown below

ChickWeight %>%
  summarise(min_weight = min(weight),
            quart_1 = quantile(weight, 0.25),
            med_weight = median(weight),
            mean_weight = mean(weight),
            quart_3 = quantile(weight, 0.75),
            max_weight = max(weight))


#Visualisation

library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(viridis)

#load our sa time data

sa_time <- read.csv("sa_time")

#edit our data

sa_time <- sa_time %>% 
  mutate(human = seq(1, n(),1))
#in sq: 1 is number of columns, N()is for numner of rows, and 1 is in increments of 1 

sa_time

View(sa_time)

#create long data
sa_long <- sa_time %>% 
  group_by(human) %>% 
  gather(key = "time_type", value = "minutes", -human) 
#gather up columns depending on how they are grouped - give it names
# key is name of column that contains our variables  
# Qualitative

#stacked bar graphs 

sa_count <- sa_long %>% 
  count(time_type) %>% 
  mutate(prop = n/sum(n)) #should be 3 rows & columns


# Stacked bar graphs

SatimeBar <- ggplot(data = sa_count,aes(x = "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Stacked Bar Graph", subtitle = "cumulative sum",
       x = NULL, y = "Count") +
  theme_minimal()

#Stacked proportion bar graph
  #a way we can represent a proportion of categories in a study
  #we are counting the answers for the various categories
  #the other way is a pie chart, but these arent used professioanlly

# A pie chart

satimepie <- ggplot(data = sa_count, aes(x = "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Pie Chart", subtitle = "but why though?",
       x = NULL, y = NULL) +
  coord_polar("y", start = 0) +
  theme_minimal()



# Histogram ---------------------------------------------------------------

ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram()
#this wasnt a  good looking graph/error in dataset layout whn we used sa_long so we made sa_clean

# Try again

satimehist1 <- ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(fill = time_type), position = "dodge") +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

#getting rid of one value

sa_clean <- sa_long %>% 
  filter(minutes < 10000) #filtering minutes column by anything less than 10k


satimeHist2 <- ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(fill = time_type), position = "dodge")


ggarrange(satimepie, SatimeBar, satimeHist2, ncol = 1, nrow = 3, labels = NULL,
          label.x = 0, label.y = 1, hjust = -0.5, vjust = 1.5,
          font.label = list(size = 14, color = "black", face = "bold", family = NULL),
          align = c("none", "h", "v", "hv"), widths = 2, heights = 1,
          legend = NULL, common.legend = FALSE)
#doesnt really look great.

satimehist1






