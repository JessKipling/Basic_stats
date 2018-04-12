#RDay1.R
#Purpose
# to practise some fo the concepts that we will encounter
# 12 April 2018


# Integers ----------------------------------------------------------------

integer_r <- as.integer(seq(5, 14, by = 1))

integer_r #lists all numbers in this list

summary(integer_r)  #a brief summary of the data

#integers are discrete/nominal data

#continuous data contains an integer part and a decimal part

#generating a sequence of numeric value
numeric_r <- seq(23, 43, length.out = 10)
#legnth decides amount of values in dataset

#Dates in R 
dates_r <- seq(as.Date("2005-12-16"), as.Date("2005-12-25"), by = "day")
#asks what is the difference in days between these two dates
#format "yyyy-mm-dd" is important
#we wont go into much detail on this
dates_r
summary(dates_r)


# load libraries ----------------------------------------------------------

library(tidyverse)


# Dataframes --------------------------------------------------------------

df_r <- data.frame(integers = integer_r,
                  numeric = numeric_r,
                  dates = dates_r)
#create a tibble, each column will be named as above
#then upgrade it to a tibble

df_r <- as_tibble(df_r)
summary(df_r)

# different types of data can be combined into a dataframe called a tibble

df_r

#qualitative data classfies things into various categories, is descriptive
#categories

# People

people_r <- as.factor(c("funny hair", "beautiful",
                        "beanies"))

# colours
colour_r <- as.factor(c("red", "blue"))

# electronics
elec_r <- as.factor(c("laptops",
                      "desktops",
                      "smartphones"))

summary(colour_r)

#factor variables differ from continuous and real data



# Ordinal Data ------------------------------------------------------------

# classes are somewhat subjective

levels(colour_r)

#ordering colours from coldest to warmest - could be used to order density in High, medium,low
#tells computer what is better than the other
colour_qual <- ordered(c("blue", "green",
                            "yellow", "orange", "red"),
                       levels = c("blue", "green",
                                  "yellow", "orange",
                                  "red"))

colour_qual


# Binary Data -------------------------------------------------------------

# Measurement has one of only two outcomes. Like flipping a coin. Abscence and Prescence. True/False

binary_r <- c(TRUE, FALSE, TRUE, TRUE)
summary(binary_r)

# Characters --------------------------------------------------------------
#creating a character vector
sites_r <- c("Ystervarkpunt", "Betty's Bay", 
             "Gaansbaai", "Sea Point")
summary(sites_r)


# Missing Values ----------------------------------------------------------

#in R its specified as N/A

chicks_nest <- c(3, 2, 0, 10, 5, 6, 8, 2, 4, NA)
summary(chicks_nest)
#there is a difference between an NA and a 0
#influences SD, mean, E.T.C



# Viewing our Data --------------------------------------------------------

summary(ChickWeight)
Chick <- ChickWeight

#seek first or last few lines
head(Chick, 7)
tail(Chick, 8)

#Choosing specific data in rows 1, 54,61,12 and only from the second column
Chick[c(1,54,61,12), 2]



# Chapter 3: Decsriptive Statistics  -------------------------------------------------
#cleaned our environment

#create a dataframe
chicks <- as.tibble(ChickWeight)

#Pipe lets us chain commands together into logical order

chicks %>% 
  summarise(n())
#scientific notation "n" tells us how many in dataset
#we are starting to qunatify real data


# Measures of Central Tendency --------------------------------------------

mean_wt <- chicks %>% 
  summarise(mean_wt = mean(weight))

# Be more specific 
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>%
  summarise(mean_wt = mean(weight),
            median_wt = median(weight))

#means can be less accurate with outliers, which is why median is useful


# Visuaulising the denisty of Data ----------------------------------------

library(ggplot2)

ggplot(data = filter(chicks, Time == 21),
       aes(x = weight, fill = Diet)) +
         geom_density(alpha = 0.4)  

library(e1071)


# Skewness ----------------------------------------------------------------
 # compare differences in mean and median against skewness
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>%
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
              skew_wt = skewness(weight))

#if it is negatively skewed, the mean is behind the median
#skewness shows where majority of the data is
#tails of the data are different


# Kurtosis ----------------------------------------------------------------

#bell shaped curve with misbehaving tails
#standard bell curve lips - mesocurvic
#thin tailed distribution has few outliers
# fat tail is a squashed bell curve

#calculate the kurtosis of the tails of a distribution
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>%
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            skew_wt = skewness(weight),
            kurtosis_wt = kurtosis(weight))
#kurtosis looks at the tails


# Variation and Spread ----------------------------------------------------

#variance first finds the mean
#it finds the variation around the mean


# Standard Deviation ------------------------------------------------------

#square root of variance
#you tend to show your mean plus minus standard deviation 
# it gives people a good idea of your sample


# A summary of many statistical properties --------------------------------

wt_summary <- chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(wt_mean = mean(weight),
            wt_median = median(weight),
            wt_var = var(weight),
            wt_sd = sd(weight))

# Quantiles / Quartiles  --------------------------------------------------

#the median is the 50th percentile, the middle quartile
#a median another half way below the median is the lower quartile
#same above is the upper quartile


wt_summary <- chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(wt_mean = mean(weight),
            wt_median = median(weight),
            wt_quart1 = quantile(weight, 0.25),
            wt_quart3 = quantile(weight, 0.75),
            wt_quart2 = quantile(weight, 0.5),
            wt_var = var(weight),
            wt_sd = sd(weight))




