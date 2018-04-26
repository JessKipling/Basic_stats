# Thursday 26 April
#Jess Kipling



# Confidence Intervals ----------------------------------------------------

library(rcompanion)

Input <- ("
Student  Sex     Teacher  Steps  Rating
          a        female  Jacob    8000   7
          b        female  Jacob    9000  10
          c        female  Jacob   10000   9
          d        female  Jacob    7000   5
          e        female  Jacob    6000   4
          f        female  Jacob    8000   8
          g        male    Jacob    7000   6
          h        male    Jacob    5000   5
          i        male    Jacob    9000  10
          j        male    Jacob    7000   8
          k        female  Sadam    8000   7
          l        female  Sadam    9000   8
          m        female  Sadam    9000   8
          n        female  Sadam    8000   9
          o        male    Sadam    6000   5
          p        male    Sadam    8000   9
          q        male    Sadam    7000   6
          r        female  Donald   10000  10
          s        female  Donald    9000  10
          t        female  Donald    8000   8
          u        female  Donald    8000   7
          v        female  Donald    6000   7
          w        male    Donald    6000   8
          x        male    Donald    8000  10
          y        male    Donald    7000   7
          z        male    Donald    7000   7
          ")

data <- read.table(textConnection(Input),header = TRUE)
summary(data)

#Execute function: calculate confidence intervals

groupwiseMean(Steps ~ 1,data = data, conf = 0.95, digits = 3) #where the data resides, calc 95 CI, report data to
   #3 signif figures (dgits)
  #means steps taken by students across all class, irrespective of various treatments

#What is the mean and 95 CI of males anf females seperately
#simply putting that info into the equation

groupwiseMean(Steps ~ Sex, data = data, conf = 0.95,digits = 3)

#the affect of both sex and teacher name, on influencing steps taken

grpwise <-groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)


#plot mean as a point, and plot around the point the confidence intervals, and display the affect of teacher and sex
#on the mean steps


library(ggplot2)
library(tidyverse)

# Create a graph with CIs -------------------------------------------------

CIbySex <- ggplot(data = grpwise, aes(x = Teacher, y = Mean)) +
  geom_point(aes(x = Teacher, y = Mean, colour = Sex), position = "dodge") +
  geom_line(aes(group = Teacher)) +
  geom_errorbar(aes(ymin = Trad.lower, ymax = Trad.upper), width = 0.2)+
  facet_wrap(~Sex)

CIbyTeacher <- ggplot(data = grpwise, aes(x = Sex, y = Mean)) +
  geom_point(aes(x = Sex, y = Mean, colour = Teacher), position = "dodge") +
  geom_line() +
  geom_errorbar(aes(ymin = Trad.lower, ymax = Trad.upper)) +
  facet_wrap(~Teacher)


#bootstrapping takes multiple samples from data and calculates outputs repeatedly




# Chapter 11: Testing Assumptions -----------------------------------------

#testing normality and homoscedasticity

# independent, normal and homoscedasttic
# IID: independent and identity distributed
# dependent variable must be a continuous variable

#equal variances require:
#t.test(..., var.equal=TRUE)

#Kinds of transformations

#Log transformations

#logtransform the data thats called steps

#nat log, logbased 10, sqrrt, cubrrt

dat2 <- data %>% 
  mutate(log = log(Steps),
         LogB10 = log10(Steps),
         Sqrt = sqrt(Steps),
         cubert = ((Steps)^(1/3)))


logplot <- ggplot(data = dat2, aes(x = log)) +
  geom_histogram(binwdith = 30, aes(fill = Sex),
                 position = "Dodge")

log10plot <- ggplot(data = dat2, aes(x = LogB10)) +
  geom_histogram(binwdith = NULL, aes(fill = Sex),
                 position = "Dodge")

Sqrtplot <- ggplot(data = dat2, aes(x = Sqrt)) +
  geom_histogram(binwdith = 30, aes(fill = Sex),
                 position = "Dodge")

Cubeplot <- ggplot(data = dat2, aes(x = cubert)) +
  geom_histogram(binwdith = 30, aes(fill = Sex),
                 position = "Dodge")

library(ggpubr)

FinalLogsplots <- ggarrange(logplot, log10plot, Sqrtplot,Cubeplot,
          ncol = 2, nrow = 2, common.legend = TRUE)





