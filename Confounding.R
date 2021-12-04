# Confounding Overview

# Correlation Is Not Causation
# 1) Identify examples of spurious correlation and explain how data dredging can lead to spurious correlation.
# 2) Explain how outliers can drive correlation and learn to adjust for outliers using Spearman correlation.
# 3) Explain how reversing cause and effect can lead to associations being confused with causation.
# 4) Understand how confounders can lead to the misinterpretation of associations.
# 5 ) Explain and give examples of Simpson's Paradox.

# When using the cor() function we can add the argument for method = "Spearman"
# to use the spearman method of correlating that ADJUSTS FOR OUTLIERS

# Switching the cause & effect
# Children who get more tutoring perform worse on test |VS| 
# Children who perform worse on test get more tutoring
# High correlation, but INTERPRETATION is false

# Another example is sons being tall CAUSING fathers to be tall

# ------------------------------------------------------------------------------
# Example 1: Admissions data for UC Berkey in 1973

library(tidyverse)
library(ggplot2)
library(dplyr)
library(dslabs)
data(admissions) # Major, Gender, Admitted, Applicants
admissions


# Calculate the total percent admitted for women & men in 1973
admissions %>% group_by(gender) %>% 
  summarize(total_admitted = sum(admitted*applicants)/sum(applicants))

# Are gender and admissions independent - Low P value thus not independent
admissions %>% group_by(gender) %>%
  summarize(total_admitted = round(sum(admitted/100*applicants)),
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>%
  chisq.test()

# Do different majors play a role in the gender acceptance gap? Using spread
# function to list out admitted numbers by gender then calculate the difference.
# Women are admitted more in 4/6 of the major types
admissions %>% select(major, gender, admitted) %>% spread(gender, admitted) %>%
  mutate(women_minus_men = women - men)

# Creating a plot to show the total percent admitted to major versus the percent
# of women applicants to that specific major. Majors with greater selectivity were
# applied to by more women compared to the majors with the highest selectivity

admissions %>% group_by(major) %>% 
  summarize(major_selectivity = sum(admitted*applicants)/sum(applicants),
            percent_women_applicants = sum(applicants* (gender=="women"))/sum(applicants)*100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text() + labs(title = "Percent Women Applicants vs Major Selectivity by Major",
                     x = "Major Selectivity",
                     y = "Percent of Women Applicants")

# Plot number of applicants admitted and not by major type and seperate the data
# out by gender

admissions %>% 
  mutate(yes = round(admitted/100*applicants), no  = applicants - yes) %>%       # Creating yes / no groups for plot colors
  select(-applicants, -admitted) %>%                                             # Removes applicants and admitted column 
  gather(admission, number_of_students, -c("major","gender"))  %>%              # adds the yes and nos to one admission column 
  ggplot(aes(gender, number_of_students, fill = admission)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(. ~ major) +
  labs(title = "Total Number of Students Accepted By Gender & Major  - UC Berkely 1973",
       x = "Gender Of Student",
       y = "Total Students")

# Plotting an overall view of the percentage of admittance by gender and major
# Overall men have been accepted more, but most of these acceptances come from 
# majors that women have a low total number of applicants. Taking away the top two
# majors, the women would be in the lead for percentage admitted

admissions %>% mutate(percent_admitted = admitted*applicants / sum(applicants)) %>%
  ggplot(aes(gender, y = percent_admitted, fill = major)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Percentage of Students Admitted by Gender & Major - UC Berkely 1973",
      x = "Gender Of Student",
      y = "Percentage Admitted")

admissions %>% ggplot(aes(major, admitted, col = gender, size = applicants)) + geom_point()

admissions %>%  group_by(gender) %>% summarize(average = mean(admitted))








