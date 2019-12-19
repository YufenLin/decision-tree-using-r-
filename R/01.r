#
# Author:   Yu Fen Lin
# Purpose:  
# Date:     Dec. 18, 2019
#
# remove all objects in the Global Environment -----
rm(list = ls())

# install necessary packages ------
# data scource: https://www.rdocumentation.org/packages/titanic/versions/0.1.0 
install.packages("titanic")
install.packages("tidyverse")
# load necessary packages ------
library(titanic)
library(tidyverse)
library(stringi)
# load necessary data ---------
titanic <-
  titanic_train

class(titanic)  # "data.frame"

titanic %>% dim() # 891  12

titanic %>%
  View(title = "Titanic")
head(titanic)

titanic %>% glimpse()
# Below is a brief description of the 12 variables in the data set
# PassengerId:<int> Serial Number
# Survived:<int> Contains binary Values of 0 & 1. Passenger did not survive — 0, Passenger Survived — 1.
# Pclass: <int> Ticket Class - 1st Class, 2nd Class or 3rd Class Ticket
# Name: <chr> Name of the passenger
# Sex: <chr> Male or Female
# Age: <dbl> Age in years 
# SibSp: <int> No. of Siblings / Spouses — brothers, sisters and/or husband/wife
# Parch: <int> No. of parents/children — mother/father and/or daughter, son
# Ticket: <chr> Serial Number
# Fare: <dbl> Passenger fare
# Cabin: <chr> Cabin Number
# Embarked: <chr> Port of Embarkment | C- Cherbourg, Q — Queenstown, S — Southhampton

names(titanic) # check out all the variables in the data set
# [1] "PassengerId" "Survived"    "Pclass"      "Name"        "Sex"         "Age"        
# [7] "SibSp"       "Parch"       "Ticket"      "Fare"        "Cabin"       "Embarked"  

summary(titanic)

summary(titanic$Age) # There is 177 NA's in Age
titanic$Embarked[grepl("^\\s*$", titanic$Embarked)] # There is two "" in Embarked
# Drop Embark = "" ------
titanic <- droplevels(titanic[!grepl("^\\s*$", titanic$Embarked),,drop=FALSE] )
# fill NA's in Age using mean of Age -----
titanic$Age[is.na(titanic$Age)] <- 
  round(mean(titanic$Age, na.rm = TRUE))

titanic$Survived = if_else(titanic$Survived == 0
                           , "Did not survive"
                           , "Survived")

# catogorical variables ------
titanic$Survived = as.factor(titanic$Survived)
titanic$Pclass = as.factor(titanic$Pclass)
titanic$Sex = as.factor(titanic$Sex)
titanic$Embarked = as.factor(titanic$Embarked)


summary(titanic)

# Analysis & Visualisations ------
ggplot(titanic, 
       aes(x=Sex, fill=Survived)) + 
  geom_bar() +
  labs(title = "Survival Rate by Gender"
       , caption = "Source: Titanic Passenger Survival Data Set v 0.1.0 by RDocumentation") +
  xlab("Gender") +
  ylab("Number of Passenger") +
  theme_minimal()




prop.table(table(titanic$Sex))  
    

