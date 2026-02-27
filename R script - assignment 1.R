setwd("C:/Users/mitch/Desktop/leeann stuff/PSYR 6003 - Statistics") ## this is setting my working directory to tell R where to pull and save data from##
library(tidyverse) ## I am loading in packages I will be using##
library(dplyr)
library(ggplot2)


avengers <- read_csv("avengers.csv") ##this is loading in the .CSV file "avengers.csv" which is saved in my working directory and assigning it as an object names avengers##
head(avengers) ##this inspects the data and shows you the columns names and variable type for each column
View(avengers)##This opens the full avengers data frame in a new tab so that I can inspect to data for unusual cases##


###the following code inspects each variable to tell you which cases have missing values. Using the View function I found 2 rows with missing values I just wanted to double check that I didn't miss any###
is.na(avengers$iq)
is.na(avengers$agility)
is.na(avengers$speed)
is.na(avengers$strength)
is.na(avengers$damage.resistance)
is.na(avengers$superpower)
is.na(avengers$flexibility)
is.na(avengers$willpower)
is.na(avengers$ptsd)
is.na(avengers$north_south)
is.na(avengers$died)
is.na(avengers$kills)
is.na(avengers$injuries)
is.na(avengers$minutes.fighting)
is.na(avengers$shots.taken)


AV<-drop_na(avengers) ### the drop_na function is part of the tidyr package and it removes missing values from a dataset so I am using it to remove missing values from the avengers dataset and I am assigning the cleaned dataset as a new object called AV##
View(AV) ##again I just like to view a dataset when I make changes just as a double check##


AV_CE<-AV%>%
  mutate(CombatEffectiveness = agility+speed+strength+willpower) ### this is making a new column for the variable combat effectiveness which is a sum of an individuals agility, speed, strength, and willpower###

View(AV_CE) ## again I am opening the new dataset to make sure the new variable has been made##


AV_SN_D<- AV_CE %>%
  filter(superpower == "no")%>%
  filter(died == "yes") ## this is making a subset of the avengers dataset with only those that died and with no superpowers using the tidyverse package###

View(AV_SN_D)### opening the new dataset to ensure it filtered correctly###

write_csv(AV_SN_D, "avengers_nopowers_dead.csv") ## this saves the new dataset as a .csv file in my working directory##

library(haven) ## this package is needed to be able to create a .sav file###

write_sav(AV_SN_D, "avengers_nopowers_dead.sav") ## this saves the dataset as a .sav or SPSS file in my working directory###


descriptives1<-AV_SN_D%>%
  summarise(mean(CombatEffectiveness),
            sd(CombatEffectiveness),
            min(CombatEffectiveness),
            max(CombatEffectiveness),
            mean(kills),
            sd(kills),
            min(kills),
            max(kills),
            mean(injuries),
            sd(injuries),
            min(injuries),
            max(injuries)) ##I am creating an object containing the descriptive statistics for combat effectiveness, Kills, and injuries###
View(descriptives1) ##this opens the object in a new tab so I can see the values###


descriptives2<-AV_SN_D%>%
  group_by(north_south)%>% ## use the group by function so that the summarise function will give the descriptive statistics for north and south battlefield separately###
  summarise(mean(CombatEffectiveness),
                 sd(CombatEffectiveness),
                 min(CombatEffectiveness),
                 max(CombatEffectiveness),
                 mean(kills),
                 sd(kills),
                 min(kills),
                 max(kills),
                 mean(injuries),
                 sd(injuries),
                 min(injuries),
                 max(injuries)) ###then the same as above this just specifies which descriptive statistics to calculate and puts them in the object "descriptives 2"###
View(descriptives2) ## again opens the object in a new tab so I can see the statistics###


library(pwr)
library(metafor)
library(effectsize) ###loading in the packages I will need to the power calculation###


pwr.t.test(n = NULL, d = 0.2, sig.level = 0.05 , power = 0.8, 
           alternative = "two.sided") ### this conducts a power calculation for the independent samples t-test, with an effect size of 0.2, alpha 0.05 and power of 0.8###


library(TOSTER)### this package is needed to conduct equivalence tests (i.e. to answer question 9)###


power_t_TOST(n = NULL, alpha = 0.05, power = 0.8, eqb = 0.2)###this is the code to ru the equivalence test###


AV_SY<-AV_CE%>%
  filter(superpower == "yes") ### this is just splitting up the clean dataset into those with superpowers and then AV_SN is those without so I can get my sample size for the effect size calculation, I'm sure there is a faster way to find sample size based on a specific variable but I couldn't think of it###
View(AV_SY)

AV_SN<-AV_CE%>%
  filter(superpower == "no")
View(AV_SN)


stats <- escalc(measure = "SMD", 
                ti = 4.25,
                n1i = 32,
                n2i = 780,
                var.names = c("d","variance")) ### this calculates the effect size and 95% CI for the t-test, ti is the t-stat of 4.25, n1i is the sample size for those with superpowers, n2i is sample size for those without superpowers###
summary(stats) ## this prints the stats###


