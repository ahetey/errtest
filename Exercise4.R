################
# plotting
###############
# create a plot with percentage dead patients per gender and per treatment (HEP/ASP yes,no)
#############
# load libraries
library(tidyverse)
library(formattable)
# read in data
setwd("c:/Users/heteya/Dropbox/bih/devel/r/bih-academy-kurs2/")
# my_data<-read_csv("IST_corrected.csv")
my_data<-read.csv(url("https://datashare.is.ed.ac.uk/bitstream/handle/10283/128/IST_corrected.csv?sequence=5&isAllowed=y"))
write.csv(my_data, "IST_corrected2.csv")

####
# create a data frame for plotting
####
my_data$HEP<-ifelse(my_data$RXHEP=="N","No Hep","Hep")
my_data$ASP<-ifelse(my_data$RXASP=="Y","Asp","No Asp")

plot_data <- 
  # based on my_data
  my_data %>% 
  # create a new variable (T/F) whether a patient is dead from DDEAD variable
  # reason for this is: we cannot calculate means across "Y" and "N" values
  mutate(pat_dead = ifelse((my_data$DDEAD == "Y"), TRUE, FALSE)) %>%
  # group the analysis by the variables of interest (SEX, Heparin or Aspirin Treatment)
  group_by(SEX, HEP, ASP) %>%
  # Now summarise the percentage and SEM, calculate for the plot also mean plus/minus sem
  summarize(n=n(), percent_dead=mean(pat_dead), SEM_dead=sd(pat_dead)/sqrt(n), SEM_pos=percent_dead+SEM_dead, SEM_neg=percent_dead-SEM_dead)

# now create a barplot with error bars
# how can you create a combined factor from ASP HEP?
# also create two separate facets for male and female



#######
# often we need to visualize binary variables
# here we use an example
# death of patient after # days after randomisation in dependence of AGE  
# FDEADD vs AGE
#######
my_data$DEAD_bin<-ifelse(my_data$DEAD!=9,1,0)
ggplot(aes(y=DEAD_bin,x=AGE),data=my_data)+geom_point()+geom_smooth()
# this is not a nice representation!
# we need something better!
# see for example here: 
# https://doi.org/10.1890/0012-9623(2004)85[100:ANMOPT]2.0.CO;2
# also as pdf on osf
# first summarise the data in a histogram format
# Summarise data to create histogram counts
# what is the min and max age?
min(my_data$AGE)
max(my_data$AGE)
# looking at this I decide to start at 10 years until 100 years in steps of 1
hist_data = my_data %>% 
  #first add new variable that codes breaks
  
  #then group by dead/alive and the breaks
  
  #count
  
  #if patients are dead, we want them to show on top with histogram on top so you need to 
  #calculate in this case the percentage as 1-percentage
  

####
# now plot this
####
ggplot() + #this just sets an empty frame to build upon
  #first add a histopgram with geom_segment use the help of geom_segment
  geom_segment()+
  #then predict a logistic regression via stat_smooth and the glm method (we will cover the details in the next session)
  stat_smooth()+
  #some cosmetics 
  scale_y_continuous(limits=c(-0.02,1.02)) +
  scale_x_continuous(limits=c(15,101)) +
  theme_bw(base_size=12)+
  ylab("Patient Alive=0/Dead=1")+xlab("Age")

#####
# find here more interesting visualisations
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
#####

