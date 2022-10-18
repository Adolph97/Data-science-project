#Ensure the current working directory is set to the path where your script and csv files are located
setwd("C:/Users/DELL/Desktop/Data ass")
#load the tidyverse package in order to manipulate your data in a dataframe and 
#gain access to a world of functionality while handling data
#import the necessary libraries
# The code below will install all the required packages that will be required for the analysis of this assignment
#install.packages("tidyversse")
library(tidyverse)

#import the first set of data from the escape.csv and analysis.csv
escape <- read.csv("escapes CMM 535 Coursework - clean.csv",header=TRUE,sep=",",stringsAsFactors = T)

#Now to clean and explore the dataset in order to gain a better understanding  
summary(escape$Age)
#We have to take into consideration that there are some outliers in the dataset imported 

escape$Age[escape$Age > 100] <- NA #remove outlier
escape$Age[is.na(escape$Age)] <- mean(escape$Age,na.rm = T)

# We then proceed to the next column of interest, which is the 
#Deal with 
escape$Average.Weight.Kg. <- as.numeric(as.character(escape$Average.Weight.Kg.))
escape <- escape[!is.na(escape$Average.Weight.Kg.),]
escape$Average.Weight.Kg.[escape$Average.Weight.Kg. > 50] <- NA
escape$Average.Weight.Kg.[is.na(escape$Average.Weight.Kg.)] <- mean(escape$Average.Weight.Kg.,na.rm = T)

str(escape$Final.Number.Escaped)
str(escape$Final.Number.Recovered)

#Fix the last numerical columns that will be used for typing
escape$Final.Number.Escaped <- as.integer(as.character(escape$Final.Number.Escaped))
escape$Final.Number.Recovered <- as.integer(as.character(escape$Final.Number.Recovered))


escape$Final.Number.Escaped[is.na(escape$Final.Number.Escaped)] <- 0
escape$Final.Number.Recovered[is.na(escape$Final.Number.Recovered)] <- 0

#Another thing that must be considered while considering the columns that must be used for the analysis, is editing the unique names of the final reasons for fishes escaping
#It would be a desirable approach to change the default values in the column of interest
#Let's proceed to rename the variables
unique(escape$Final.Escape.Reason)

#Something that was unique about this is the fact that there was a reason that stated "no actual escape of fish"
#Going forward, we should exclude that from our query through a filter function
realEsc <- escape %>% 
  replace_na(list(Final.Escape.Reason = "unknown")) %>% 
  filter(Final.Escape.Reason != "no actual escape of fish") %>%
  mutate(Final.Escape.Reason = recode(Final.Escape.Reason,
                                      `other` = "unknown"))

#We will now attempt to clean up some entries as they related 
#Exploratory Data Analysis using some descriptive statistical analysis


library(ggplot2)
#Going forward during our data exploration, we will hope to create further queries
#Questions to ask under EDA
#1  What were the major regions had the most frequency for our dataset?
region <- escape %>% 
  count(Region) %>% 
  arrange(desc(n))
  
region %>% 
  ggplot(aes(x=reorder(Region,n),y=n, fill = Region))+
  geom_col() + coord_flip() +
  scale_fill_brewer(palette="Spectral")+
  theme_bw()+
  ggtitle("Distribution of regions")+
  xlab("Regions") + ylab("")

#Going forward, the top 5 should be of heavy interest 


#2 What fish specie had the most number of escape?
fish <- escape %>% 
  count(Escaped.Species) %>% 
  arrange(desc(n))

fish %>% 
  ggplot(aes(x=reorder(Escaped.Species,n),y=n, fill = Escaped.Species))+
  geom_col() + coord_flip() +
  scale_fill_brewer(palette="Spectral")+
  theme_bw()+
  ggtitle("Distribution of regions")+
  xlab("Number of fishes") + ylab("")
# create a new data frame that filters the essential values to be analyzed
primaryFish <- escape %>% 
  filter(Escaped.Species == "atlantic salmon" |
           Escaped.Species == "rainbow trout")

#3 Relationship between the different regions and their health status
locat <- escape %>%
  drop_na(Operator.at.Time.of.Escape) %>% 
  select(Operator.at.Time.of.Escape) %>%
  count(Operator.at.Time.of.Escape) %>% 
  arrange(desc(n)) %>% 
  slice(1:10)

locat %>% 
  ggplot(aes(x=reorder(Operator.at.Time.of.Escape,n),y=n, fill = Operator.at.Time.of.Escape))+
  geom_col() + coord_flip() +
  scale_fill_brewer(palette="Spectral")+
  theme_bw()+
  ggtitle("Top 10 operators at the time of escape")+
  xlab("Operators") + ylab("")  

#top 10 locations that were responsible for losing fish
locatLoss <- escape %>% 
  filter(Final.Number.Escaped > 0) %>% 
  group_by(Site.Name) %>% 
  summarise(total.Loss = sum(Final.Number.Escaped)) %>% 
  arrange(-total.Loss) %>% 
  slice(1:10)


  
locatLoss %>% 
  ggplot(aes(x=reorder(Site.Name,total.Loss),y=total.Loss, fill = Site.Name))+
  geom_col() + coord_flip() +
  scale_fill_brewer(palette="Spectral")+
  theme_bw()+
  ggtitle("Top 10 locations that lost fish")+
  xlab("Locations") + ylab("") 



#4 An investigation of water type of the escaped species
#A simple barplot visualization would suffice here
barplot(table(escape$Water.Type),main = "The distribution of fishes across their water types")
# Seawater is the most dominant habitat




#5 What is the age distribution and average weight distribution?

escape %>% 
  ggplot(aes(Age))+
  geom_histogram(binwidth = 5 ,fill= "#0d2894")+
  theme_bw()+
  labs(title = "Histogram of the Age(in months) of the escaped fishes",
       x = "Age (in months)",
       y = "Frequency")
mean(escape$Average.Weight.Kg.)

escape %>% 
  ggplot(aes(Average.Weight.Kg.))+
  geom_histogram(binwidth = 5 ,fill= "#2a2c85")+ 
  theme_bw()+
  labs(title = "Histogram of the Average weight of the escaped fishes",
       x = "Average weight)",
       y = "Frequency")

#6 What were the most common reasons for the loss of fishes
freason <- realEsc %>%
  drop_na(Final.Escape.Reason) %>% 
  select(Final.Escape.Reason) %>%
  count(Final.Escape.Reason) %>% 
  arrange(desc(n)) %>% 
  slice(1:10)  

#Now we continue with the visualization of the data



freason %>% 
  ggplot(aes(x=reorder(Final.Escape.Reason,n),y=n, fill = Final.Escape.Reason))+
  geom_col() + coord_flip() +
  scale_fill_brewer(palette="Spectral")+
  theme_bw()+
  ggtitle("Top 10 reasons that caused the escape of the fishes")+
  xlab("Reasons") + ylab("")  


#7 What the representation of health for the fishes considered
h <- escape %>% 
  filter(Health.Surveillance == "medium"|Health.Surveillance == "low"|Health.Surveillance == "high") %>% 
  count(Health.Surveillance) %>% 
  arrange(desc(Health.Surveillance)) %>% 
  mutate(percentage = n/sum(n) * 100,
         pos_pie = round(cumsum(percentage) - 0.5 * percentage,2))

ggplot(data = h) +
  geom_col(mapping = aes(x="",y=percentage,fill=Health.Surveillance))+
  coord_polar(theta = "y") +
  geom_text(aes(x="",y=pos_pie, label = scales::percent(percentage, scale = 1)))



#8 What operator had the largest number of lost fishes
# A look at operators that loss the most amount of fishes
opCum <- escape %>% 
  group_by(Operator) %>% 
  summarise(cum = sum(Final.Number.Escaped)) %>% 
  arrange(desc(cum)) %>% 
  slice(1:10)

ggplot(data = opCum) +
  geom_bar(aes(x = reorder(Operator, cum), y = cum, fill = Operator),
           stat = "identity",show.legend = F) +
  #theme_bw() +
  ggtitle("Operators at the time of escape of the escaped fish species") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 10)) +
  xlab("") + ylab("Number of operators") +
  coord_flip()

analysis <- read.csv("analysis CMM 535 coursework.csv",header = T,stringsAsFactors = T)

#I conducted a full join on the two available datasets since they had no similar columns
escapePlus <- merge(escape,analysis,by = "Site.Name", all.x = T, all.y = T)
write.csv(escapePlus, file="escapesPlus.csv", row.names = F)
#Another justification for the full based on the requirements of this assignment, is due to the fact there was no corresponding primary to be utilized


