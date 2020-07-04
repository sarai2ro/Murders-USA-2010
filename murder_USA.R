#Adding packages
library(dslabs)
library(dplyr)
data("murders")     #database to analyze
library(tidyverse)
library(ggplot2)    #ggplot2 is part of the tidyverse suite of packages

#get a general view of the database 
str(murders)
head(murders)

#for a better analysis of the data, 
#we are using murder per capita (defined as murder rate), 
#because every state has different population.
murder_rate <- murders$total/murders$population *100000

#adding murder_rate to the data set
murders <- mutate (murders, rate = murder_rate)

#average (mean) murder rate
rate_average <- mean(murders$rate)
rate_average

#state with highest murders
max_index <- which.max(murders$rate)
murders$rate[max_index]   #maximum rate
murders$state[max_index]  #maximum state

#state with lowest murders
min_index <- which.min(murders$rate)
murders$rate[min_index]   #minimum rate
murders$state[min_index]  #minimum state 

#data frame, state, region and rate
murders_dataframe <- data.frame(murders %>% select(state, region, rate))

#ordering states by murder rate - descending order
murders_state_by_rate <- murders_dataframe[with(murders_dataframe, order(-rate)),]

#arrange by region then murder rate - descending order
murders_region_by_rate <- murders_dataframe %>% arrange(desc(region,rate))
murders_region_by_rate


#top 10 states with the highest murders rate
top_10 <- murders_dataframe %>% arrange(desc(murder_rate)) %>% top_n(10)
top_10

#states with murder rate below average
below_average <- murders %>% select(state, rate) %>% filter(rate <= rate_average)
below_average %>% arrange(rate)

#boxplot - murder rate by region
region_boxplot <- murders %>% ggplot (aes(region, rate)) + geom_boxplot()
print(region_boxplot +ggtitle ("Murder rate by region 2010"))

#scatterplot population vs total murders
population_point <- murders %>% ggplot(aes(population/10^6, total, label= abb, color= region)) + 
  geom_point(size =3)+ geom_text(nudge_x = 0.05) + scale_x_continuous(trans='log10') + 
  scale_y_continuous(trans='log10')+ xlab ('Population in milions(log scale)') + 
  ylab('Total (log scale)') + ggtitle ('Murders in the USA vs Population 2010')

#adding average line to last plot
av <- murders %>% summarise(rate = sum(total)/ sum(population) *10^6) %>%
  pull(rate)
population_point + geom_point(aes(col=region), size=3) +
  geom_abline(intercept = log10(av), lty = 2, color = 'darkgrey')
population_point
