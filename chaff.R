
#import data

chaff <- read.table("raw_data/chaff.txt", header = T)

#tidy data

library(tidyverse)

chaff2 <- gather(chaff, key = sex, mass)

#summarise data

chaff_sum <- chaff2 %>% 
  group_by(sex) %>% 
  summarise(mean = mean(mass),
            n = length(mass),
            std= sd(mass),
            se = std/sqrt(n))
#analyse data

anova <- aov(mass ~ sex, data = chaff2)
summary(anova)

#there is a sig diff in mass btw sexes

#create a figure

library(ggplot2)

chaff2 %>% ggplot(aes(x = sex, y = mass)) +
  geom_boxplot()


