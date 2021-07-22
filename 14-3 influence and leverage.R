library(tidyverse)
library(lindia)

# create data
airport <- c(seq(1,22,1))
revenue <- c(233, 272, 253, 296, 268, 296, 276, 235, 253, 233, 240,  267, 338,
             243, 252, 269, 242, 233, 234, 450, 340, 200)
distance <- c(233, 209, 206, 232, 125, 245, 213, 134, 140, 165, 234, 205, 214,
              183, 230, 238, 144, 220, 170, 170, 290, 340)
population <- c(56, 74, 67, 78, 73, 54, 100, 98, 95, 81, 52, 96, 96, 73,
                55, 91, 64, 60, 60, 240, 70, 75)
data <- tibble(airport, revenue, distance, population)

# model of interest
m1 <- lm(revenue ~ distance + population, data = data)

# view results in the summary() function
summary(m1)

# check for influence/leverage
# this constructs the Cook's D graph
gg_cooksd(m1) + 
  geom_point(size=3) + 
  theme_minimal() + 
  theme(text = element_text(size=20))

# want to filter out the observations that were "spikes" on the graph
data2 <- filter(data, !(airport %in% c(20, 22)))
# we are saying to remove (the ! does this) when airport=20 or airport=22

# model of interest, without influence/leverage points
m2 <- lm(revenue ~ distance + population, data = data2)

# view results in the summary() function
summary(m2)
