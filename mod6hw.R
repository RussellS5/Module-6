########################################################################################
# Summary: Tidying and analyzing cotton production trends in NC
# Date: September 25, 2019
########################################################################################

# Clear workspace & load packages ----
rm(list=ls(all=TRUE))
library(tidyverse)
library(dplyr)
library(tidyr)

# 2. Read & inspect the dataset ----
cotton <- readr::read_csv("https://raw.githubusercontent.com/BAE-R-Coding/Module-6/master/data/cotton-usda-nass.csv")
str(cotton)
head(cotton)
tail(cotton)
dim(cotton)
summary(cotton)


# 3.1. Create a NC data subset ----
#isolates NC and selects only relevant variables
cotton_1 <- filter(cotton, state == "NORTH CAROLINA")
cotton_2 <- select(cotton_1, year, state, ag_district, county, data_item, value)
head(cotton_2)


# 3.2. Divide the data_item column ----
#separates the data_item column into two so that each piece of data has their own cell
cotton_3 <- separate(cotton_2, data_item, sep = " - ", into = c("cotton_type", "measurement"))
head(cotton_3)


# 3.3. Convert the value column to numeric type ---- 
#gets rid of unused data, converts value into numeric data
cotton_4 <- filter(cotton_3, value != "(D)")
cotton_4$value <- as.numeric(cotton_4$value)
str(cotton_4)
tail(cotton_4)


# 4. Visualizing trends ----  
#creates plot and points, makes a grid and puts plots on a 2nd row, applies a theme, names axes and title
ggplot(data = cotton_4) +
  geom_point(mapping = aes(x = year, y = value)) +
  facet_grid(rows = vars(measurement), cols = vars(ag_district), scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "Value", x = "Year", title = "Cotton Production in NC", caption = "Source: USDA NASS")


# 5. Summarize data from 2018 ---- 
#filter to only include 2018
cotton_5 <- filter(cotton_4, year == 2018)
head(cotton_5)

#separates the acres harvest from the yield in order to multiply them together
cotton_6 <- spread(cotton_5, measurement, value)
cotton_6 <- mutate(cotton_6, total_lbs = cotton_6$'ACRES HARVESTED' * cotton_6$'YIELD, MEASURED IN LB / ACRE')
top_n(cotton_6,3)

#The three counties in NC that produce the most total lbs of cotton in 2018 are Halifax, Martin, and Northhampton.
