library(dplyr)
library(ggplot2)
library(gganimate)

# Importing New Zealand greenhouse gases data for 2018
import <- read.csv("greenhouseGases2018.csv")

# Narrowing down data to useful columns
usefulData <- data.frame(import$region, import$anzsic_descriptor, import$gas, import$year, import$data_val)

# Ordering data based on years
dataYears <- usefulData %>% arrange(import.year)

# Changing column names of dataset
colnames(dataYears) <- c("region", "emission_type", "gas", "year", "emission_amount")

# Unique year values
uniqueYears <- unique(data$year)

# Finding indices of "Total" for emission_type
total <- as.data.frame(which(dataYears == "Total", arr.ind = TRUE))
totalData <- dataYears[total$row,]

# Removing total carbon dioxide equivalents rows
carbon <- as.data.frame(which(totalData == "Carbon dioxide equivalents", arr.ind = TRUE))
gasData <- totalData[-c(carbon$row),]

# Total carbon dioxide equivalents
totalEmissions <- totalData[c(carbon$row),]
emissionRegions <- totalEmissions %>% arrange(region)

# Creating animated graph
plot <- ggplot(emissionRegions, aes(x = year, y = emission_amount, group = region, color = region)) +
  geom_line() +
  geom_point() +
  ggtitle("Emissions in New Zealand from 2007 to 2018") +
  ylab("Emissions (Kilotonnes)") +
  transition_reveal(year) + 
  enter_fade() +
  exit_fade()
 



