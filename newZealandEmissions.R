library(dplyr)

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
finalData <- totalData[-c(carbon$row),]

