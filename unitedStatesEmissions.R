library(dplyr)
library(ggplot2)
library(gganimate)

# Importing US emissions data
import <- read.csv("united-states.csv")

# Transposing data frame
importTranspose <- as.data.frame(t(import))
importTranspose <- importTranspose[-c(2),-c(1,309,310)]

# Cleaning data
index <- 3
flag <- 2
counter <- 1
len <- as.integer(length(importTranspose[1,]))
yarn <- as.data.frame(c(1, 2))
while (index <= len)
{
  if (index == (flag + 6))
  {
    yarn[,counter] <- index
    flag <- index 
    index <- index + 1
    counter <- counter + 1
  }
  else 
  {
    index <- index + 1
  }
}

poop <- unlist(yarn[2,])
pee <- c(1,2,poop)
data <- importTranspose[,pee]
data$V2 <- as.integer(data$V2)

years <- as.data.frame(data$V2)
years <- years[-1,]
data <- data[-1,-1]
data <- as.vector(unlist(data))
data <- data[-c(233:261)]

index2 <- 1
counter2 <- 1
year <- rep(0, length(data))
while (counter2 <= length(data))
{
  if (index2 <= length(years))
  {
    year[counter2] <- years[index2]
    index2 <- index2 + 1
    counter2 <- counter2 + 1
  }
  else
  {
    index2 <- 1
    year[counter2] <- years[index2]
    counter2 <- counter2 + 1
    index2 <- index2 + 1
  }
}
  
index3 <- 1
counter3 <- 1
butt <- 1
states <- rep(0,length(data))
while (counter3 <= length(data))
{
  if (index3 <= length(years))
  {
    states[counter3] <- state.name[butt]
    index3 <- index3 + 1
    counter3 <- counter3 + 1
  }
  else
  {
    index3 <- 2
    butt <- butt + 1
    states[counter3] <- state.name[butt]
    counter3 <- counter3 + 1
  }
}

data <- as.numeric(data)    
final <- cbind.data.frame(states,year,data)
alabamaToCalifornia <- final[1:145,]
coloradoToGeorgia <- final[146:290,]
hawaiiToIowa <- final[291:435,]
kansasToMaryland <- final[436:580,]
massachusettsToMissouri <- final[581:725,]
montanaToNewjersey <- final[726:870,]
newmexicoToOhio <- final[871:1015,]
oklahomaToSouthcarolina <- final[1016:1160,]
southdakotaToVermont <- final[1161:1305,]
virginiaToWyoming <- final[1306:1450,]

# Creating animated graph for total carbon emissions
plot <- ggplot(virginiaToWyoming, aes(x = year, y = data, group = states, color = states)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = states), hjust=0, vjust=0) +
  ggtitle("Carbon Emissions from Fossil Fuels in the United States from 1990 to 2018") +
  ylab("Emissions (Millions of Metric Tons)") +
  transition_reveal(year) + 
  enter_fade() +
  exit_fade()  

# Emissions per-capita 
census <- read.csv("census.csv")
censusEdit <- as.data.frame(census[9:59,2])
censusFinal <- as.data.frame(censusEdit[-9,])
dup <- rep(29, nrow(censusFinal))
idx <- rep(1:nrow(censusFinal), dup)
censusExpand <- censusFinal[idx,]
censusExpand <- as.numeric(gsub(",","",censusExpand))
perCap <- cbind.data.frame(states, year, data, censusExpand)
perCap$divide <- (perCap$data/perCap$censusExpand) * 1000000

AL_CA_percap <- perCap[1:145,]
CO_GA_percap <- perCap[146:290,]
HI_IA_percap <- perCap[291:435,]
KA_MD_percap <- perCap[436:580,]
MA_MO_percap <- perCap[581:725,]
MT_NJ_percap <- perCap[726:870,]
NM_OH_percap <- perCap[871:1015,]
OK_SC_percap <- perCap[1016:1160,]
SC_VT_percap <- perCap[1161:1305,]
VA_WY_percap <- perCap[1306:1450,]

# Plotting per capita data
plot2 <- ggplot(VA_WY_percap, aes(x = year, y = divide, group = states, color = states)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = states), hjust=0, vjust=0) +
  ggtitle("Per Capita Carbon Emissions from Fossil Fuels in the US from 1990 to 2018") +
  ylab("Emissions (Metric Tons per Person)") +
  transition_reveal(year) + 
  enter_fade() +
  exit_fade()
