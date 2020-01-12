################################################################################
##
## title: "TidyTuesday 2020/02 - Australian Fires by Bureau of Meteorology"
## author: "Cedric Scherer"
## date: "6th of January 2020"
##
################################################################################
library(sf)
library(mapview)
library(tidyverse)

## Get the Data from Tidy Tuesday github page
rainfall <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')
temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

## Get Fire JSON data from aussie gov
url <- "http://www.rfs.nsw.gov.au/feeds/majorIncidents.json"
fires <- sf::st_read(url)
fires
mapview(fires)

## Plot years for each city by Z-Score
temp <- temperature %>%
    separate(col=date,into = c("year","month","day"),sep="-") %>% ## Split date
    filter(year >= 1950 & year < 2019) %>% ## filter for year
    group_by(city_name,year) %>% ## subset by city and year
    summarize(avgtemp = mean(temperature, na.rm = TRUE)) %>% ## Get avg temp/year/city
    mutate(z_score_group = (avgtemp - mean(avgtemp)) / sd(avgtemp)) %>% ## Calculate z-score
    mutate(abAvg = avgtemp - mean(avgtemp))

pdf("../../img/2020/2020_2_AustralianFires_1.pdf",height=6,width=10)
ggplot(data = temp, aes(x = year, y = city_name, fill = abAvg)) + 
    geom_tile(color="black", height=0.90) +
    scale_fill_distiller(name = "Temperature\nDifference",
                         palette = "RdBu", limits = c(-1,1)*max(abs(temp$abAvg)))+
    scale_x_discrete(breaks=seq(1950, 2020, 10)) +
    ggtitle("Annual temperature above or below 1950-2018 average") +
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(vjust=5,size=10),
          plot.title = element_text(vjust = -3))
dev.off()

## Had to remove 2019 because yearly data was incomplete.
## But what if I only compare the beginnings of all the years
temp2019 <- temperature %>%
    separate(col=date,into = c("year","month","day"),sep="-",convert=TRUE) %>% ## Split date
    filter(month < 6) %>%
    filter(year >= 1950 & year < 2019) %>% ## filter for year
    group_by(city_name,year) %>% ## subset by city and year
    summarize(avgtemp = mean(temperature, na.rm = TRUE)) %>% ## Get avg temp/year/city
    mutate(z_score_group = (avgtemp - mean(avgtemp)) / sd(avgtemp)) %>% ## Calculate z-score
    mutate(abAvg = avgtemp - mean(avgtemp))

pdf("../../img/2020/2020_2_AustralianFires_2.pdf",height=6,width=10)
ggplot(data = temp2019, aes(x = year, y = city_name, fill = abAvg)) + 
    geom_tile(color="black", height=0.90) +
    scale_fill_distiller(name = "Temperature\nDifference",
                         palette = "RdBu", limits = c(-1,1)*max(abs(temp$abAvg)))+
    scale_x_discrete(breaks=seq(1950, 2020, 10)) +
    ggtitle("January to May temperature above or below city average (1950-2018)") +
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(vjust=5,size=10),
          plot.title = element_text(vjust = -3))
dev.off()
