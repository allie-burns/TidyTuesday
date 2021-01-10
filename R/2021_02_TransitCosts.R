library(ggsci)
library(tidyverse)
library(countrycode)

################################################################################
## Load Data
################################################################################
## Read Data
transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')
## correct country
transit_cost$country[transit_cost$country == "UK"]  <-  "GB"  
##translate countries
transit_cost$country <-  countrycode(transit_cost$country, 
                                     origin = 'ecb',
                                     destination = 'country.name')

################################################################################
## Explore data
################################################################################
## Tunnel Lengths By Country
tun_len <- transit_cost %>%
    filter(start_year >= 2020) %>%
    group_by(country) %>%
    summarize(tot_length = sum(length, na.rm=TRUE)) %>%
    arrange(desc(tot_length))%>%
    mutate(continent = countrycode(country,
                                   origin = "country.name",
                                   destination = "continent")) %>%
    drop_na()

tun_len$country <- factor(tun_len$country,
                          levels = tun_len$country[order(tun_len$tot_length)])


################################################################################
## Graph Data
################################################################################
pdf("../plots/2021_02_TransitCosts.pdf",width = 10)
ggplot(tun_len, aes(y = country, x = tot_length, color = continent)) +
    geom_segment(aes(x=0, xend=tot_length, y=country, yend= country),
                 size = 1) +
    geom_point(stat = "identity") +
    theme_classic() +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold" )) +
    labs(title = "Length of train lines to be built in each country (2020-2030)",
         x = "Total Length (km)") +
    scale_color_nejm()
dev.off()
