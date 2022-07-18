#Updated 5/7/2022
library(here)
library(tidyverse)

defl.data <- read.csv(here::here("Econ/Data", "gdp_deflator.csv"))
defl.data <- defl.data[1:2,-c(2:4)]

defl.data <- defl.data %>% 
  pivot_longer(-Country.Name, names_to = "Year", values_to = "WB.Deflator") %>% 
  mutate(Year = str_extract(Year, "(\\d)+"))

country.name <- "Kenya"
#gdp.defl.base.year <- 2010

if(!exists("gdp.defl.base.year"))  {  
  gdp.defl.base.year = defl.data$Year[which(as.integer(defl.data$WB.Deflator) == 100 & 
                                              defl.data$Country.Name == country.name)]
  
}

mod.deflator <- defl.data$WB.Deflator[which(defl.data$Year == gdp.defl.base.year & 
                                              defl.data$Country.Name == country.name)]
defl.data <- defl.data %>% 
  filter(Country.Name == country.name) %>% 
  mutate(
    Deflator_base_year = gdp.defl.base.year,
    My.Deflator = WB.Deflator*100/mod.deflator)

defl.data.ken <- defl.data %>% 
  filter(between(Year, 2010, 2019))
rm(defl.data, country.name, gdp.defl.base.year, mod.deflator)
