#0- About Project ----

#The ECOSTRESS GRASSAT Git repository is a comprehensive and dynamic collection 
#of data and code that showcases a project centered around the utilization of
#MODIS and ECOSTRESS data in the GRASSAT field sites located in the Wielkopolska region of Poland. 

#The project's core focus lies in conducting an in-depth analysis of temperature and 
#vapotranspiration datasets, unveiling critical insights into the local and regional
#environmental dynamics.

##0.1 -Read CSV Files ----

library(tidyverse)

install.packages("dplyr")

modis <- read.csv("/Users/maciejjurzyk/Downloads/GRASSAT-WLKP-MOD11A1-061-results.csv")

ecostress_lst <- read.csv("/Users/maciejjurzyk/Downloads/GRASSAT-WLKP-ECO2LSTE-001-results.csv")

ecostress <- read.csv("/Users/maciejjurzyk/Downloads/ET_G - GRASSAT-WLKP-ECO3ETPTJPL-001-results.csv")

temp_2m <- read.csv("/Users/maciejjurzyk/Downloads/Arkusz kalkulacyjny bez tytułu - Temp_2m - odczyty_GRASSAT_ERA5_opad_2020-.csv")

opad_meteo <- read.csv("/Users/maciejjurzyk/Downloads/Arkusz kalkulacyjny bez tytułu - odczyty_GRASSAT_ERA5_opad_2020-.csv")


#1 Packages loading ----

library(lubridate)

library(dplyr)

library(ggplot2)

ecostress %>% glimpse()

ecostress %>% ggplot(mapping = aes(x=Date,y=ET_canopy))+
  geom_point()+
  facet_wrap(~Category)


### Changing character type of data into  ----

e2 <- read.csv("/Users/maciejjurzyk/Downloads/ET_G - GRASSAT-WLKP-ECO3ETPTJPL-001-results.csv")

e <- e %>%
  mutate(
    Date = ymd_hms(Date),      # Konwersja na format daty i czasu z pakietu lubridate
    Time = format(Date, "%H:%M:%S")   # Wydobycie czasu z daty i konwersja na format godziny:minuty:sekundy
  ) %>%
  select(-Date)

e %>% glimpse()

e1 <- e %>%
  mutate(
    Date = ymd_hms(Date)     # Konwersja na format daty i czasu z pakietu lubridate

    
e3 <- e2%>% mutate(e2$First_10_Characters <- substr(e2$Date, 1, 10))

e2 %>% glimpse()

e3 <- e2 %>%
  mutate(
    Date = ymd_hms(Date),      # Konwersja na format daty i czasu z pakietu lubridate
    Time = format(Date, "%H:%M:%S")   # Wydobycie czasu z daty i konwersja na format godziny:minuty:sekundy
  ) %>%
  select(-Date)

e3 %>% glimpse()

e4 <- e3 %>% mutate(day=ymd(e3$First_10_Characters))

e4 %>% glimpse()


e3 %>% hms(e3$Time)

et<- e4 %>% mutate(godz=hms(e3$Time))

et%>% glimpse() # full success! 





                    