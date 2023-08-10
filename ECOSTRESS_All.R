#0.0 About the project  ---- 

#1.0 Operations on CSV files ----

library(tidyverse)

modis <- read.csv("/Users/maciejjurzyk/Downloads/GRASSAT-Calosc-MOD11A1-061-results.csv")

modis %>% glimpse()

lai_modis <- read.csv("/Users/maciejjurzyk/Downloads/GRASSAT-Calosc-MCD15A2H-061-results.csv")

lai_modis %>% glimpse()

eco_lst <- read.csv("/Users/maciejjurzyk/Downloads/GRASSAT-Calosc-ECO2LSTE-001-results.csv")

eco_et <- read.csv("/Users/maciejjurzyk/Downloads/GRASSAT-Calosc-ECO3ETPTJPL-001-results.csv")


##1.1 Packages loading ----

library(lubridate)
library(dplyr)
library(ggplot2)

##1.12 Changing character type of data into  ----


###1.13 Evapotranpsiration Data  ---- 

e3 <- eco_et%>% mutate(eco_et$First_10_Characters <- substr(eco_et$Date, 1, 10))

e3 %>% glimpse()

e4 <- e3 %>%
  mutate(
    Date = ymd_hms(Date),      
    Time = format(Date, "%H:%M:%S")  
  )

e4 %>% glimpse()

e5 <- e4 %>% mutate(day=ymd(e4$'eco_et$First_10_Characters <- substr(eco_et$Date, 1, 10)'))

e5 %>% glimpse()

e5%>% hms(l$Time)

et<- e5 %>% mutate(godz=hms(e5$Time))

e5%>% glimpse() # full success! 

###1.14 LST ECOSTRESS  Data  ---- 



l2 <- eco_lst%>% mutate(eco_lst$First_10_Characters <- substr(eco_lst$Date, 1, 10))

l2 %>% glimpse()

l3 <- l2 %>%
  mutate(
    Date = ymd_hms(Date),      
    Time = format(Date, "%H:%M:%S")  
  ) %>%
  select(-Date)

l3 %>% glimpse()

l <- l3 %>% mutate(day=ymd(l3$'eco_lst$First_10_Characters'))

l %>% glimpse()


l %>% hms(l$Time)

lst1<- l %>% mutate(godz=hms(l$Time))

lst1%>% glimpse() # full success! 

####1.141 0 into NA conversion  ---- 

lst3 <- lst1 %>%
  mutate_all(.funs = ~replace(., . == 0, NA))


####1.142 K into Celsius degree conversion ---- 

lstC <- lst3 %>%  mutate(TempC=TempK-273.15)

lstC %>% glimpse()


###1.15 LST MODIS Data  ---- 


m2 <- modis%>% mutate(modis$First_10_Characters <- substr(modis$Date, 1, 10))


m2 %>% glimpse()

m3 <- m2 %>%
  mutate(
    Date = ymd_hms(Date)
  ) %>%
  select(-Date)

m3 %>% glimpse()

m4 <- m3 %>% mutate(day=ymd(m3$'modis$First_10_Characters <- substr(modis$Date, 1, 10)'))

m4%>% glimpse() # full success! 

####1.151 0 into NA conversion  ---- 

m5 <- m4 %>%
  mutate_all(.funs = ~replace(., . == 0, NA))

m5 %>% glimpse()


####1.152 K into Celsius degree conversion ---- 

modlst <- m5 %>%  mutate(TempCday=MOD11A1_061_LST_Day_1km -273.15)

modlst1 <- modlst %>%  mutate(TempCnight=MOD11A1_061_LST_Night_1km  -273.15)

modlst1%>% glimpse()


###1.16 ERA5 Data  ---- 

ERA5 <- read.csv("/Users/maciejjurzyk/Downloads/TEMP2M_Recznie - Arkusz1-2.csv")

ERA5 %>% glimpse()

gd1 <- ERA5%>% mutate(ERA5$First_10_Characters <- substr(ERA5$Date, 1, 10))

gd1 %>% glimpse()

gd2 <- gd1 %>%
  mutate(
    Date = ymd_hms(Date),      
    Time = format(Date, "%H:%M:%S")  
  ) %>%
  select(-Date)

gd2 %>% glimpse()

gd3 <- gd2 %>% mutate(day=ymd(gd2$'ERA5$First_10_Characters <- substr(ERA5$Date, 1, 10)'))

gd3%>% glimpse() # full success! 


####1.162 temperature to numeric ----

gd5 <- gd3%>%
  mutate(Temp = as.numeric(gsub(",", ".", Temp)))

ERA5 <- gd5

ERA5 %>% glimpse()  # full success! 

###1.17 MODIS LAI  ---- 


lai <- lai_modis%>% mutate(lai_modis$First_10_Characters <- substr(lai_modis$Date, 1, 10))


lai %>% glimpse()

lai1<- lai%>%
  mutate(
    Date = ymd_hms(Date)
  ) %>%
  select(-Date)

lai1 %>% glimpse()

lai2 <- lai1 %>% mutate(day=ymd(lai1$'lai_modis$First_10_Characters <- substr(lai_modis$Date, 1, 10)'))

lai2 %>% glimpse()


# 2.0 Joining tables----


##2.1 ERA with modis data ----

modis_ERA <- ERA5 %>%
  left_join(modlst1, by = c("Category", "day"))

modis_ERA %>% glimpse()

##2.2 Joining tables with ecostress data----

eco_rest <- modis_ERA %>%
  left_join(lstC, by = c("Category", "day"))

lstmeg %>% glimpse()


GRASSAT <- lstmeg %>%
  select(
    Category,
    ID = ID.x,
    Latitude = Latitude.x,
    Longitude = Longitude.x,
    ERA5 = Temp,
    Date = day,
    LST_ECOSTRESS = TempC,
    LST_MODISday =TempCday,
    LST_MODISnight =TempCnight,
    Time='godz')

GRASSAT %>% glimpse()

GRASSAT_All <- GRASSAT %>% 
  select(Category,
         ID, Latitude,
         Longitude, ERA5,
         Date, LST_MODISday,
         LST_MODISnight,
         LST_ECOSTRESS,
         Date,
         Time) %>% 
  filter(grepl("11H", Time)) # (11:00:00 - 11:59:59))

GRASSAT %>% glimpse()


ggplot(GRASSAT) +
  geom_smooth(aes(x = Date, y = LST_ECOSTRESS, color = "LST_ECOSTRESS")) +
  geom_smooth(aes(x = Date, y = ERA5 , color = "ERA5 ")) +
  geom_smooth(aes(x = Date, y = LST_MODISday  , color = "LST_MODISday "))+
  labs(title = "Porównanie Temperatur Modis, ERA5 i ECOSTRESS ", x = "Rok", y = "Temperatura") +
  facet_wrap(~Category)+
  theme_minimal()


    