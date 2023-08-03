#0- About Project ----

#The ECOSTRESS GRASSAT Git repository is a comprehensive and dynamic collection 
#of data and code that showcases a project centered around the utilization of
#MODIS and ECOSTRESS data in the GRASSAT field sites located in the Wielkopolska region of Poland. 

#The project's core focus lies in conducting an in-depth analysis of temperature and 
#vapotranspiration datasets, unveiling critical insights into the local and regional
#environmental dynamics.

#1 Read CSV Files ----

library(tidyverse)


modis <- read.csv("/Users/maciejjurzyk/Downloads/GRASSAT-WLKP-MOD11A1-061-results.csv")

ecostress_lst <- read.csv("/Users/maciejjurzyk/Downloads/GRASSAT-WLKP-ECO2LSTE-001-results.csv")

ecostress <- read.csv("/Users/maciejjurzyk/Downloads/ET_G - GRASSAT-WLKP-ECO3ETPTJPL-001-results.csv")

temp_2m <- read.csv("/Users/maciejjurzyk/Downloads/Arkusz kalkulacyjny bez tytułu - Temp_2m - odczyty_GRASSAT_ERA5_opad_2020-.csv")

opad_meteo <- read.csv("/Users/maciejjurzyk/Downloads/Arkusz kalkulacyjny bez tytułu - odczyty_GRASSAT_ERA5_opad_2020-.csv")


##1.0 Packages loading ----

library(lubridate)

library(dplyr)

library(ggplot2)

##1.1 Changing character type of data into  ----


###1.11 Evapotranpsiration Data  ---- 

e2 <- read.csv("/Users/maciejjurzyk/Downloads/ET_G - GRASSAT-WLKP-ECO3ETPTJPL-001-results.csv")

e2 %>% glimpse()

e3 <- e2%>% mutate(e2$First_10_Characters <- substr(e2$Date, 1, 10))

e3 %>% glimpse()

e4 <- e3 %>%
  mutate(
    Date = ymd_hms(Date),      
    Time = format(Date, "%H:%M:%S")  
  )

e4 %>% glimpse()

e5 <- e4 %>% mutate(day=ymd(e4$'e2$First_10_Characters <- substr(e2$Date, 1, 10)'))

e5 %>% glimpse()

e5%>% hms(l$Time)

et<- e5 %>% mutate(godz=hms(e5$Time))

e5%>% glimpse() # full success! 

###1.12 LST ECOSTRESS  Data  ---- 

l1 <- read.csv("/Users/maciejjurzyk/Downloads/Arkusz kalkulacyjny bez tytułu - GRASSAT-WLKP-ECO2LSTE-001-results.csv")

l1 %>% glimpse()

l2 <- l1%>% mutate(l1$First_10_Characters <- substr(l1$Date, 1, 10))

l2 %>% glimpse()

l3 <- l2 %>%
  mutate(
    Date = ymd_hms(Date),      
    Time = format(Date, "%H:%M:%S")  
  ) %>%
  select(-Date)

l3 %>% glimpse()

l <- l3 %>% mutate(day=ymd(l3$'l1$First_10_Characters'))

l %>% glimpse()


l %>% hms(l$Time)

lst1<- l %>% mutate(godz=hms(l$Time))

lst1%>% glimpse() # full success! 

####1.121 0 into NA conversion  ---- 

lst3 <- lst1 %>%
  mutate_all(.funs = ~replace(., . == 0, NA))


####1.122 K into Celsius degree conversion ---- 

lstC <- lst3 %>%  mutate(TempC=TempK-273.15)

lstC %>% glimpse()

###1.12 LST MODIS Data  ---- 

mod <- read.csv("/Users/maciejjurzyk/Downloads/Arkusz kalkulacyjny bez tytułu - GRASSAT-WLKP-MOD11A1-061-results.csv")

mod %>% glimpse()


m2 <- mod%>% mutate(mod$First_10_Characters <- substr(mod$Date, 1, 10))

m2 %>% glimpse()

m3 <- m2 %>%
  mutate(
    Date = ymd_hms(Date),      
    Time = format(Date, "%H:%M:%S")  
  ) %>%
  select(-Date)

m3 %>% glimpse()

m4 <- m3 %>% mutate(day=ymd(m3$'mod$First_10_Characters <- substr(mod$Date, 1, 10)'))

m4%>% glimpse() # full success! 

####1.121 0 into NA conversion  ---- 

m5 <- m4 %>%
  mutate_all(.funs = ~replace(., . == 0, NA))

m5 %>% glimpse()


####1.122 K into Celsius degree conversion ---- 

modlst <- m5 %>%  mutate(TempCday=MOD11A1_061_LST_Day_1km -273.15)

modlst1 <- modlst %>%  mutate(TempCnight=MOD11A1_061_LST_Night_1km  -273.15)

modlst1%>% glimpse()

###1.13 LST Ground Mesurements Data  ---- 

gd <- read.csv("/Users/maciejjurzyk/Downloads/TEMP2M_Recznie - Arkusz1-2.csv")

gd %>% glimpse()

gd1 <- gd%>% mutate(gd$First_10_Characters <- substr(gd$Date, 1, 10))

gd1 %>% glimpse()

gd2 <- gd1 %>%
  mutate(
    Date = ymd_hms(Date),      
    Time = format(Date, "%H:%M:%S")  
  ) %>%
  select(-Date)

gd2 %>% glimpse()

gd3 <- gd2 %>% mutate(day=ymd(gd2$'gd$First_10_Characters <- substr(gd$Date, 1, 10)'))

gd3%>% glimpse() # full success! 

####1.121 0 into NA conversion  ---- 

gd4 <- gd3 %>%
  mutate_all(.funs = ~replace(., . == 0, NA))

gd4 %>% glimpse()


###1.2 GGPLOT ----

#### 1.21 ET Ecostress ----

et %>% ggplot(mapping = aes(x=day,y=ET_canopy))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Category)+
  theme_minimal()+
  scale_y_continuous(limits = c(0,100))+
  labs(
    title = "Ewapotranspiracja łąk GRASSAT w Wielkopolsce",
    x = "Data",
    y = "Ewapotranspiracja (%)",
    caption = "Na podstawie danych ECOSTRESS"
  )

#### 1.21 LST Ecostress ----

lstC %>% ggplot(mapping = aes(x=day,y=TempC))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Category)+
  theme_minimal()+
  scale_y_continuous(limits = c(-20, 40))+
  labs(
    title = "Temperatura powierzchni mierzona w ciągu dnia na poszczególnych łąkach GRASSAT w Wielkopolsce",
    x = "Data",
    y = "Temperatura (°C)",
    caption = "Na podstawie danych ECOSTRESS"
  )
  

#### 1.22 LST MODIS ----

## Temp Day

modlst1 %>% ggplot(mapping = aes(x=day,y=TempCday))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Category)+
  theme_minimal()+
  scale_y_continuous(limits = c(-20, 40))+
  labs(
    title = "Temperatura powierzchni mierzona w ciągu dnia na poszczególnych łąkach GRASSAT w Wielkopolsce",
    x = "Data",
    y = "Temperatura (°C)",
    caption = "Na podstawie danych MODIS"
  )


modlst1 %>% ggplot(mapping = aes(x=day,y=TempCnight))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Category)+
  theme_minimal()+
  scale_y_continuous(limits = c(-20, 40))+
  labs(
    title = "Temperatura powierzchni mierzona w nocy na poszczególnych łąkach GRASSAT w Wielkopolsce",
    x = "Data",
    y = "Temperatura (°C)",
    caption = "Na podstawie danych MODIS"
  )

#Ground measurements 

gd4 %>% ggplot(mapping = aes(x=day,y=Temp))+
  geom_smooth()+
  facet_wrap(~Category)+
  theme_minimal()

# NOT WORKING!!! 












                    