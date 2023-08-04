#0 About the project ----

#In this R GIT repository you will find a project in which 
#I study the relationships between ground-based meteorological data
#With NASA ECOSTRESS and MODIS data. In the study, I want to calculate the evapotranspiration of individual meadows in Wielkopolska, Poland
#I compare the surface temperature of individual fields with various data and the finished ECOSTRESS potential evapotranspiration product to
#Dive into this data.

#The main goal of the project is to collect data from various sources
#(satellite - NASA, Modis and Ecostress) and ground meteorological
#measurements and then calculation of evapotranspiration processes based on Ts-Ta
#formula

# Evapotransspiration data sets, revealing critical local and regional information
#dynamics of the environment.

#PS Sorry for some ggplot descriptions in Polish**

#1.0 Operations on CSV files ----

library(tidyverse)

#NASA Modis Land Surface Temperature (LST) data with 1km spatial resolution day and night time
modis <- read.csv("/Users/maciejjurzyk/Downloads/GRASSAT-WLKP-MOD11A1-061-results.csv")

#NASA ECOSTRESS LST data with 70m spatial resolution daytime
ecostress_lst <- read.csv("/Users/maciejjurzyk/Downloads/GRASSAT-WLKP-ECO2LSTE-001-results.csv")

#ECOSTRESS Evapotranspiration data
ecostress <- read.csv("/Users/maciejjurzyk/Downloads/ET_G - GRASSAT-WLKP-ECO3ETPTJPL-001-results.csv")

#Ground measurements temperature data 2m height
temp_2m <- read.csv("/Users/maciejjurzyk/Downloads/Arkusz kalkulacyjny bez tytułu - Temp_2m - odczyty_GRASSAT_ERA5_opad_2020-.csv")

#Precipitation ground measurement data 2m height 
opad_meteo <- read.csv("/Users/maciejjurzyk/Downloads/Arkusz kalkulacyjny bez tytułu - odczyty_GRASSAT_ERA5_opad_2020-.csv")


##1.1 Packages loading ----

library(lubridate)
library(dplyr)
library(ggplot2)

##1.12 Changing character type of data into  ----

###1.13 Evapotranpsiration Data  ---- 

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

###1.14 LST ECOSTRESS  Data  ---- 

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

####1.141 0 into NA conversion  ---- 

lst3 <- lst1 %>%
  mutate_all(.funs = ~replace(., . == 0, NA))


####1.142 K into Celsius degree conversion ---- 

lstC <- lst3 %>%  mutate(TempC=TempK-273.15)

lstC %>% glimpse()

###1.15 LST MODIS Data  ---- 

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

####1.151 0 into NA conversion  ---- 

m5 <- m4 %>%
  mutate_all(.funs = ~replace(., . == 0, NA))

m5 %>% glimpse()


####1.152 K into Celsius degree conversion ---- 

modlst <- m5 %>%  mutate(TempCday=MOD11A1_061_LST_Day_1km -273.15)

modlst1 <- modlst %>%  mutate(TempCnight=MOD11A1_061_LST_Night_1km  -273.15)

modlst1%>% glimpse()

###1.16 LST Ground Measurements Data  ---- 

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

####1.161 0 into NA conversion (made by mistake)  ---- 

#gd4 <- gd3 %>%
#mutate_all(.funs = ~replace(., . == 0, NA))


####1.162 temperature to numeric ----

gd5 <- gd3%>%
  mutate(Temp = as.numeric(gsub(",", ".", Temp)))

gd5 %>% glimpse()  # full success! 


#2.0 GGPLOT ----

##2.1 ET ECOSTRESS ----

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

##2.2 LST ECOSTRESS ----

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
  

## 2.3 LST MODIS ----

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

##2.4 Temp. Ground measurements 2m ----

gd5 %>% ggplot(mapping = aes(x=day,y=Temp))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Category)+
  theme_minimal()+
  scale_y_continuous(limits = c(-20, 40))+
  labs(
    title = "Temperatura powietrza na wysokości 2m na poszczególnych łąkach GRASSAT w Wielkopolsce",
    x = "Data",
    y = "Temperatura (°C)",
    caption = "Na podstawie pomiarów naziemnych"
  )

#3.0 Interactive plots----

library(plotly)

gd5 %>%
  plot_ly(x = ~day, y = ~Temp, color = ~Category, type = 'scatter', mode = 'lines') %>%
  layout(title = "Temperatura w zależności od dnia",
         xaxis = list(title = "Dzień"),
         yaxis = list(title = "Temperatura")) %>%
  subplot(titleX = 0.5)


gd5 %>%
  plot_ly(x = ~day, y = ~Temp, color = ~Category, type = 'scatter', mode = 'lines') %>%
  layout(title = "Temperatura w zależności od dnia",
         xaxis = list(title = "Dzień", range = c(min(gd4$day), max(gd4$day))),
         yaxis = list(title = "Temperatura",
                      range = c(-10, 30),
                      tickvals = seq(-10, 30, by = 2),
                      ticktext = paste(seq(-10, 30, by = 2), "°C"))) %>%
  subplot(titleX = 0.5)


gd5 %>%
  plot_ly(x = ~day, y = ~Temp, color = ~Category, type = 'scatter', mode = 'markers') %>%
  layout(title = "Temperatura w zależności od dnia",
         xaxis = list(title = "Dzień", range = c(min(gd4$day), max(gd4$day))),
         yaxis = list(title = "Temperatura",
                      range = c(-10, 30),
                      tickvals = seq(-10, 30, by = 2),
                      ticktext = paste(seq(-10, 30, by = 2), "°C"))) %>%
  subplot(titleX = 0.5)


# 4.0 Joining tables----


##4.1 Ground measurement data with modis data ----

modis_ground <- gd5 %>%
  left_join(modlst1, by = c("Category", "day"))

glimpse(modis_ground)

###4.1.1 GGplot---- 

library(ggplot2)

ggplot(modis_ground) +
  geom_smooth(aes(x = day, y = Temp, color = "Temp")) +
  geom_smooth(aes(x = day, y = TempCday, color = "TempCday")) +
  geom_smooth(aes(x = day, y = TempCnight, color = "TempCnight")) +
  labs(title = "Porównanie Temperatur Modis dzień noc i pomiarów naziemnych ", x = "Rok", y = "Temperatura") +
  theme_minimal()

##4.2 Joining tables with ecostress data----

lstmeg <- modis_ground %>%
  left_join(lstC, by = c("Category", "day"))

lstmeg %>% glimpse()


###4.2.1 GGplot----
ggplot(lstmeg) +
  geom_smooth(aes(x = day, y = Temp, color = "Temp")) +
  geom_smooth(aes(x = day, y = TempCday, color = "TempCday")) +
  geom_smooth(aes(x = day, y = TempCnight, color = "TempCnight")) +
  geom_smooth(aes(x = day, y = TempC, color="TempC"))+
  labs(title = "Porównanie Temperatur Modis dzień noc i pomiarów naziemnych i danych ECOSTRESS ", x = "Rok", y = "Temperatura") +
  theme_minimal()




##4.3 Adding evapotranspiration ecostress data----

tempevp <- lstmeg  %>%
  left_join(e5, by = c("Category", "day"))


tempevp %>% glimpse()


###4.3.1 GGplot ----
ggplot(tempevp) +
  geom_point(aes(x = day, y= ET_canopy))+
  geom_smooth(aes(x = day, y= ET_canopy))+
  geom_smooth(aes(x = day, y= ET_daily))+
  labs(title = "Porównanie Temperatur Modis dzień noc i pomiarów naziemnych i danych ECOSTRESS ", x = "Rok", y = "Temperatura") +
  theme_minimal()


# 5.0 Final Table ====

# - tempevp : Evapotranspiration data from ECOSTRESS sensor, LST ECOSTRESSS data,
# Modis LST night and day data and ground measurements data


 



                    