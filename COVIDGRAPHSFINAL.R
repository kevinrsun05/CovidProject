if (!requireNamespace("maps", quietly = TRUE))
  install.packages("maps")

if (!requireNamespace("tidyverse", quietly = TRUE))
  install.packages("tidyverse")

if (!requireNamespace("viridis", quietly = TRUE))
  install.packages("viridis")

if (!requireNamespace("tmap", quietly = TRUE))
  install.packages("tmap")

if (!requireNamespace("spData", quietly = TRUE))
  install.packages("spData")

if (!requireNamespace("lubridate", quietly = TRUE))
  install.packages("lubridate")

if (!requireNamespace("ggplot2", quietly = TRUE))
  install.packages("ggplot2")

if (!requireNamespace("readr", quietly = TRUE))
  install.packages("readr")

library(ggplot2)
library(tidyverse)
library(maps)
library(viridis)
library(readr)
library(sf)
library(tmap)
library(spData)
library(lubridate)

Confirmed <- read_csv(url("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))

Deaths <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))

Recovered <-read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))

#Confirmed data 8/31/22
Confirmed_R <- Confirmed %>%
  group_by(`Country/Region`) %>%
  summarise(Confirmed = sum(`8/31/22`, na.rm = TRUE)) %>%
  rename(Region = `Country/Region`)
head(Confirmed_R)

#Deaths cases 8/31/22
Deaths_R <- Deaths %>%
  group_by(`Country/Region`) %>%
  summarise(Deaths = sum(`8/31/22`, na.rm = TRUE)) %>%
  rename(Region = `Country/Region`)
head(Deaths_R)

#Recovered cases 8/31/22
Recovered_R <- Recovered %>%
  group_by(`Country/Region`) %>%
  summarise(Recovered = sum(`8/31/22`, na.rm = TRUE)) %>%
  rename(Region = `Country/Region`)
head(Recovered_R)

#combine the three data
comb <-Confirmed_R %>%  
  left_join(Deaths_R, by = "Region")%>%  
  left_join(Recovered_R, by ="Region")
head(comb)


if (!requireNamespace("rnaturalearth", quietly = TRUE))
  install.packages("rnaturalearth")
library(rnaturalearth)
if (!requireNamespace("rnaturalearthdata", quietly = TRUE))
  install.packages("rnaturalearthdata")
library(rnaturalearthdata)


comb <- comb %>%
  mutate(Region = replace(Region, Region == "US", "United States")) %>%
  mutate(Region = replace(Region, Region == "Korea, South", "Korea"))

world <- ne_countries(scale='medium',returnclass = 'sf')
class(world)
head(world)
dim(world)
names (world)

comb_world <- left_join(world, comb, by = c("name" = "Region")) %>%
  replace_na(list(Confirmed=0, Deaths=0, Recovered=0))
head(comb_world)

comb_world <- comb_world %>%
  mutate(DeathRate=Deaths/Confirmed, RecoveredRate=Recovered/Confirmed)

head(comb_world)

comb_world %>% 
  arrange(-DeathRate) %>% 
  select(name, Confirmed, Deaths, Recovered, DeathRate, RecoveredRate)

# map DeathRate
ggplot(data = comb_world) +
  geom_sf(aes(fill = DeathRate)) +
  scale_fill_viridis_c(option = "plasma")


#Loading usa map
usa <-map_data("usa")

us_states <- map_data("state")

#download US confirmed case data
US_Confirmed <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
head(US_Confirmed)
dim(US_Confirmed)
colnames(US_Confirmed)

#US confirmed cases of 8/31/22
US_Confirmed_061521<- dplyr::select(US_Confirmed, "Province_State", "Admin2", "Lat", "Long_","8/31/22")
head(US_Confirmed_061521)
write.csv(US_Confirmed_061521, file = "US_Confirmed_061521.csv", row.names = TRUE)

US_48_615 <- US_Confirmed_061521 %>% 
  filter(Lat > 24 & Lat < 50 & Long_ > -127 &  Long_ < -60)

# plotting US 48 state confirmed cases of 8/31/22 to the map
ggplot() +
  
  geom_polygon(data=us_states, aes(long, lat, group=group),fill = "white", color = "grey") +
  
  coord_quickmap()+
  
  geom_point(data=US_48_615,aes(x=Long_,y=Lat,size=`8/31/22`,color=`8/31/22`),alpha=0.5)



#extract data of North Carolina 
NC_all <- US_Confirmed %>% 
  filter(`Province_State`=="North Carolina")
write.csv(NC_all, file = "NC_all.csv", row.names = TRUE)

# extract 8/31/22 case numbers of North Carolina 
NC_615 <- dplyr::select(NC_all, "Admin2", "Lat", "Long_","8/31/22")
head(NC_615)

#remove county without Lat and Long 
NC_615_clean <- NC_615 %>% 
  filter(Lat > 24 & Lat < 50 & Long_ > -127 &  Long_ < -60)

library(maps)
ncmap <- map('county', 'north carolina', fill = TRUE, col = palette())

p = ggplot() +
  geom_polygon(data=ncmap, aes(long, lat, group=group),fill = "white", color = "grey") +
  coord_quickmap()+
  geom_point(data=NC_615_clean,aes(x=Long_,y=Lat,size=`8/31/22`,color=`8/31/22`),alpha=0.5)
p

ggsave("ncmap.pdf", plot=p, width=10, height=5, dpi=300)  