library(tidyverse)
library(tigris)
library(dplyr)
library(RColorBrewer)
library(awtools)

SLD <- read_csv("C:\\Users\\emmap\\Desktop\\Education & Training\\Coursera\\DataAnalytics\\Capstone\\Smart_Location_Database_.csv")
# https://www.epa.gov/smartgrowth/smart-location-mapping#SLD
HD <- read_csv("C:\\Users\\emmap\\Desktop\\Education & Training\\Coursera\\DataAnalytics\\Capstone\\PLACES__Local_Data_for_Better_Health__County_Data_2022_release.csv")
# https://chronicdata.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-County-Data-20/swc5-untb

View(SLD)
View(HD)

ny_walkability <- SLD %>%
  filter(STATEFP== '36') %>%
  select('GEOID20', 'NatWalkInd', 'TotPop','COUNTYFP') %>%
  mutate(WalkIndScaled = NatWalkInd*TotPop) %>%
  group_by(COUNTYFP)%>%
  summarise(CountyPop=sum(TotPop), CountyWalkInd=sum(WalkIndScaled)/CountyPop)
  
ny_counties <- counties("NY") %>%
  select('COUNTYFP','NAME','geometry')

ny_HD <- HD %>%
  filter(StateAbbr=='NY' & DataValueTypeID=='AgeAdjPrv') %>%
  select('LocationName','MeasureId','Data_Value','Data_Value_Unit','TotalPopulation','Geolocation') %>%
  filter(MeasureId %in% c('CHD','DEPRESSION','OBESITY','LPA') )

ny_HD_loc <- ny_HD %>%
  mutate(Geo_clean=substr(Geolocation,9,nchar(Geolocation)-1)) %>%
  mutate(Geo_split=strsplit(Geo_clean, split=" ")) %>%
  unnest_wider(Geo_split,names_sep ="_LOC", transform=as.numeric) %>%
  select(-Geo_clean) %>%
  rename(LON=Geo_split_LOC1, LAT=Geo_split_LOC2) %>%
  mutate(LON=-1*LON)
  

ny_data_loc <- ny_counties %>% merge(ny_walkability) %>%
  merge(ny_HD_loc, by.x='NAME', by.y='LocationName')

ny_data <- ny_counties %>% 
  select('COUNTYFP', 'NAME') %>%
  merge(ny_walkability) %>%
  merge(ny_HD, by.x='NAME', by.y='LocationName')

##
corr_data <- pivot_wider(ny_data_loc, names_from="MeasureId", values_from="Data_Value")
corr_OBESITY <- cor(corr_data$CountyWalkInd, corr_data$OBESITY)
corr_DEPRESSION <- cor(corr_data$CountyWalkInd, corr_data$DEPRESSION)
corr_LPA <- cor(corr_data$CountyWalkInd, corr_data$LPA)
corr_CHD <- cor(corr_data$CountyWalkInd, corr_data$CHD)
graphLabels <- data.frame(MeasureId = c("CHD","DEPRESSION", "LPA","OBESITY"),
                          corcoefs= paste("correlation coefficient: ", c(format(corr_CHD, digits=2),format(corr_DEPRESSION, digits=2), 
                          format(corr_LPA, digits=2),format(corr_OBESITY, digits=2))),
                          x     = c(11, 10.5, 10,11),
                          y     = c(6, 22,30,35))

pal1 <- brewer.pal(9,'Blues')
pal2 <- a_palette
pal2 <- c("#97FFFF","#2F4F4F",pal2[1:8])

ggplot(ny_data_loc) + 
  geom_sf(aes(fill=CountyWalkInd)) + 
  theme_minimal() +
  scale_fill_gradientn(colors=pal1) +
  geom_point(mapping=aes(x=LON,y=LAT, color=Data_Value), size=1) +
  scale_color_gradientn(colors=pal2) +
  labs(title='',x='', y='', color='Prevalence (%)') +
  facet_wrap(~MeasureId,labeller = as_labeller(c(`CHD`="Coronary Heart Disease",`DEPRESSION` = "Depression", `LPA` = "Leisure Time Physical Activity", `OBESITY` = "Obesity"))) +
  guides(size='none')
   
ggplot(ny_data) +
  geom_point(aes(x=CountyWalkInd, y=Data_Value, color=MeasureId), show.legend = FALSE) +
  facet_wrap(~MeasureId, scale='free', labeller = as_labeller(c(`CHD`="Coronary Heart Disease",`DEPRESSION` = "Depression", `LPA` = "Leisure Time Physical Activity", `OBESITY` = "Obesity"))) +
  geom_smooth(aes(x=CountyWalkInd, y=Data_Value, color=MeasureId), method='lm',show.legend = FALSE) +
  labs(title='Health Indicators vs Walkability Index',x='County Walkability Index', y='Prevalence (%)', color='Measure') +
  geom_text(data=graphLabels,aes(x=x, y=y,label=corcoefs))


