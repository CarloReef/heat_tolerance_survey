####Temperature data from experiment
library(tidyverse);library(janitor);library(readxl);library (lubridate); library(matrixStats)

##import data and join
set1<-read_xlsx("data/Temperature/TemperatureData.xlsx",sheet="tanks1to4")
set2<-read_xlsx("data/Temperature/TemperatureData.xlsx",sheet="tanks5to6")
set3<-read_xlsx("data/Temperature/TemperatureData.xlsx",sheet="tanks7to10")
fullset<-set1 %>% left_join(set2, by ="Date")%>%left_join(.,set3, by ="Date")
##standardize datetime column format
fullset$Date <- parse_date_time(fullset$Date, orders = c("HM mdY", "mdY HM"))
##add column for mean temperature and other derived values, calculate cumulative values, adjust to per week measures
tempset<-fullset %>% mutate(meantemp = rowMeans(dplyr::select(.,tidyselect::vars_select(names(fullset), starts_with("Tank"))),na.rm=TRUE))%>%
  clean_names()%>%
  rename(datetime=date)%>%
  dplyr::select(datetime, meantemp)%>%
  mutate(day=date(datetime),hour=hour(datetime))%>%
  mutate(diff25=meantemp-25,diffNBT=meantemp-28.5)%>%
  mutate(diff25=case_when(datetime <='2018-02-05 00:00:00'~0,TRUE~as.numeric(diff25)))%>%
  mutate(diffNBT=case_when(datetime <='2018-02-05 00:00:00'~0,TRUE~as.numeric(diffNBT)))%>%
  mutate(diffNBT=case_when(diffNBT<0~0,TRUE~as.numeric(diffNBT)))%>%
  arrange(datetime)%>%
  mutate(cumulative25=cumsum(diff25)/24/7/4,cumulativeNBT=cumsum(diffNBT)/24/7/4)%>% #4 because of 15 minute intervals
  group_by(day)%>%
  summarize(cumulative25=mean(cumulative25),cumulativeNBT=mean(cumulativeNBT),meantemp=mean(meantemp))
  
#saveRDS(tempset,"output/tempset")

## visualize  
ggplot(tempset, aes(x=day, y=meantemp))+
  geom_point()+
  geom_line()

#tank temperatures
tanktemps<-fullset %>% mutate(meantemp = rowMeans(dplyr::select(.,tidyselect::vars_select(names(fullset), starts_with("Tank"))),na.rm=TRUE))%>%
  mutate(sd = rowSds(as.matrix(dplyr::select(.,tidyselect::vars_select(names(fullset), starts_with("Tank")))),na.rm=TRUE))%>%
  mutate(sd_high = meantemp+sd, sd_low = meantemp-sd)%>%
  mutate(max = rowMaxs(as.matrix(dplyr::select(.,tidyselect::vars_select(names(fullset), starts_with("Tank")))),na.rm=TRUE))%>%
  mutate(min = rowMins(as.matrix(dplyr::select(.,tidyselect::vars_select(names(fullset), starts_with("Tank")))),na.rm=TRUE))%>%
  clean_names()%>%
  dplyr::filter(date > as.POSIXct('2018-02-02 00:00:00', tz="HST"))%>%
  dplyr::filter(date < as.POSIXct('2018-05-08 00:00:00', tz="HST"))%>%
  #reduce timepoint density
  mutate(date=ceiling_date(date,"8 hours"))%>%
  group_by(date)%>%
  summarise(meantemp=mean(meantemp),min=mean(min),max=mean(max),sd_high=mean(sd_high),sd_low=mean(sd_low))

#write_rds(tanktemps,"data/Temperature/tankstemps")

#temperature table creation
mmm=27.41
table_temps<-tempset%>%dplyr::select(day, meantemp, cumulative25)%>%
  filter(day>="2018-02-01")%>%
  mutate(diff=meantemp-mmm)%>%
  mutate(diff = if_else(diff >= 1, diff,0))


timeseries<-unique(table_temps$day)
dhw_calc<-tribble(  ~enddate,  ~dhw)
for (i in 1:(length(timeseries)-1))
{
  enddate<-timeseries[i+1]
  dhwstartdate<-(timeseries[i+1])-7257600 #seconds in 84 days for Posix calculations
  dhw_data<-table_temps%>%filter(day>dhwstartdate & day<=enddate)
  dhw<-sum(dhw_data$diff/7) #daily intervals transform to dhw
  dhw_calc<-add_row(dhw_calc, enddate=enddate, dhw=dhw) 
}

library(formattable)
temps<-left_join(dhw_calc%>%rename(day=enddate), table_temps, by = "day")%>%
  rename(Date=day,eDHW=dhw, `Average Temperature`=meantemp, `Degree Weeks > 25`=cumulative25)%>%
  dplyr::select(Date,`Average Temperature`,`Degree Weeks > 25`,eDHW)%>%
  mutate_if(is.numeric, round, 1)%>%mutate(Date=as.character(Date))
  

temperature_table<-formattable(temps, 
                               align=c("l","c","c","c"), 
                               table.attr = 'style="font-size: 20px;";\"',
                               list(`Date` = formatter("span", style = x ~ style(color = "grey",font.weight = "bold")),
                                    `Average Temperature`=color_tile("#71CA97","#ff7f7f"),
                                    `Degree Weeks > 25` =color_bar("lightgrey"),
                                    `eDHW`= color_bar("lightgrey")));temperature_table

##cleanup
rm(fullset,set1,set2,set3,tempset,tanktemps,temperature_table, dhw_calc,timeseries, temperature, temps, table_temps, mmm, i, dhw, dhwstartdate, enddate, dhw_data, temp_table_data)



