library(tidyverse);library(readxl);library(janitor);library(lubridate)

### import data (already in a "tidy" format) and calculate relative fv/fm value compared to baseline initial value
pamdata<-read_xlsx("data/LD50DATA.xlsx",sheet="longdata")%>% # warnings for "_" OK, missing fv_fm values -> NA and dropped
  drop_na()%>%
  clean_names()%>%
  select(plug_number,date,fv_fm)%>%
  rename(id=plug_number)%>%
  mutate(date=ymd(date))%>%
  mutate(group=case_when(date<="2018-02-08"~"initial"))%>%
  arrange(id,date)%>%
  group_by(id,group)%>%
  mutate(initial=case_when(group=="initial"~mean(fv_fm)))%>%
  group_by(id)%>%fill(initial, .direction="down")%>%select(-group)%>%
  ungroup()%>%
  mutate(relative=fv_fm/initial)

##visualize
ggplot(pamdata)+geom_boxplot(aes(as.factor(date),relative))

#saveRDS(pamdata, "output/pamdata")

rm(pamdata)
