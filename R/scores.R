library(readxl); library(tidyverse);library(janitor);library(lubridate)
#### bleaching scores on frags during experiment ####
positions<-read_excel("data/RackPositions.xlsx", sheet="0202_0308")
scoring<-read_excel("data/frag_bleach_scores/frag_bleach_scores.xlsx", sheet="scoring_info")%>%
  mutate (nom_time=strftime(nom_time, format="%H:%M",tz="UTC"))%>%
  rename(timepoint=data_timepoint)

frag_bleach<-read_excel("data/frag_bleach_scores/frag_bleach_scores.xlsx", sheet="score_series")%>%
  mutate(position= paste0(row, column))%>%
  filter(!(t1=="NA"))%>%
  rowwise()%>%
  mutate(mean_exp_bleach=round(mean(c_across(starts_with("t"))),2))%>%
  left_join(positions, by=c("rack","position"))%>%
  gather(.,key="timepoint", value="exp_bleach", t1:t15)%>%
  dplyr::select(id,timepoint,exp_bleach,mean_exp_bleach)%>%
  mutate(timepoint=as.numeric(str_remove(timepoint,"t")))%>%
  left_join(scoring, by="timepoint")%>%
  dplyr::select(-period)

#saveRDS(frag_bleach, "output/frag_bleachdata")
rm(positions, scoring)

#### tissue coverage and bleach scores ####
path <- "data/SurvivalScores.xlsx"
all_sheets<-excel_sheets(path)
sheets <- all_sheets[grepl("score", all_sheets)]

early <- map_dfr(sheets, ~read_excel(path = path, sheet = .x), id = .x)%>%
  dplyr::filter(date == as.POSIXct("2018-03-22", tz="UTC"))

late <- map_dfr(sheets, ~read_excel(path = path, sheet = .x), id = .x)%>%
  dplyr::filter(!(date == as.POSIXct("2018-03-22", tz="UTC")))

early_rack<-read_excel("data/RackPositions.xlsx", sheet="0313_0327")%>%
  separate(position, into = c("row", "column"), sep = 1)%>%
  mutate(column = as.numeric(column))

late_rack<-read_excel("data/RackPositions.xlsx", sheet="0405_onward")%>%
  separate(position, into = c("row", "column"), sep = 1)%>%
  mutate(column = as.numeric(column))

early_scores<-full_join(early,early_rack)%>%
  relocate(id)

late_scores<-full_join(late,late_rack)%>%
  relocate(id)

#july and august scores
july<-read_excel("data/SurvivalScores.xlsx",sheet="0705_tissue")
august<-read_excel("data/SurvivalScores.xlsx",sheet="0806_tissue")
later<-bind_rows(july,august)%>%
  mutate(id=as.character(plug))%>%
  relocate(id)

#combine score data
scores<-bind_rows(early_scores,late_scores,later)%>%
  dplyr::select(id,date,bleach,dead_tissue,score)%>%
  dplyr::filter(!(id=="NA"))%>%
  mutate(bleach=as.numeric(bleach))%>%
  mutate(dead_tissue=as.numeric(dead_tissue))%>%
  dplyr::filter(!id %in% c("2764","2030","2030a","2030b"))
#discard b/c 2030 is duplicate id, 2764 does not match original plug list

#### final survivors
survive_list<-read_excel("data/SurvivalScores.xlsx", sheet="final_survivors")%>%
  mutate(id=as.character(plug))%>%
  dplyr::select(id,survivor)

plug_list<-read_excel("data/LD50DATA.xlsx", sheet="Plug Expt")%>%
  clean_names()%>%
  dplyr::select(id=plug_number,group)

survivors<-full_join(survive_list,plug_list)%>%
  mutate(survivor=replace_na(survivor, "N"))%>%
  filter(!id %in% c("2764","2030"))
#discard bc/ 2030 is duplicate id, 2764 does not match original plug list
#NOTE score data is all internally consistent except that #3316 got scored as having 50% final tissue, but was not scored as a long-term survivor

#saveRDS(survivors,"output/survivors")
#saveRDS(scores,"output/healthdata")
rm(august, early, early_rack, early_scores, frag_bleach, july, late, late_rack, late_scores, later, plug_list, scores, survive_list, survivors,all_sheets, path, sheets)
