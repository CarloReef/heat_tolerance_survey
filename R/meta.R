library(tidyverse);library(readxl);library(janitor)

## make dataframe of meta data associated with fragments

fragdata<-read_xlsx("data/LD50DATA.xlsx",sheet="Plug Expt")%>%
  clean_names()%>%
  dplyr::select(plug_number,genotype,group)%>%
  rename(id = plug_number)

###check for duplicates 
fragdata %>% get_dupes(id)
###plug id "2030" is used twice in expt.  note "2030a" "2030b" suffix in some dataframes 

clonality_bleach<-read_xlsx("data/MontiporaBleachingScoring.xlsx")%>%
  clean_names()%>%
  rename(genotype=colony)%>%
  dplyr::select(genotype,mean_bleach)

clonality_site<-read_xlsx("data/Site Data/ClonalitySites.xlsx")%>%
  clean_names()%>%
  rename(genotype=tag)

clonality_meta<-left_join(clonality_site,clonality_bleach)%>%
  #mutate(block=str_split(site, "", simplify=FALSE))%>%
  mutate(block=str_extract(site, "^.*(?=_)"))%>%
  mutate(group="clonality")%>%
  mutate(genotype=as.character(genotype))

join<-left_join(fragdata,clonality_meta, by=c("genotype","group"))

### including 2019 prop_d
symdata<-readRDS("./data/Symbiodinium/Dataall")%>%
  ungroup()%>%
  clean_names()%>%
  dplyr::select(genotype=id,year,prop_d)%>%
  distinct()%>%
  pivot_wider(names_from = year, values_from = prop_d)%>%
  #NOTE "2017" samples actually collected in January 2018
  rename(prop_d_2018="2017",prop_d_2019="2019")%>%
  mutate(group="clonality")

protometadata<-left_join(join,symdata,by=c("genotype","group"))%>%
  rename(experiment=group)%>%
  mutate(group=case_when(experiment=="RTE"~genotype,experiment=="clonality"~experiment))

clones<-read_tsv("data/summary_clonality.txt")%>%
  clean_names()%>%
  get_dupes(cluster)%>%
  dplyr::rename(genotype=sample)%>%
  mutate(genotype=as.character(genotype))%>%
  left_join(protometadata%>%dplyr::filter(experiment=="clonality"), by="genotype")%>%
  dplyr::filter(!is.na(experiment))%>%
  get_dupes(cluster)
# a few  (6) clone situations: 5 pairs and 1 triplet = 13 frags in the study

metadata<-protometadata%>%
  #make column to drop population samples that are clonal so only one of each genotype remains, keeping sample with most data (least na's in overall dataset)
  mutate(drop_clones=case_when(id %in% c("2021","308","2586","2314","827","3374","793")~"drop",TRUE~"keep"))
  
#saveRDS(metadata,"output/metadata")

rm(clonality_bleach,clonality_meta, clonality_site,clones,fragdata,join, metadata, protometadata, symdata)
