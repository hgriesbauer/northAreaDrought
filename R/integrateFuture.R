# Integrate future data into current dataset for North

library(forestDroughtTool)
library(tidyverse)

# Load current asmr North data
load(here::here("dat","asmrNorth.rda"))
load(here::here("dat","climData_cleaned.rda"))

stnName="DOME CREEK"
bgcName="ICHvk2"

stnData<- asmrNorth$current$stnYears

clim<-
  climData_cleaned %>% 
  filter(stn==stnName)
  
yearList<-
  filter(stnData,stn==stnName) %>% 
  dplyr::select(contains("year")) %>% 
  as.numeric()

X<-asmr(stnData=clim,bgc=bgcName,future=TRUE,years=yearList)
  
filter(X$asmr,Scenario=="current") %>% 
  write.table("clipboard", sep="\t")

filter(asmrNorth$current$asmr,bgc=="SBSmk1")     

# Come up with a station and bgc list
climList<-
  climData_cleaned %>% 
  group_by(stn) %>% 
  summarise(bgc=first(bgc)) %>% 
  arrange(bgc)

filter(climList,bgc=="SBSwk1")


bgc.subF("SBSwk3","SBSwk1") %>% 
  rbind(bgc.subF("ESSFmv4","ESSFmv2")) %>% 
  rbind(bgc.subF("ESSFmv3","ESSFmv2")) %>% 
  rbind(bgc.subF("ESSFmv1","ESSFmv2")) %>% 
  rbind(bgc.subF("SBSmc1","SBSmc2")) %>% 
  rbind(bgc.subF("SBSmc3","SBSmc2")) %>% 
  rbind(bgc.subF("ICHmc1","ICHmc2")) %>%
  rbind(bgc.subF("ESSFwk2","ESSFwk1")) %>% 
  rbind(bgc.subF("ICHwk3","ICHwk1")) %>% 
  rbind(bgc.subF("ICHwk4","ICHwk1"))

