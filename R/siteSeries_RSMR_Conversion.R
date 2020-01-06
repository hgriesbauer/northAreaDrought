# Do site series - RSMR conversion

x<-readxl::read_excel("docs/droughtToolTableGIS_December2019.xlsx")
ss<-read_csv("dat/siteSeries_RSMR.csv") %>% 
  mutate(BGC=paste(Zone,Subzone,Variant,sep="")) %>% 
  mutate(BGC=stringr::str_remove(BGC,"NA"))

xRSMR<-
  x %>% 
  left_join(ss,by=c("BGC","RSMR"))
  
  
xRSMR %>% 
  filter(is.na(`Site series`)) %>% 
  group_by(BGC) %>% 
  summarise(n())
