# Generate Excel table and graphics of asmr for North Area

library(forestDroughtTool)
library(tidyverse)

# Compile current and future ASMR by scenario

# Class
scen="MeanRCP"
per=2025

currClass<-asmrNorth$current$annual$class
names(currClass)<-gsub(".mean",".current",names(currClass))

asmrCompile<-function(scen,per) {
    x<-
      asmrNorth$future$annual$class %>% 
      filter(Scenario==scen & period==as.character(per)) %>% 
      dplyr::select(-Scenario,-period)
    
    names(x)<-gsub("ASMR",as.character(per),names(x)) 
      
    return(x)
  }  

scen="MeanRCP"

meanRCP<-
  currClass %>% 
  inner_join(asmrCompile(scen,per=2025),by="bgc") %>% 
  inner_join(asmrCompile(scen,per=2055),by="bgc") %>% 
  inner_join(asmrCompile(scen,per=2085),by="bgc")

scen="rcp45"
rcp45<-
  currClass %>% 
  inner_join(asmrCompile(scen,per=2025),by="bgc") %>% 
  inner_join(asmrCompile(scen,per=2055),by="bgc") %>% 
  inner_join(asmrCompile(scen,per=2085),by="bgc")

scen="rcp85"
rcp85<-
  currClass %>% 
  inner_join(asmrCompile(scen,per=2025),by="bgc") %>% 
  inner_join(asmrCompile(scen,per=2055),by="bgc") %>% 
  inner_join(asmrCompile(scen,per=2085),by="bgc")
  

write.table(meanRCP, "clipboard", sep="\t", row.names=FALSE)
write.table(rcp45, "clipboard", sep="\t", row.names=FALSE)
write.table(rcp85, "clipboard", sep="\t", row.names=FALSE)

## Do it for ASMR now
currASMR<-asmrNorth$current$annual$asmr
names(currASMR)<-gsub(".mean",".current",names(currASMR))

asmrCompile<-function(scen,per) {
  x<-
    asmrNorth$future$annual$asmr %>% 
    filter(Scenario==scen & period==as.character(per)) %>% 
    dplyr::select(-Scenario,-period)
  
  names(x)<-gsub("ASMR",as.character(per),names(x)) 
  
  return(x)
}  

meanRCP<-
  currASMR %>% 
  inner_join(asmrCompile(scen,per=2025),by="bgc") %>% 
  inner_join(asmrCompile(scen,per=2055),by="bgc") %>% 
  inner_join(asmrCompile(scen,per=2085),by="bgc") %>% 
  arrange(bgc)

scen="rcp45"
rcp45<-
  currASMR %>% 
  inner_join(asmrCompile(scen,per=2025),by="bgc") %>% 
  inner_join(asmrCompile(scen,per=2055),by="bgc") %>% 
  inner_join(asmrCompile(scen,per=2085),by="bgc")%>% 
  arrange(bgc)

scen="rcp85"
rcp85<-
  currASMR %>% 
  inner_join(asmrCompile(scen,per=2025),by="bgc") %>% 
  inner_join(asmrCompile(scen,per=2055),by="bgc") %>% 
  inner_join(asmrCompile(scen,per=2085),by="bgc")%>% 
  arrange(bgc)



write.table(meanRCP, "clipboard", sep="\t", row.names=FALSE)
write.table(rcp45, "clipboard", sep="\t", row.names=FALSE)
write.table(rcp85, "clipboard", sep="\t", row.names=FALSE)


# Write table for GIS
x<-
  meanRCP %>% 
  pivot_longer(-bgc) %>% 
  mutate(RSMR=str_split_fixed(x$name,pattern=fixed("."),n=2)[,1]) %>% 
  mutate(RSMR=str_remove(RSMR,pattern="[S]")) %>% 
  mutate(Period=str_split_fixed(x$name,pattern=fixed("."),n=2)[,2]) %>%
  dplyr::select(-name) %>% 
  pivot_wider(names_from="Period",values_from="value") %>% 
  setNames(c("BGC","RSMR","Current","2025","2055","2085"))

write.table(x, "clipboard", sep="\t", row.names=FALSE)

