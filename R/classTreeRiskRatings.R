# Script to take drought table, cut the AET/PET ratio into different classes and
# create tree risk codes

library(forestDroughtTool)
library(tidyverse)

# Read in .csv
x<-read_csv("docs/droughtTable.csv")

# ASMR CUT F
# Define asmrCut function
asmrCut<-function(x) cut(x,breaks=c(0,asmrClass$asmrUL),labels=asmrClass$class)

# cut table into asmr classes
x.class<-
  cbind(x[,1:2],  apply(x[,3:6],MARGIN=2,asmrCut))

# TREE RISK CLASSES
load("dat/treeRisk.RData")

# Define tree-specific risk function
treeRiskFunc<-function(spp) {
  
  # Set species-specific breaks and risk classes
  brks<-
    filter(treeRisk,Species==spp) %>% 
    t(.) %>% 
    .[-1] %>% 
    as.numeric() %>% 
    c(0,.,2)
  
  
  lbls=c(names(treeRisk)[-1],"L")
  
  # apply
  riskCut<-function(y) cut(y,breaks=brks,labels=lbls)
  
  # cut table into asmr classes
  sppRisk<-
    cbind(x[,1:2],  apply(x[,3:6],MARGIN=2,riskCut)) 
  
  
  names(sppRisk)=c("BGC","RSMR",paste(spp,"Current",sep="."),
               paste(spp,"2025",sep="."),
               paste(spp,"2055",sep="."),
               paste(spp,"2085",sep=".")
               )  
    
    return(sppRisk)
    
  
  
}

treeRiskClasses<-
  x.class %>% 
  inner_join(treeRiskFunc("Pl"),by=c("BGC","RSMR")) %>% 
  inner_join(treeRiskFunc("Sx"),by=c("BGC","RSMR")) %>% 
  inner_join(treeRiskFunc("Bl"),by=c("BGC","RSMR")) %>%
  inner_join(treeRiskFunc("Fd"),by=c("BGC","RSMR")) %>%
  inner_join(treeRiskFunc("At"),by=c("BGC","RSMR")) %>%
  inner_join(treeRiskFunc("Ep"),by=c("BGC","RSMR")) %>%
  inner_join(treeRiskFunc("Ac"),by=c("BGC","RSMR")) 
  
# Write output into csv
write.csv(treeRiskClasses,file="asmrTreeRiskClasses.csv")  

