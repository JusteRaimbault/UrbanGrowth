


loadUCDBData <- function(dsn,layer){
  ucdb <- readOGR(dsn,layer,stringsAsFactors = F)
  
  areas = ucdb@data
  for(j in 1:ncol(areas)){if(length(which(areas[,j]=="NAN"))>0){
    areas[areas[,j]=="NAN",j]="0"
    areas[,j]=as.numeric(areas[,j])
  }
  }
  
  # compute aggreg indicators
  # co2 emissions
  areas$E75 = areas$E_EC2E_A75 + areas$E_EC2E_T75 + areas$E_EC2E_I75 + areas$E_EC2E_R75 +  areas$E_EC2E_E75 +
    areas$E_EC2O_A75 + areas$E_EC2O_T75 + areas$E_EC2O_I75 + areas$E_EC2O_R75 +  areas$E_EC2O_E75
  areas$E90 = areas$E_EC2E_A90 + areas$E_EC2E_T90 + areas$E_EC2E_I90 + areas$E_EC2E_R90 +  areas$E_EC2E_E90 +
    areas$E_EC2O_A90 + areas$E_EC2O_T90 + areas$E_EC2O_I90 + areas$E_EC2O_R90 +  areas$E_EC2O_E90
  areas$E00 = areas$E_EC2E_A00 + areas$E_EC2E_T00 + areas$E_EC2E_I00 + areas$E_EC2E_R00 +  areas$E_EC2E_E00 +
    areas$E_EC2O_A00 + areas$E_EC2O_T00 + areas$E_EC2O_I00 + areas$E_EC2O_R00 +  areas$E_EC2O_E00
  areas$E15 = areas$E_EC2E_A12 + areas$E_EC2E_T12 + areas$E_EC2E_I12 + areas$E_EC2E_R12 +  areas$E_EC2E_E12 +
    areas$E_EC2O_A12 + areas$E_EC2O_T12 + areas$E_EC2O_I12 + areas$E_EC2O_R12 +  areas$E_EC2O_E12
  
  areas$G15 = areas$GDP15_SM 
  areas$G00 = areas$GDP00_SM
  areas$G90 = areas$GDP90_SM
  
  # deltas
  areas$DP90 = ifelse(areas$P75>0,(areas$P90 - areas$P75)/areas$P75,NA)
  areas$DP00 = ifelse(areas$P90>0,(areas$P00 - areas$P90)/areas$P90,NA)
  areas$DP15 = ifelse(areas$P00>0,(areas$P15 - areas$P00)/areas$P00,NA)
  
  areas$DB90 = ifelse(areas$B75>0,(areas$B90 - areas$B75)/areas$B75,NA)
  areas$DB00 = ifelse(areas$B90>0,(areas$B00 - areas$B90)/areas$B90,NA)
  areas$DB15 = ifelse(areas$B00>0,(areas$B15 - areas$B00)/areas$B00,NA)
  
  areas$DG00 = ifelse(areas$GDP90_SM>0,(areas$GDP00_SM - areas$GDP90_SM)/areas$GDP90_SM,NA)
  areas$DG15 = ifelse(areas$GDP00_SM>0,(areas$GDP15_SM - areas$GDP00_SM)/areas$GDP00_SM,NA)
  
  areas$DE90 = ifelse(areas$E75>0,(areas$E90 - areas$E75)/areas$E75,NA)
  areas$DE00 = ifelse(areas$E90>0,(areas$E00 - areas$E90)/areas$E90,NA)
  areas$DE15 = ifelse(areas$E00>0,(areas$E15 - areas$E00)/areas$E00,NA)
  
  areas$country = as.character(areas$CTR_MN_NM)
  
  ucdb@data = areas
  
  return(ucdb)
}

