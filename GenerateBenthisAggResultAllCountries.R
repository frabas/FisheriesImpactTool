

#-------------------------------------------------------------------------------
#
# Benthis WP2 workflow
#
# Designed by: Francois Bastardie, Niels Hintzen
# Runs with: R3.0.2 and R2.15.x
#
# VMStools version: 0.70
#
#-------------------------------------------------------------------------------



# GENERAL SETTINGS---------------
 myPath         <- file.path("C:", "Users", "fbas", "Documents", "GitHub")
 dataPath       <- file.path(myPath, "FisheriesImpactTool", "Data")
 nameAggFile  <- "ALL_AggregatedSweptArea_12062015.RData"
#-------------------------------- 
 
 
# JUST GIVEN FOR INFO BECAUSE THE DATA PER COUNTRY ARE NOT DISTRIBUTED HERE

# collate the data from all coutries in one big data.frame
 aggResultAll <- NULL
 countries <- c('SWE', 'DEU', 'DEN', 'UK', 'IRL', 'BEL', 'NLD', 'MED1', 'MED2', 'PRT', 'GRK', 'NOR', 'ITA') # + rapido ITA TBB
 for(ctry in countries){
  load(file=file.path(dataPath, paste(ctry,"_AggregatedSweptArea.RData",sep='')))
  
  if(ctry=="NOR")  {
    dd        <- aggResult[aggResult$LE_MET=="SSC_DEM" & aggResult$Year=="2011",]
    dd$Year   <- "2010"
    aggResult <- rbind.data.frame(aggResult, dd) # assume copy/paste for seiners to fill in the gap of 2010
    }
  if(ctry=="ITA")  {
    colnames(aggResult) <- c("LE_MET","grID","Year","SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER","SWEPT_AREA_KM2_UPPER","CELL_LONG","CELL_LATI")
  }  
  
  aggResultAll <- rbind.data.frame(aggResultAll, cbind.data.frame(aggResult, ctry=ctry) )
 }
 colnames(aggResult)[1:3] <- c("LE_MET","grID", "Year")
 aggResult <- aggResultAll 
 if(!file.exists(file.path(dataPath, nameAggFile))) save(aggResult, file=file.path(dataPath, nameAggFile))  # + rapido TBB ITA





