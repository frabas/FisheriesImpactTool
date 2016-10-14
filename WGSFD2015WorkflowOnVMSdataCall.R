 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##
 ## Workflow of WGSFD 2015 to process VMS data from ICES data call
 ## Author: Niels Hintzen, Francois Bastardie, Josephine Egekvist (DTU-Aqua) 
 ## and the ICES WGSFD 2015 members
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##


#- Workflow of WGSFD 2015

rm(list=ls())
memory.limit(size=40000)

library(vmstools)


dataPath    <- "~/WGSFD/data"
outPath     <- "~/WGSFD/output"
codePath    <- "~/WGSFD/R"

#-------------------------------------------------------------------------------
#- Read in the raw WGSFD data table 1 only
#-------------------------------------------------------------------------------
#VMS_data    <- read.table(file=file.path(dataPath,"VMS_Table_11062015.csv"), sep=";", header=TRUE)
#wgsfd       <- VMS_data
#Log_data    <- read.table(file=file.path(dataPath,"LE_Table_11062015.csv"), sep=";", header=TRUE)

#-------------------------------------------------------------------------------
#- Clean file and add speeds
#-------------------------------------------------------------------------------
#source(file.path(codePath,"speed_histograms.R"))

  #- sortcut
  wgsfd     <- read.csv(file.path(dataPath,"VMS_Table_wSpeed2.csv"),header=T,stringsAsFactors=F,sep=",")

#  wgsfdlog  <- read.csv(file.path(dataPath,"LE_Table_wSpeed.csv"),header=T,stringsAsFactors=F,sep=";")


#-------------------------------------------------------------------------------
#- Read in metier-conversion table
#-------------------------------------------------------------------------------
metiers_DCF_BENTHIS_lookup  <- read.table(file=file.path(dataPath, "Lookup_Metiers_incl_log.csv"), sep=",", header=TRUE)
metiers_DCF_BENTHIS_lookup$Benthis_metiers  <- ac(metiers_DCF_BENTHIS_lookup$Benthis_metiers)
metiers_DCF_BENTHIS_lookup$JNCC_grouping    <- ac(metiers_DCF_BENTHIS_lookup$JNCC_grouping)
metiers_DCF_BENTHIS_lookup$Fishing_category <- ac(metiers_DCF_BENTHIS_lookup$Fishing_category)
metiers_DCF_BENTHIS_lookup$LE_MET_level6    <- ac(metiers_DCF_BENTHIS_lookup$LE_MET_level6)
metiers_DCF_BENTHIS_lookup$Metier_level4    <- ac(metiers_DCF_BENTHIS_lookup$Metier_level4)
metiers_DCF_BENTHIS_lookup$Description    <- ac(metiers_DCF_BENTHIS_lookup$Description)


#-------------------------------------------------------------------------------
#- Assign a metier & gear category from a look up table
#-------------------------------------------------------------------------------

wgsfd$benthis_met                   <- NA
wgsfd$JNCC                          <- NA
wgsfd$Category                      <- NA
wgsfd$Description                   <- NA
tmpwgsfd                            <- as.matrix(wgsfd[,c("LE_MET_level6","gear_code","benthis_met","JNCC","Category","Description")])
for(i in 1:nrow(metiers_DCF_BENTHIS_lookup)){
  print(i)
  idx1                              <- which(tmpwgsfd[,"LE_MET_level6"] == metiers_DCF_BENTHIS_lookup$LE_MET_level6[i] & is.na(tmpwgsfd[,"Category"]))
  idx2                              <- which(tmpwgsfd[,"LE_MET_level6"] == metiers_DCF_BENTHIS_lookup$LE_MET_level6[i] & is.na(tmpwgsfd[,"JNCC"])==T)
  idx3                              <- which(tmpwgsfd[,"LE_MET_level6"] == metiers_DCF_BENTHIS_lookup$LE_MET_level6[i] & is.na(tmpwgsfd[,"benthis_met"])==T)
  idx4                              <- which(tmpwgsfd[,"LE_MET_level6"] == metiers_DCF_BENTHIS_lookup$LE_MET_level6[i] & is.na(tmpwgsfd[,"Description"]))
  if(length(idx3)>0)
    tmpwgsfd[idx3,"benthis_met"]      <- metiers_DCF_BENTHIS_lookup$Benthis_metiers[i]
  if(length(idx2)>0)
    tmpwgsfd[idx2,"JNCC"]             <- metiers_DCF_BENTHIS_lookup$JNCC_grouping[i]
  if(length(idx1)>0)
    tmpwgsfd[idx1,"Category"]         <- metiers_DCF_BENTHIS_lookup$Fishing_category[i]
  if(length(idx4)>0)
    tmpwgsfd[idx4,"Description"]      <- metiers_DCF_BENTHIS_lookup$Description[i]
  idx1                              <- which(tmpwgsfd[,"gear_code"] == metiers_DCF_BENTHIS_lookup$Metier_level4[i] & is.na(tmpwgsfd[,"Category"]))
  idx2                              <- which(tmpwgsfd[,"gear_code"] == metiers_DCF_BENTHIS_lookup$Metier_level4[i] & is.na(tmpwgsfd[,"JNCC"])==T)
  idx3                              <- which(tmpwgsfd[,"gear_code"] == metiers_DCF_BENTHIS_lookup$Metier_level4[i] & is.na(tmpwgsfd[,"benthis_met"])==T)
  idx4                              <- which(tmpwgsfd[,"gear_code"] == metiers_DCF_BENTHIS_lookup$Metier_level4[i] & is.na(tmpwgsfd[,"Description"])==T)
  
  if(length(idx3)>0)
    tmpwgsfd[idx3,"benthis_met"]    <- metiers_DCF_BENTHIS_lookup$Benthis_metiers[i]
  if(length(idx2)>0)
    tmpwgsfd[idx2,"JNCC"]           <- metiers_DCF_BENTHIS_lookup$JNCC_grouping[i]
  if(length(idx1)>0)
    tmpwgsfd[idx1,"Category"]       <- metiers_DCF_BENTHIS_lookup$Fishing_category[i]
  if(length(idx4)>0)
    tmpwgsfd[idx4,"Description"]    <- metiers_DCF_BENTHIS_lookup$Description[i]
}
wgsfd[,c("benthis_met","JNCC","Category","Description")] <- tmpwgsfd[,c("benthis_met","JNCC","Category","Description")]

#wgsfdlog$benthis_met                    <- NA
#wgsfdlog$JNCC                           <- NA
#wgsfdlog$Category                       <- NA
#wgsfdlog$Description                    <- NA
#tmpwgsfdlog                             <- as.matrix(wgsfdlog[,c("LE_MET_level6","gear_code","benthis_met","JNCC","Category","Description")])
#for(i in 1:nrow(metiers_DCF_BENTHIS_lookup)){
#  print(i)
#  idx1                                  <- which(tmpwgsfdlog[,"LE_MET_level6"] == metiers_DCF_BENTHIS_lookup$LE_MET_level6[i] & is.na(tmpwgsfdlog[,"Category"]))
#  idx2                                  <- which(tmpwgsfdlog[,"LE_MET_level6"] == metiers_DCF_BENTHIS_lookup$LE_MET_level6[i] & is.na(tmpwgsfdlog[,"JNCC"])==T)
#  idx3                                  <- which(tmpwgsfdlog[,"LE_MET_level6"] == metiers_DCF_BENTHIS_lookup$LE_MET_level6[i] & is.na(tmpwgsfdlog[,"benthis_met"])==T)
#  idx4                                  <- which(tmpwgsfdlog[,"LE_MET_level6"] == metiers_DCF_BENTHIS_lookup$LE_MET_level6[i] & is.na(tmpwgsfdlog[,"Description"])==T)
#  if(length(idx3)>0)
#    tmpwgsfdlog[idx3,"benthis_met"]     <- metiers_DCF_BENTHIS_lookup$Benthis_metiers[i]
#  if(length(idx2)>0)
#    tmpwgsfdlog[idx2,"JNCC"]            <- metiers_DCF_BENTHIS_lookup$JNCC_grouping[i]
#  if(length(idx1)>0)
#    tmpwgsfdlog[idx1,"Category"]        <- metiers_DCF_BENTHIS_lookup$Fishing_category[i]
#  if(length(idx4)>0)
#    tmpwgsfdlog[idx4,"Description"]     <- metiers_DCF_BENTHIS_lookup$Description[i]
#  idx1                                  <- which(tmpwgsfdlog[,"gear_code"] == metiers_DCF_BENTHIS_lookup$Metier_level4[i] & is.na(tmpwgsfdlog[,"Category"]))
#  idx2                                  <- which(tmpwgsfdlog[,"gear_code"] == metiers_DCF_BENTHIS_lookup$Metier_level4[i] & is.na(tmpwgsfdlog[,"JNCC"])==T)
#  idx3                                  <- which(tmpwgsfdlog[,"gear_code"] == metiers_DCF_BENTHIS_lookup$Metier_level4[i] & is.na(tmpwgsfdlog[,"benthis_met"])==T)
#  idx4                                  <- which(tmpwgsfdlog[,"gear_code"] == metiers_DCF_BENTHIS_lookup$Metier_level4[i] & is.na(tmpwgsfdlog[,"Description"])==T)
  
#  if(length(idx3)>0)
#    tmpwgsfdlog[idx3,"benthis_met"]     <- metiers_DCF_BENTHIS_lookup$Benthis_metiers[i]
#  if(length(idx2)>0)
#    tmpwgsfdlog[idx2,"JNCC"]            <- metiers_DCF_BENTHIS_lookup$JNCC_grouping[i]
#  if(length(idx1)>0)
#    tmpwgsfdlog[idx1,"Category"]        <- metiers_DCF_BENTHIS_lookup$Fishing_category[i]
#  if(length(idx4)>0)
#    tmpwgsfdlog[idx4,"Description"]     <- metiers_DCF_BENTHIS_lookup$Description[i]
#}
#wgsfdlog[,c("benthis_met","JNCC","Category","Description")] <- tmpwgsfdlog[,c("benthis_met","JNCC","Category","Description")]

save(wgsfd,wgsfdlog,file=file.path(outPath,"WGSFD_metier.RData"))
save(wgsfd,file="C:/joeg/WGSFD/2015/Niels_workflow/output/wgsfd.RData")
#save(wgsfdlog,file="C:/joeg/WGSFD/2015/Niels_workflow/output/wgsfdlog.RData")

load("C:/joeg/WGSFD/2015/Niels_workflow/output/wgsfd.RData")
write.csv(wgsfd,file="C:/joeg/WGSFD/2015/Niels_workflow/output/WGSFD.csv")
load("C:/joeg/WGSFD/2015/Niels_workflow/output/wgsfdlog.RData")
wgsfd_fra                      <- subset(wgsfd,country=='FRA')
wgsfdp                                  <- wgsfd
#wgsfdlogp                               <- wgsfdlog
#-------------------------------------------------------------------------------
#- Add gear widths to dataset & calculate swept area
#-------------------------------------------------------------------------------
source(file.path(codePath,"02_gearCharacteristics.r"))
BenthisGearWidths$gear_width_log <- NA 

BenthisGearWidths           <- rbind(BenthisGearWidths,
                                     data.frame(benthis_met=c("Beam_Trawl","Nephrops_Trawl","Pair_TrawlSeine","Otter_Trawl","Otter_Trawl_Twin","Boat_dredge","Otter_Other"),
                                                av_kw=NA,
                                                av_loa=NA,
                                                av_fspeed=NA,
                                                subsurface_prop=c(100,3.333333333,0.8,5,5,100,3.333333333),
                                                gear_width=c(18,60,250,60,100,12,60),
                                                gear_width_log=NA))
wgsfdp$benthis_met          <- ifelse(is.na(wgsfdp$benthis_met)==T,   wgsfdp$JNCC,   wgsfdp$benthis_met)
#wgsfdlogp$benthis_met       <- ifelse(is.na(wgsfdlogp$benthis_met)==T,wgsfdlogp$JNCC,wgsfdlogp$benthis_met)
wgsfdp                      <- merge(wgsfdp,     BenthisGearWidths, by.x="benthis_met", by.y= "benthis_met",all.x=TRUE)
#wgsfdlogp                   <- merge(wgsfdlogp,  BenthisGearWidths[,c("benthis_met","subsurface_prop","gear_width_log")], by="benthis_met",all.x=TRUE)

wgsfdp_fra                      <- subset(wgsfdp,country=='FRA')


#- calculate the area swept
idx                         <- !wgsfdp$benthis_met %in% c("SDN_DMF", "SSC_DMF") # exclude seiners
wgsfdp$swept_area           <- 0
wgsfdp$swept_area[idx]      <- an(wgsfdp$fishing_hours[idx]) * an(wgsfdp$gear_width[idx]) * an(wgsfdp$avg_fishing_speed_new[idx])
#idx                         <- !wgsfdlogp$benthis_met %in% c("SDN_DMF", "SSC_DMF") # exclude seiners
#wgsfdlogp$swept_area        <- 0
#wgsfdlogp$swept_area[idx]   <- an(wgsfdlogp$fishing_days[idx])*24 * an(wgsfdlogp$gear_width_log[idx]) * an(wgsfdlogp$avg_fishing_speed[idx])

#- caution: specific rules for Seiners
idx                         <- wgsfdp$benthis_met %in% c("SDN_DMF") # for seiners
wgsfdp$swept_area[idx]      <- (pi*(an(wgsfdp$gear_width[idx])/(2*pi))^2) *  an(wgsfdp$fishing_hours)[idx]   / haul_duration_seiners["SDN_DMF"]
#idx                         <- wgsfdlogp$benthis_met %in% c("SDN_DMF") # for seiners
#wgsfdlogp$swept_area[idx]    <- (pi*(an(wgsfdlogp$gear_width_log[idx])/(2*pi))^2) *  an(wgsfdlogp$fishing_days)[idx]*24   / haul_duration_seiners["SDN_DMF"]

idx                         <- wgsfdp$benthis_met %in% c("SSC_DMF") # scottish seiners
wgsfdp$swept_area[idx]      <- (pi*(an(wgsfdp$gear_width[idx])/(2*pi))^2) *  an(wgsfdp$fishing_hours)[idx]   / haul_duration_seiners["SSC_DMF"]
wgsfdp$swept_area[idx]      <- wgsfdp$swept_area[idx]  *1.5 # ad hoc correction to account for the SSC specificities
#idx                         <- wgsfdlogp$benthis_met %in% c("SSC_DMF") # scottish seiners
#wgsfdlogp$swept_area[idx]   <- (pi*(an(wgsfdlogp$gear_width_log[idx])/(2*pi))^2) *  an(wgsfdlogp$fishing_days)[idx]*24   / haul_duration_seiners["SSC_DMF"]
#wgsfdlogp$swept_area[idx]   <- wgsfdlogp$swept_area[idx]  *1.5 # ad hoc correction to account for the SSC specificities

#-------------------------------------------------------------------------------
#- Add Csquares to logbook data
#-------------------------------------------------------------------------------

#ICESrectangle2CSquare <- function(rectangles,degrees,onLand=T){
#  
#  lonlats     <- ICESrectangle2LonLat(rectangles)
#  
#  if(degrees >= 1)
#    ret       <- CSquare(lonlats$SI_LONG,lonlats$SI_LATI,degrees=degrees)
#  if(degrees < 1){
#    squares   <- lapply(as.list(1:nrow(lonlats)),function(x){
#      return(expand.grid(SI_LONG=seq(lonlats[x,"SI_LONG"],lonlats[x,"SI_LONG"]+1-degrees,degrees)+degrees/2,SI_LATI=seq(lonlats[x,"SI_LATI"],lonlats[x,"SI_LATI"]+0.5-degrees,degrees)+degrees/2))})
#    ret       <- lapply(squares,function(x){return(CSquare(x$SI_LONG,x$SI_LATI,degrees=degrees))})
#  }
#  names(ret)  <- rectangles
#  if(onLand==F){
#    if(!exists("ICESareas"))
#      data(ICESareas)
#    require(sp)
#    idx       <- lapply(ret,function(x){idx <- over(SpatialPoints(CSquare2LonLat(x,degrees)[,c("SI_LONG","SI_LATI")]-1e-05),ICESareas);
#                                        return(which(is.na(idx)==F))})  #1e-5 is rounding error introduced in CSquare2LonLat
#    res       <- lapply(as.list(1:length(ret)),function(x){return(ret[[x]][idx[[x]]])})
#    names(res)<- names(which(unlist(lapply(idx,length))>0))
#    ret       <- res
#  }
#  return(ret)}

#rects                       <- unique(wgsfdlogp$ICES_rectangle)
#idx                         <- which(is.na(ICESrectangle2LonLat(rects))[,1] | is.na(ICESrectangle2LonLat(rects))[,2])
#total_csquare               <- ICESrectangle2CSquare(rects[-idx],degrees=0.05,onLand=FALSE)
#count_csquare               <- lapply(total_csquare,length)
#idx                         <- which(unlist(count_csquare)>0 & is.na(names(total_csquare))==F)
#tot_csquare                 <- do.call(rbind,lapply(as.list((1:length(total_csquare))[idx]),function(x){return(cbind(names(total_csquare)[x],
                                                                                                                   total_csquare[[x]],
                                                                                                                   length(total_csquare[[x]])))}))
#colnames(tot_csquare)       <- c("ICES_rectangle","c_square","count_csquare")
#save(tot_csquare,file=file.path(outPath,"tot_csquare.Rdata"))
#save(tot_csquare,file="C:/joeg/WGSFD/2015/Niels_workflow/output/tot_csquare.Rdata")
#wgsfdlogp                   <- merge(wgsfdlogp,tot_csquare,by="ICES_rectangle",all.x=T)
#save(wgsfdlogp,file="C:/joeg/WGSFD/2015/Niels_workflow/output/wgsfdlogp.Rdata")
#wgsfdlogp$kw_fishing_days   <- an(wgsfdlogp$kw_fishing_days)/an(wgsfdlogp$count_csquare)
#wgsfdlogp$fishing_days       <- an(wgsfdlogp$fishing_days)/an(wgsfdlogp$count_csquare)
#wgsfdlogp$totweight         <- an(wgsfdlogp$totweight)/an(wgsfdlogp$count_csquare)
#wgsfdlogp$totvalue          <- an(wgsfdlogp$totvalue)/an(wgsfdlogp$count_csquare)
#wgsfdlogp$swept_area        <- an(wgsfdlogp$swept_area)/an(wgsfdlogp$count_csquare)

#save(wgsfdlogp,wgsfdp,file=file.path(outPath,"csquaredWGSFD.RData"))
#save(wgsfdlogp,file="C:/joeg/WGSFD/2015/Niels_workflow/output/wgsfdlogp2.Rdata")
#load("C:/joeg/WGSFD/2015/Niels_workflow/output/wgsfdlogp2.Rdata")

#-------------------------------------------------------------------------------
#- Combine VMS and Logbook data
#-------------------------------------------------------------------------------
#wgsfdlogp$Category[which(wgsfdlogp$gear_code %in% c("LLD","LLS","LL"))] <- "Longlines"
wgsfdp$Category[which(wgsfdp$gear_code %in% c("LLD","LLS","LL"))] <- "Longlines"

#wgsfdlogp$kw_fishinghours   <- wgsfdlogp$kw_fishing_days/24
#wgsfdlogp$fishing_hours      <- wgsfdlogp$fishing_days/24
cols2keep                   <- c("benthis_met","JNCC","country","year","month","c_square","vessel_length_category","totweight","totvalue","fishing_hours","kw_fishinghours","Category","Description","subsurface_prop","swept_area")
wgsfdp                      <- cbind(wgsfdp[,cols2keep],type="VMS")
#wgsfdlogp                   <- cbind(wgsfdlogp[,cols2keep],type="LOG")
wgsfdtot <- wgsfdp
#wgsfdtot                    <- rbind(wgsfdp,wgsfdlogp)
#wgsfdtot                    <- wgsfdtot[,-grep("Category.1",colnames(wgsfdtot))]
#save(wgsfdtot,file=file.path(outPath,"wgsfdtot.RData"))
save(wgsfdtot,file="wgsfdtot.RData")
load("wgsfdtot.RData")


#-------------------------------------------------------------------------------
#- Calculate surface of CSquares
#-------------------------------------------------------------------------------
#data(ICESareas)
xrange                      <- summary(ICESareas)$bbox["x",]
yrange                      <- summary(ICESareas)$bbox["y",]
#grd                         <- createGrid(xrange,yrange,resx=0.05,resy=0.05,type="SpatialGridDataFrame")
#SP                          <- as(as(grd,"SpatialGrid"),"SpatialPolygons")
#save(SP,file=file.path(outPath,"SP.RData"))

#-------------------------------------------------------------------------------
#- Aggregate data by CSquare, year, quarter, Category
#-------------------------------------------------------------------------------

#- Add column with indication if its NEAFC or not
wgsfdtot$NEAFC                <- ifelse(wgsfdtot$country=="NEA",1,0)
wgsfdtot$Category[wgsfdtot$Description=="Danish_seine"]             <-"Demersal seine"
wgsfdtot$Category[wgsfdtot$Description=="Scottish_Seine"]           <-"Demersal seine"
wgsfdtot$Category[wgsfdtot$Description=="Pair_seine"]               <-"Demersal seine"
wgsfdtot$Category[wgsfdtot$Description=="Beach seines"]               <-"Demersal seine"
wgsfdtot$Category[wgsfdtot$Description=="Seine"]               <-"Demersal seine"
wgsfdtot$Category[wgsfdtot$Description=="Purse_seine"]               <-"Purse seine"

wgsfdtot$bottomgear           <- wgsfdtot$Category %in% c("Beam","Dredge","Otter","Demersal seine")
wgsfdtot$HELCOMgroups         <- wgsfdtot$Category %in% c("Beam","Dredge","Otter","Demersal Seine","Longlines","Midwater")
wgsfdtot$swept_area_subsurface<- wgsfdtot$swept_area * wgsfdtot$subsurface_prop/100
wgsfdtot$swept_area_surface   <- wgsfdtot$swept_area * (1- wgsfdtot$subsurface_prop/100)
wgsfdtot$quarter              <- (an(wgsfdtot$month)-1)%/%3+1

save(wgsfdtot,file="C:/joeg/WGSFD/2015/Niels_workflow/output/wgsfdtot2_VMS.RData")
load("C:/joeg/WGSFD/2015/Niels_workflow/output/wgsfdtot2.RData")
write.csv(wgsfdtot,file="C:/joeg/WGSFD/2015/Niels_workflow/output/wgsfdtot2.csv")

wgsfdtot_uk                      <- subset(wgsfdtot,country=='GBR')
wgsfdtot_fra                     <- subset(wgsfdtot,country=='FRA')
wgsfdtot_fra                     <- subset(wgsfdtot,country=='NEA')

#- Select only total dataset
completeSubset                <- complete.cases(wgsfdtot[,c("fishing_hours","kw_fishinghours","totweight","c_square","year","quarter","vessel_length_category","Category","Description","NEAFC","type")])
NEAFCtot                      <- complete.cases(wgsfdtot[,c("fishing_hours","kw_fishinghours","totweight","c_square","year","quarter","vessel_length_category","Category","Description","NEAFC","type")])
NEAFCtot                      <- wgsfdtot[NEAFCtot,]
wgsfdtot                      <- wgsfdtot[completeSubset,]
wgsfdtot$totweight            <- an(wgsfdtot$totweight)
wgsfdtot$totvalue             <- an(wgsfdtot$totvalue)

HELCOMagg1                    <- aggregate(wgsfdtot[,c("swept_area","kw_fishinghours","fishing_hours")],by=as.list(wgsfdtot[,c("c_square","year","Category","HELCOMgroups","type")]),FUN=sum,na.rm=T)
HELCOMagg2                    <- aggregate(wgsfdtot[,c("swept_area","kw_fishinghours","fishing_hours")],by=as.list(wgsfdtot[,c("c_square","year","quarter","Category","HELCOMgroups","type")]),FUN=sum,na.rm=T)

subOSPAR                      <- subset(wgsfdtot,NEAFC==0)
OSPARagg1                     <- aggregate(subOSPAR[,c("swept_area","swept_area_subsurface","swept_area_surface","kw_fishinghours","fishing_hours")],by=as.list(subOSPAR[,c("c_square","year","quarter","Category","vessel_length_category","type")]),FUN=sum,na.rm=T)

subWGDEC                      <- subset(NEAFCtot,NEAFC==1 & year %in% 2013:2014)
WGDECagg1                     <- aggregate(subWGDEC[,c("swept_area_subsurface","swept_area_surface","kw_fishinghours","fishing_hours")],by=as.list(subWGDEC[,c("c_square","year","Category","type")]),FUN=sum,na.rm=T)

subDCF                        <- subset(wgsfdtot,NEAFC==0)
DCFagg1                       <- aggregate(subDCF[,c("swept_area","swept_area_subsurface","swept_area_surface")],by=as.list(subDCF[,c("c_square","year","bottomgear","type")]),FUN=sum,na.rm=T)


#-------------------------------------------------------------------------------
#- Calculate fishing intensity
#-------------------------------------------------------------------------------
DCFagg1                       <- cbind(DCFagg1,CSquare2LonLat(DCFagg1$c_square,degrees=0.05))
DCFagg1$cell_area             <- (cos(DCFagg1$SI_LATI *pi/180) * distance(0,0,0,1) )/20  * (distance(0,0,0,1)/20)

DCFagg1$intensity_total      <- DCFagg1$swept_area / DCFagg1$cell_area
DCFagg1$intensity_subsurf    <- DCFagg1$swept_area_subsurface / DCFagg1$cell_area
DCFagg1$intensity_surf       <- DCFagg1$swept_area_surface / DCFagg1$cell_area

OSPARagg1                       <- cbind(OSPARagg1,CSquare2LonLat(OSPARagg1$c_square,degrees=0.05))
OSPARagg1$cell_area             <- (cos(OSPARagg1$SI_LATI *pi/180) * distance(0,0,0,1) )/20  * (distance(0,0,0,1)/20)

OSPARagg1$intensity_total      <- OSPARagg1$swept_area / OSPARagg1$cell_area
OSPARagg1$intensity_subsurf    <- OSPARagg1$swept_area_subsurface / OSPARagg1$cell_area
OSPARagg1$intensity_surf       <- OSPARagg1$swept_area_surface / OSPARagg1$cell_area

HELCOMagg1                       <- cbind(HELCOMagg1,CSquare2LonLat(HELCOMagg1$c_square,degrees=0.05))
HELCOMagg1$cell_area             <- (cos(HELCOMagg1$SI_LATI *pi/180) * distance(0,0,0,1) )/20  * (distance(0,0,0,1)/20)

HELCOMagg1$intensity_total      <- HELCOMagg1$swept_area / HELCOMagg1$cell_area
HELCOMagg1$intensity_subsurf    <- HELCOMagg1$swept_area_subsurface / HELCOMagg1$cell_area
HELCOMagg1$intensity_surf       <- HELCOMagg1$swept_area_surface / HELCOMagg1$cell_area

HELCOMagg2                       <- cbind(HELCOMagg2,CSquare2LonLat(HELCOMagg2$c_square,degrees=0.05))
HELCOMagg2$cell_area             <- (cos(HELCOMagg2$SI_LATI *pi/180) * distance(0,0,0,1) )/20  * (distance(0,0,0,1)/20)

HELCOMagg2$intensity_total      <- HELCOMagg2$swept_area / HELCOMagg2$cell_area
HELCOMagg2$intensity_subsurf    <- HELCOMagg2$swept_area_subsurface / HELCOMagg2$cell_area
HELCOMagg2$intensity_surf       <- HELCOMagg2$swept_area_surface / HELCOMagg2$cell_area

WGDECagg1                       <- cbind(WGDECagg1,CSquare2LonLat(WGDECagg1$c_square,degrees=0.05))
WGDECagg1$cell_area             <- (cos(WGDECagg1$SI_LATI *pi/180) * distance(0,0,0,1) )/20  * (distance(0,0,0,1)/20)

WGDECagg1$intensity_subsurf    <- WGDECagg1$swept_area_subsurface / WGDECagg1$cell_area
WGDECagg1$intensity_surf       <- WGDECagg1$swept_area_surface / WGDECagg1$cell_area

