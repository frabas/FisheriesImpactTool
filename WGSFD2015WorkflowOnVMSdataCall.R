 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##
 ## Workflow of WGSFD 2015 to process VMS data from ICES data call
 ## Author: Niels Hintzen, Francois Bastardie, Josephine Egekvist (DTU-Aqua) 
 ## and the ICES WGSFD 2015 members
 ##
 ## ALSO LOOK AT https://github.com/ices-eg/VMS-datacall
 ##
 
 ## citations:
 ## Bastardie, F., Nielsen, J. R., Ulrich, C., Egekvist, J., and Degel, H. 2010.
 ## Detailed mapping of fishing effort and landings by coupling fishing logbooks with satellite-recorded vessel geo-location. Fisheries Research, 106: 41�53

 ## Hintzen, N. T., Bastardie, F., Beare, D., Piet, G. J., Ulrich, C., Deporte, N., Egekvist, J., et al. 2012. VMStools: Open-source software for the processing, analysis and visualization of fisheries logbook and VMS data. Fisheries Research, 115�116: 31�43.

 ##Eigaard O.R., Bastardie F., Hintzen N., Buhl-Mortensen L., Buhl-Mortensen P., Catarino R., Dinesen G.E., et al., 2016b. Benthic impact of fisheries in European waters: the distribution and intensity of bottom trawling. ICES Journal of Marine Science, in press.
 
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##

 # GENERAL SETTINGS---------------
 myPath         <- file.path("C:", "Users", "fbas", "Documents", "GitHub")  # to adapt to your own path.
 dataPath       <- file.path(myPath, "FisheriesImpactTool", "Data")
 outPath        <- file.path(myPath, "FisheriesImpactTool", "Outputs")
 shapePath      <- file.path(myPath, "FisheriesImpactTool", "Shapes")
 codePath       <- file.path(myPath, "FisheriesImpactTool")
 dir.create(outPath)
   

 # subset for a year period
 years     <- 2015
 lat_range <- c(53,60)
 lon_range <- c(-5,13)
 raster_res<- c(0.0167,0.0167) # 1 by 1 minute
 #--------------------------------

#- Workflow of WGSFD 2015

##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##
##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##
##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##
##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##
##!##!##!##!##!##!##!##!##!##!   IMPORTANT NOTE     #!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##
##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##
##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##
##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##
##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##
##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##
# this worflow is no longer used for obtaining the ICES VMS database,
# it has been replaced by a SAS routine for WGSFD 2016 (written by Carlos Pinto)
# to make it consistent with the ICES data treatment centered on SAS. 
# therefore the below R code is deprecated and only given for info
# ....also not working given input data are not provided as there are confidential.
##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##
##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##
##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##
##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##!##



library(vmstools)

#-------------------------------------------------------------------------------
#- Read in the raw WGSFD data table 1 only with speeds
#-------------------------------------------------------------------------------
wgsfd                  <- read.csv(file.path(dataPath,"VMS_Table_wspeed.csv"), header=T, stringsAsFactors=F, sep=";")
#test
wgsfd1                 <- subset(wgsfd, wgsfd$c_square=='1500:457:383:4')

head(wgsfd1)

wgsfd                  <- wgsfd1

wgsfd1$cell_area       <- (cos(55.875 *pi/180) * distance(0,0,0,1) )/20  * (distance(0,0,0,1)/20)


#-------------------------------------------------------------------------------
#- Read in metier-conversion table
#-------------------------------------------------------------------------------
metiers_DCF_BENTHIS_lookup                  <- read.table(file=file.path(dataPath, "lookup_metiers_WGSFD2016.csv"), sep=";", header=TRUE, stringsAsFactors=FALSE)
metiers_DCF_BENTHIS_lookup$Benthis_metiers  <- ac(metiers_DCF_BENTHIS_lookup$Benthis_metiers)
metiers_DCF_BENTHIS_lookup$JNCC_grouping    <- ac(metiers_DCF_BENTHIS_lookup$JNCC_grouping)
metiers_DCF_BENTHIS_lookup$Fishing_category <- ac(metiers_DCF_BENTHIS_lookup$Fishing_category)
metiers_DCF_BENTHIS_lookup$LE_MET_level6    <- ac(metiers_DCF_BENTHIS_lookup$LE_MET_level6)
metiers_DCF_BENTHIS_lookup$Metier_level4    <- ac(metiers_DCF_BENTHIS_lookup$Metier_level4)
metiers_DCF_BENTHIS_lookup$Description      <- ac(metiers_DCF_BENTHIS_lookup$Description)

metiers_tjek <- subset(metiers_DCF_BENTHIS_lookup, metiers_DCF_BENTHIS_lookup$Benthis_metiers_reviewed=='OT_DMF')

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
    tmpwgsfd[idx3,"benthis_met"]      <- metiers_DCF_BENTHIS_lookup$Benthis_metiers_reviewed[i]
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
    tmpwgsfd[idx3,"benthis_met"]    <- metiers_DCF_BENTHIS_lookup$Benthis_metiers_reviewed[i]
  if(length(idx2)>0)
    tmpwgsfd[idx2,"JNCC"]           <- metiers_DCF_BENTHIS_lookup$JNCC_grouping[i]
  if(length(idx1)>0)
    tmpwgsfd[idx1,"Category"]       <- metiers_DCF_BENTHIS_lookup$Fishing_category[i]
  if(length(idx4)>0)
    tmpwgsfd[idx4,"Description"]    <- metiers_DCF_BENTHIS_lookup$Description[i]
}
wgsfd[,c("benthis_met","JNCC","Category","Description")] <- tmpwgsfd[,c("benthis_met","JNCC","Category","Description")]

wgsfdp                                  <- wgsfd
#-------------------------------------------------------------------------------
#- Add gear widths to dataset & calculate swept area
#-------------------------------------------------------------------------------
source(file.path(codePath, "gearCharacteristics.R"))

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
wgsfdp                      <- merge(wgsfdp,     BenthisGearWidths, by.x="benthis_met", by.y= "benthis_met",all.x=TRUE)

#- calculate the area swept
idx                         <- !wgsfdp$benthis_met %in% c("SDN_DMF", "SSC_DMF") # exclude seiners
wgsfdp$swept_area           <- 0
wgsfdp$swept_area[idx]      <- an(wgsfdp$fishing_hours[idx]) * an(wgsfdp$gear_width[idx]) * an(wgsfdp$avg_fishing_speed_new[idx])*1.852 #conversion from knots to km/hour
wgsfdp$swept_area[idx]      <- an(wgsfdp$fishing_hours[idx]) * an(wgsfdp$gear_width[idx]) * an(wgsfdp$avg_fishing_speed[idx])*1.852 #conversion from knots to km/hour


#- caution: specific rules for Seiners
idx                         <- wgsfdp$benthis_met %in% c("SDN_DMF") # for seiners
wgsfdp$swept_area[idx]      <- (pi*(an(wgsfdp$gear_width[idx])/(2*pi))^2) *  an(wgsfdp$fishing_hours)[idx]   / haul_duration_seiners["SDN_DMF"]

idx                         <- wgsfdp$benthis_met %in% c("SSC_DMF") # scottish seiners
wgsfdp$swept_area[idx]      <- (pi*(an(wgsfdp$gear_width[idx])/(2*pi))^2) *  an(wgsfdp$fishing_hours)[idx]   / haul_duration_seiners["SSC_DMF"]
wgsfdp$swept_area[idx]      <- wgsfdp$swept_area[idx]  *1.5 # ad hoc correction to account for the SSC specificities


wgsfdp$Category[which(wgsfdp$gear_code %in% c("LLD","LLS","LL"))] <- "Longlines"

cols2keep                   <- c("benthis_met","JNCC","country","year","month","c_square","vessel_length_category","totweight","totvalue","fishing_hours","kw_fishinghours","Category","Description","subsurface_prop","swept_area")
wgsfdp                      <- cbind(wgsfdp[,cols2keep],type="VMS")
wgsfdtot <- wgsfdp

#-------------------------------------------------------------------------------
#- Calculate surface of CSquares
#-------------------------------------------------------------------------------
data(ICESareas)
xrange                      <- summary(ICESareas)$bbox["x",]
yrange                      <- summary(ICESareas)$bbox["y",]

#-------------------------------------------------------------------------------
#- Aggregate data by CSquare, year, quarter, Category
#-------------------------------------------------------------------------------

wgsfdtot$Category[wgsfdtot$Description=="Danish_seine"]             <- "Demersal seine"
wgsfdtot$Category[wgsfdtot$Description=="Scottish_Seine"]           <- "Demersal seine"
wgsfdtot$Category[wgsfdtot$Description=="Pair_seine"]               <- "Demersal seine"
wgsfdtot$Category[wgsfdtot$Description=="Beach seines"]             <- "Demersal seine"
wgsfdtot$Category[wgsfdtot$Description=="Seine"]                    <- "Demersal seine"
wgsfdtot$Category[wgsfdtot$Description=="Purse_seine"]              <- "Purse seine"

wgsfdtot$bottomgear           <- wgsfdtot$Category %in% c("Beam","Dredge","Otter","Demersal seine")
wgsfdtot$HELCOMgroups         <- wgsfdtot$Category %in% c("Beam","Dredge","Otter","Demersal Seine","Longlines","Midwater")
wgsfdtot$swept_area_subsurface<- wgsfdtot$swept_area * wgsfdtot$subsurface_prop/100
wgsfdtot$quarter              <- (an(wgsfdtot$month)-1)%/%3+1

#- Select  dataset
completeSubset                <- complete.cases(wgsfdtot[,c("fishing_hours","kw_fishinghours","totweight","c_square","year","quarter","vessel_length_category","Category","Description","type")])
wgsfdtot                      <- wgsfdtot[completeSubset,]
wgsfdtot$totweight            <- an(wgsfdtot$totweight)
wgsfdtot$totvalue             <- an(wgsfdtot$totvalue)

HELCOMagg1                    <- aggregate(wgsfdtot[,c("swept_area","swept_area_subsurface", "kw_fishinghours","fishing_hours")],by=as.list(wgsfdtot[,c("c_square","year","Category","HELCOMgroups","type")]), FUN=sum, na.rm=TRUE)
HELCOMagg2                    <- aggregate(wgsfdtot[,c("swept_area","swept_area_subsurface", "kw_fishinghours","fishing_hours")],by=as.list(wgsfdtot[,c("c_square","year","quarter","Category","HELCOMgroups","type")]), FUN=sum, na.rm=TRUE)

#-------------------------------------------------------------------------------
#- Calculate fishing intensity
#-------------------------------------------------------------------------------

var <- distance(0,0,0,1)

HELCOMagg1                       <- cbind(HELCOMagg1,CSquare2LonLat(HELCOMagg1$c_square,degrees=0.05))
HELCOMagg1$cell_area             <- (cos(HELCOMagg1$SI_LATI *pi/180) * distance(0,0,0,1) )/20  * (distance(0,0,0,1)/20)

HELCOMagg1$intensity_total      <- HELCOMagg1$swept_area / HELCOMagg1$cell_area
HELCOMagg1$intensity_subsurf    <- HELCOMagg1$swept_area_subsurface / HELCOMagg1$cell_area

HELCOMagg2                       <- cbind(HELCOMagg2,CSquare2LonLat(HELCOMagg2$c_square,degrees=0.05))
HELCOMagg2$cell_area             <- (cos(HELCOMagg2$SI_LATI *pi/180) * distance(0,0,0,1) )/20  * (distance(0,0,0,1)/20)

HELCOMagg2$intensity_total      <- HELCOMagg2$swept_area / HELCOMagg2$cell_area
HELCOMagg2$intensity_subsurf    <- HELCOMagg2$swept_area_subsurface / HELCOMagg2$cell_area


#-------------------------------------------------------------------------------
#- Export
#-------------------------------------------------------------------------------

write.csv(HELCOMagg2,  file=file.path(outPath, "HELCOM2015_2.csv"))


#-------------------------------------------------------------------------------
#- Rasterize
#-------------------------------------------------------------------------------
#eco                            <- readShapePoly(file.path(dataPath, "ices_ecoregions"))

 example_data <- TRUE
 if(!example_data){
 # rasterize with e.g. 1 by 1 minute
 library(raster)
 xrange      <- range(HELCOMagg2$SI_LONG, na.rm=TRUE)
 yrange      <- range(HELCOMagg2$SI_LATI, na.rm=TRUE)

 # surface impact, all gears
 r           <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], res=raster_res, crs=CRS("+proj=longlat +datum=WGS84"))
 some_coords <- SpatialPoints(cbind(SI_LONG=HELCOMagg2$SI_LONG, SI_LATI=HELCOMagg2$SI_LATI))
 rstr        <- rasterize(x=some_coords, y=r, field=HELCOMagg2$swept_area /length(unique(HELCOMagg2$years)), fun="sum")  # divided by nb of years to obtain an annual average fishing intensity
 plot(rstr, xlim=c(-5,13), ylim=c(53,60))
 #a           <- area(rstr)
 #rstr_std    <- rstr/a  # already done earlier in the source
 rstr_std     <- rstr
 library(rgdal)
 rstr_eea     <- projectRaster(rstr_std, crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")  # European EEA projection
 rstr_eea[is.na(rstr_eea)] <- -999  # arbitrary code, to get rid of true 0s in GIS
 rstr_eea[rstr_eea<0.001]  <- -999
 writeRaster(rstr_eea, file.path(outPath,"SurfaceHELCOMagg2rasterEEA"), format = "GTiff", overwrite=TRUE)
 }



