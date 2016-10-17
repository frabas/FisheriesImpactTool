
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##
 ## Produce a lattice plot for footprint (i.e. FP metrics) per (EUNIS) habitat
 ## Author: Francois Bastardie (DTU-Aqua), Niels Hintzen (IMARES)
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##


 # GENERAL SETTINGS---------------
 myPath         <- file.path("C:", "Users", "fbas", "Documents", "GitHub")  # to adapt to your own path.
 dataPath       <- file.path(myPath, "FisheriesImpactTool", "Data")
 outPath        <- file.path(myPath, "FisheriesImpactTool", "Outputs")
 shapePath      <- file.path(myPath, "FisheriesImpactTool", "Shapes")
 dir.create(outPath)
 nameAggFile  <- "ALL_AggregatedSweptArea_12062015.RData"
 load(file=file.path(inPath,nameAggFile)) # get aggResult from e.g. the WP2 BENTHIS workflow


 # subset for a year period
 years     <- 2010:2012
 lat_range <- c(53,60)
 lon_range <- c(-5,13)
 raster_res<- c(0.0167,0.0167) # 1 by 1 minute
 #--------------------------------



# subset for the area of interest
library(maptools)
handmade            <- readShapePoly(file.path(shapePath, "wbaltic_wgs84"))  # built in ArcGIS 10.1
the_area            <- sapply(slot(handmade, "polygons"), function(x) lapply(slot(x, "Polygons"), function(x) x@coords)) 
in_area             <- point.in.polygon(aggResult[,'CELL_LONG'], aggResult[,'CELL_LATI'], the_area[[1]][,1],the_area[[1]][,2])
aggResult           <- aggResult[in_area==1,]


# remove unexpected metier(s) in this area
aggResult           <- aggResult[!aggResult$LE_MET %in% c("TBB_DMF", "TBB_CRU", "TBB_DES"),]
aggResult$LE_MET    <- factor(aggResult$LE_MET)


 # combine with (Eunis) Habitat
 pol                 <- readShapePoly(file.path(shapePath, "EUNIS_codes_Combined_ICES_FAO9_clipped")) 
 library(doBy)
 aggResult           <- orderBy(~grID,data=aggResult)
 uniqueCoords        <- aggResult[,c("CELL_LONG","CELL_LATI")]
 coords              <- SpatialPoints(data.frame(SI_LONG=uniqueCoords$CELL_LONG,SI_LATI=uniqueCoords$CELL_LATI))
 idx                 <- over(coords,as(pol,"SpatialPolygons"))
 aggResult$EUNIS     <- pol@data$EUNIS_code[idx]
 aggResult$FAO       <- pol@data$ICES_FAO[idx]

 
  # rename EUNIS:
 #levels(aggResult$EUNIS)
 #[1] "A3.1" "A3.2" "A3.3" "A3.5" "A4.1" "A4.2" "A4.3" "A4.5" "A4.6" "A4.7" "A5.1" "A5.2" "A5.3" "A5.4" "A5.5" "A5.6" "A6.1" "A6.2" "A6.3" "A6.4" "A6.5" "NA"  

 levels(aggResult2$EUNIS) [!levels(aggResult2$EUNIS) %in% 
               c("A3.1", "A3.2", "A3.3", "A3.5", "A4.1", "A4.2", "A4.3", "A4.5", "A4.6", "A4.7", "A5.1", "A5.2", "A5.3", "A5.4", "A5.5", "A5.6", "A6.1", "A6.2", "A6.3", "A6.4", "A6.5"  )]                                                              <- "Unknown"
 levels(aggResult2$EUNIS) [levels(aggResult2$EUNIS) %in% c("A3.1", "A3.2", "A3.3", "A3.5" )]                    <- "Infralittoral rock A3"
 levels(aggResult2$EUNIS) [levels(aggResult2$EUNIS) %in% c("A4.1", "A4.2", "A4.3", "A4.5", "A4.6", "A4.7" )]    <- "Circalittoral rock A4"
 levels(aggResult2$EUNIS) [levels(aggResult2$EUNIS) %in% c("A5.1" )]                                            <- "Sublittoral coarse sediment A5.1"
 levels(aggResult2$EUNIS) [levels(aggResult2$EUNIS) %in% c("A5.2" )]                                            <- "Sublittoral sand A5.2"
 levels(aggResult2$EUNIS) [levels(aggResult2$EUNIS) %in% c("A5.3" )]                                            <- "Sublittoral mud A5.3"
 levels(aggResult2$EUNIS) [levels(aggResult2$EUNIS) %in% c("A5.4" )]                                            <- "Sublittoral mixed sediments A5.4"
 levels(aggResult2$EUNIS) [levels(aggResult2$EUNIS) %in% c("A5.5" )]                                            <- "Sublittoral macrophyte-dominated sediments A5.5"
 levels(aggResult2$EUNIS) [levels(aggResult2$EUNIS) %in% c("A5.6" )]                                            <- "Sublittoral biogenic reefs A5.6"
 levels(aggResult2$EUNIS) [levels(aggResult2$EUNIS) %in% c("A6.1" )]                                            <- "Deep sea rock A6.1"
 levels(aggResult2$EUNIS) [levels(aggResult2$EUNIS) %in% c("A6.2" )]                                            <- "Deep sea mixed sediments A6.2"
 levels(aggResult2$EUNIS) [levels(aggResult2$EUNIS) %in% c("A6.3" )]                                            <- "Deep sea sand A6.3"
 levels(aggResult2$EUNIS) [levels(aggResult2$EUNIS) %in% c("A6.4" )]                                            <- "Deep sea muddy sand A6.4"
 levels(aggResult2$EUNIS) [levels(aggResult2$EUNIS) %in% c("A6.5" )]                                            <- "Deep sea mud A6.5"
 

 # getting info on the surface area of Eunis habitats in the study area
 if(TRUE){
 library(rgeos)
 ly <- gIntersection(pol, handmade,  byid=TRUE)
 plot(ly)
 }
 
 # tricky. we need to recreate a SpatialPolygonsDataFrame for handmade_WGS84_reduced and the ids should be from the original untouched shp i.e handmade_WGS84....
 new.attribs                       <- data.frame(do.call(rbind,strsplit(row.names(ly)," ")),stringsAsFactors = FALSE)    # get the attributes back!
 new.attrib.data                   <- pol[new.attribs$X1,]@data
 row.names(ly) <- row.names(new.attrib.data)
 ly            <- SpatialPolygonsDataFrame(ly, new.attrib.data)
 ly_df         <- as.data.frame(ly) # get attributes(pol)$data
 ly_df$SP_ID   <- as.numeric(row.names(ly_df))   # that one is really tricky

 # check if some holes
 is_hole <- sapply(slot(ly, "polygons"), function(x) {
   xi <- slot(x, "Polygons")
   any(sapply(xi, slot, "hole"))
 }) 
 # get the area
 library(rgdal)
 proj4string(ly) <- CRS("+proj=longlat +datum=WGS84") 
 ly_utm          <- spTransform(ly, CRS(paste("+proj=utm  +ellps=intl +zone=",32," +towgs84=-84,-107,-120,0,0,0,0,0", sep='')))
 areas           <- sapply(slot(ly_utm, "polygons"), slot, "area")   # area in m2

 #proj4string(handmade) <- CRS("+proj=longlat +datum=WGS84") 
 #handmade_utm    <- spTransform(handmade, CRS(paste("+proj=utm  +ellps=intl +zone=",32," +towgs84=-84,-107,-120,0,0,0,0,0", sep='')))
 #area_entire_region<- sapply(slot(handmade_utm, "polygons"), slot, "area")   # area in m2
 #=> no, this polygon is much larger
 
 # remove the holes
 #areas <- areas[!is_hole]

 #ly_df <- ly_df[!is_hole,]

 surface_area_in_ly_per_pol <- cbind.data.frame(areas, as.character(ly_df[,'EUNIS_code']))
 surface_area_in_ly         <- tapply(surface_area_in_ly_per_pol$areas, surface_area_in_ly_per_pol[,2], sum, na.rm=TRUE)

 # finally, get the prop of the habitats in the studied area
 surf_hab <- cbind.data.frame(Eunis=names(surface_area_in_ly), Surface_area=surface_area_in_ly, prop_habitat=surface_area_in_ly/sum(surface_area_in_ly))
 surf_hab$Eunis <- as.character(surf_hab$Eunis)
 surf_hab[surf_hab$Eunis=="NA", "Eunis"] <- "Unknown" 
 
 #reclassify Eunis levels & aggregate
 surf_hab$Eunis         <- factor(surf_hab$Eunis)
 levels(surf_hab$Eunis) <- c("Infralittoral rock A3", "Infralittoral rock A3", "Infralittoral rock A3", "Infralittoral rock A3", "Circalittoral rock A4", "Circalittoral rock A4", "Circalittoral rock A4", "Circalittoral rock A4",                                "Sublittoral coarse sediment A5.1", "Sublittoral sand A5.2", "Sublittoral mud A5.3", "Sublittoral mixed sediments A5.4", "Unknown")  
 surf_hab               <- aggregate(surf_hab[,c('Surface_area', 'prop_habitat')], list(surf_hab$Eunis), sum)
 colnames(surf_hab)     <- c('Eunis','Surface_area', 'prop_habitat')

 sum(surf_hab$Surface_area)/1e6  # total surface in km2
  
 # Add sub surface percentages and average over the 3 years
 surSubsur                          <- as.data.frame(rbind(
                                            c("SDN_DMF",        100.0,    0.0),
                                            c("OT_SPF",         97.2,     2.8),
                                            c("SSC_DMF",        95.0,     5.0),
                                            c("OT_DMF",         92.2,     7.8),
                                            c("OT_MIX_DMF_BEN", 91.4,     8.6),
                                            c("OT_MIX",         85.3,     14.7),
                                            c("OT_MIX_DMF_PEL", 78.0,     22.0),
                                            c("OT_MIX_NEP",     77.1,     22.9),
                                            c("SSC_DEM",        95.0,     5.0),
                                            c("OT_MIX_DMF",     91.4,     8.6),
                                            c("SDN_DEM",        100.0,    0.0),
                                            c("OT_MIX_PEL",     78.0,     22.0),
                                            c("OT_MIX_DPS",     70.8,     29.2),
                                            c("TBB_DES",        0.0,      100.0),
                                            c("OT_MIX_CRU_DMF", 77.1,     22.9),
                                            c("OT_MIX_CRU",     70.8,     29.2),
                                            c("OT_CRU",         67.9,     32.1),
                                            c("TBB_CRU",        47.8,     52.2),
                                            c("TBB_DMF",        0.0,      100.0),
                                            c("TBB_MOL",        0.0,      100.0),
                                            c("DRB_MOL",        0.0,      100.0)),stringsAsFactors=F)

 colnames(surSubsur)                 <- c("LE_MET","surface","subsurface")
 

 
 aggResult$surf <- (cos(aggResult$CELL_LATI *pi/180) * 111.325 )/60  * (111/60) # approx. cell area of 1 by 1 minute in km2
 # a check  
 dd <- aggResult[aggResult$SWEPT_AREA_KM2>0 & aggResult$Year=="2012",]
 dd <- dd[!duplicated(dd$grID),]
 sum(dd$surf) # fished area in km2

 
 sauv           <- aggResult 
 aggResult      <- merge(aggResult, surSubsur, by.x="LE_MET",by.y="LE_MET")
 an             <- function(x) as.numeric(as.character(x))
 aggResult      <- cbind(aggResult[,c("LE_MET","CELL_LONG","CELL_LATI","Year","EUNIS","surf","FAO")],
                   data.frame(SURF_SWEPT_AREA_KM2           =aggResult$SWEPT_AREA_KM2*100,
                              SUBSURF_SWEPT_AREA_KM2        =aggResult$SWEPT_AREA_KM2*an(aggResult$subsurface),
                              SURF_SWEPT_AREA_KM2_LOWER     =aggResult$SWEPT_AREA_KM2_LOWER*100,
                              SUBSURF_SWEPT_AREA_KM2_LOWER  =aggResult$SWEPT_AREA_KM2_LOWER*an(aggResult$subsurface),
                              SURF_SWEPT_AREA_KM2_UPPER     =aggResult$SWEPT_AREA_KM2_UPPER*100,
                              SUBSURF_SWEPT_AREA_KM2_UPPER  =aggResult$SWEPT_AREA_KM2_UPPER*an(aggResult$subsurface)))

  # simplify/reclassify the metiers
 levels(aggResult$LE_MET) <- c('Otter Trawl', 'Otter Trawl', 'Otter Trawl', 'Otter Trawl', 'Seine', 'Unknown', 'Dredge','Otter Trawl', 'Seine') 

 #----------
 # check visually and by numbers
 xrange  <- c(9,15) # ALL
 yrange  <- c(53.5,56.5) # ALL
 
  #Take the mean over the years
 aggResult2 <- aggregate(aggResult[,grep("SWEPT_AREA_KM2",colnames(aggResult))],
                           by=as.list(aggResult[,c("CELL_LONG", "CELL_LATI", "LE_MET","EUNIS", "surf", "FAO")]), FUN=mean,na.rm=T)

 # rasterize before loading in GIS
 library(raster)
 r           <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], res=raster_res, crs=CRS("+proj=longlat +datum=WGS84"))
 some_coords <- SpatialPoints(cbind(CELL_LONG=aggResult2$CELL_LONG, CELL_LATI=aggResult2$CELL_LATI))
 rstr        <- rasterize(x=some_coords, y=r, field=aggResult2$SURF_SWEPT_AREA_KM2, fun="sum") 
 plot(ly)
 plot(rstr, add=T, xlim=xrange, ylim=yrange)
 #a           <- area(rstr)
 #rstr_std    <- rstr/a
 #writeRaster(rstr_std, file.path("C:","BENTHIS","outputs", paste("aggResult", sep='')), format = "GTiff")    # check in ArcGIS


 r_utm                    <- raster(xmn= extent(ly_utm)@xmin, xmx=extent(ly_utm)@xmax, ymn=extent(ly_utm)@ymin, ymx=extent(ly_utm)@ymax, 
                                  res=c(2000,2000), crs=CRS(paste("+proj=utm  +ellps=intl +zone=",32," +towgs84=-84,-107,-120,0,0,0,0,0", sep='')))
 some_coords              <- SpatialPoints(cbind(CELL_LONG=aggResult2$CELL_LONG, CELL_LATI=aggResult2$CELL_LATI))
 proj4string(some_coords) <- CRS("+proj=longlat +datum=WGS84") 
 some_coords_utm          <- spTransform(some_coords, CRS(paste("+proj=utm  +ellps=intl +zone=",32," +towgs84=-84,-107,-120,0,0,0,0,0", sep='')))
 rstr_utm                 <- rasterize(x=some_coords_utm, y=r_utm, field=aggResult2$SURF_SWEPT_AREA_KM2, fun="sum") 
 plot(ly_utm)
 rstr_utm[rstr_utm <= 0] <- NA
 plot(rstr_utm,  add=T)
 sum(area(ly_utm)) /1e6 # in km2  ## BUT COUNTING HOLES ?
 fishedarea    <-  length(rstr_utm[rstr_utm>0])  *  prod(res(rstr_utm )) / 1e6 # nb of cells with presence times the resolution in meters and then converted in km2
 print(fishedarea)
 print(sum(surf_hab$Surface_area)/1e6)
 
  # a check  
 dd <- aggResult2[aggResult2$SURF_SWEPT_AREA_KM2>0,]
 dd <- dd[!duplicated(data.frame(dd$CELL_LONG, dd$CELL_LATI)),]
 sum(dd$surf) # fished area in km2

 #----------
 


 # take the mean over the years
 aggResultPeriod <- aggregate(aggResult[,grep("SWEPT_AREA_KM2",colnames(aggResult))],
                           by=as.list(aggResult[,c("LE_MET","EUNIS", "CELL_LONG", "CELL_LATI", "surf", "FAO")]), FUN=mean,na.rm=T)

 aggResultPeriod$EUNIS <- as.character(aggResultPeriod$EUNIS)
 aggResultPeriod[is.na(aggResultPeriod$EUNIS) | aggResultPeriod$EUNIS=="NA", 'EUNIS'] <- "Unknown"
 aggResultPeriod$EUNIS <- factor(aggResultPeriod$EUNIS)

 # a check  
 dd <- aggResultPeriod[aggResultPeriod$SURF_SWEPT_AREA_KM2>0,]
 sum(aggResultPeriod$surf) # fished area in km2
 #=> this is ok not the same as previous check because the previous one was removing the duplicates while that one is donig the proper averaging...



# Footprint seabed
# Greater of smaller than 1
idxst1        <- which(aggResultPeriod$SURF_SWEPT_AREA_KM2<=1)
idxgt1        <- which(aggResultPeriod$SURF_SWEPT_AREA_KM2>1)
res           <- aggregate(aggResultPeriod$SURF_SWEPT_AREA_KM2[idxst1],by=list(aggResultPeriod$EUNIS[idxst1],aggResultPeriod$LE_MET[idxst1]),FUN=sum,na.rm=T)
colnames(res) <- c("Eunis","Metier","Footprint seabed")
res           <- orderBy(~Eunis+Metier,data=res)
res2          <- aggregate(aggResultPeriod$surf[idxgt1],by=list(aggResultPeriod$EUNIS[idxgt1],aggResultPeriod$LE_MET[idxgt1]),FUN=sum,na.rm=T)
colnames(res2)<- c("Eunis","Metier","Footprint seabed")
res2          <- orderBy(~Eunis+Metier,data=res2)
dat1          <- rbind.data.frame(res, res2)
dat1          <- aggregate(dat1$"Footprint seabed",by=list(dat1$Eunis,dat1$Metier),FUN=sum,na.rm=T)
colnames(dat1)<- c("Eunis","Metier","Footprint seabed")
dat1          <- orderBy(~Eunis+Metier,data=dat1)


# Footprint grid cells
dat2           <- aggregate(aggResultPeriod$surf, by=list(aggResultPeriod$EUNIS,aggResultPeriod$LE_MET),FUN=sum,na.rm=T)
colnames(dat2) <- c("Eunis","Metier","Footprint grid cells")
dat2           <- orderBy(~Eunis+Metier,data=dat2)

# stack all
dat            <- cbind(dat1, "Footprint grid cells" = dat2[, "Footprint grid cells"])
dat            <- merge(dat, surf_hab)

# standardize to 1
dat$"Footprint grid cells" <- dat$"Footprint grid cells" /(dat$Surface_area / 1e6)  # converted in km2
# standardize to 1
dat$"Footprint seabed" <- dat$"Footprint seabed" /(dat$Surface_area / 1e6)  # converted in km2



dat1           <- dat[,c('Metier', 'Eunis', 'Surface_area', 'prop_habitat', 'Footprint grid cells')]
colnames(dat1) <- c('Metier', 'Eunis', 'Surface_area','prop_habitat', 'y')
dat1           <- cbind.data.frame(dat1, typearea="Footprint grid cells")

dat2           <- dat[,c('Metier', 'Eunis', 'Surface_area', 'prop_habitat', 'Footprint seabed')]
colnames(dat2) <- c('Metier', 'Eunis', 'Surface_area', 'prop_habitat', 'y')
dat2           <- cbind.data.frame(dat2, typearea="Footprint seabed")

dat_long <- rbind.data.frame(dat1, dat2)


# trick for prop_habitat
dat3 <- dat1[, c('Metier', 'Eunis', 'Surface_area', 'prop_habitat', 'typearea')]
dat3$typearea <- "Habitat surface area"
dat3$y         <- dat3$prop_habitat
dat_long <-  rbind.data.frame(dat_long, dat3)

dat_long$typearea  <- factor(dat_long$typearea, levels=c("Footprint seabed", "Footprint grid cells", "Habitat surface area"))
dat_long$Eunis  <- factor(dat_long$Eunis, levels=rev(levels(dat_long$Eunis)))

dat_long$y  <- dat_long$y  *100 # in percent
 


# do the plot
sc <- 2
library(lattice)
sty <- list()
sty$strip.border$col <- NA
sty$strip.background$col <- grey(0.5)

png(file.path(outPath, "Footprint_seabed_bottom_gears_in_wbaltic.png", width=sc*3*300, height=sc*3.6*300, res=300)

trellis.par.set(strip.border=list(col="black"))
 
barchart(factor(Eunis)~y  | factor(Metier), groups=typearea, dat_long, horizontal=T, layout=c(1,3), xlim=c(0,100), aspect=0.7, 
               xlab=expression("Habitat %"), 
                auto.key = list(columns = 1, cex=0.8),
                par.settings = list(superpose.polygon = list(col = c('white','grey', 'black') )),
                strip = strip.custom( bg =  "white"), 
                #strip=stripfun, par.settings=sty,
                par.strip.text=list(cex=0.8) , between = list(x = 1, y=1)
                )
dev.off()

