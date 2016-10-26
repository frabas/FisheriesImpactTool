 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##
 ## Produce Raster layers that can be imported in a GIS environment
 ## Author: Francois Bastardie (DTU-Aqua)
 
 ## citations:
 ## Bastardie, F., Nielsen, J. R., Ulrich, C., Egekvist, J., and Degel, H. 2010.
 ## Detailed mapping of fishing effort and landings by coupling fishing logbooks with satellite-recorded vessel geo-location. Fisheries Research, 106: 41–53

 ## Hintzen, N. T., Bastardie, F., Beare, D., Piet, G. J., Ulrich, C., Deporte, N., Egekvist, J., et al. 2012. VMStools: Open-source software for the processing, analysis and visualization of fisheries logbook and VMS data. Fisheries Research, 115–116: 31–43.
 
 ## Eigaard O.R., Bastardie F., Hintzen N., Buhl-Mortensen L., Buhl-Mortensen P., Catarino R., Dinesen G.E., et al., 2016b. Benthic impact of fisheries in European waters: the distribution and intensity of bottom trawling. ICES Journal of Marine Science, in press.
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##
 


 # GENERAL SETTINGS---------------
 myPath         <- file.path("C:", "Users", "fbas", "Documents", "GitHub")
 dataPath       <- file.path(myPath, "FisheriesImpactTool", "Data")
 outPath        <- file.path(myPath, "FisheriesImpactTool", "Outputs")
 dir.create(outPath)
 nameAggFile  <- "ALL_AggregatedSweptArea_12062015.RData"
 load(file=file.path(inPath,nameAggFile)) # get aggResult from e.g. the WP2 BENTHIS workflow


 # subset for a year period
 years     <- 2010:2012
 lat_range <- c(53,60)
 lon_range <- c(-5,13)
 raster_res<- c(0.0167,0.0167) # 1 by 1 minute
 #--------------------------------


 # subset for the defined period
 aggResult <- aggResult[aggResult$Year %in% c(2010:2012),]

 # compute an approx. grid cell area (to make it quick) and standardize to obtain annual fishing intensity
 aggResult2           <- aggResult
 aggResult2$cell_area <- (cos(aggResult2$CELL_LATI *pi/180) * 111.325 )/60  * (111/60)
 aggResult2[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] <-
                  aggResult2[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] /  aggResult2$cell_area # standardize


 ##----------------------------------------------------------------------------
 ##----------------------------------------------------------------------------
 # split in main fishing activities
 # caution: replace levels to simplify the naming of metiers in 4 categories
 aggResult2$LE_MET_rough <- factor(aggResult2$LE_MET) # init

 # rename
 levels(aggResult2$LE_MET_rough) [levels(aggResult2$LE_MET_rough) %in% c("OT","OTB","OTT",  "OTM", "PTB", "PTM" )]   <- "Otter Trawl"
 levels(aggResult2$LE_MET_rough) [levels(aggResult2$LE_MET_rough) %in% c("PS_", "SDN", "SSC")]                       <- "Seine"
 levels(aggResult2$LE_MET_rough) [levels(aggResult2$LE_MET_rough) %in% c("TBB" )]                                    <- "Beam Trawl"
 levels(aggResult2$LE_MET_rough) [levels(aggResult2$LE_MET_rough) %in% c("DRB")]                                     <- "Dredge"

 aggResult2_Otter           <- aggResult2 [aggResult2$LE_MET2 %in% "Otter Trawl", ]
 aggResult2_Seine           <- aggResult2 [aggResult2$LE_MET2 %in% "Seine", ]
 aggResult2_Beam            <- aggResult2 [aggResult2$LE_MET2 %in% "Beam Trawl", ]
 aggResult2_Dredge          <- aggResult2 [aggResult2$LE_MET2 %in% "Dredge", ]


 ##----------------------------------------------------------------------------
 ##----------------------------------------------------------------------------
 # split in surface - subsurface components
 # compute the subsurface swept area

  matprop <- data.frame(
     LE_MET=c( 'OT_SPF','SDN_DEM','OT_DMF','OT_MIX_DMF_BEN','SSC_DEM','OT_MIX','OT_MIX_DMF_PEL','OT_MIX_CRU_DMF','OT_MIX_CRU','OT_CRU','TBB_CRU','TBB_DMF','TBB_MOL','DRB_MOL','OT_MIX_NEP','SDN_DMF','SSC_DMF','TBB_SPF','OT_MIX_PEL','OT_MIX_DPS'),
     LE_MET_paper=c('OT_SPF','SDN_DEM','OT_DMF','OT_MIX_DMF_BEN','SSC_DEM','OT_MIX','OT_MIX_DMF_PEL','OT_MIX_CRU_DMF','OT_MIX_CRU','OT_CRU','TBB_CRU','TBB_DMF','TBB_MOL','DRB_MOL','OT_MIX_CRU','SDN_DEM','SSC_DEM','TBB_CRU','OT_MIX','OT_MIX'),
     target=c('Sprat or sandeel','plaice. cod','Cod or plaice or Norway pout','Benthic fish ','cod. haddock. flatfish','Otter trawl - miscellanious','Bentho-pelagic fish ','Nephrops and mixed demersal','Shrimp','Nephrops or shrimps','Crangon','Sole and plaice','Conk snails in Black Sea','Scallops. mussels','Shrimp','plaice. cod','cod. haddock. flatfish','Crangon','Otter trawl - miscellanious','Otter trawl - miscellanious'),
     LE_MET2=c('Otter trawl - sprat. sandeel. herring','Danish seine - plaice. cod. haddock','Otter-trawl - cod. plaice. Norway pout','Otter trawl - mixed benthic species','Scottish seining - cod. haddock. flatfish','Otter trawl - mixed miscellanious','Otter trawl - mixed bentho-pelagic species','Otter trawl - mixed Nephrops and demersal','Otter trawl - mixed shrimp','Otter trawl - crustaceans','Beam trawl - Crangon','Beam trawl - sole and plaice','Beam trawl - conk snails','Dredge - scallops. mussels','Otter trawl - mixed shrimp','Danish seine - plaice. cod. haddock','Scottish seining - cod. haddock. flatfish','Beam trawl - Crangon','Otter trawl - mixed miscellanious','Otter trawl - mixed miscellanious'),
     Surface=c(97.2,95,92.2,91.4,86,85.3,78,77.1,70.8,67.9,47.8,0,0,0,70.8,95,86,47.8,85.3,85.3),
     Subsurface=c(2.8,5,7.8,8.6,14,14.7,22,22.9,29.2,32.1,52.2,100,100,100,29.2,5,14,52.2,14.7,14.7)
  )  # Figure10_Eigaard_et_al 2016
   #=> small discrepencies in naming the metier between the paper and the BENTHIS workflow corrected by hand. (e.g. OT_MIX_NEP=>OT_MIX_CRU, etc.)


  rownames(matprop)              <- matprop$LE_MET
  aggResult2$multiplier          <- factor(aggResult2$LE_MET)
  levels(aggResult2$multiplier)  <- matprop[levels(aggResult2$multiplier), "Subsurface"]

  aggResult2_Subsurface          <- aggResult2[c("SWEPT_AREA_KM2", "CELL_LONG", "CELL_LATI")]
  aggResult2_Subsurface[,c("SWEPT_AREA_KM2")] <-
                  aggResult2_Subsurface[,c("SWEPT_AREA_KM2")] * as.numeric(as.character(aggResult2$multiplier))/100
 ##----------------------------------------------------------------------------
 ##----------------------------------------------------------------------------


 ##----------------------------------------------------------------------------
 ##----------------------------------------------------------------------------
 # restrict to the area of interest

 aggResult2  <- aggResult2 [
                   aggResult2$CELL_LONG>lon_range[1] &
                   aggResult2$CELL_LONG<lon_range[2] &
                   aggResult2$CELL_LATI>lat_range[1] &
                   aggResult2$CELL_LATI<lat_range[2]  ,
                   ]




 ##----------------------------------------------------------------------------
 ##----------------------------------------------------------------------------
 # rasterize with e.g. 1 by 1 minute
 library(raster)
 xrange      <- range(aggResult2$CELL_LONG, na.rm=TRUE)
 yrange      <- range(aggResult2$CELL_LATI, na.rm=TRUE)

 # surface impact, all gears
 r           <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], res=c(0.0167,0.0167), crs=CRS("+proj=longlat +datum=WGS84"))
 some_coords <- SpatialPoints(cbind(SI_LONG=aggResult2$CELL_LONG, SI_LATI=aggResult2$CELL_LATI))
 rstr        <- rasterize(x=some_coords, y=r, field=aggResult2$SWEPT_AREA_KM2 /length(years), fun="sum")  # divided by nb of years to obtain an annual average fishing intensity
 plot(rstr, xlim=c(-5,13), ylim=c(53,60))
 #a           <- area(rstr)
 #rstr_std    <- rstr/a
 # equivalent to:
 # tacsatSweptArea$cell_area <- (cos(tacsatSweptArea$CELL_LATI *pi/180) * 111.325 )/60  * (111/60)
 # tacsatSweptArea[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] <-
 #                  tacsatSweptArea[,c("SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] /  tacsatSweptArea$cell_area # standardize

 library(rgdal)
 rstr_eea     <- projectRaster(rstr, crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")  # European EEA projection
 rstr_eea[is.na(rstr_eea)] <- -999  # arbitrary code, to get rid of true 0s in GIS
 rstr_eea[rstr_eea<0.001]  <- -999
 writeRaster(rstr_eea, file.path(outPath,"SurfaceAllGearsAgg2rasterEEA"), format = "GTiff", overwrite=TRUE)

 # sub-surface impact, all gears
 r           <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], res=raster_res, crs=CRS("+proj=longlat +datum=WGS84"))
 some_coords <- SpatialPoints(cbind(SI_LONG=aggResult2_Subsurface$CELL_LONG, SI_LATI=aggResult2_Subsurface$CELL_LATI))
 rstr        <- rasterize(x=some_coords, y=r, field=aggResult2_Subsurface$SWEPT_AREA_KM2 /length(years), fun="sum")  # divided by nb of years to obtain an annual average intensity
 plot(rstr, xlim=c(-5,13), ylim=c(53,60))
 library(rgdal)
 rstr_eea     <- projectRaster(rstr, crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")  # European EEA projection
 rstr_eea[is.na(rstr_eea)] <- -999  # arbitrary code, to get rid of true 0s in GIS
 rstr_eea[rstr_eea<0.001]  <- -999
 writeRaster(rstr_eea, file.path(outPath,"SubSurfaceAllGearsAgg2rasterEEA"), format = "GTiff", overwrite=TRUE)

 # surface impact, otter trawl only
 r           <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], res=raster_res, crs=CRS("+proj=longlat +datum=WGS84"))
 some_coords <- SpatialPoints(cbind(SI_LONG=aggResult2_Otter$CELL_LONG, SI_LATI=aggResult2_Otter$CELL_LATI))
 rstr        <- rasterize(x=some_coords, y=r, field=aggResult2_Otter$SWEPT_AREA_KM2 /length(years), fun="sum")
 plot(rstr, xlim=c(-5,13), ylim=c(53,60))
 proj4string(rstr) <- CRS("+proj=longlat +ellps=WGS84")
 library(rgdal)
 rstr_eea     <- projectRaster(rstr, crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
 rstr_eea[is.na(rstr_eea)] <- -999
 rstr_eea[rstr_eea<0.001]  <- -999
 writeRaster(rstr_eea, file.path(outPath,"SurfaceOtterTAgg2RasterEEA"), format = "GTiff", overwrite=TRUE)

 # surface impact, for seine
 r           <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], res=raster_res, crs=CRS("+proj=longlat +datum=WGS84"))
 some_coords <- SpatialPoints(cbind(SI_LONG=aggResult2_Seine$CELL_LONG, SI_LATI=aggResult2_Seine$CELL_LATI))
 rstr        <- rasterize(x=some_coords, y=r, field=aggResult2_Seine$SWEPT_AREA_KM2 /length(years), fun="sum")
 plot(rstr, xlim=c(-5,13), ylim=c(53,60))
 proj4string(rstr) <- CRS("+proj=longlat +ellps=WGS84")
 library(rgdal)
 rstr_eea     <- projectRaster(rstr, crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
 rstr_eea[is.na(rstr_eea)] <- -999
 rstr_eea[rstr_eea<0.001]  <- -999
 writeRaster(rstr_eea, file.path(outPath,"SurfaceSeineAgg2RasterEEA"), format = "GTiff", overwrite=TRUE)

 # surface impact,  beam trawl
 r           <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], res=raster_res, crs=CRS("+proj=longlat +datum=WGS84"))
 some_coords <- SpatialPoints(cbind(SI_LONG=aggResult2_Beam$CELL_LONG, SI_LATI=aggResult2_Beam$CELL_LATI))
 rstr        <- rasterize(x=some_coords, y=r, field=aggResult2_Beam$SWEPT_AREA_KM2, fun="sum")
 plot(rstr, xlim=c(-5,13), ylim=c(53,60))
 proj4string(rstr) <- CRS("+proj=longlat +ellps=WGS84")
 library(rgdal)
 rstr_eea     <- projectRaster(rstr, crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
 rstr_eea[is.na(rstr_eea)] <- -999
 rstr_eea[rstr_eea<0.001]  <- -999
 writeRaster(rstr_eea, file.path(outPath,"SurfaceBeamTAgg2RasterEEA"), format = "GTiff", overwrite=TRUE)

 # surface impact, for dredge
 r           <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], res=raster_res, crs=CRS("+proj=longlat +datum=WGS84"))
 some_coords <- SpatialPoints(cbind(SI_LONG=aggResult2_Dredge$CELL_LONG, SI_LATI=aggResult2_Dredge$CELL_LATI))
 rstr        <- rasterize(x=some_coords, y=r, field=aggResult2_Dredge$SWEPT_AREA_KM2 /length(years), fun="sum")
 plot(rstr, xlim=c(-5,13), ylim=c(53,60))
 proj4string(rstr) <- CRS("+proj=longlat +ellps=WGS84")
 library(rgdal)
 rstr_eea     <- projectRaster(rstr, crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
 rstr_eea[is.na(rstr_eea)] <- -999
 rstr_eea[rstr_eea<0.001]  <- -999
 writeRaster(rstr_eea, file.path(outPath,"SurfaceDredgeAgg2RasterEEA"), format = "GTiff",  overwrite=TRUE)
 ##----------------------------------------------------------------------------
 ##----------------------------------------------------------------------------






