

 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##
 ## Overlay with shapefile (e.g. EUNIS habitats) or raster file (e.g. BALANCE habitat)
 ## Author: Francois Bastardie (DTU-Aqua)
 
 ## citations:
 ## Bastardie, F., Nielsen, J. R., Ulrich, C., Egekvist, J., and Degel, H. 2010.
 ## Detailed mapping of fishing effort and landings by coupling fishing logbooks with satellite-recorded vessel geo-location. Fisheries Research, 106: 41–53

 ## Hintzen, N. T., Bastardie, F., Beare, D., Piet, G. J., Ulrich, C., Deporte, N., Egekvist, J., et al. 2012. VMStools: Open-source software for the processing, analysis and visualization of fisheries logbook and VMS data. Fisheries Research, 115–116: 31–43.
 
 ## Eigaard O.R., Bastardie F., Hintzen N., Buhl-Mortensen L., Buhl-Mortensen P., Catarino R., Dinesen G.E., et al., 2016b. Benthic impact of fisheries in European waters: the distribution and intensity of bottom trawling. ICES Journal of Marine Science, in press.
 
 ## For BALANCE map:
 #Al-Hamdani, Z. and Reker, J. (editors), Alanen. U., Andersen, J.H., Bendtsen, J., Bergstrm, U., Dahl, K., Dinesen, G., Erichsen, A., Elhammer, A., Geitner, K., Hermansen, B., Holmberg, J., Kotilainen, A., Leth, J., Lindberg, T., Lindeberg, G., Nyberg, J., Reijonen, A., Ryabchuk, D., Sparrevohn, C., Sørensen, T., Söderkvist, J., Thougaard, L., Vestergaard, O., Zetterlund, S., 2007. Towards marine landscapes in the Baltic Sea. BALANCE Interim Report No 10.  
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##


 # GENERAL SETTINGS---------------
 myPath         <- file.path("C:", "Users", "fbas", "Documents", "GitHub")
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




  #--------------------------
  # SHAPEFILE EXAMPLE--------
  #--------------------------

  # combine with (Eunis) Habitat
  pol                 <- readShapePoly(file.path(shapePath, "EUNIS_codes_Combined_ICES_FAO9_clipped"))
  library(doBy)
  aggResult           <- orderBy(~grID,data=aggResult)
  uniqueCoords        <- aggResult[,c("CELL_LONG","CELL_LATI")]
  coords              <- SpatialPoints(data.frame(SI_LONG=uniqueCoords$CELL_LONG,SI_LATI=uniqueCoords$CELL_LATI))
  idx                 <- over(coords,as(pol,"SpatialPolygons"))
  aggResult$EUNIS     <- pol@data$EUNIS_code[idx]
  aggResult$FAO       <- pol@data$ICES_FAO[idx]


  #-------------------------
  # RASTER EXAMPLE----------
  #-------------------------

  ## use point-raster overlay.......
  library(raster)
  landscapes              <- raster(file.path(polPath, "BALANCElandscapes.tif"))  # first, you´ll need to unzip the file in \Shapes.
  newproj                 <- "+proj=longlat +datum=WGS84"
  landscapes_proj         <- projectRaster(landscapes, crs=newproj)

  anf                     <- function(x) as.numeric(as.character(x))
  coord                   <- cbind(x=anf(aggResult[,'CELL_LONG']), y=anf(aggResult[,'CELL_LATI']))
  dd                      <- extract (landscapes_proj, coord[,1:2]) # get the landscape on the coord points!
  coord                   <- cbind(coord,  landscapes_code=cut(dd, breaks=c(0,100,200,300,400,500,600)))
  aggResult               <- cbind(aggResult, landscapes_code= coord[,'landscapes_code'])


