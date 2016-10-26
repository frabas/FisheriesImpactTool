
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##
 ## Find FP on stations
 ## Author: Francois Bastardie (DTU-Aqua)
 
 ## citations:
 ## Bastardie, F., Nielsen, J. R., Ulrich, C., Egekvist, J., and Degel, H. 2010.
 ## Detailed mapping of fishing effort and landings by coupling fishing logbooks with satellite-recorded vessel geo-location. Fisheries Research, 106: 41–53

 ## Hintzen, N. T., Bastardie, F., Beare, D., Piet, G. J., Ulrich, C., Deporte, N., Egekvist, J., et al. 2012. VMStools: Open-source software for the processing, analysis and visualization of fisheries logbook and VMS data. Fisheries Research, 115–116: 31–43.

  
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##

 # GENERAL SETTINGS---------------
 myPath         <- file.path("C:", "Users", "fbas", "Documents", "GitHub")
 dataPath       <- file.path(myPath, "FisheriesImpactTool", "Data")
 outPath        <- file.path(myPath, "FisheriesImpactTool", "Outputs")
 nameAggFile    <- "ALL_AggregatedSweptArea_12062015.RData"
 load(file=file.path(dataPath, nameAggFile)) # get aggResult
 FP             <- "SWEPT_AREA_KM2"
 years          <- c(2010:2012)
 utm_zone       <- 34
 is_continuous_FP <- TRUE;  is_gridded_FP <- FALSE # make your choice
 #--------------------------------

 
 
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##
 # load stations...
 stations            <- read.table(file=file.path(dataPath, 'some_stations.csv'), sep=',', dec=".", header=TRUE)
 
 
   
  # convert to UTM...
  library(sp)
  library(rgdal)
  SP       <- SpatialPoints(cbind(as.numeric(as.character(stations[,'Longitude'])), as.numeric(as.character(stations[,'Latitude']))),
                       proj4string=CRS("+proj=longlat +ellps=WGS84"))
  stations <- cbind.data.frame(stations,
                 spTransform(SP, CRS(paste("+proj=utm +zone=",utm_zone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep=''))))    # convert to UTM

 
 
 
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##
if(is_continuous_FP){
    # ...then sum up the pressure within a circle centered on the station locality 
  
  
  # for this, turn the stations into spatial polygons
  lst                   <- list()
  stations_and_pressure <- NULL
  a_dist                <- 1500 # in meters # TO BE CHOSEN...if 2000m then area of polygon is (2^2)*pi= 12.566 km2
  
  
  # do a circle 
  for(i in 1:nrow(stations)){
      x <- stations$coords.x1[i] + a_dist * cos(seq(1,360, by=5)*pi/180)
      y <- stations$coords.x2[i] + a_dist * sin(seq(1,360, by=5)*pi/180)
      assign(paste("station", i, sep=""), 
        Polygon(cbind(  c(x,x[1]),
                   c(y, y[1])
                    )))
      assign  (paste("St", i, sep=""),  Polygons(list(get(paste("station", i, sep=""))), ID=paste(stations$Station[i])) )
      lst[[i]] <- get(paste("St", i, sep=""))
  }            
  
     
  # add area of the station info
  stations$area_m2  <- unlist(lapply(lst, function(x) x@area)) # in squared meter 
  stations$area_km2 <- stations$area_m2 /1e6
  
  # subset for some years 
  aggResult <- aggResult[aggResult$Year %in% years,]
 
  # subset for a given fishery 
  #aggResult <- aggResult[aggResult$LE_MET %in% c("OT_SPF"),]
 
  sauv           <- aggResult
     
  sp             <- SpatialPolygons(lst, 1:nrow(stations))
  plot(sp) #=> OVERLAPPED! the sp over() cannot assign a unique polygon to points because points could lie within several polygons....   
 #then replace by a loop because the stations/samplings are overlapping:
 
 
 
 
 ####
  for (i in 1:nrow(stations)) { # (inelegant) loop over stations/samplings
     cat(paste(round(i/nrow(stations)*100), "% \n"))
     cat(paste(i, " ", stations$Station[i], " \n"))
     sp             <- SpatialPolygons(lst[i])
 
     aggResult <- sauv
 
  
     projection(sp) <- CRS(paste("+proj=utm +zone=",utm_zone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='') )

     # transform back to decimal longlat
     sp            <- spTransform(sp, CRS("+proj=longlat +ellps=WGS84"))
                      

     # check
     check <- FALSE
     if(check){
     points(stations[,'Long'], stations[,'Lat'], pch="+", cex=2, col="green")
     plot(sp, add=TRUE, col = 1:3, pbg="white")
     axis(1)
     axis(2)
     }
 
   
  
     coord <-  aggResult[, c('CELL_LONG', 'CELL_LATI')]
     names(sp) # return the name of the coding variable


     # turn the point into a spatial point
     spo                           <- SpatialPoints(coordinates(data.frame(CELL_LONG=coord[,1],
                                             CELL_LATI=coord[,2])))
     projection(spo)               <-  CRS("+proj=longlat +ellps=WGS84")

     # use the magic 'over' function to see in which polygon it is located
     idx                           <- over(spo,sp); #print(idx)
     aggResult$STATION       <- names(sp)[idx]

     aggResult[!is.na(aggResult$STATION),]

     aggResult$color         <- as.factor(aggResult$STATION)
     levels(aggResult$color) <- 1: length(levels(aggResult$color))
     aggResult$color         <- as.numeric(as.character(aggResult$color))
 
     aggResult[!is.na(aggResult$color),]
 
     # plot
     check <- FALSE
     if(check){
     plot(sp,xlim=c(10,13),ylim=c(54,58), border=grey(0.5), lty=2)
     axis(1) ; axis(2, las=2) ; box()
     points(aggResult[, c("CELL_LONG","CELL_LATI")], col=grey(0.9), pch=".")
     points(aggResult[, c("CELL_LONG","CELL_LATI")], col=aggResult$color, pch=".")
     text(aggResult[!is.na(aggResult$STATION) & aggResult$STATION==stations[i, "station.name"], c("CELL_LONG","CELL_LATI")][1,], labels=stations[i, "station.name"])
     # etc.
     plot(sp, add=TRUE,  border=grey(0.5), lty=2)
     box()
     }

    # then, kept only the points in the boxes and aggregate
    aggResult_stations_and_pressure                    <-  aggResult[!is.na(aggResult$STATION),]
    if(nrow(aggResult_stations_and_pressure)>0){
        aggResult_stations_and_pressure             <- aggregate(data.frame(aggResult_stations_and_pressure[, FP]),
                                                              list(aggResult_stations_and_pressure$Year, 
                                                                   aggResult_stations_and_pressure$STATION)
                                                                   , sum, na.rm=TRUE)
        colnames(aggResult_stations_and_pressure)   <- c('Year',"Station", FP)
        stations_and_pressure                             <- rbind.data.frame(stations_and_pressure, aggResult_stations_and_pressure)
    }
    
  } # end (inelegant) loop over stations


  # merge with info on localities 
  stations_and_pressure                    <- merge(stations_and_pressure, stations, by.x="Station",  by.y="Station")
  
  # standardize
  stations_and_pressure$fishing_pressure   <- stations_and_pressure[, FP]/ stations_and_pressure$area_km2

   
  # export
  write.table(stations_and_pressure, file=file.path(outPath, "stations_and_pressure.txt"), row.names=FALSE, col.names=TRUE, sep=";", quote=FALSE)

 }




 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##
 if(is_gridded_FP){
    # ...then find the nearest cell grid center and link the station to it 
    
    # utils - great circle distance
  `distance` <- function(lon,lat,lonRef,latRef){
                    x1 <- lon
                    y1 <- lat
                    x2 <- lonRef
                    y2 <- latRef

                    pd <- pi/180

                    a1<- sin(((y2-y1)*pd)/2)
                    a2<- cos(y1*pd)
                    a3<- cos(y2*pd)
                    a4<- sin(((x2-x1)*pd)/2)
                    a <- a1*a1+a2*a3*a4*a4

                                      c <- 2*atan2(sqrt(a),sqrt(1-a));
                                      R <- 6371;
                                      dx1 <- R*c
                    return(dx1)}





 # load some stations locations
 stations$grID     <- rep(NA, nrow(stations))
 stations$distance <- rep(NA, nrow(stations))

 stations_and_pressure <- list()
 for(year in years){
    print(year)
    aggResultThisYear <- aggResult[aggResult$Year==year,]
     for(metier in unique(aggResultThisYear$LE_MET)){
      print(metier)
      aggResultThisYearThisMet <- aggResultThisYear[aggResultThisYear$LE_MET==metier,]
      for (iStat in 1: nrow(stations)){  # brute force
         cat(paste(iStat, "\n"))
         latRef <- as.numeric(as.character(stations[iStat,'Latitude']))
         lonRef <- as.numeric(as.character(stations[iStat,'Longitude']))
         dists  <- distance (aggResultThisYearThisMet$CELL_LONG, aggResultThisYearThisMet$CELL_LATI, lonRef, latRef)
         stations[iStat,"grID"]     <-  aggResultThisYearThisMet[which.min(dists), "grID"]
         stations[iStat,"distance"] <-  dists[which.min(dists)]
         }
     stations_and_pressure[[paste(year,"_",metier,sep='')]] <- merge(stations, aggResultThisYearThisMet)
     }
 }
 sauv <-  stations_and_pressure
 stations_and_pressure  <- do.call("rbind", stations_and_pressure)
 stations_and_pressure  <- orderBy(~ source+stn.code+LE_MET, stations_and_pressure)
 head(stations_and_pressure, 20)

 # export
 # ...but caution: all the points in aggResult got a FP while the distance could be not relevant:
 # so we expect the user to filter out all the distance > 2 (km) before the use of stations_and_pressure!
 save(stations_and_pressure, file=file.path(outPath, "stations_and_pressure.RData"))
 write.table(stations_and_pressure, file=file.path(outPath, "stations_and_pressure.txt"), row.names=FALSE, col.names=TRUE, sep=";", quote=FALSE)



  #- Plot for a check
 if(FALSE){
 xrange <-  range(stations_and_pressure$Longitude)
 yrange <-  range(stations_and_pressure$Latitude)
 plot(1,1,col="white", xlim=xrange, ylim=yrange, asp=1,
     xlab="Longitude",ylab="Latitude",las=1)
 # plot to see which stations are acceptable given the data so far
 # points(stations$Longitude, stations$Latitude,  pch="+", col="red")
 points(stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF",]$Longitude,
        stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF",]$Latitude,
        pch="+", col="green")
 the_colors          <- cut(stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF",]$distance, breaks=c(seq(0, 6, by=1),1000))
 ramp                <- colorRampPalette(c('blue','red'))   # if red then distance to high, meaning the FP estimate is not relevant and the record should be flitered out....
 levels (the_colors) <- ramp(7)
 points(stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF",]$Longitude, 
        stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF",]$Latitude, 
        pch="+", col=as.character(the_colors))
 library(mapdata)
 map("worldHires",  add=TRUE, col=rgb(0,0,0), fill=TRUE)
 points(stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF" & stations_and_pressure$distance>2,]$Longitude, 
        stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF" &stations_and_pressure$distance>2,]$Latitude, pch="+", col="red")
 }
 
 

 }


