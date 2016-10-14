
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##
 ## Find FP on stations
 ## Author: Francois Bastardie (DTU-Aqua), Niels Hintzen (IMARES)
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##


 # GENERAL SETTINGS---------------
 myPath         <- file.path("C:", "Users", "fbas", "Documents", "GitHub")
 dataPath       <- file.path(myPath, "FisheriesImpactTool", "Data")
 outPath        <- file.path(myPath, "FisheriesImpactTool", "Outputs")
 nameAggFile    <- "ALL_AggregatedSweptArea_12062015.RData"
 load(file=file.path(dataPath, nameAggFile)) # get aggResult
#--------------------------------



    # great circle distance
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
 stations            <- read.table(file=file.path(dataPath, 'some_stations.csv'), sep=',', dec=".", header=TRUE)
 stations$Latitude   <- as.numeric(as.character(stations[,'Latitude']))
 stations$Longitude  <- as.numeric(as.character(stations[,'Longitude']))

 stations$grID     <- rep(NA, nrow(stations))
 stations$distance <- rep(NA, nrow(stations))

 stations_and_pressure <- list()
 for(year in c(2010:2012)){
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

 save(stations_and_pressure, file=file.path(outPath, "some_stations.RData"))

 write.table(stations_and_pressure, file=file.path(outPath, "some_stations.txt"), row.names=FALSE, col.names=TRUE, sep=";", quote=FALSE)



  #- Plot all of it FOR CHECKING
 xrange <-  range(stations_and_pressure$Longitude)
 yrange <-  range(stations_and_pressure$Latitude)
 aggResult <- aggResult[aggResult$CELL_LONG >xrange[1] &
                        aggResult$CELL_LONG <xrange[2] &
                        aggResult$CELL_LATI >yrange[1] &
                        aggResult$CELL_LATI <yrange[2],
                        ]

 library(RColorBrewer)
 colintens             <- brewer.pal(9,"YlOrRd")
 #- Create polygon set of gridcells to plot (takes a bit longer)
 uniqueCells           <- aggResult[!duplicated(aggResult$grID),c("grID","CELL_LONG","CELL_LATI")]
 resx                  <- 1/60 #1 minute
 resy                  <- 1/60 #1 minute
 grdc2plot             <- lonLat2SpatialPolygons(lst=lapply(as.list(1:nrow(uniqueCells)),
                                                function(x){
                                                  data.frame(SI_LONG=c(uniqueCells[x,"CELL_LONG"]-resx/2,
                                                                       rep(uniqueCells[x,"CELL_LONG"]+resx/2,2),uniqueCells[x,"CELL_LONG"]-resx/2),
                                                             SI_LATI=c(rep(uniqueCells[x,"CELL_LATI"]-resy/2,2),rep(uniqueCells[x,"CELL_LATI"]+resy/2,2)))}))
 # a subset
 subAggResult          <- aggResult [aggResult$LE_MET=="OT_DMF", ]
 data2plot             <- aggregate(subAggResult[,c("SWEPT_AREA_KM2",
                                                   "SWEPT_AREA_KM2_LOWER",
                                                   "SWEPT_AREA_KM2_UPPER")],by=list(subAggResult$grID),FUN=sum,na.rm=T)

 colnames(data2plot)[1]<- "grID"
 idx                   <- match(uniqueCells$grID, data2plot$grID)
 col2plot              <- c("white",colintens)[cut(data2plot[idx,"SWEPT_AREA_KM2"],breaks=c(-1,0,0.25,2,4,8,16,32, 10000))]

 plot(1,1,col="white", xlim=xrange, ylim=yrange, asp=1/lonLatRatio(mean(xrange),mean(yrange)),
     xlab="Longitude",ylab="Latitude",las=1)
 plot(grdc2plot,add=TRUE,col=col2plot, border=0)   # sp:::plot.SpatialPolygons
 map("worldHires",  add=TRUE, col=rgb(0,0,0), fill=TRUE)


 stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF",]



 # plot to see which stations are acceptable given the data so far
 # points(stations$Longitude, stations$Latitude,  pch="+", col="red")
 points(stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF",]$Longitude,   stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF",]$Latitude, pch="+", col="green")
 the_colors          <- cut(stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF",]$distance, breaks=c(seq(0, 6, by=1),1000))
 ramp                <- colorRampPalette(c('blue','red'))
 levels (the_colors) <- ramp(7)
 points(stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF",]$Longitude,   stations_and_pressure[stations_and_pressure$LE_MET=="OT_DMF",]$Latitude, pch="+", col=as.character(the_colors))
 library(mapdata)
 map("worldHires",  add=TRUE, col=rgb(0,0,0), fill=TRUE)
 points(stations_and_pressure[stations_and_pressure$distance>2,]$Long, stations_and_pressure[stations_and_pressure$distance>2,]$Lat, pch="+", col="red")



