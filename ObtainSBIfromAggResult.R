
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##
 ## Obtain the SeaBed Impact index from the longevity BENTHIS approach
 ## Author:  Niels Hintzen (IMARES), Francois Bastardie (DTU-Aqua)

 ## citations:
 ## Bastardie, F., Nielsen, J. R., Ulrich, C., Egekvist, J., and Degel, H. 2010.
 ## Detailed mapping of fishing effort and landings by coupling fishing logbooks with satellite-recorded vessel geo-location. Fisheries Research, 106: 41–53

 ## Hintzen, N. T., Bastardie, F., Beare, D., Piet, G. J., Ulrich, C., Deporte, N., Egekvist, J., et al. 2012. VMStools: Open-source software for the processing, analysis and visualization of fisheries logbook and VMS data. Fisheries Research, 115–116: 31–43.

  ## Rijnsdorp, A. D., Bastardie, F., Bolam, S. G., Buhl-Mortensen L., Eigaard O.R., Hamon K.G., Hiddink, J.G., et al. 2016. Towards a framework for the quantitative assessment of trawling impacts on the seabed and benthic ecosystem. ICES Journal of Marine Science (2016), 73(Supplement 1), i127–i138. doi:10.1093/icesjms/fsv207
  
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##


 # GENERAL SETTINGS---------------
 myPath         <- file.path("C:", "Users", "fbas", "Documents", "GitHub")
 dataPath       <- file.path(myPath, "FisheriesImpactTool", "Data")
 outPath        <- file.path(myPath, "FisheriesImpactTool", "Outputs")
 shapePath      <- file.path(myPath, "FisheriesImpactTool", "Shapes")
 dir.create(outPath)
 nameAggFile  <- "aggResultTot2_aug2016.RData"
 load(file=file.path(inPath,nameAggFile)) # get aggResult from e.g. the WP2 BENTHIS workflow


 # subset for a year period
 years     <- 2010:2012
 lat_range <- c(53,60)
 lon_range <- c(-5,13)
 raster_res<- c(0.0167,0.0167) # 1 by 1 minute
 #--------------------------------




aggResultTot2 <- aggResultTot2[!is.na(aggResultTot2$FAO),]


aggTot    <- aggregate(aggResultTot2[,c(grep("SURF_SWEPT_AREA",colnames(aggResultTot2)),9,10)],by=list(aggResultTot2[,"lgrID"]),FUN=sum,na.rm=T)
colnames(aggTot)[1] <- "lgrID"
uniqueTot <- aggResultTot2[!duplicated(aggResultTot2$lgrID),c("lgrID","CELL_LONG","CELL_LATI","surf","EUNIS","FAO")]
aggResult <- merge(aggTot,uniqueTot,by="lgrID",all=T)
aggResult[,c(grep("SURF_SWEPT_AREA",colnames(aggResult)))] <- aggResult[,c(grep("SURF_SWEPT_AREA",colnames(aggResult)))]/100

#data from V. Denderen et al. (caution: estimates obtained from fitting on North Sea data...best available data so far):
cumbio    <- as.data.frame(rbind( c("A5.1",-4.774150,2.645160,0.05268523),
                                  c("A5.2",-7.685695,4.424618,0.05268523),
                                  c("A5.3",-5.728134,4.151049,0.05268523),
                                  c("A5.4",-4.110208,2.801233,0.05268523)),stringsAsFactors=F)
colnames(cumbio)  <- c("EUNIS","alfa","beta.ll","beta.freq")
cumbio$alfa       <- an(cumbio$alfa)
cumbio$beta.ll    <- an(cumbio$beta.ll)
cumbio$beta.freq  <- an(cumbio$beta.freq)

intensitymatch    <- subset(aggResult,EUNIS %in% c("A5.1","A5.2","A5.3","A5.4"))
totdatmatch       <- merge(intensitymatch,cumbio,by="EUNIS",all.x=TRUE)
intensitylessfive <- subset(aggResult,EUNIS %in% c("A3.1","A3.2","A3.3","A3.5","A4.1","A4.2","A4.3","A4.5","A4.6","A4.7"))
intensitylessfive$EUNIS <- "A5.1"
totdatless        <- merge(intensitylessfive,cumbio,by="EUNIS",all.x=TRUE)
intensitymorefive <- subset(aggResult,EUNIS %in% c("A5.5","A5.6","A6.1","A6.2","A6.3","A6.4","A6.5"))
intensitymorefive$EUNIS <- "A5.3"
totdatmore        <- merge(intensitymorefive,cumbio,by="EUNIS",all.x=TRUE)
totdat            <- rbind(totdatmatch,totdatless,totdatmore)


totdat$SBI        <- exp(totdat$alfa + totdat$beta.ll*(log(1/(totdat$SUBSURF_SWEPT_AREA_KM2/totdat$surf.x)))) / (1 + exp( totdat$alfa + totdat$beta.ll*(log(1/(totdat$SUBSURF_SWEPT_AREA_KM2/totdat$surf.x)))))
totdat$SBI[is.na(totdat$SBI)] <- 1


# rasterize before loading in GIS
# North
library(raster)
xrange <- c(-20,15)
yrange <- c(48,66)
r           <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], res=raster_res, crs=CRS("+proj=longlat +datum=WGS84"))
some_coords <- SpatialPoints(cbind(SI_LONG=totdat$CELL_LONG, SI_LATI=totdat$CELL_LATI.y))
rstr        <- rasterize(x=some_coords, y=r, field=totdat$SBI, fun="last") 
plot(rstr, xlim=xrange, ylime=yrange)

#rstr <- reclassify(rstr, c(-Inf,0.17,0.17,  0.33,0.5,0.5,   0.5,0.67,0.67,   0.67,0.83,0.83,    0.83,1,1 ))
#legend(x=25,y=51,fill=c('white',rev(the_colors),"black"),legend=rev(c("0","<0.17","<0.33","<0.5","<0.67","<0.83","<1","1")),bg='white',title="Seafloor integrity",box.lty=1)
#plot(rstr, xlim=c(-5,13), ylime=c(53,60))

writeRaster(rstr, file.path(outPath, paste("SBI_NorthAtlantic_V3", sep='')), format = "GTiff", overwrite=TRUE)


# Med
library(raster)
xrange                <- c(5,30);
yrange                <- c(33,48);
r           <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], res=raster_res, crs=CRS("+proj=longlat +datum=WGS84"))
some_coords <- SpatialPoints(cbind(SI_LONG=totdat$CELL_LONG, SI_LATI=totdat$CELL_LATI.y))
rstr        <- rasterize(x=some_coords, y=r, field=totdat$SBI, fun="last") 
plot(rstr, xlim=xrange, ylim=yrange)

#rstr <- reclassify(rstr, c(-Inf,0.17,0.17,  0.33,0.5,0.5,   0.5,0.67,0.67,   0.67,0.83,0.83,    0.83,1,1 ))
#legend(x=25,y=51,fill=c('white',rev(the_colors),"black"),legend=rev(c("0","<0.17","<0.33","<0.5","<0.67","<0.83","<1","1")),bg='white',title="Seafloor integrity",box.lty=1)
#plot(rstr, xlim=c(-5,13), ylime=c(53,60))

writeRaster(rstr, file.path(outPath, paste("SBI_MED_V2_inEEZ", sep='')), format = "GTiff", overwrite=TRUE)




