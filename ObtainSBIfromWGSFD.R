
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##
 ## SBI calculation and maps from WGSFD 2016
 ## Author: Niels Hintzen (IMARES), Francois Bastardie (DTU-Aqua)
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##


library(vmstools)
library(rgdal)
library(rgeos)
library(RColorBrewer)



 # GENERAL SETTINGS---------------
 myPath         <- file.path("C:", "Users", "fbas", "Documents", "GitHub")
 dataPath       <- file.path(myPath, "FisheriesImpactTool", "Data")
 outPath        <- file.path(myPath, "FisheriesImpactTool", "Outputs")
 polPath        <- file.path(myPath, "FisheriesImpactTool", "Shapes")
 nbyears        <- 1
 #--------------------------------



#-------------------------------------------------------------------------------
# 1) Data preparation
#-------------------------------------------------------------------------------


#- Load habitats
pol       <- readShapePoly(file.path(polPath,"EUNIS_codes_Combined_ICES_FAO9_clipped"))

#- Load surface/subsurface data
dat       <- read.csv(file.path(dataPath,"19052016_VMS_FishingCategory_Baltic2015.csv"), stringsAsFactors=FALSE, header=TRUE, sep=",")
colnames(dat)[c(2,3)] <- c("year","NoRecs")
keepCols  <- c("year","totweight","totvalue","SubSurFaceSweptArea","area","c_square","Latitude","Longitude")
dat       <- dat[,keepCols]

#- Get unique csquares
uniquedat <- dat[!duplicated(dat$c_square),c("c_square","Latitude","Longitude")]
coords    <- SpatialPoints(cbind(SI_LONG=uniquedat$Longitude,SI_LATI=uniquedat$Latitude))

#- Overlay polygon so we get the habitat and FAO area
inPol                         <- numeric(length(coords))
steps                         <- seq(1,length(coords)+1000,10000)
stepsList                     <- lapply(as.list(1:(length(steps)-1)),function(x){return(seq(steps[x],steps[x+1]-1))})
stepsList[[length(steps)-1]]  <- steps[(length(steps)-1)]:length(coords)
polSP                         <- as(pol,"SpatialPolygons")
for(i in 1:length(stepsList))
  inPol[stepsList[[i]]]       <- over(coords[stepsList[[i]]],polSP)
idx                           <- inPol

#- Assign EUNIS habitat to data
uniquedat$EUNIS <- pol@data$EUNIS_code[idx]
dat             <- merge(dat,uniquedat,by=c("c_square","Latitude","Longitude"))
dat$EUNIS       <- ac(dat$EUNIS)
dat$totweight   <- an(dat$totweight)
dat$totvalue    <- an(dat$totvalue)

save(dat, file=file.path(dataPath, "overlaidEUNISVMSData.RData"))

#-------------------------------------------------------------------------------
#- 2) Aggregation of the data
#-------------------------------------------------------------------------------

datsum                  <- aggregate(dat[,c("totweight","totvalue","SubSurFaceSweptArea")],by=as.list(dat[,c("c_square","year")]),FUN=sum,na.rm=T)
datmean                 <- aggregate(datsum[,c("totweight","totvalue","SubSurFaceSweptArea")],by=list(datsum$c_square),FUN=sum,na.rm=T)
colnames(datmean)[1]    <- "c_square"
datmean                 <- merge(datmean,dat[!duplicated(dat$c_square),c("c_square","area","EUNIS")],by="c_square")
datmean$intensity       <- (datmean$SubSurFaceSweptArea / nbyears) / datmean$area  # annual average

datintlow               <- aggregate(datsum[,c("totweight","totvalue","SubSurFaceSweptArea")],by=list(datsum$c_square),FUN=quantile,probs=0.025,na.rm=T)
colnames(datintlow)[1:4]<- c("c_square","totweightlow","totvaluelow","SubSurFaceSweptArea")
datintlow               <- merge(datintlow,dat[!duplicated(dat$c_square),c("c_square","area","EUNIS")],by="c_square")
datintlow$intensitylow  <- datintlow$SubSurFaceSweptArea / datintlow$area
datinthigh              <- aggregate(datsum[,c("totweight","totvalue","SubSurFaceSweptArea")],by=list(datsum$c_square),FUN=quantile,probs=0.975,na.rm=T)
colnames(datinthigh)[1:4]<- c("c_square","totweighthigh","totvaluehigh","SubSurFaceSweptArea")
datinthigh              <- merge(datinthigh,dat[!duplicated(dat$c_square),c("c_square","area","EUNIS")],by="c_square")
datinthigh$intensityhigh<- datinthigh$SubSurFaceSweptArea / datinthigh$area
datint                  <- merge(datintlow,datinthigh,by=c("c_square","area","EUNIS"))

#-------------------------------------------------------------------------------
#- 3) Calculation of SI for mean and intenstiy high and low
#-------------------------------------------------------------------------------
cumbio                <- as.data.frame(rbind( c("A5.1",-4.43714,2.81834,0.05268523),
                                              c("A5.2",-5.81818,3.81229,0.05268523),
                                              c("A5.3",-5.43069,3.92402,0.05268523),
                                              c("A5.4",-3.95437,2.86272,0.05268523)),stringsAsFactors=F)
colnames(cumbio)      <- c("EUNIS","alfa","beta.ll","beta.freq")
cumbio$alfa           <- an(cumbio$alfa)
cumbio$beta.ll        <- an(cumbio$beta.ll)
cumbio$beta.freq      <- an(cumbio$beta.freq)

#- First for average
intensitymatch        <- subset(datmean,EUNIS %in% c("A5.1","A5.2","A5.3","A5.4"))
totdatmatch           <- merge(intensitymatch,cumbio,by="EUNIS",all.x=TRUE)
intensitylessfive     <- subset(datmean,EUNIS %in% c("A3.1","A3.2","A3.3","A3.5","A4.1","A4.2","A4.3","A4.5","A4.6","A4.7"))
intensitylessfive$EUNIS <- "A5.1"
totdatless            <- merge(intensitylessfive,cumbio,by="EUNIS",all.x=TRUE)
intensitymorefive     <- subset(datmean,EUNIS %in% c("A5.5","A5.6","A6.1","A6.2","A6.3","A6.4","A6.5"))
intensitymorefive$EUNIS <- "A5.3"
totdatmore            <- merge(intensitymorefive,cumbio,by="EUNIS",all.x=TRUE)
datmean               <- rbind(totdatmatch,totdatless,totdatmore)

datmean$SBI           <- exp(datmean$alfa + datmean$beta.ll*(log(1/(datmean$intensity)))) / (1 + exp( datmean$alfa + datmean$beta.ll*(log(1/(datmean$intensity)))))
datmean$SBI[is.na(datmean$SBI)] <- 1
datmean               <- orderBy(~c_square,data=datmean)

#- Second for 95% CI
intensitymatch        <- subset(datint,EUNIS %in% c("A5.1","A5.2","A5.3","A5.4"))
totdatmatch           <- merge(intensitymatch,cumbio,by="EUNIS",all.x=TRUE)
intensitylessfive     <- subset(datint,EUNIS %in% c("A3.1","A3.2","A3.3","A3.5","A4.1","A4.2","A4.3","A4.5","A4.6","A4.7"))
intensitylessfive$EUNIS <- "A5.1"
totdatless            <- merge(intensitylessfive,cumbio,by="EUNIS",all.x=TRUE)
intensitymorefive     <- subset(datint,EUNIS %in% c("A5.5","A5.6","A6.1","A6.2","A6.3","A6.4","A6.5"))
intensitymorefive$EUNIS <- "A5.3"
totdatmore            <- merge(intensitymorefive,cumbio,by="EUNIS",all.x=TRUE)
datint               <- rbind(totdatmatch,totdatless,totdatmore)

datint$SBIlow           <- exp(datint$alfa + datint$beta.ll*(log(1/(datint$intensitylow)))) / (1 + exp( datint$alfa + datint$beta.ll*(log(1/(datint$intensitylow)))))
datint$SBIlow[is.na(datint$SBIlow)] <- 1
datint$SBIhigh          <- exp(datint$alfa + datint$beta.ll*(log(1/(datint$intensityhigh)))) / (1 + exp( datint$alfa + datint$beta.ll*(log(1/(datint$intensityhigh)))))
datint$SBIhigh[is.na(datint$SBIhigh)] <- 1
datint               <- orderBy(~c_square,data=datint)

#-------------------------------------------------------------------------------
#- 4) Make plots
#-------------------------------------------------------------------------------
datmean               <- merge(datmean,dat[!duplicated(dat$c_square),c("c_square","Latitude","Longitude")],by="c_square")
uniqueCells           <- datmean[!duplicated(datmean$c_square),c("c_square","Longitude","Latitude")]
colnames(uniqueCells) <- c("lgrID","CELL_LONG","CELL_LATI")
resx                  <- 1/20 #5 minute
resy                  <- 1/20 #5 minute
grdc2plot             <- lonLat2SpatialPolygons(lst=lapply(as.list(1:nrow(uniqueCells)),
                                            function(x){
                                              data.frame(SI_LONG=c(uniqueCells[x,"CELL_LONG"]-resx/2,
                                                                   rep(uniqueCells[x,"CELL_LONG"]+resx/2,2),uniqueCells[x,"CELL_LONG"]-resx/2),
                                                         SI_LATI=c(rep(uniqueCells[x,"CELL_LATI"]-resy/2,2),rep(uniqueCells[x,"CELL_LATI"]+resy/2,2)))}))
#the_breaks            <- seq(0,1,length.out=8)
the_breaks            <- c(0,0.5, 0.8, 0.85, 0.9, 0.95, 0.99,1) # consistent with approach SI from Equilibrium biomass (B/K)  
the_colors            <- rev(c('#d73027','#fdae61','#fee090','#abd9e9','#74add1','#4575b4', "white"))   # http://colorbrewer2.org/
the_colors            <- rev(brewer.pal(9,"Greys")[-c(1,9)])
col2plot              <- the_colors[cut(datmean$SBI,breaks=the_breaks)]


#- Mean seafloor index
col2plot              <- the_colors[cut(datmean$SBI,breaks=the_breaks)]
xrange                <- c(9,30)
yrange                <- c(53,65)
plot(1,1,col="white", xlim=xrange, ylim=yrange, asp=1/lonLatRatio(mean(xrange),mean(yrange)),
   xlab="Longitude",ylab="Latitude",las=1)
plot(grdc2plot, add=TRUE, col=col2plot, border=0)
legend(x=9,y=65,fill=c('white',rev(the_colors),"black"),legend=rev(c("<0","<0.5", "<0.8", "<0.85", "<0.9", "<0.95", "<0.99","1")),bg='white',title="Seafloor integrity", box.lty=1, cex=0.8)
savePlot(filename=file.path(outPath, "SBI_All_Area.pdf"),type="pdf")


#- 95% CI seafloor index: Low
col2plot              <- the_colors[cut(datint$SBIlow,breaks=the_breaks)]
xrange                <- c(9,30)
yrange                <- c(53,65)
plot(1,1,col="white", xlim=xrange, ylim=yrange, asp=1/lonLatRatio(mean(xrange),mean(yrange)),
   xlab="Longitude",ylab="Latitude",las=1)
plot(grdc2plot, add=TRUE, col=col2plot, border=0)   # sp:::plot.SpatialPolygons
legend(x=9,y=65,fill=c('white',rev(the_colors),"black"),legend=rev(c("<0","<0.5", "<0.8", "<0.85", "<0.9", "<0.95", "<0.99","1")),bg='white',title="Seafloor integrity", box.lty=1, cex=0.8)
savePlot(filename=file.path(outPath,"SBIlow_All_Area.pdf"),type="pdf")


#- 95% CI seafloor index: high
col2plot              <- the_colors[cut(datint$SBIhigh,breaks=the_breaks)]
xrange                <- c(9,30)
yrange                <- c(53,65)
plot(1,1,col="white", xlim=xrange, ylim=yrange, asp=1/lonLatRatio(mean(xrange),mean(yrange)),
   xlab="Longitude",ylab="Latitude",las=1)
plot(grdc2plot, add=TRUE, col=col2plot, border=0)   # sp:::plot.SpatialPolygons
legend(x=9,y=65,fill=c('white',rev(the_colors),"black"),legend=rev(c("<0","<0.5", "<0.8", "<0.85", "<0.9", "<0.95", "<0.99","1")),bg='white',title="Seafloor integrity", box.lty=1, cex=0.8)
savePlot(filename=file.path(outPath,"SBIhigh_All_Area.pdf"),type="pdf")


#- KGs/Euros vs SI
datplot <- orderBy(~SBI,data=datmean)
datplot$id  <- 1:nrow(datplot)
idx         <- apply(abs(outer(datplot$SBI,seq(0,1,0.1),"-")),2,which.min)[2:10]
idx[c(1,11)]<- c(1,nrow(datplot))
par(oma=c(1,2,0,1))
plot(x=datplot$id,y=cumsum(datplot$totweight)/sum(datplot$totweight)*100,type="l",xlim=c(0,nrow(datplot)),ylim=c(0,100),xlab="Seafloor Integrity",ylab="Cummulative proportional catch / value / swept area",las=1,xaxs="i",yaxs="i",font.lab=2,xaxt="n")
abline(v=idx,lty=2,col="grey")
axis(1,at=idx,labels=seq(0,1,0.1))
mtext(side=2,text="",at=0.5,outer=T,line=1,font=2)
lines(x=datplot$id,y=cumsum(datplot$totweight)/sum(datplot$totweight)*100)
lines(x=datplot$id,y=cumsum(datplot$SubSurFaceSweptArea)/sum(datplot$SubSurFaceSweptArea)*100,col="blue")
lines(x=datplot$id,y=cumsum(datplot$totvalue)/sum(datplot$totvalue)*100,type="l",col=2)
box()
legend("bottomright",lwd=2,lty=1,col=c(1,2,4),legend=c("Total weigth","Total value","Swept area"),box.lty=0)
savePlot(filename=file.path(outPath, "SBI_All_Area_CumulPlot.pdf"),type="pdf")

