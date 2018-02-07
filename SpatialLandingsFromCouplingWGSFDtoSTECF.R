
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##
 ## Coupling STECF annual landing data per ICES rectangle to VMS data
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
 raster_res     <- c(0.08333333,0.08333333) # 5 by 5 minute
 raster_res     <- c(0.0501,0.0501) # 3 by 3 minute (slightly more than the strictly 3x3 input data)
 nameAggFile    <- "ALL_AggregatedSweptArea_12062015.RData"
 load(file=file.path(dataPath, nameAggFile)) # get aggResult
 #--------------------------------

  library(maptools)
  library(maps)
     


#----------------
# WGSFD 2015
#----------------
# get the data
ICESrectangle <- function (dF)
{
    rectChar1n2 <- as.integer(2 * (dF[, "mid_lat"] - 35.5))
    rectChar3 <- ifelse(dF[, "mid_lon"] > -50 & dF[, "mid_lon"] <=
        -40, "A", ifelse(dF[, "mid_lon"] > -40 & dF[, "mid_lon"] <=
        -30, "B", ifelse(dF[, "mid_lon"] > -30 & dF[, "mid_lon"] <=
        -20, "C", ifelse(dF[, "mid_lon"] > -20 & dF[, "mid_lon"] <=
        -10, "D", ifelse(dF[, "mid_lon"] > -10 & dF[, "mid_lon"] <
        0, "E", ifelse(dF[, "mid_lon"] >= 0 & dF[, "mid_lon"] <
        10, "F", ifelse(dF[, "mid_lon"] >= 10 & dF[, "mid_lon"] <
        20, "G", ifelse(dF[, "mid_lon"] >= 20 & dF[, "mid_lon"] <
        30, "H", "I"))))))))
    rectChar4 <- rep(NA, nrow(dF))
    idxlowzero <- which(dF[, "mid_lon"] < 0)
    idxabozero <- which(dF[, "mid_lon"] >= 0)
    if (length(idxlowzero) > 0)
        rectChar4[idxlowzero] <- ceiling(dF[idxlowzero, "mid_lon"]%%10 -
            1 + 10)%%10
    if (length(idxabozero) > 0)
        rectChar4[idxabozero] <- floor(dF[idxabozero, "mid_lon"]%%10)
    rectID <- paste(rectChar1n2, rectChar3, rectChar4, sep = "")
    return(rectID)
}

# HELCOM_effort_year_vms_2009.shp
# C:\BENTHIS\wgsfd\HELCOM_mapping_fishing_intensity_and_effort_data_outputs_2015\Effort

# import
is_all_gears <- FALSE
all_vms      <- NULL
if(is_all_gears){
     all_years    <-  c(2009:2013)
     
     for (a_year in all_years){
        filename  <- paste("HELCOM_effort_year_vms_",a_year ,".shp", sep='')     # ALL GEARS
        vms_path  <- file.path (dataPath, "HELCOM_mapping_fishing_intensity_and_effort_data_outputs_2015", "Effort")
        vms_shp   <- readShapePoly(file.path(vms_path, filename), proj4string=CRS(as.character("+proj=longlat +ellps=WGS84")) )
        vms <- as.data.frame(vms_shp)
        vms$rectangle <- ICESrectangle(vms)
        vms$gear       <- factor("All")
        vms$quarter    <- factor("All")
        vms$vesselsize <- factor("All")
        all_vms <- rbind(all_vms, vms)  
        }
     
      } else{
      
      


    all_years    <-  c(2013:2013)
    all_quarters <-  paste("q", 1:4, sep='')
    all_gears    <-  c("Longlines_VMS", "Midwater_trawl_vms", "MBCG_vms")   # MBCG: Mobile Bottom Contact Gear


for (a_year in all_years){
  for (a_gear in all_gears){
    for (a_quarter in all_quarters){

       filename  <- paste("HELCOM_effort_quarter",a_year,"_",a_gear,"_",a_quarter,".shp", sep="") # PER GEAR, PER QUARTER, A YEAR
       vms_path  <- file.path (dataPath, "HELCOM_mapping_fishing_intensity_and_effort_data_outputs_2015", "Effort")
       vms_shp   <- readShapePoly(file.path(vms_path, filename), proj4string=CRS(as.character("+proj=longlat +ellps=WGS84")) )
       

       # check with plot
       if(TRUE){
        library(maptools)
        library(maps)
        library(mapdata)
        plot(vms_shp, border=FALSE)
        names(vms_shp) # return the name of the coding variable
        #Turn the polygons into spatial polygons
        sp                  <- SpatialPolygons(vms_shp@polygons, proj4string=CRS("+proj=longlat +ellps=WGS84"))
        dd                  <- cut(vms_shp$fishing_ho, breaks=c(-1,10,100,250, 500,1000,5000,10000))
        some_colors         <- dd
        levels(some_colors) <- rev(heat.colors(7))
        plot(sp, border=FALSE, col= as.character(some_colors))
        map("worldHires",add=TRUE, res=0,fill=TRUE,col="darkgreen", xlim=c(5,25), ylim=c(48,62)); map.axes()
        savePlot(file.path(getwd(), "Outputs", paste("HELCOM_effort_quarter",a_year,"_",a_gear,"_",a_quarter,".jpeg", sep="")), type="jpeg")
       }
       

       vms <- as.data.frame(vms_shp)
       vms$rectangle <- ICESrectangle(vms)


      if("geargroup" %in% colnames(vms)){
         vms$gear <- factor(vms$geargroup)
         levels(vms$gear) <- a_gear  
      }
      if("quarter" %in% colnames(vms)){
         vms$quarter    <- factor(paste("q", vms$quarter, sep=""))
         }
     
     
      if(!"geargroup" %in% colnames(vms)){
         vms$gear       <- factor("All")
         }
      if(!"quarter" %in% colnames(vms)){
         vms$quarter    <- factor("All")
         }
      if(!"vesselsize" %in% colnames(vms)){
         vms$vesselsize <- factor("All")
         }

  
  all_vms <- rbind(all_vms, vms)
  
  }}}

} # end else




#----------------
# STECF 2015
#----------------
# You need to go to this site
# https://datacollection.jrc.ec.europa.eu/dd/effort/maps

# This has a table of effort by rectangle and quarter and another of landings by rectangle and quarter.

# The other web page
# https://datacollection.jrc.ec.europa.eu/dd/effort/tables

# These tables only deal with annual information. For landings and discards the STECF FDI chair took the decision not to show discards by quarter as there are concerns raised about the appropriateness of the discards data at such a level of disaggregation even as annual figures.

# Landings_(t)_by_rectangle_and_quarter_data(1).csv
# C:\BENTHIS\STECF

# also look at https://stecf.jrc.ec.europa.eu/dd/effort/graphs-quarter

# CAUTION: You might change the encoding to UTF-8 e.g. in NotePad++ before using the dowmloaded file from JRC!!

stecf_path <- file.path (dataPath, "STECF")

if(is_all_gears){
   # import
   filename   <- "BAL_Landings_t_by_rectangle_and_quarter_data.txt"    # ALL GEARS, PER YEAR


   stecf      <- read.table(file=file.path(stecf_path, filename), header=TRUE, sep=",", dec=".")
   stecf$gear       <- factor("All")
   stecf$quarter    <- factor("All")
   stecf$vesselsize <- factor("All")

} else{
   # import
   filename2  <- "BAL_Landings_t_by_rectangle_and_quarter_data_full.txt"    # PER GEAR, PER VESSEL SIZE, PER QUARTER, PER YEAR

   stecf <- read.table(file=file.path(stecf_path, filename2), header=TRUE, sep=",", dec=".")

   # CAUTION (RELEVEL NEED A CHECK IF YOU ARE CHANGING THE INPUT DATA!):
   stecf$gear       <- factor(stecf$Regulated.gear) # init
   #levels(factor(stecf_full$Regulated.gear)) 
   #  "BEAM"        "DEM_SEINE"   "DREDGE"      "GILL"        "NONE"        "OTTER"       "PEL_SEINE"   "PEL_TRAWL"   "POTS"        "R-BEAM"      "R-DEM_SEINE" "R-GILL"      "R-LONGLINE"  "R-OTTER"     "R-PEL_TRAWL"#  "R-TRAMMEL"   "TRAMMEL"  
  
   # should be consistent with vms data naming:
   levels(stecf$gear) [levels(stecf$gear) %in% c("BEAM", "DEM_SEINE", "DREDGE", "OTTER", "R-BEAM", "R-DEM_SEINE", "R-OTTER")]                            <- "MBCG_vms"
   levels(stecf$gear) [levels(stecf$gear) %in% c("GILL", "NONE", "PEL_SEINE", "PEL_TRAWL", "POTS", "R-GILL", "R-PEL_TRAWL", "R-TRAMMEL", "TRAMMEL" )]    <- "Midwater_trawl_vms"
   levels(stecf$gear) [levels(stecf$gear) %in% c("R-LONGLINE" )]                                                                                         <- "Longlines_VMS"

   
   stecf$quarter            <- factor(stecf$Quarter)        # init
   levels(stecf$quarter)    <- paste ("q", 1:4, sep='')
   stecf$vesselsize         <- factor(stecf$Vessel.length)  # init
   levels(stecf$vesselsize) <- rep("All", length(levels(stecf$vesselsize)))

} # end else

                    

#----------------
# MERGE AND DISPACH LANDINGS ON VMS CELLS
#----------------

# first, remove/aggregate the dimension(s) which differ between vms and stecf data (e.g. stecf$vesselsize and stecf$Country not present in vms)
if(!is_all_gears){
  stecf <- aggregate( stecf$Measure.Values, list(stecf$Year, stecf$Rectangle, stecf$gear, stecf$quarter, stecf$Species, stecf$vesselsize, stecf$Measure.Names), sum, na.rm=TRUE)
 } else{
  stecf <- aggregate( stecf$Values, list(stecf$Year, stecf$Rectangle, stecf$gear, stecf$quarter, stecf$Species, stecf$vesselsize, stecf$Measure), sum, na.rm=TRUE)
 
 }

 colnames(stecf) <- c("year", "rectangle", "gear", "quarter", "species", "vesselsize", "measure", "values")


#...and then do the merging
vms_bt   <- all_vms [all_vms$gear=="MBCG_vms", ]
stecf_bt <- stecf [stecf$gear=="MBCG_vms", ]
head(vms_bt)   # fine info on hours per c_square
head(stecf_bt) # fine info on species landings but per rectangle
stecf_bt$year_rectangle    <- paste(stecf_bt$year, stecf_bt$rectangle, stecf_bt$gear, stecf_bt$quarter, stecf_bt$vesselsize, sep="_")
vms_bt$year_rectangle      <- paste(vms_bt$year, vms_bt$rectangle, vms_bt$gear, vms_bt$quarter, vms_bt$vesselsize, sep="_")
sum_effort              <- aggregate(vms_bt$fishing_ho, list(vms_bt$year_rectangle), sum, na.rm=TRUE)
colnames(sum_effort)    <- c("year_rectangle", "tot_effort")
vmsp_bt                    <- merge(vms_bt, sum_effort, by.x="year_rectangle", by.y="year_rectangle")
vmsp_bt$share_effort       <- vmsp_bt$fishing_ho / vmsp_bt$tot_effort # for dispatching depending on the contribution of that cell to the total effort in that cell
vmspp_bt                    <- merge(vmsp_bt, stecf_bt[,c("year_rectangle", "species", "values", "measure")], by.x="year_rectangle", by.y="year_rectangle")
vmspp_bt$landings_from_cell <- vmspp_bt$values * vmspp_bt$share_effort  # dispatch on cell per species

#...and then do the merging
vms_ll   <- all_vms [all_vms$gear=="Longlines_VMS", ]
stecf_ll <- stecf [stecf$gear=="Longlines_VMS", ]
head(vms_ll)   # fine info on hours per c_square
head(stecf_ll) # fine info on species landings but per rectangle
stecf_ll$year_rectangle    <- paste(stecf_ll$year, stecf_ll$rectangle, stecf_ll$gear, stecf_ll$quarter, stecf_ll$vesselsize, sep="_")
vms_ll$year_rectangle      <- paste(vms_ll$year, vms_ll$rectangle, vms_ll$gear, vms_ll$quarter, vms_ll$vesselsize, sep="_")
sum_effort              <- aggregate(vms_ll$fishing_ho, list(vms_ll$year_rectangle), sum, na.rm=TRUE)
colnames(sum_effort)    <- c("year_rectangle", "tot_effort")
vmsp_ll                    <- merge(vms_ll, sum_effort, by.x="year_rectangle", by.y="year_rectangle")
vmsp_ll$share_effort       <- vmsp_ll$fishing_ho / vmsp_ll$tot_effort # for dispatching depending on the contribution of that cell to the total effort in that cell
vmspp_ll                    <- merge(vmsp_ll, stecf_ll[,c("year_rectangle", "species", "values", "measure")], by.x="year_rectangle", by.y="year_rectangle")
vmspp_ll$landings_from_cell <- vmspp_ll$values * vmspp_ll$share_effort  # dispatch on cell per species


#...and then do the merging
vms_pt   <- all_vms [all_vms$gear=="Midwater_trawl_vms", ]
stecf_pt <- stecf [stecf$gear=="Midwater_trawl_vms", ]
head(vms_pt)   # fine info on hours per c_square
head(stecf_pt) # fine info on species landings but per rectangle
stecf_pt$year_rectangle    <- paste(stecf_pt$year, stecf_pt$rectangle, stecf_pt$gear, stecf_pt$quarter, stecf_pt$vesselsize, sep="_")
vms_pt$year_rectangle      <- paste(vms_pt$year, vms_pt$rectangle, vms_pt$gear, vms_pt$quarter, vms_pt$vesselsize, sep="_")
sum_effort              <- aggregate(vms_pt$fishing_ho, list(vms_pt$year_rectangle), sum, na.rm=TRUE)
colnames(sum_effort)    <- c("year_rectangle", "tot_effort")
vmsp_pt                    <- merge(vms_pt, sum_effort, by.x="year_rectangle", by.y="year_rectangle")
vmsp_pt$share_effort       <- vmsp_pt$fishing_ho / vmsp_pt$tot_effort # for dispatching depending on the contribution of that cept to the total effort in that cept
vmspp_pt                    <- merge(vmsp_pt, stecf_pt[,c("year_rectangle", "species", "values", "measure")], by.x="year_rectangle", by.y="year_rectangle")
vmspp_pt$landings_from_cell <- vmspp_pt$values * vmspp_pt$share_effort  # dispatch on cell per species




# join all
vmspp <- rbind.data.frame(vmspp_bt, vmspp_ll, vmspp_pt)
vmspp <- aggregate( vmspp$landings_from_cell, 
                       list(vmspp$year_rectangle, vmspp$c_square, vmspp$year,  
                       vmspp$mid_lat,  vmspp$mid_lon, vmspp$rectangle, vmspp$species), sum, na.rm=TRUE )
colnames(vmspp) <-  c("year_rectangle","c_square","year","mid_lat","mid_lon","rectangle","species", "landings_from_cell")
vmspp$landings_from_cell <- as.numeric(as.character(vmspp$landings_from_cell))


#----------------
# save
#----------------
if(!is_all_gears){
  write.table(vmspp, file=file.path(file.path(stecf_path, "wgfsd_per_gear_stecf_merged.txt")), row.names=FALSE, col.names=TRUE, sep=";") 
} else{
  write.table(vmspp, file=file.path(file.path(stecf_path, "wgfsd_all_gears_stecf_merged.txt")), row.names=FALSE, col.names=TRUE, sep=";") 
}

#----------------
# check with a plot
#----------------
resy <-  diff(unique(vmspp$mid_lat)[order(unique(vmspp$mid_lat))])  [1]
resx <-  diff(unique(vmspp$mid_lon)[order(unique(vmspp$mid_lon))])  [1]
cutbreakval            <- c(-1,0,20,40,80,160,320,3000000)  # kilos
colyellowred           <- terrain.colors(length(cutbreakval))

a_species              <- "HER"
cols                   <- c("white", colyellowred)[cut(vmspp[vmspp$species==a_species, "landings_from_cell"]*1000, breaks=cutbreakval)]
coord                  <- vmspp[vmspp$species==a_species, c("mid_lon", "mid_lat")]
plot(coord, pch="")
for (i in 1: nrow(coord)) rect(coord[i, "mid_lon"]-resx/2, coord[i,"mid_lat"]-resy/2, coord[i,"mid_lon"]+resx/2, coord[i,"mid_lat"]+resy/2, col=cols[i], border=FALSE)


#----------------
# create raster layers
#----------------

 # subset for species
 for (a_species in unique(vmspp$species)){ 
    vmspp_this <-  vmspp[vmspp$species==a_species, ]

    # then, rasterize
    library(raster)
    xrange      <- range(vmspp$mid_lon, na.rm=TRUE)
    yrange      <- range(vmspp$mid_lat, na.rm=TRUE)

    r           <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], res=raster_res, crs=CRS("+proj=longlat +datum=WGS84"))
    some_coords <- SpatialPoints(cbind(SI_LONG=vmspp_this$mid_lon, SI_LATI=vmspp_this$mid_lat))
    rstr        <- rasterize(x=some_coords, y=r, field=vmspp_this$landings_from_cell*1000, fun="sum")  # converted in kilo
    #plot(rstr, xlim=c(-5,13), ylim=c(53,60))
    plot(log(rstr), xlim=c(5,25), ylim=c(48,62))
    map("worldHires",add=TRUE, res=0,fill=TRUE,col="darkgreen", xlim=c(5,25), ylim=c(48,62)); map.axes()
    title("log of landings per kg in 3' by 3´cells")
    savePlot(file.path(outPath, paste("DispatchedStecfLandingsOnVMS4log", a_species, sep='')), type="jpeg")
   
    
    library(rgdal)
    rstr_eea     <- projectRaster(rstr, crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")  # European EEA projection
    rstr_eea[is.na(rstr_eea)] <- -999  # arbitrary code, to get rid of true 0s in GIS
    rstr_eea[rstr_eea<0.001]  <- -999
    writeRaster(rstr_eea, file.path(outPath, paste("DispatchedStecfLandingsOnVMS4", a_species, sep='')), format = "GTiff", overwrite=TRUE)

    # ...and for info:
    #rstr2        <- rasterize(x=some_coords, y=r, field=log(vmspp_this$landings_from_cell*1000), fun="sum")  # converted in kilo
    #brks <- seq(-10, 15, by=0.2) 
    
    par(mar=c(3,3,3,5))
    brks <- exp(seq(0, 9, by=0.2))*10
    nb <- length(brks)-1 
    cols <- rev(terrain.colors(nb))
    plot(rstr, breaks=brks, col=cols, legend=FALSE)
    plot(rstr, legend.only=TRUE, legend.shrink=1, legend.width=2, zlim=c(0, max(brks)) )
    map("worldHires",add=TRUE, res=0,fill=TRUE,col="darkgreen", xlim=c(5,25), ylim=c(48,62)); map.axes()
    title("Landings per kg in 3' by 3´cells")
    savePlot(file.path(outPath, paste("DispatchedStecfLandingsOnVMS4", a_species, sep='')), type="jpeg")
   
 }




