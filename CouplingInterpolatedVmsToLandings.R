
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##
 ## Obtain spatial (retained) catches distribtuion by dispaching declared landings
 ## on interpolated fishing VMS positions
 ## Author: Francois Bastardie (DTU-Aqua), Niels Hintzen (IMARES)
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##


 # GENERAL SETTINGS---------------
 myPath         <- file.path("C:", "Users", "fbas", "Documents", "GitHub")  # to adapt to your own path.
 dataPath       <- file.path(myPath, "FisheriesImpactTool", "Data")
 outPath        <- file.path(myPath, "FisheriesImpactTool", "Outputs")
 shapePath      <- file.path(myPath, "FisheriesImpactTool", "Shapes")
 dir.create(outPath)
 ctry           <- "DNK"
   

 # subset for a year period
 years     <- 2015
 lat_range <- c(53,60)
 lon_range <- c(-5,13)
 raster_res<- c(0.0167,0.0167) # 1 by 1 minute
 #--------------------------------


 library(vmstools)
  

 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!COUPLE WITH CATCHES!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## Note that this portion should gain at being included to the BENTHIS WP2 workflow.

 if(FALSE){  # do not re-run...this takes ages!

  #- PER YEAR
  #  for (a_year in c(2005:2012)){
  for (a_year in years){

    dir.create(file.path(dataPath, a_year, "interpolated", "plus"))

     # per year, load eflalo data,
     load(file.path(dataPath, paste("eflalo_", a_year,"_example.RData", sep=''))); # get the eflalo object
     eflalo <- formatEflalo(eflalo) # format each of the columns to the specified class
     eflalo <- eflalo[ grep(ctry, as.character(eflalo$VE_REF)),]  # keep the national vessels only.

     # Gear codes to keep (with assumed severe bottom impact)
     gears2keep            <- c("TBB","OTT","OTB","SSC","SDN","PTB","DRB")
     eflalo                <- eflalo[which(eflalo$LE_GEAR %in% gears2keep),]

     fls <- dir(file.path(dataPath, a_year, "interpolated"))
     fls <- fls[fls!="plus"]

     lst <- list(); count <- 0
     for(iFile in fls){
         cat(paste(a_year, "\n"))
         cat(paste(iFile, "\n"))
         count <- count+1
         load(file.path(dataPath, a_year, "interpolated", iFile))  # get  tacsatIntGearVEREF

         a_vid   <-  tacsatIntGearVEREF$VE_REF [1]
         a_gear  <-  tacsatIntGearVEREF$LE_GEAR[1]

         if(length(grep("LE_KG", colnames(tacsatIntGearVEREF)))>1 ) cat('this tacsat object has already been merged!! likely to fail.\n')

         tacsatIntGearVEREF      <- splitAmongPings(tacsat=subset(tacsatIntGearVEREF, LE_GEAR == a_gear & VE_REF == a_vid),
                                              eflalo=subset(eflalo, LE_GEAR == a_gear & VE_REF == a_vid),
                                              variable="all", level="day", conserve=T)
                                          # note that we can safely ignore the warning as it corresponds to the 0 catch
                                          # this is because sometimes the declaration of rectangle (in eflalo) does not match the rectangle from VMS points

         # check e.g. for cod
         plotTools(tacsatIntGearVEREF, level="gridcell", xlim=c(5,25), ylim=c(52,65), zlim=NULL, log=F, gridcell=c(0.1,0.05), color=NULL, control.tacsat=list(clm="LE_KG_COD"))
         plotTools(subset(eflalo, LE_GEAR == a_gear & VE_REF == a_vid), level="ICESrectangle", xlim=c(5,25), ylim=c(52,65), zlim=NULL,log=F, color=NULL,control.eflalo=list(clm="LE_KG_COD"))
    

         save(tacsatIntGearVEREF, file=file.path(dataPath, a_year, "interpolated", "plus",
                                                paste("tacsatSweptAreaPlus_",a_vid, "_", a_gear, ".RData", sep="")),compress=T)


         }  # end a_vessel

   }   # end a_year



   ##--------------
   ## then create on big dataset per year


  #-----------------------------------------------------------------------------
  # A FUNCTION FROM BENTHIS WP2 (FOR LATER USE)
  #-----------------------------------------------------------------------------
  compute_swept_area <- function(
                              tacsatIntGearVEREF=tacsatIntGearVEREF,
                              gear_param_per_metier=gear_param_per_metier,
                              towedGears=towedGears,
                              seineGears=seineGears,
                              VMS_ping_rate_in_hour=VMS_ping_rate_in_hour,
                              already_informed_width_for=NULL
                              ){

  if(is.null(already_informed_width_for)){
     tacsatIntGearVEREF <- tacsatIntGearVEREF[,!colnames(tacsatIntGearVEREF) %in%
                          c('GEAR_WIDTH', 'GEAR_WIDTH_LOWER', 'GEAR_WIDTH_UPPER', 'SWEPT_AREA_KM2', 'SWEPT_AREA_KM2_LOWER', 'SWEPT_AREA_KM2_UPPER')]  # remove columns if exists
     } else{
     tacsatIntGearVEREF <- tacsatIntGearVEREF[,!colnames(tacsatIntGearVEREF) %in%
                          c('SWEPT_AREA_KM2', 'SWEPT_AREA_KM2_LOWER', 'SWEPT_AREA_KM2_UPPER')]  # remove columns if exists
     
     }
  
  if(is.null(already_informed_width_for)){
  # MERGE WITH GEAR WIDTH
  GearWidth                   <- tacsatIntGearVEREF[!duplicated(data.frame(tacsatIntGearVEREF$VE_REF,tacsatIntGearVEREF$LE_MET,tacsatIntGearVEREF$VE_KW,tacsatIntGearVEREF$VE_LEN)), ]
  GearWidth                   <- GearWidth[,c('VE_REF','LE_MET','VE_KW', 'VE_LEN') ]
  GearWidth$GEAR_WIDTH        <- NA
  GearWidth$GEAR_WIDTH_LOWER  <- NA
  GearWidth$GEAR_WIDTH_UPPER  <- NA
  for (i in 1:nrow(GearWidth)) { # brute force...
    kW      <- GearWidth$VE_KW[i]
    LOA     <- GearWidth$VE_LEN[i]
    this    <- gear_param_per_metier[gear_param_per_metier$a_metier==as.character(GearWidth$LE_MET[i]),]
    a <- NULL ; b <- NULL
    a       <- this[this$param=='a', 'Estimate']
    b       <- this[this$param=='b', 'Estimate']
    GearWidth[i,"GEAR_WIDTH"]  <-   eval(parse(text= as.character(this[1, 'equ']))) / 1000 # converted in km
    a       <- this[this$param=='a', 'Estimate']
    b       <- this[this$param=='b', 'Estimate'] +2*this[this$param=='b', 'Std..Error']
    GearWidth[i,"GEAR_WIDTH_UPPER"]  <-  eval(parse(text= as.character(this[1, 'equ']))) / 1000 # converted in km
    a       <- this[this$param=='a', 'Estimate']
    b       <- this[this$param=='b', 'Estimate'] -2*this[this$param=='b', 'Std..Error']
    GearWidth[i,"GEAR_WIDTH_LOWER"]  <-  eval(parse(text= as.character(this[1, 'equ']))) / 1000 # converted in km
  }
  tacsatIntGearVEREF                    <- merge(tacsatIntGearVEREF, GearWidth,by=c("VE_REF","LE_MET","VE_KW","VE_LEN"),
                                              all.x=T,all.y=F)

  }
                                              
  #  the swept area (note that could work oustide the loop area as well....)
  # for the trawlers...
  if(tacsatIntGearVEREF$LE_GEAR[1] %in% towedGears){
        tacsatIntGearVEREF$SWEPT_AREA_KM2 <- NA
        tacsatIntGearVEREF <- orderBy(~SI_DATIM,data=tacsatIntGearVEREF)
        a_dist             <- distance(c(tacsatIntGearVEREF$SI_LONG[-1],0),  c(tacsatIntGearVEREF$SI_LATI[-1],0),
                                         tacsatIntGearVEREF$SI_LONG, tacsatIntGearVEREF$SI_LATI)
        a_dist[length(a_dist)] <- rev(a_dist)[2]
        tacsatIntGearVEREF$SWEPT_AREA_KM2 <- a_dist * tacsatIntGearVEREF$GEAR_WIDTH
        tacsatIntGearVEREF$SWEPT_AREA_KM2_LOWER <- a_dist * tacsatIntGearVEREF$GEAR_WIDTH_LOWER
        tacsatIntGearVEREF$SWEPT_AREA_KM2_UPPER <- a_dist * tacsatIntGearVEREF$GEAR_WIDTH_UPPER
        # correct the transition between sequential fishing events
        idx <- which(diff(tacsatIntGearVEREF$SI_DATIM)/60 > 15)   # if interval > 15 min then points belong to a different fishing event
        tacsatIntGearVEREF[ idx, c('SWEPT_AREA_KM2', 'SWEPT_AREA_KM2_LOWER', 'SWEPT_AREA_KM2_UPPER')] <- NA
  }

  # for the seiners...
  if(tacsatIntGearVEREF$LE_GEAR[1]  %in% seineGears){
      tacsatIntGearVEREF$SWEPT_AREA_KM2         <- pi*(tacsatIntGearVEREF$GEAR_WIDTH/(2*pi))^2
      tacsatIntGearVEREF$SWEPT_AREA_KM2_LOWER   <- pi*(tacsatIntGearVEREF$GEAR_WIDTH_LOWER/(2*pi))^2
      tacsatIntGearVEREF$SWEPT_AREA_KM2_UPPER   <- pi*(tacsatIntGearVEREF$GEAR_WIDTH_UPPER/(2*pi))^2

      haul_duration                           <- 3 # assumption of a mean duration based from questionnaires to seiners
      tacsatIntGearVEREF$SWEPT_AREA_KM2         <- tacsatIntGearVEREF$SWEPT_AREA_KM2 * VMS_ping_rate_in_hour / haul_duration # correction to avoid counting the same circle are several time.
      tacsatIntGearVEREF$SWEPT_AREA_KM2_LOWER   <- tacsatIntGearVEREF$SWEPT_AREA_KM2_LOWER * VMS_ping_rate_in_hour / haul_duration # correction to avoid counting the same circle are several time.
      tacsatIntGearVEREF$SWEPT_AREA_KM2_UPPER   <- tacsatIntGearVEREF$SWEPT_AREA_KM2_UPPER * VMS_ping_rate_in_hour / haul_duration # correction to avoid counting the same circle are several time.
      idx                                     <- grep('SSC', as.character(tacsatIntGearVEREF$LE_GEAR))
      tacsatIntGearVEREF[idx, 'SWEPT_AREA_KM2'] <- tacsatIntGearVEREF[idx, 'SWEPT_AREA_KM2'] *1.5 # ad hoc correction to account for the SSC specificities
      tacsatIntGearVEREF[idx, 'SWEPT_AREA_KM2_LOWER'] <- tacsatIntGearVEREF[idx, 'SWEPT_AREA_KM2_LOWER'] *1.5 # ad hoc correction to account for the SSC specificities
      tacsatIntGearVEREF[idx, 'SWEPT_AREA_KM2_UPPER'] <- tacsatIntGearVEREF[idx, 'SWEPT_AREA_KM2_UPPER'] *1.5 # ad hoc correction to account for the SSC specificities
   }

   return(tacsatIntGearVEREF)
   }

  #-----------------------------------------------------------------------------
  # Add "gear width-vessel size" relationships table of parameters.
  #-----------------------------------------------------------------------------
  #gear_param_per_metier       <- read.table(file=file.path(dataPath, "estimates_for_gear_param_per_metier.txt"))
  # an equivalent is:

  gear_param_per_metier <- data.frame(
  a_metier=c('OT_CRU','OT_CRU','OT_DMF','OT_DMF','OT_MIX','OT_MIX','OT_MIX_ARA','OT_MIX_ARA','OT_MIX_DMF_BEN','OT_MIX_DMF_BEN','OT_MIX_DMF_PEL','OT_MIX_DMF_PEL','OT_MIX_DPS','OT_MIX_DPS','OT_MIX_NEP','OT_MIX_NEP','OT_MIX_TGS_CTC','OT_MIX_TGS_CTC','OT_MIX_TGS_OCC','OT_MIX_TGS_OCC','OT_SPF','OT_SPF','TBB_CRU','TBB_CRU','TBB_DMF','TBB_DMF','TBB_MOL','TBB_MOL','DRB_MOL','DRB_MOL','SDN_DEM','SDN_DEM','SSC_DEM','SSC_DEM'),
  param=c('a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b'),
  Estimate=c(5.10393560454806,0.468985756915913,9.6053549509854,0.433672763959314,10.6607888271164,0.292055014993337,37.5271604597435,0.149004797319136,3.21410379943408,77.981158829069,6.63707197355847,0.770594580782091,26.6738247840508,0.210221545999405,3.92727763464472,35.8253721834011,6.23686411376723,0.767375050454527,0.0192465419797634,119.140335982507,0.965238378524667,68.3889717127507,1.48117115311386,0.457788539321641,0.660086393453441,0.507845311175148,0.953001905566232,0.709356826689359,0.314245137194503,1.24544036138755,1948.83466676682,0.236271746198865,4461.27004311913,0.117589220782479),
  Std..Error=c(1.81527145191998,0.0597519960969362,3.98228885098937,0.067572002767068,6.69386377505425,0.104413257104915,10.6717875588847,0.044963446750424,1.67854244656697,40.9297885227685,2.69086696344053,0.126123213329976,5.37466576335144,0.030829495804396,0.928442484509969,21.0228522096513,1.46159830273852,0.0732116002636393,0.000552819642352548,0.510207569180525,0.205245990518183,7.45180177818494,0.278399892100703,0.0346555048025894,0.172902115850281,0.0388684340513048,0.315715856194751,0.138412196798781,0.110027479611801,0.10614681568516,637.25152416296,0.0636712369543136,1665.50234108383,0.118756519107319),
  t.value=c(2.81166521907769,7.84887179593252,2.41201864314765,6.41793562718951,1.59262112068153,2.79710664230959,3.51648308708138,3.31390958851994,1.91481830322951,1.90524216331315,2.46651806415295,6.10985527910701,4.96288066244663,6.81884476260001,4.22996329893018,1.70411568450042,4.26715336360309,10.4816046595234,34.8152281598731,233.513462322532,4.70283670871103,9.17750817164227,5.32030074414718,13.2096918492278,3.81768835047121,13.0657517744299,3.01854305657162,5.12495894939517,2.8560604887363,11.733186279291,3.05818753329251,3.71080816866175,2.67863330664306,0.990170658978435),
  Pr...t..=c(0.00613312535554725,1.21619365805854e-11,0.021410083292817,2.48114253493853e-07,0.114790848188445,0.00631861326022122,0.000513087659147687,0.0010462790834138,0.0692370736030276,0.0705334706657513,0.0147045751318625,7.39218704723967e-09,1.2637878625965e-05,2.97113026239585e-08,0.000166717383514359,0.097483711710908,0.000314181622785133,5.0948672020349e-10,9.05842416252619e-12,5.10054218622276e-20,0.000204968683311441,5.36482029322678e-08,0.00313939649832079,4.44157761915604e-05,0.000458495488420268,5.11509704563588e-16,0.00678642704689924,5.16047183433098e-05,0.0075895814688592,6.18091407283774e-13,0.00391206507124518,0.000614325243514857,0.0438919330122769,0.367557330382699),
  equ=c('DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=(a*LOA)+b','DoS=(a*LOA)+b','DoS=a*(LOA^b)','DoS=a*(LOA^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=(a*LOA)+b','DoS=(a*LOA)+b','DoS=a*(LOA^b)','DoS=a*(LOA^b)','DoS=(a*kW)+b','DoS=(a*kW)+b','DoS=(a*LOA)+b','DoS=(a*LOA)+b','beamw=a*(kW^b)','beamw=a*(kW^b)','beamw=a*(kW^b)','beamw=a*(kW^b)','beamw=a*(LOA^b)','beamw=a*(LOA^b)','dredgew=a*(LOA^b)','dredgew=a*(LOA^b)','seineropel=a*(kW^b)','seineropel=a*(kW^b)','seineropel=a*(LOA^b)','seineropel=a*(LOA^b)'),
  nb_records=c(124,124,39,39,94,94,271,271,48,48,190,190,45,45,53,53,24,24,12,12,19,19,7,7,42,42,22,22,33,33,47,47,8,8)
  )


  # calls PER YEAR
      
  # Gear codes to keep (with assumed severe bottom impact)
  gears2keep            <- c("TBB","OTT","OTB","SSC","SDN","PTB","DRB")
  towedGears            <- c("TBB","OTT","OTB","PTB","DRB")
  seineGears            <- c("SSC","SDN")
  VMS_ping_rate_in_hour <- 1 # e.g. 1 hour for Denmark (rev(sort(table(intervalTacsat(sortTacsat(tacsat),level="vessel")$INTV))[1])
  

   spp <- c('COD', 'CSH', 'DAB', 'ELE', 'FLE', 'HAD', 'HER', 'HKE', 'HOM',
         'LEM', 'MAC', 'MON', 'MUS', 'NEP', 'NOP', 'OYF', 'PLE', 'POK', 'PRA', 'SAN',
           'SOL', 'SPR', 'TUR', 'WHB', 'WIT', 'WHG',
            'OTH')

   cols2keep <- c("VE_REF", "VE_LEN", "VE_KW", "SI_LATI","SI_LONG","SI_DATE", "SI_DATIM", "LE_GEAR","LE_MET","SWEPT_AREA_KM2","SWEPT_AREA_KM2_LOWER","SWEPT_AREA_KM2_UPPER")

   for (a_year in years){
    cat(paste(a_year, "\n"))

    fls <- dir(file.path(dataPath, a_year,"interpolated", "plus"))
    fls <- fls[grep('.RData', fls)]

    load(file.path(dataPath,a_year,"interpolated", "plus", fls[1])) # get one as an example for the right columns

    colkg <- colnames(tacsatIntGearVEREF) [ grep('KG', colnames(tacsatIntGearVEREF)) ]
    coleuro <- colnames(tacsatIntGearVEREF) [grep('EURO', colnames(tacsatIntGearVEREF))]

    #colums_to_keep  <- colnames(tacsatIntGearVEREF) [ ! c(1:ncol(tacsatIntGearVEREF))  %in%  c(colkg, coleuro)  ]
    cols2keep

    colkg_to_keep   <- c(paste('LE_KG_', spp, sep=''))
    coleuro_to_keep <- c(paste('LE_EURO_', spp, sep=''))

    colkg_to_sum    <-  colkg[!colkg %in%  colkg_to_keep]
    coleuro_to_sum  <-  coleuro[!coleuro %in%  coleuro_to_keep]


    lst <- list(); count <- 0  ;vid_with_errors <- NULL
    for(iFile in fls){
     cat(paste(iFile, "\n"))
     count <- count+1
     load(file.path(dataPath, a_year, "interpolated", "plus", iFile))



     tacsatIntGearVEREF$LE_KG_OTH   <- apply(tacsatIntGearVEREF[,colkg_to_sum], 1, sum, na.rm=TRUE)
     tacsatIntGearVEREF$LE_EURO_OTH <- apply(tacsatIntGearVEREF[,coleuro_to_sum], 1, sum, na.rm=TRUE)

     # compute the swept area
     tacsatIntGearVEREF <- compute_swept_area (tacsatIntGearVEREF, gear_param_per_metier, towedGears, seineGears, VMS_ping_rate_in_hour, already_informed_width_for=NULL)

     if(any(tacsatIntGearVEREF$SWEPT_AREA_KM2>100, na.rm = TRUE) ) {
        print(paste('check for lat long at 0!! for ', iFile))
        vid_with_errors <- c(vid_with_errors, iFile)
        tacsatIntGearVEREF[!is.na(tacsatIntGearVEREF$SWEPT_AREA_KM2) & tacsatIntGearVEREF$SWEPT_AREA_KM2>100, c("SWEPT_AREA_KM2", "SWEPT_AREA_KM2_LOWER", "SWEPT_AREA_KM2_UPPER")] <- NA
     }

     
     lst[[count]] <- tacsatIntGearVEREF[, c(cols2keep, colkg_to_keep, coleuro_to_keep)]

     print(ncol( lst[[count]]))

     }
   tacsatSweptArea   <- do.call(rbind, lst)
   save(tacsatSweptArea, file=file.path(dataPath, a_year, "interpolated", "plus",
                                                paste("tacsatSweptAreaPlus_", a_year, ".RData", sep="")),compress=T)
   } # end year


   } # end FALSE







   #---------------------------------------------------
   #---------------------------------------------------
   #---------------------------------------------------
   # Gridding for a given year
 
    for (a_year in years){
      cat(paste(a_year, "\n"))
    
    load(file=file.path(dataPath, a_year, "interpolated", "plus",
                                                paste("tacsatSweptAreaPlus_", a_year, ".RData", sep="")))
 
    library(vmstools)
    xrange  <- c(-30,50) # ALL
    yrange  <- c(30,81)  # ALL
 
    #- Set grid
    resx    <- 1/60 #1 minute
    resy    <- 1/60 #1 minute
    grd     <- createGrid(xrange, yrange, resx=resx, resy=resy, type="SpatialGrid", exactBorder=T)

    #- Grid all tacsatSweptArea data
    # Convert all tacsat poins first to SpatialPoints
    coords <- SpatialPoints(cbind(SI_LONG=tacsatSweptArea$SI_LONG, SI_LATI=tacsatSweptArea$SI_LATI))
    idx <- over(coords,grd)
    tacsatSweptArea$grID <- idx

    #- Remove records that are not in the study area
    tacsatSweptArea <- subset(tacsatSweptArea,is.na(grID)==F)

    #-1 Aggregate the results by metier and grid ID (aggregate() can be slow: be patient)
    c.listquote <- function (...)
   {
    args <- as.list(match.call()[-1])
    lstquote <- list(as.symbol("list"))
    for (i in args) {
        if (class(i) == "name" || (class(i) == "call" && i[[1]] !=
            "list")) {
            i <- eval(substitute(i), sys.frame(sys.parent()))
        }
        if (class(i) == "call" && i[[1]] == "list") {
            lstquote <- c(lstquote, as.list(i)[-1])
        }
        else if (class(i) == "character") {
            for (chr in i) {
                lstquote <- c(lstquote, list(parse(text = chr)[[1]]))
            }
        }
        else stop(paste("[", deparse(substitute(i)), "] Unknown class [",
            class(i), "] or is not a list()", sep = ""))
    }
    return(as.call(lstquote))
   }
   library(data.table)
    nm <- names(tacsatSweptArea)
    idx.col.euro   <- grep('LE_EURO_', nm)
    idx.col.kg     <- grep('LE_KG_', nm)
    idx.col        <- c(idx.col.euro, idx.col.kg)
    DT             <- data.table(tacsatSweptArea) # library data.table for fast grouping replacing aggregate()
    # AGGREGATE PER SPECIES -----> SUM (IF WEIGHT) 
    eq1                           <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
    tacsatSweptArea_agg           <- DT[,eval(eq1), by=list(grID)]
    tacsatSweptArea_agg           <- data.frame(tacsatSweptArea_agg)
    colnames(tacsatSweptArea_agg) <- c("grID",  nm[idx.col.euro], nm[idx.col.kg])

    
    # add midpoint of gridcell to dataset
    aggResult <- cbind(tacsatSweptArea_agg, CELL_LONG=coordinates(grd)[tacsatSweptArea_agg$grID,1],
                        CELL_LATI=coordinates(grd)[tacsatSweptArea_agg$grID,2])

    # remove records that are not in the study area 
    aggResult       <- subset(aggResult,is.na(grID)==F)

    save(aggResult, file=file.path(dataPath, a_year, "interpolated", "plus",
                                                paste("AggregatedSweptAreaPlus_", a_year, ".RData", sep=""))) 

    } # end a_year
     
    
    
    


