# Francois Bastardie to WGSFD 2015


#outPath   <- file.path("C:", "BENTHIS", "data_gear_spec_questionnaire")
outPath   <- file.path("V:", "USER", "JSV","WGSFD2015","Sharepoint_downloads","Data")

##-------------------------------------------------
##-------------------------------------------------
# BENTHIS input (Eigaard et al 2015 ICESJMS)
#read.table(file=file.path(outPath, "estimates_for_OT_speed_matrix_13Jan15.txt"))
#read.table(file=file.path(outPath, "estimates_for_TBB_speed_matrix_13Jan15.txt"))
#read.table(file=file.path(outPath, "estimates_for_DRB_speed_matrix_13Jan15.txt"))
#read.table(file=file.path(outPath, "estimates_for_Seine_haul_duration_matrix_13Jan15.txt"))

av_fspeed_per_metier <- data.frame(
                                benthis_met=c( "OT_CRU", "OT_DMF", "OT_MIX",    "OT_SPF", "TBB_CRU", "TBB_DMF", "TBB_MOL", "DRB_MOL"),
                                av_fspeed=c(2.479630, 3.064286, 2.788636,  2.900000, 2.975000, 5.230851, 2.428571, 2.5),
                                sd_fspeed=c(0.3092167, 0.2286086, 0.2375833,  0.1414214, 0.4862392, 1.3101839, 0.3051931, 0 ),
                                nb_obs=c(54,   7,  66,   2, 8, 47, 21, 33)
                                )
rownames(av_fspeed_per_metier) <- av_fspeed_per_metier[,1]


av_fspeed_per_metier <- data.frame(
                                benthis_met=c(  "OT_CRU",  "OT_DMF", "OT_MIX",  "OT_MIX_ARA",     "OT_MIX_CRU",     "OT_MIX_CRU_DMF", "OT_MIX_DMF_BEN", "OT_MIX_DMF_PEL", "OT_MIX_TGS_CTC", "OT_MIX_TGS_OCC", "OT_SPF",  "TBB_CRU", "TBB_DMF", "TBB_MOL", "DRB_MOL"),
                                av_fspeed=c(2.479630, 3.064286, 2.788636, 2.989410, 3.008889, 2.629000, 2.911111, 3.410385, 4.100000, 3.283333, 2.900000, 2.975000, 5.230851, 2.428571, 2.5),
                                sd_fspeed=c(0.3092167, 0.2286086, 0.2375833, 0.3470858, 0.1578581, 0.3853292, 0.1577000, 0.4455617, 0.1021508, 0.2367712, 0.1414214, 0.4862392, 1.3101839, 0.3051931, 0 ),
                                nb_obs=c(54,   7,  66, 271,  45,  50,  18, 182,  24,  12,   2, 8, 47, 21, 33)
                                )
rownames(av_fspeed_per_metier) <- av_fspeed_per_metier[,1]


haul_duration_seiners <- c(SDN_DMF=2.591234, SSC_DMF=1.912500)




##-------------------------------------------------
##-------------------------------------------------
#read.table(file=file.path(outPath, "estimates_for_OT_kW_matrix_14Jan15.txt"))
#read.table(file=file.path(outPath, "estimates_for_TBB_kW_matrix_14Jan15.txt"))
#read.table(file=file.path(outPath, "estimates_for_DRB_kW_matrix_14Jan15.txt"))
#read.table(file=file.path(outPath, "estimates_for_Seine_kW_matrix_14Jan15.txt"))

# BENTHIS input (Eigaard et al 2015 ICESJMS)
av_kw_per_metier <- data.frame(
                                benthis_met=c("OT_CRU","OT_DMF","OT_MIX","OT_SPF", "TBB_CRU", "TBB_DMF", "TBB_MOL", "DRB_MOL", "SDN_DMF", "SSC_DMF"),
                                av_kw=c(345.5205, 441.6667, 400.6089,  883.8421, 210.6250, 822.1667, 107.1773, 382.4375, 167.6765, 481.7950 ),
                                sd_kw=c(210.0096, 265.3305, 186.3328,  763.5883, 62.63942, 376.23099, 70.66806, 258.8038, 54.89195, 188.39958),
                                nbobs=c(122,  33,  93,   19, 8, 48, 22, 32, 46, 8)
                                )
rownames(av_kw_per_metier) <- av_kw_per_metier[,1]


av_kw_per_metier <- data.frame(
                                benthis_met=c("OT_CRU",   "OT_DMF",         "OT_MIX",         "OT_MIX_ARA",     "OT_MIX_CRU",     "OT_MIX_CRU_DMF", "OT_MIX_DMF_BEN", "OT_MIX_DMF_PEL", "OT_MIX_TGS_CTC", "OT_MIX_TGS_OCC", "OT_SPF", "TBB_CRU", "TBB_DMF", "TBB_MOL", "DRB_MOL", "SDN_DMF", "SSC_DMF"),
                                av_kw=c(345.5205, 441.6667, 400.6089, 578.9852, 681.0000, 473.0970, 691.0217, 690.3574, 966.0833, 832.1667, 883.8421, 210.6250, 822.1667, 107.1773, 382.4375, 167.6765, 481.7950 ),
                                sd_kw=c( 210.0096, 265.3305, 186.3328, 331.4033, 358.3082, 323.8634, 439.3729, 402.5262, 446.9678, 416.8420, 763.5883, 62.63942, 376.23099, 70.66806, 258.8038, 54.89195, 188.39958),
                                nbobs=c(122,  33,  93, 271,  45,  66,  46, 197,  24,  12,  19, 8, 48, 22, 32, 46, 8)
                                )
rownames(av_kw_per_metier) <- av_kw_per_metier[,1]


##-------------------------------------------------
##-------------------------------------------------
#read.table(file=file.path(outPath, "estimates_for_OT_LOA_matrix_14Jan15.txt"))
#read.table(file=file.path(outPath, "estimates_for_TBB_LOA_matrix_14Jan15.txt"))
#read.table(file=file.path(outPath, "estimates_for_DRB_LOA_matrix_14Jan15.txt"))
#read.table(file=file.path(outPath, "estimates_for_Seine_LOA_matrix_14Jan15.txt"))

# BENTHIS input (Eigaard et al 2015 ICESJMS)
av_loa_per_metier <- data.frame(
                                benthis_met=c("OT_CRU",         "OT_DMF",         "OT_MIX",        "OT_SPF", "TBB_CRU", "TBB_DMF", "TBB_MOL", "DRB_MOL", "SDN_DMF", "SSC_DMF"  ),
                                av_loa=c(18.67739, 19.80000, 20.13774,  34.38526, 20.76500, 33.88660, 10.14545, 24.59848, 18.91915, 23.11750),
                                sd_loa=c(5.301704,  9.339311,  5.254701,  11.973938, 2.342611, 5.947364, 2.753825, 5.621879, 15.019933, 4.502256),
                                nbobs=c( 88,  17,  31,   19, 8, 47, 22, 33, 47, 8)
                                )
rownames(av_loa_per_metier) <- av_loa_per_metier[,1]

# BENTHIS input (Eigaard et al 2015 ICESJMS)
av_loa_per_metier <- data.frame(
                                benthis_met=c("OT_CRU",         "OT_DMF",         "OT_MIX",         "OT_MIX_ARA",     "OT_MIX_CRU",     "OT_MIX_CRU_DMF", "OT_MIX_DMF_BEN", "OT_MIX_DMF_PEL", "OT_MIX_TGS_CTC", "OT_MIX_TGS_OCC", "OT_SPF",   "TBB_CRU", "TBB_DMF", "TBB_MOL", "DRB_MOL", "SDN_DMF", "SSC_DMF"  ),
                                av_loa=c(18.67739, 19.80000, 20.13774, 20.47996, 21.68500, 19.89515, 24.36896, 23.74500, 23.83000, 22.85667, 34.38526, 20.76500, 33.88660, 10.14545, 24.59848, 18.91915, 23.11750),
                                sd_loa=c( 5.301704,  9.339311,  5.254701,  4.810143,  4.133593,  6.159984 , 6.469180,  5.569759,  3.897495,  4.108814, 11.973938, 2.342611, 5.947364, 2.753825, 5.621879, 15.019933, 4.502256),
                                nbobs=c( 88,  17,  31, 233,  44,  66,  48, 192,  24,  12,  19, 8, 47, 22, 33, 47, 8)
                                )
rownames(av_loa_per_metier) <- av_loa_per_metier[,1]


##-------------------------------------------------
##-------------------------------------------------

# BENTHIS input (Eigaard et al 2015 ICESJMS)

#read.table(file=file.path(outPath, "estimates_for_OT_nls_DoS_vs_LOA_or_kW_per_metier_10Oct14.txt"))
#read.table(file=file.path(outPath, "estimates_for_DRB_nls_dredgew_vs_LOA_or_kW_per_metier.txt"))
#read.table(file=file.path(outPath, "estimates_for_TBB_nls_beamw_vs_LOA_or_kW_per_metier.txt"))
#read.table(file=file.path(outPath, "estimates_for_DS_nls_seineropel_vs_LOA_or_kW_per_metier.txt"))



  gear_param_per_metier <- data.frame(
  a_metier=c('OT_CRU','OT_CRU','OT_DMF','OT_DMF','OT_MIX','OT_MIX','OT_MIX_DMF_BEN','OT_MIX_DMF_BEN','OT_MIX_DMF_PEL','OT_MIX_DMF_PEL','OT_MIX_CRU','OT_MIX_CRU', 'OT_MIX_CRU_DMF', 'OT_MIX_CRU_DMF', 'OT_SPF', 'OT_SPF',
             'DRB_MOL', 'DRB_MOL',
             "TBB_CRU","TBB_CRU","TBB_DMF","TBB_DMF","TBB_MOL","TBB_MOL",
             "SDN_DMF", "SDN_DMF", "SSC_DMF", "SSC_DMF"),
  param=c( 'a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b'),
  Estimate=c(5.2101805,  0.4649839,  8.0831287,  0.8603214, 10.6607888,  0.2920550,  3.2141038, 77.9811588,  6.6370720,  0.7705946, 26.6738248,  0.2102215,  3.9272776, 35.8253722,  0.9652384, 68.3889717,
             0.3142451, 1.2454404,
             1.4811712, 0.4577885, 2.2320556, 0.3287448, 0.9530019, 0.7093568,
             1948.8346668,    0.2362717, 4461.2700431,    0.1175892),
  equ=c( "DoS=a*(kW^b)",  "DoS=a*(kW^b)",  "DoS=a*(LOA^b)", "DoS=a*(LOA^b)", "DoS=a*(kW^b)",  "DoS=a*(kW^b)",  "DoS=(a*LOA)+b", "DoS=(a*LOA)+b", "DoS=a*(LOA^b)", "DoS=a*(LOA^b)", "DoS=a*(kW^b)", "DoS=a*(kW^b)",  "DoS=(a*LOA)+b", "DoS=(a*LOA)+b", "DoS=(a*LOA)+b", "DoS=(a*LOA)+b",
         "dredgew=a*(LOA^b)", "dredgew=a*(LOA^b)",
         "beamw=a*(kW^b)",  "beamw=a*(kW^b)",  "beamw=a*(kW^b)",  "beamw=a*(kW^b)",  "beamw=a*(LOA^b)", "beamw=a*(LOA^b)",
         "seineropel=a*(kW^b)",  "seineropel=a*(kW^b)",  "seineropel=a*(LOA^b)", "seineropel=a*(LOA^b)"),
  nb_records=c(124,124,39,39,94,94,48,48,190,190,45,45,53,53,19,19,
               33, 33,
               7,7,43,43,22,22,
               47, 47, 8, 8)
  )




##-------------------------------------------------
##-------------------------------------------------
# BENTHIS input (Eigaard et al 2015 ICESJMS)
#read.table(file=file.path(outPath, "estimates_for_OT_subcomponents_matrix_6Jan15_13degrees.txt")) # this one is not corresponding to the one in the paper!
#read.table(file=file.path(outPath, "estimates_for_Seine_haul_duration_matrix_13Jan15.txt"))

#read.table(file=file.path(outPath, "Subsurface_proportion_by_metier_Figure10_Eigaard_et_al.txt"), sep="\t", header=TRUE)

subsurface_proportion_by_metier <- data.frame(
                                benthis_met=c('OT_SPF' ,'SDN_DMF', 'OT_DMF', 'SSC_DMF',   'OT_MIX', 'OT_CRU','TBB_CRU', 'TBB_DMF' , 'TBB_MOL', 'DRB_MOL' ),
                                subsurface_prop= c(2.8,   5.0,   7.8,   14.0,  14.7,   32.1,  52.2, 100.0, 100.0, 100.0)
                                )
rownames(subsurface_proportion_by_metier) <- subsurface_proportion_by_metier[,1]


subsurface_proportion_by_metier <- data.frame(
                                benthis_met=c( 'OT_SPF','SDN_DMF','OT_DMF','OT_MIX_DMF_BEN','SSC_DMF','OT_MIX','OT_MIX_DMF_PEL','OT_MIX_CRU_DMF','OT_MIX_CRU','OT_CRU','TBB_CRU','TBB_DMF', 'TBB_MOL','DRB_MOL'   ),
                                subsurface_prop= c(2.8,   5.0,   7.8,   8.6,  14.0,  14.7,  22.0,  22.9,  29.2,  32.1,  52.2, 100.0, 100.0, 100.0)
                                )
rownames(subsurface_proportion_by_metier) <- subsurface_proportion_by_metier[,1]


##-------------------------------------------------
##-------------------------------------------------
# OUTPUT to WGSFD

# evaluate the equation e.g. from average engine/LOA from surveyed vessels
metiers <- c( 'OT_SPF','SDN_DMF','OT_DMF','OT_MIX_DMF_BEN','SSC_DMF','OT_MIX','OT_MIX_DMF_PEL','OT_MIX_CRU_DMF','OT_MIX_CRU','OT_CRU','TBB_CRU','TBB_DMF', 'TBB_MOL','DRB_MOL'   )
BenthisGearWidths <- data.frame(benthis_met=metiers,
                          av_kw=av_kw_per_metier[metiers, 'av_kw'],
                          av_loa=av_loa_per_metier[metiers, 'av_loa'],
                          av_fspeed=av_fspeed_per_metier[metiers, 'av_fspeed'],
                          subsurface_prop=subsurface_proportion_by_metier[metiers, 'subsurface_prop'])

  for(i in 1:nrow(BenthisGearWidths)){
     kW      <- BenthisGearWidths$av_kw[i]
     LOA     <- BenthisGearWidths$av_loa[i]
     this    <- gear_param_per_metier[gear_param_per_metier$a_metier==as.character(BenthisGearWidths$benthis_met[i]),]
     a <- NULL ; b <- NULL
     a       <- this[this$param=='a', 'Estimate']
     b       <- this[this$param=='b', 'Estimate']
     BenthisGearWidths[i,"gear_width"]  <-   eval(parse(text= as.character(this[1, 'equ']))) / 1000 # converted in km
   }



# export
write.table (BenthisGearWidths, file=file.path(outPath, "benthisGearWidthsForWGSFD15.txt"), row.names=FALSE, col.names=TRUE, quote=FALSE)



##-------------------------------------------------
##-------------------------------------------------
# Example of use

# import data
wgsfd                        <- read.table(file=file.path(dataPath,"VMS_Table_09062015_fixed.txt"), sep=";", header=TRUE)
head(wgsfd)

metiers_DCF_BENTHIS_lookup   <- read.table(file=file.path( dataPath, "Lookup_Metiers_incl_log.csv"), sep=",", header=TRUE)



# assign a benthis metier from a look up table
wgsfd$benthis_met          <- factor(wgsfd$LE_MET_level6) # init
levels (wgsfd$benthis_met) <- metiers_DCF_BENTHIS_lookup$Benthis_metiers[match(levels (wgsfd$benthis_met), metiers_DCF_BENTHIS_lookup$LE_MET_level6)]


# merge with info from BENTHIS
nrow(wgsfd)
wgsfdp <- merge(wgsfd, BenthisGearWidths, by.x="benthis_met", by.y= "benthis_met")
nrow(wgsfdp)   # some losses (likely gillnetters or some metiers at NA)

an <- function(x) as.numeric(as.character(x))

idx                             <- !wgsfdp$benthis_met %in% c("SDN_DMF", "SSC_DMF") # exclude seiners
wgsfdp$swept_area               <- NA
wgsfdp$swept_area[idx]          <- an(wgsfdp$fishing_hours[idx]) * an(wgsfdp$gear_width[idx]) * an(wgsfdp$avg_fishing_speed[idx])

## caution: specific rules for Seiners
idx                             <- wgsfdp$benthis_met %in% c("SDN_DMF") # for seiners
wgsfdp$swept_area[idx]          <- (pi*(an(wgsfdp$gear_width[idx])/(2*pi))^2) *  an(wgsfdp$fishing_hours)[idx]   / haul_duration_seiners["SDN_DMF"]

idx                             <- wgsfdp$benthis_met %in% c("SSC_DMF") # scottish seiners
wgsfdp$swept_area[idx]          <- (pi*(an(wgsfdp$gear_width[idx])/(2*pi))^2) *  an(wgsfdp$fishing_hours)[idx]   / haul_duration_seiners["SSC_DMF"]
wgsfdp$swept_area[idx]          <- wgsfdp$swept_area[idx]  *1.5 # ad hoc correction to account for the SSC specificities




# match to grid cell area in km2 and standardize the swept areas
# to correct for the latitude bias (the longitude goes to 0 when direction to North pole) eg a cell of 1 by 1 minute is:
# (1/60)*78.847*(1/60)*111 =  2.431116 km^2  at 45 degree in lat
# (1/60)*28.902*(1/60)*111 =  0.891145 km^2  at 75 degree in lat
# Length of a degree of longitude = cos (latitude) * 111.325 kilometers

wgsfdp$cell_area     <- (cos(wgsfdp$cell_lati *pi/180) * 111.325 )/60  * (111/60)
wgsfdp$swept_area    <- wgsfdp$swept_area /  wgsfdp$cell_area # standardize



