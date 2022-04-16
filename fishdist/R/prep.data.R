#' @name prep.data
#' @title Prepare data
#' @param data Data set
#' @param aphiaID AphiaIDs with species to select. If provided as column of data frame, all additional columns are added. If NULL (default), data is not subsetted for species.
#' @return List containing hh and hl data sets.
#' @importFrom worrms wm_record
#' @export
prep.data <- function(data, aphiaID = NULL){

    ## two data frames
    if(class(data) != "list" || length(data) != 2 || names(data) != c("HH","HL"))
        stop("Function requires data list with two DATRAS data sets 'hh' and 'hl' as elements.")
    hh <- data$HL
    hl <- data$HH

    ## flags
    saflag <- ifelse(any("SweptAreaDSKM2" == colnames(hh)),1,0)
    specflag <- ifelse(is.null(aphiaID[1]) || is.na(aphiaID[1]),0,1)


    if(specflag){
        specs <- as.data.frame(aphiaID)
        if(!any(colnames(specs) == "aphiaID"))
            stop("At least one of the columns in aphiaID needs to indicate the aphia ID and be called 'aphiaID'")
    }


    ## Subset data sets to reduce memory usage and prevent R collapse
    if(saflag){
        hh <- hh[,c("Survey","Quarter","Country","Ship","Gear","StNo",
                    "HaulNo","Year","Month","Day","TimeShot",
                    "DepthStratum","HaulDur","DayNight",
                    "ShootLat","ShootLong","StatRec",
                    "HaulVal","StdSpecRecCode","BySpecRecCode","DataType",
                    "SweptAreaDSKM2","SweptAreaWSKM2","SweptAreaBWKM2","Depth_gam")]
    }else{
        hh <- hh[,c("Survey","Quarter","Country","Ship","Gear","StNo",
                    "HaulNo","Year","Month","Day","TimeShot",
                    "DepthStratum","HaulDur","DayNight",
                    "ShootLat","ShootLong","StatRec","Depth",
                    "HaulVal","StdSpecRecCode","BySpecRecCode","DataType")]
    }


    ## HaulID
    hl$HaulID <- paste(hl$Survey, hl$Year,hl$Quarter, hl$Country, hl$Ship, hl$Gear,
                       hl$StNo, hl$HaulNo, sep = ":")
    hh$HaulID <- paste(hh$Survey, hh$Year,hh$Quarter, hh$Country, hh$Ship, hh$Gear,
                       hh$StNo, hh$HaulNo, sep = ":")


    ## HaulID unique in hh?
    writeLines(paste0("Removing duplicated hauls."))
    tmp <- duplicated(hh$HaulID)
    ind <- which(tmp)
    if(length(ind) > 0){
        for(i in 1:length(ind)){
            dups <- hh[which(hh$HaulID==hh$HaulID[ind[i]]),]
            flag <- all(apply(dups,2,function(x) all(x == x[1],na.rm = TRUE)))
            ## writeLines(paste0(i, ": ", flag))
            if(!flag){
                for(j in 2:nrow(dups)){
                    writeLines(paste0(colnames(dups)[which(dups[j,] != dups[1,])]))
                    writeLines(paste0(dups[c(1,j),which(dups[j,] != dups[1,])]))
                }
            }
        }
        ## Only differences between duplicated HaulIDs in WindSpeed -> delete duplicates
        hh <- hh[-ind,]
    }
    nrow(hh) == length(unique(hh$HaulID))

    ## Only keep hauls where there is the length composition.
    writeLines(paste0("Removing hauls not in hl."))
    hh <- subset(hh, hh$HaulID %in% hl$HaulID)
    hl <- subset(hl, hl$HaulID %in% hh$HaulID)


    ## Merge hh and hl
    ## subset hl (also to avoid double info in hh and hl after merging)
    hl <- hl[,c("HaulID","SpecCodeType","SpecCode","SpecVal",
                "Sex","TotalNo","CatIdentifier","SubFactor",
                "LngtClass","HLNoAtLngt","Valid_Aphia")]

    haulidhl <- sort(unique(hl$HaulID))
    haulidhh <- sort(unique(hh$HaulID))
    identical(haulidhh, haulidhl)

    survey <- merge(hh, hl, by="HaulID", all.y = TRUE)
    nrow(survey)==nrow(hl)

    rm(hh, hl, data)
    gc()
    ## pryr::mem_used()


    ## Add missing StatRec
    ## -------------
    data("ices.rectangles")
    survey$StatRec2 <- NA
    ind <- which(is.na(survey$StatRec))
    if(length(ind) > 0){
        for(i in 1:length(ind)){
            tmp2 <- ices.rectangles$ICESNAME[which(survey$ShootLat[ind[i]] >= ices.rectangles$SOUTH &
                                                   survey$ShootLat[ind[i]] < ices.rectangles$NORTH &
                                                   survey$ShootLong[ind[i]] >= ices.rectangles$WEST &
                                                   survey$ShootLong[ind[i]] < ices.rectangles$EAST)]
            print(tmp2)
            if(length(tmp2) == 1) survey$StatRec2[ind[i]] <- tmp2
        }
    }else{
        survey$StatRec2 <- survey$StatRec
    }
    ind <- which(!is.na(survey$StatRec2) & is.na(survey$StatRec))

    survey$StatRec[ind] <- survey$StatRec2[ind]
    survey$StatRec2 <- NULL

    ## TODO: remove entries without StatRec?


    ## Add Ecoregion and Area 27
    ## -------------
    tmp <- ices.rectangles[,c("ICESNAME","Ecoregion","Area_27")]
    survey <- merge(survey, tmp, by.x="StatRec",by.y="ICESNAME",all.x=TRUE)
    ind <- which(is.na(survey$Ecoregion))
    length(ind) ## only the ones where StatRec is NA


    ## Rename some surveys
    ## -------------
    survey$Survey[which(survey$Survey == "SCOWCGFS")] <- "SWC-IBTS"
    survey$Survey[which(survey$Survey == "SCOROC")] <- "ROCKALL"
    survey$Survey[which(survey$Survey == "BTS-VIII")] <- "BTS"


    ## Remove other gears than TVL and TVS from BITS
    ## -------------
    survey <- subset(survey, Survey != "BITS" | (Survey == "BITS" & Gear %in% c("TVS","TVL")))


    ## Gear Categories
    ## -------------
    data("gear.categories")
    survey <- merge(survey, gear.categories, by=c('Survey','Gear'), all.x = TRUE)
    ind <- which(is.na(survey$GearCat))
    length(ind)


    ## Manual gear corrections
    ## 1. 27.7g and 27.7a
    unique(survey$Area_27)
    ind <- which(survey$Survey == "IE-IGFS" & survey$Gear == "GOV" & survey$Area_27 %in% c("7.g","7.a"))
    length(ind)
    if(length(ind) > 0) survey$GearCat[ind] <- "GOV_CL"
    ## 2.
    ind <- which(survey$Survey == "SWC-IBTS" & survey$Gear == "GOV" & survey$ShootLat < 57.5)
    length(ind)
    if(length(ind) > 0) survey$GearCat[ind] <- "GOV_CL"

    ## any gear categories NA?
    ind <- which(is.na(survey$GearCat))
    length(ind)

    ## Rename some variables
    ## -------------
    colnames(survey)[which(colnames(survey) == "ShootLong")] <- "Lon"
    colnames(survey)[which(colnames(survey) == "ShootLat")] <- "Lat"
    colnames(survey)[which(colnames(survey) == "Valid_Aphia")] <- "AphiaID"


    ## Overwrite depth
    ## -------------
    if(any(colnames(survey) == "Depth_gam")) survey$Depth <- survey$Depth_gam
    survey$Depth_gam <- NULL

    ## Combine swept area
    ## -------------
    if(saflag){
        survey$SweptArea <- survey$SweptAreaBWKM2
        survey$SweptArea[is.na(survey$SweptArea)] <- survey$SweptAreaWSKM2[is.na(survey$SweptArea)]
        survey$SweptArea[is.na(survey$SweptArea)] <- survey$SweptAreaDSKM2[is.na(survey$SweptArea)]
        any(is.na(survey$SweptArea))
        ind <- which(is.na(survey$SweptArea))
        length(ind)
        any(survey$SweptArea == 0)
        ind <- which(survey$SweptArea == 0)
        length(ind)
        survey$SweptAreaWSKM2 <- NULL
        survey$SweptAreaBWKM2 <- NULL
        survey$SweptAreaDSKM2 <- NULL
    }


    ## Remove invalid data and clean data set
    ## --------------------------------------------------------------

    ## Convert -9 to NA
    ## ---------
    range(survey$SubFactor)
    range(survey$HaulDur)
    range(survey$HLNoAtLngt)
    writeLines(paste0("Hauls with -9 in SubFactor or HLNoAtLngt: ",
                      length(unique(survey$HaulID[which(survey$SubFactor == -9 | survey$HLNoAtLngt == -9)])),
                      ". Setting -9 equal to NA."
                      ))
    survey <- minus9toNA(survey)



    ## DataType
    ## ---------
    ## 43 Hauls (73 entries) with DataType = NA, but for all TotalNo = HLNoAtLngt = -9
    ## Thus, keep them as they might still be important for zero catches OR
    ind <- which(is.na(survey$DataType))
    ## length(unique(survey$HaulID[ind]))
    ## survey$TotalNo[ind]
    ## survey$HLNoAtLngt[ind]
    ## survey$AphiaID[ind]
    if(length(ind) > 0) survey$DataType[ind] <- "Z"
    ## EVHOE datatype is entered as ‘C’ in 2018. This is a mistake it should be ‘R’.
    ind <- which(survey$Survey == "EVHOE" & survey$Year == 2018)
    unique(survey$DataType[ind])
    if(length(ind) > 0) survey$DataType[ind] <- "R"


    ## AphiaID
    ## ---------
    ## 4585 Hauls (6612 entries) with AphiaID == NA
    ## Keep these Hauls for Zero data set
    ind <- which(is.na(survey$AphiaID))
    length(unique(survey$HaulID[ind]))
    unique(survey$TotalNo[ind])
    unique(survey$Year[ind])  ## over many years
    unique(survey$Survey[ind]) ## and surveys
    ## Hauls which are AphiaID = NA, have a SpecCode:
    tmp <- unique(survey$SpecCode[ind])
    ## for(i in 1:length(tmp)){
    ##     print(survey$AphiaID[which(survey$SpecCode == tmp[i])])
    ## }
    ## For all of them AphiaID = NA, no back-transforming possible


    ## HaulVal
    ## ---------
    ## 3656 Hauls (129945 entries) with HaulVal not equal to V
    ## Remove hauls!
    ind <- which(survey$HaulVal != "V")
    length(unique(survey$HaulID[ind]))
    length(survey$HaulID[ind])


    ## SpecVal
    ## ---------
    ## 15712 hauls (583750 entries) with SpecVal not equal to 1,4,7,10
    ## unique(survey$SpecVal) ## see: http://vocab.ices.dk/?ref=5
    ind <- which(!survey$SpecVal %in% c(1,4,7,10))
    length(unique(survey$HaulID[ind]))
    length(survey$HaulID[ind])

    ## 7369 Hauls (68248 entries) with SpecVal equal to 5 or 6
    ## The other half either NA, 0, or 2 (not useful)
    ind2 <- which(survey$SpecVal %in% c(5,6))
    length(unique(survey$HaulID[ind2]))
    length(survey$HaulID[ind2])


    ## StdSpecRecCode
    ## ---------
    ## http://vocab.ices.dk/?ref=88
    ## 0 	No standard species recorded
    ## 1 	All standard species recorded
    ## 2 	Pelagic standard species recorded
    ## 3 	Roundfish standard species recorded
    ## 4 	Individual standard species recorded
    unique(survey$StdSpecRecCode)
    ind <- which(survey$StdSpecRecCode != 1)
    length(unique(survey$HaulID[ind]))
    table(survey$Year[ind])
    ## only keep StdSpecRecCode == 1


    ## Lat and Lon
    ## ---------
    ind <- which(is.na(survey$Lat))
    length(unique(survey$HaulID[ind]))
    ind <- which(is.na(survey$Lon))
    length(unique(survey$HaulID[ind]))



    ## Apply selection
    ## ---------
    survey <- subset(survey, HaulVal == "V" &
                             SpecVal %in% c(1,10,4,7,5,6) &
                             StdSpecRecCode == 1 &
                             !is.na(Lat) & !is.na(Lon))


    ## Merge species list
    ## --------------------------------------------------------------
    if(specflag){


        ## Try to connect AphiaID that is NA by using TSN_code
        ## ----------
        ## In historical submissions TSN and NODC species codes were used, which is
        ## reflected in the SpecCodeTypes T and N in older data.
        length(is.na(survey$AphiaID))
        aphia_list <- unique(survey$AphiaID)
        aphia_list <- aphia_list[!duplicated(aphia_list)]
        length(aphia_list)
        ##

        ## Subset relevant columns from species list
        ## ----------
        ind <- which(is.na(specs$aphiaID))
        if(length(ind)>0) specs <- specs[-ind,]
        ind <- which(duplicated(specs$aphiaID))
        if(length(ind)>0) specs <- specs[-ind,]


        ## Merge species list to survey
        ## ----------
        survey <- merge(survey, specs, by.x="AphiaID",by.y="aphiaID",all.x = TRUE)

        ## Code to integrate from Anna on species bycatch corrections
        ## ----------
        if(any(colnames(survey) == "Species")){
            survey$Species[which(survey$Species == "Synaphobranchus kaupii")] <- "Synaphobranchus kaupi"
            survey$Species[which(survey$Species == "Dipturus batis")] <- "Dipturus spp"
            survey$Species[which(survey$Species == "Dipturus flossada")] <- "Dipturus spp"
            survey$Species[which(survey$Species == "Dipturus batis-complex")] <- "Dipturus spp"
            survey$Species[which(survey$Species == "Dipturus intermedia")] <- "Dipturus spp"
            survey$Species[which(survey$Species == "Dipturus")] <- "Dipturus spp"
            survey$Species[which(survey$Species == "Liparis montagui")] <- "Liparis spp"
            survey$Species[which(survey$Species == "Liparis liparis")] <- "Liparis spp"
            survey$Species[which(survey$Species == "Liparis liparis liparis")] <- "Liparis spp"
            survey$Species[which(survey$Species == "Chelon aurata")] <- "Chelon spp"
            survey$Species[which(survey$Species == "Chelon ramada")] <- "Chelon spp"
            survey$Species[which(survey$Species == "Mustelus mustelus/asterias")] <- "Mustelus spp"
            survey$Species[which(survey$Species == "Mustelus")] <- "Mustelus spp"
            survey$Species[which(survey$Species == "Mustelus mustelus")] <- "Mustelus spp"
            survey$Species[which(survey$Species == "Mustelus asterias")] <- "Mustelus spp"
            survey$Species[which(survey$Species == "Alosa")] <- "Alosa spp"
            survey$Species[which(survey$Species == "Alosa alosa")] <- "Alosa spp"
            survey$Species[which(survey$Species == "Alosa fallax")] <- "Alosa spp"
            survey$Species[which(survey$Species == "Argentina")] <- "Argentina spp"
            survey$Species[which(survey$Species == "Argentinidae")] <- "Argentina spp"
            survey$Species[which(survey$Species == "Argentina silus")] <- "Argentina spp"
            survey$Species[which(survey$Species == "Argentina sphyraena")] <- "Argentina spp"
            survey$Species[which(survey$Species == "Callionymus reticulatus")] <- "Callionymus spp"
            survey$Species[which(survey$Species == "Callionymus maculatus")] <- "Callionymus spp"
            survey$Species[which(survey$Species == "Ciliata mustela")] <- "Ciliata spp"
            survey$Species[which(survey$Species == "Ciliata septentrionalis")] <- "Ciliata spp"
            survey$Species[which(survey$Species == "Gaidropsarus")] <- "Gaidropsarus spp"
            survey$Species[which(survey$Species == "Gaidropsaurus macrophthalmus")] <- "Gaidropsarus spp"
            survey$Species[which(survey$Species == "Gaidropsaurus mediterraneus")] <- "Gaidropsarus spp"
            survey$Species[which(survey$Species == "Gaidropsaurus vulgaris")] <- "Gaidropsarus spp"
            survey$Species[which(survey$Species == "Sebastes")] <- "Sebastes spp"
            survey$Species[which(survey$Species == "Sebastes norvegicus")] <- "Sebastes spp"
            survey$Species[which(survey$Species == "Sebastes mentella")] <- "Sebastes spp"
            survey$Species[which(survey$Species == "Sebastes marinus")] <- "Sebastes spp"
            survey$Species[which(survey$Species == "Syngnathus")] <- "Syngnatus spp"
            survey$Species[which(survey$Species == "Syngnathus rostellatus")] <- "Syngnatus spp"
            survey$Species[which(survey$Species == "Syngnathus acus")] <- "Syngnatus spp"
            survey$Species[which(survey$Species == "Syngnathus typhle")] <- "Syngnatus spp"
            survey$Species[which(survey$Species == "Nerophis ophidion")] <- "Syngnatus spp"
            survey$Species[which(survey$Species == "Pomatoschistus")] <- "Pomatoschistus spp"
            survey$Species[which(survey$Species == "Pomatoschistus microps")] <- "Pomatoschistus spp"
            survey$Species[which(survey$Species == "Pomatoschistus minutus")] <- "Pomatoschistus spp"
            survey$Species[which(survey$Species == "Pomatoschistus pictus")] <- "Pomatoschistus spp"
            survey$Species[which(survey$Species == "Lesueurigobius")] <- "Gobius spp"
            survey$Species[which(survey$Species == "Gobius cobitis")] <- "Gobius spp"
            survey$Species[which(survey$Species == "Gobius niger")] <- "Gobius spp"
            survey$Species[which(survey$Species == "Leusueurigobius friesii")] <- "Gobius spp"
            survey$Species[which(survey$Species == "Neogobius melanostomus")] <- "Gobius spp"
            survey$Species[which(survey$Species == "Neogobius")] <- "Gobius spp"

            survey <- subset(survey, !(BySpecRecCode==0 & Survey == "BTS" &
                                       !Species %in% c("Chelidonichthys cuculus","Chelidonichthys lucerna","Eutrigla gurnardus",
                                                       "Gadus morhua","Limanda limanda","Lophius piscatorius",
                                                       "Merlangius merlangus","Microstomus kitt","Mullus surmuletus",
                                                       "Mustelus asterias","Pegusa lascaris","Platichthys flesus",
                                                       "Pleuronectes platessa","Raja brachyura","Raja clavata",
                                                       "Raja montagui","Scophthalmus maximus","Scophthalmus rhombus",
                                                       "Scyliorhinus canicula","Solea solea","Trispoterus luscus")))



            survey <- subset(survey, !(BySpecRecCode==0 & Survey == "SP-NORTH" &
                                       !Species %in% c("Chelidonichthys lucerna","Conger conger","Eutrigla gurnardus",
                                                       "Galeus melastomus","Helicolenus dactylopterus","Lepidorhombus boscii",
                                                       "Lepidorhombus whiffiagoni","Leucoraja circularis",
                                                       "Leucoraja naevus","Lophius budegassa","Lophius piscatorius","Merluccius merluccius",
                                                       "Micromesistius poutassou","Phycis blennoides", "Raja clavata","Raja montagui",
                                                       "Scomber scombrus","Scyliorhinus canicula","Trachurus trachurus",
                                                       "Trisopterus luscus","Zeus faber")))
            survey <- subset(survey, !(BySpecRecCode==0 & Survey == "SP-PORC" &
                                       !Species %in% c("Argentina silus","Chelidonichthys lucerna","Conger conger",
                                                       "Eutrigla gurnardus","Gadus morhua","Galeus melastomus",
                                                       "Glyptocephalus cynoglossu","Helicolenus dactylopterus","Hexanchus griseus",
                                                       "Hippoglossoides platessoi","Lepidorhombus boscii","Lepidorhombus whiffiagoni",
                                                       "Leucoraja circularis","Leucoraja naevus", "Lophius budegassa","Lophius piscatorius",
                                                       "Melanogrammus aeglefinus","Merluccius merluccius","Micromesistius poutassou",
                                                       "Molva dypterygia","Molva molva","Phycis blennoides","Raja clavata",
                                                       "Raja montagui","Scomber scombrus","Scyliorhinus canicula",
                                                       "Trachurus trachurus","Zeus faber")))
            survey <- subset(survey, !(BySpecRecCode==0 & Survey %in% c("NS-IBTS1","NS-IBTS3") &
                                       !Species %in% c("Clupea harengus","Sprattus sprattus","Scomber scombrus","Gadus morhua",
                                                       "Melanogrammus aeglefinus","Merlangius merlangus","Trisopterus esmarkii")))
            survey <- subset(survey, !(BySpecRecCode==2 &
                                       !Species %in% c("Ammodytidae","Anarhichas lupus","Argentina silus","Argentina sphyraena",
                                                       "Chelidonichthys cuculus","Callionymus lyra","Eutrigla gurnardus",
                                                       "Lumpenus lampretaeformis", "Mullus surmuletus","Squalus acanthias",
                                                       "Trachurus trachurus", "Platichthys flesus","Pleuronectes platessa","Limanda limanda",
                                                       "Lepidorhombus whiffiagoni","Hippoglossus hippoglossus","Hippoglossoides platessoi",
                                                       "Glyptocephalus cynoglossu","Microstomus kitt","Scophthalmus maximus",
                                                       "Scophthalmus rhombus","Solea solea", "Pollachius virens","Pollachius pollachius",
                                                       "Trisopterus luscus","Trisopterus minutus","Micromesistius poutassou","Molva molva",
                                                       "Merluccius merluccius","Brosme brosme", "Clupea harengus","Sprattus sprattus",
                                                       "Scomber scombrus","Gadus morhua","Melanogrammus aeglefinus" ,"Merlangius merlangus",
                                                       "Trisopterus esmarkii")))
            survey <- subset(survey, !(BySpecRecCode==3 &
                                       !Species %in% c("Pollachius virens","Pollachius pollachius","Trisopterus luscus","Trisopterus minutus",
                                                       "Micromesistius poutassou","Molva molva", "Merluccius merluccius","Brosme brosme",
                                                       "Clupea harengus","Sprattus sprattus","Scomber scombrus","Gadus morhua",
                                                       "Melanogrammus aeglefinus","Merlangius merlangus","Trisopterus esmarkii")))
            survey <- subset(survey, !(BySpecRecCode==4 &
                                       !Species %in% c("Platichthys flesus","Pleuronectes platessa","Limanda limanda",
                                                       "Lepidorhombus whiffiagoni","Hippoglossus hippoglossus","Hippoglossoides platessoi",
                                                       "Glyptocephalus cynoglossu","Microstomus kitt","Scophthalmus maximus","Scophthalmus rhombus",
                                                       "Solea solea", "Clupea harengus","Sprattus sprattus","Scomber scombrus","Gadus morhua",
                                                       "Melanogrammus aeglefinus","Merlangius merlangus","Trisopterus esmarkii")))
            survey <- subset(survey, !(BySpecRecCode==5 &
                                       !Species %in% c("Ammodytidae","Anarhichas lupus","Argentina silus","Argentina sphyraena",
                                                       "Chelidonichthys cuculus","Callionymus lyra","Eutrigla gurnardus",
                                                       "Lumpenus lampretaeformis", "Mullus surmuletus","Squalus acanthias","Trachurus trachurus",
                                                       "Clupea harengus","Sprattus sprattus","Scomber scombrus","Gadus morhua",
                                                       "Melanogrammus aeglefinus","Merlangius merlangus","Trisopterus esmarkii")))
        }


    }



    ## Some data transformation
    ## ---------
    survey$abstime <- survey$Year+(survey$Month-1)*1/12+(survey$Day-1)/365
    survey$timeOfYear <- (survey$Month-1)*1/12+(survey$Day-1)/365
    survey$ctime <- as.numeric(as.character(survey$Year)) + round(survey$timeOfYear,1)
    survey$TimeShotHour <- as.integer(survey$TimeShot/100) + (survey$TimeShot%%100)/60
    survey$Depth <- replace(survey$Depth, survey$Depth<0, NA)
    ## two different gear sizes are used in the Baltic -
    ##they cannot converted reliably for sensitives, so they are treated as separate surveys in my analysis.
    ## mutate(Survey = if_else(Survey=='BITS' & Gear=='TVS', 'BITSS', Survey)) %>%
    ## mutate(Survey = if_else(Survey=='BITS' & Gear=='TVL', 'BITSL', Survey)) %>%
    survey$Ship <- as.factor(survey$Ship)
    survey$Gear <- as.factor(survey$Gear)
    survey$ShipG <- factor(paste(survey$Ship, survey$Gear, sep=":"))


    ## Remove surveys with issues
    ## -------
    ## Remove NS-IBTS quarter 2 and 4
    survey <- subset(survey, Survey != "NS-IBTS" | (Survey == "NS-IBTS" & Quarter %in% c(1,3)))
    ## IGFS survey only sampled at depths greater than 200m from 2005 to present
    survey <- subset(survey, Survey != "NIGFS" | (Survey != "NIGFS" & Year >= 2006))

    ## TODO: apply these?
    ## !(Survey=='NS-IBTS' & Quarter==1 & Year<1967), ## 0
    ## !(Survey=='SWC-IBTS' & Quarter %in% c(2,3)), ## 3300
    ## !(Survey=='BITSS' & Quarter %in% c(2,3)),  ## 0
    ## !(Survey %in% c('BITS')), ## 667825  ## BITS and Gear not TVS or TVL ## before: removing BITSL, why?
    ## !(Survey=='BTS' & Year<1987),    ## 7618
    ## !(Survey=='IE-IGFS' & Year<2003),  ## 0



    ## Double-check key variables
    ## -------
    any(is.na(survey$Year))
    any(is.na(survey$Lat))
    any(is.na(survey$Lon))
    any(is.na(survey$timeOfYear))
    any(is.na(survey$Depth))
    any(is.na(survey$SweptArea))
    any(survey$SweptArea == 0)
    any(is.na(survey$DayNight))
    any(is.na(survey$DepthStratum))
    length(unique(survey$HaulID[is.na(survey$DepthStratum)])) ## many NAs (useful?)



    ## Zero data
    ## --------------------------------------------------------------
    ## Dummy survey with zeros (important so that all Survey * Year * Quarter combis are included (with 0)),
    ## even if there where no individuals caught
    if(!saflag) survey$SweptArea <- NA
    survey0 <- survey[,c("HaulID", "Survey", "Year", "Month", "Day", "Quarter", "StatRec", "Lat",
                         "Lon", "HaulDur", "Ship", "Gear", "Depth", "DayNight",
                         "SweptArea", "DepthStratum", "ShipG", "GearCat", "Ecoregion", "Area_27",
                         "BySpecRecCode", "TimeShotHour", "abstime", "timeOfYear","ctime")]
    survey0 <- survey0[!duplicated(survey0),]
    survey0$N <- 0
    colnames(survey0)[which(colnames(survey0) == "HaulID")] <- "haul.id"

    dat_sum <- survey0[,c("Survey","Quarter","Year","N")]
    dat_sum <- dat_sum[!duplicated(dat_sum),]
    dat_sum <- aggregate(list(Years=dat_sum$Year), by = list(Survey = dat_sum$Survey, Quarter = dat_sum$Quarter), range)
    dat_sum <- dat_sum[order(dat_sum$Survey),]
    dat_sum[,3] <- apply(dat_sum[,3],1,paste,collapse="-")


    dat_sum2 <- aggregate(list(StatRec=survey0$StatRec),
                          by = list(Survey = survey0$Survey, Quarter = survey0$Quarter),
                          function(x) length(unique(x)))

    print(cbind(dat_sum,StatRec=dat_sum2[,3]))



    ## Subset data and sum up counts
    ## --------------------------------------------------------------
    if(specflag){

        ## Species that could not be matched
        ## ---------
        ind <- which(!specs$aphiaID %in% survey$AphiaID)
        if(length(ind) > 0){
            writeLines("These aphiaIDs could not be matched:")
            print(specs[ind,])
        }

        ## Subset based on species list
        ## ---------
        survey <- subset(survey, survey$AphiaID %in% specs$aphiaID)

    }


    ## Day and Night
    ## ---------
    unique(survey$DayNight)
    ## meaningful


    ## HaulDur
    ## ---------
    unique(survey$HaulDur)
    range(survey$HaulDur)
    ## meaningful


    ## Swept Area
    ## ---------
    range(survey$SweptArea)
    ## meaningful


    ## SpecVal
    ## ---------
    ## SpecVal (5,6) only useful for presence-absence
    unique(survey$SpecVal)
    ind <- which(survey$SpecVal %in% c(5,6))
    tmp <- survey[ind,]

    writeLines(paste0("Species with SpecVal %in% c(5,6): "))
    if(any(colnames(survey) == "Species")){
        tmp <- as.data.frame(table(tmp$Species))
        if(nrow(tmp)>0){
            colnames(tmp) <- c("Species", "Frequency")
            print(tmp)
            writeLines(paste0("Removing these for now."))
            if(length(ind) > 0) survey <- survey[-ind,]
        }
    }


    ## HLNoAtLngt
    ## -------------
    ind <- which(is.na(survey$HLNoAtLngt))
    writeLines(paste0("Entries with HLNoAtLngt == NA: ",length(ind)))
    survey$LngtClass[ind] ## None of them has LngtClass info
    survey$TotalNo[ind]  ## Most of them have still TotalNo info!
    survey$SubFactor[ind]

    tmp <- cbind(paste0(survey$HaulID[ind],"-",survey$AphiaID[ind]),
                 survey$CatIdentifier[ind], survey$TotalNo[ind],
                 survey$NoMeas[ind], survey$SubFactor[ind])

    ## DECISION: Using TotalNo when HLNoAtLngt == NA and setting subFactor to 1.
    ## only two duplicated but Sex different: => summing up okay
    survey[which(paste0(survey$HaulID, "-", survey$AphiaID) %in% tmp[which(duplicated(tmp[,1]))][1]),]
    survey[which(paste0(survey$HaulID, "-", survey$AphiaID) %in% tmp[which(duplicated(tmp[,1]))][2]),]

    survey$HLNoAtLngt[ind] <- survey$TotalNo[ind]
    survey$SubFactor[ind] <- 1


    ## SubFactor
    ## -------------
    ind <- which(is.na(survey$SubFactor))
    writeLines(paste0("Entries with SubFactor == NA: ",length(ind)))
    ind <- which(survey$SubFactor == 0)
    writeLines(paste0("Entries with SubFactor == 0: ",length(ind)))
    range(survey$SubFactor)  ## high values ... realistic? keeping them for now

    ##     unique(survey$Species[survey$SubFactor > 20])

    ## Using HLNoAgeLngt for now
    ## -------------------
    ## account for datatype
    ## https://www.ices.dk/data/Documents/DATRAS/DATRAS_FAQs.pdf:
    ## DataType R,S: TotalNo –report the total number of fish of one species, sex, and category in the given haul
    ## DataType C: TotalNo –report the total number of fish of one species and sex in the given haul, raised to 1 hour hauling;
    survey$multiplier <- ifelse(survey$DataType=="C", survey$HaulDur/60, survey$SubFactor)  ## not using SubFactor if DataType == "C"
    survey$N <- survey$HLNoAtLngt * survey$multiplier
    ## sum up counts
    survey[is.na(survey)] <- "NA"  ## needed because aggregate deletes all NA
    survey$N <- as.numeric(survey$N)

    dontUse <- c("N","multiplier","CatIdentifier","Subfactor","LngtClass","HLNoAtLngt")
    survey <- aggregate(as.formula(
        paste0("N ~ ",
               paste0(colnames(survey)[-which(colnames(survey)%in%dontUse)],
                      collapse=" + "))),
                        function(x) sum(x, na.rm = TRUE), data = survey, na.action = na.pass)
    survey$N <- round(survey$N)
    colnames(survey)[which(colnames(survey) == "HaulID")] <- "haul.id"


    ## check that all entries in survey have N > 0!
    ## if(!all(survey$N > 0)) stop("All surveys should have more than 0 observations. Check data and code!")

    ## Worms list
    ## -----------
    ## creating taxonomy tables for each species
    worms.rec <- try(worrms::wm_record(id = aphia_list[!is.na(aphia_list)]),
                     silent = TRUE)
    if(!inherits(worms.rec,"try-error")){
        str(worms.rec)
        worms.rec <- as.data.frame(worms.rec)[,c("AphiaID","scientificname","genus","family","order","class")]
        any(duplicated(worms.rec$AphiaID))
        survey <- merge(survey, worms.rec, by='AphiaID', all.x=TRUE)
    }

    ## Order
    survey0 <- survey0[order(survey0$Year, survey0$Month, survey0$Day),]
    survey <- survey[order(survey$Year, survey$Month, survey$Day),]


    ## Return
    return(list(survey0 = survey0, survey = survey))

}




#' @name prep.data.light
#' @title Prepare data light
#' @param data Data set
#' @param aphiaID AphiaIDs with species to select. If provided as column of data frame, all additional columns are added. If NULL (default), data is not subsetted for species.
#' @return List containing hh and hl data sets.
#' @importFrom worrms wm_record
#' @export
prep.data.light <- function(data, aphiaID = NULL){

    ## two data frames
    if(class(data) != "list" || length(data) != 2 || names(data) != c("hh","hl"))
        stop("Function requires data list with two DATRAS data sets 'hh' and 'hl' as elements.")
    hh <- data$HH
    hl <- data$HL

    ## flags
    saflag <- ifelse(any("SweptAreaDSKM2" == colnames(hh)),1,0)
    specflag <- ifelse(is.null(aphiaID[1]) || is.na(aphiaID[1]) || aphiaID[1] %in% c("all","All","ALL"),0,1)


    if(specflag){
        specs <- as.data.frame(aphiaID)
        if(!any(colnames(specs) == "aphiaID"))
            stop("At least one of the columns in aphiaID needs to indicate the aphia ID and be called 'aphiaID'")
    }


    ## NEW: light difference: first species selection (not all species needed in hl)
    ## Subset data and sum up counts
    ## --------------------------------------------------------------
    if(specflag){

        any(hl$Valid_Aphia == specs$aphiaID)

        ## Species that could not be matched
        ## ---------
        ind <- which(!specs$aphiaID %in% hl$Valid_Aphia)
        if(length(ind) > 0){
            writeLines("These aphiaIDs could not be matched:")
            print(specs[ind,])
        }

        ## Subset based on species list
        ## ---------
        hl <- subset(hl, Valid_Aphia %in% specs$aphiaID)

    }


    ## Subset data sets to reduce memory usage and prevent R collapse
    if(saflag){
        hh <- hh[,c("Survey","Quarter","Country","Ship","Gear","StNo",
                    "HaulNo","Year","Month","Day","TimeShot",
                    "DepthStratum","HaulDur","DayNight",
                    "ShootLat","ShootLong","StatRec",
                    "HaulVal","StdSpecRecCode","BySpecRecCode","DataType",
                    "SweptAreaDSKM2","SweptAreaWSKM2","SweptAreaBWKM2","Depth_gam")]
    }else{
        hh <- hh[,c("Survey","Quarter","Country","Ship","Gear","StNo",
                    "HaulNo","Year","Month","Day","TimeShot",
                    "DepthStratum","HaulDur","DayNight",
                    "ShootLat","ShootLong","StatRec","Depth",
                    "HaulVal","StdSpecRecCode","BySpecRecCode","DataType")]
    }


    ## HaulID
    hl$HaulID <- paste(hl$Survey, hl$Year,hl$Quarter, hl$Country, hl$Ship, hl$Gear,
                       hl$StNo, hl$HaulNo, sep = ":")
    hh$HaulID <- paste(hh$Survey, hh$Year,hh$Quarter, hh$Country, hh$Ship, hh$Gear,
                       hh$StNo, hh$HaulNo, sep = ":")

    ## HaulID unique in hh?
    writeLines(paste0("Removing duplicated hauls."))
    tmp <- duplicated(hh$HaulID)
    ind <- which(tmp)
    if(length(ind) > 0){
        for(i in 1:length(ind)){
            dups <- hh[which(hh$HaulID==hh$HaulID[ind[i]]),]
            flag <- all(apply(dups,2,function(x) all(x == x[1],na.rm = TRUE)))
            ## writeLines(paste0(i, ": ", flag))
            if(!flag){
                for(j in 2:nrow(dups)){
                    writeLines(paste0(colnames(dups)[which(dups[j,] != dups[1,])]))
                    writeLines(paste0(dups[c(1,j),which(dups[j,] != dups[1,])]))
                }
            }
        }
        ## Only differences between duplicated HaulIDs in WindSpeed -> delete duplicates
        hh <- hh[-ind,]
    }
    nrow(hh) == length(unique(hh$HaulID))

    ## NA in haul id can cause merging issues
    if(any(is.na(hh$HaulID))){
        hh <- hh[-is.na(hh$HaulID),]
    }
    if(any(is.na(hl$HaulID))){
        hl <- hl[-is.na(hl$HaulID),]
    }

    ## Only keep hauls where there is the length composition.
    writeLines(paste0("Removing hauls not in hl."))
    ## hh <- subset(hh, hh$HaulID %in% hl$HaulID) ## not if species subsetting in beginning
    hl <- subset(hl, hl$HaulID %in% hh$HaulID)

    ## Merge hh and hl
    ## subset hl (also to avoid double info in hh and hl after merging)
    hl <- hl[,c("HaulID","SpecCodeType","SpecCode","SpecVal",
                "Sex","TotalNo","CatIdentifier","SubFactor",
                "LngtClass","HLNoAtLngt","Valid_Aphia")]

    survey <- merge(hh, hl, by="HaulID", all = TRUE)

    rm(hh, hl, data)
    gc()
    ## pryr::mem_used()

    ## Add missing StatRec
    ## -------------
    data("ices.rectangles")
    survey$StatRec2 <- NA
    ind <- which(is.na(survey$StatRec))
    if(length(ind) > 0){
        for(i in 1:length(ind)){
            tmp2 <- ices.rectangles$ICESNAME[which(survey$ShootLat[ind[i]] >= ices.rectangles$SOUTH &
                                                   survey$ShootLat[ind[i]] < ices.rectangles$NORTH &
                                                   survey$ShootLong[ind[i]] >= ices.rectangles$WEST &
                                                   survey$ShootLong[ind[i]] < ices.rectangles$EAST)]
            print(tmp2)
            if(length(tmp2) == 1) survey$StatRec2[ind[i]] <- tmp2
        }
    }else{
        survey$StatRec2 <- survey$StatRec
    }
    ind <- which(!is.na(survey$StatRec2) & is.na(survey$StatRec))

    survey$StatRec[ind] <- survey$StatRec2[ind]
    survey$StatRec2 <- NULL

    ## TODO: remove entries without StatRec?


    ## Add Ecoregion and Area 27
    ## -------------
    tmp <- ices.rectangles[,c("ICESNAME","Ecoregion","Area_27")]
    survey <- merge(survey, tmp, by.x="StatRec",by.y="ICESNAME",all.x=TRUE)
    ind <- which(is.na(survey$Ecoregion))
    length(ind) ## only the ones where StatRec is NA


    ## Rename some surveys
    ## -------------
    survey$Survey[which(survey$Survey == "SCOWCGFS")] <- "SWC-IBTS"
    survey$Survey[which(survey$Survey == "SCOROC")] <- "ROCKALL"
    survey$Survey[which(survey$Survey == "BTS-VIII")] <- "BTS"


    ## Remove other gears than TVL and TVS from BITS
    ## -------------
    survey <- subset(survey, Survey != "BITS" | (Survey == "BITS" & Gear %in% c("TVS","TVL")))


    ## Gear Categories
    ## -------------
    data("gear.categories")
    survey <- merge(survey, gear.categories, by=c('Survey','Gear'), all.x = TRUE)
    ind <- which(is.na(survey$GearCat))
    length(ind)


    ## Manual gear corrections
    ## 1. 27.7g and 27.7a
    unique(survey$Area_27)
    ind <- which(survey$Survey == "IE-IGFS" & survey$Gear == "GOV" & survey$Area_27 %in% c("7.g","7.a"))
    length(ind)
    if(length(ind) > 0) survey$GearCat[ind] <- "GOV_CL"
    ## 2.
    ind <- which(survey$Survey == "SWC-IBTS" & survey$Gear == "GOV" & survey$ShootLat < 57.5)
    length(ind)
    if(length(ind) > 0) survey$GearCat[ind] <- "GOV_CL"

    ## any gear categories NA?
    ind <- which(is.na(survey$GearCat))
    length(ind)

    ## Rename some variables
    ## -------------
    colnames(survey)[which(colnames(survey) == "ShootLong")] <- "Lon"
    colnames(survey)[which(colnames(survey) == "ShootLat")] <- "Lat"
    colnames(survey)[which(colnames(survey) == "Valid_Aphia")] <- "AphiaID"

    ## Overwrite depth
    ## -------------
    if(any(colnames(survey) == "Depth_gam")) survey$Depth <- survey$Depth_gam
    survey$Depth_gam <- NULL

    ## Combine swept area
    ## -------------
    if(saflag){
        survey$SweptArea <- survey$SweptAreaBWKM2
        survey$SweptArea[is.na(survey$SweptArea)] <- survey$SweptAreaWSKM2[is.na(survey$SweptArea)]
        survey$SweptArea[is.na(survey$SweptArea)] <- survey$SweptAreaDSKM2[is.na(survey$SweptArea)]
        any(is.na(survey$SweptArea))
        ind <- which(is.na(survey$SweptArea))
        length(ind)
        any(survey$SweptArea == 0)
        ind <- which(survey$SweptArea == 0)
        length(ind)
        survey$SweptAreaWSKM2 <- NULL
        survey$SweptAreaBWKM2 <- NULL
        survey$SweptAreaDSKM2 <- NULL
    }


    ## Remove invalid data and clean data set
    ## --------------------------------------------------------------

    ## Convert -9 to NA
    ## ---------
    survey <- minus9toNA(survey)
    range(survey$SubFactor)
    range(survey$HaulDur)
    range(survey$HLNoAtLngt)

    ## DataType
    ## ---------
    ## 43 Hauls (73 entries) with DataType = NA, but for all TotalNo = HLNoAtLngt = -9
    ## Thus, keep them as they might still be important for zero catches OR
    ind <- which(is.na(survey$DataType))
    ## length(unique(survey$HaulID[ind]))
    ## survey$TotalNo[ind]
    ## survey$HLNoAtLngt[ind]
    ## survey$AphiaID[ind]
    if(length(ind) > 0) survey$DataType[ind] <- "Z"
    ## EVHOE datatype is entered as ‘C’ in 2018. This is a mistake it should be ‘R’.
    ind <- which(survey$Survey == "EVHOE" & survey$Year == 2018)
    unique(survey$DataType[ind])
    if(length(ind) > 0) survey$DataType[ind] <- "R"


    ## AphiaID
    ## ---------
    ## 4585 Hauls (6612 entries) with AphiaID == NA
    ## Keep these Hauls for Zero data set
    ind <- which(is.na(survey$AphiaID))
    length(unique(survey$HaulID[ind]))
    unique(survey$TotalNo[ind])
    unique(survey$Year[ind])  ## over many years
    unique(survey$Survey[ind]) ## and surveys
    ## Hauls which are AphiaID = NA, have a SpecCode:
    tmp <- unique(survey$SpecCode[ind])
    ## for(i in 1:length(tmp)){
    ##     print(survey$AphiaID[which(survey$SpecCode == tmp[i])])
    ## }
    ## For all of them AphiaID = NA, no back-transforming possible


    ## HaulVal
    ## ---------
    ## 3656 Hauls (129945 entries) with HaulVal not equal to V
    ## Remove hauls!
    ind <- which(survey$HaulVal != "V")
    length(unique(survey$HaulID[ind]))
    length(survey$HaulID[ind])


    ## SpecVal
    ## ---------
    ## 15712 hauls (583750 entries) with SpecVal not equal to 1,4,7,10
    ## unique(survey$SpecVal) ## see: http://vocab.ices.dk/?ref=5
    ind <- which(!survey$SpecVal %in% c(1,4,7,10))
    length(unique(survey$HaulID[ind]))
    length(survey$HaulID[ind])

    ## 7369 Hauls (68248 entries) with SpecVal equal to 5 or 6
    ## The other half either NA, 0, or 2 (not useful)
    ind2 <- which(survey$SpecVal %in% c(5,6))
    length(unique(survey$HaulID[ind2]))
    length(survey$HaulID[ind2])


    ## StdSpecRecCode
    ## ---------
    ## http://vocab.ices.dk/?ref=88
    ## 0 	No standard species recorded
    ## 1 	All standard species recorded
    ## 2 	Pelagic standard species recorded
    ## 3 	Roundfish standard species recorded
    ## 4 	Individual standard species recorded
    unique(survey$StdSpecRecCode)
    ind <- which(survey$StdSpecRecCode != 1)
    length(unique(survey$HaulID[ind]))
    table(survey$Year[ind])
    ## only keep StdSpecRecCode == 1


    ## Lat and Lon
    ## ---------
    ind <- which(is.na(survey$Lat))
    length(unique(survey$HaulID[ind]))
    ind <- which(is.na(survey$Lon))
    length(unique(survey$HaulID[ind]))

    ## Apply selection
    ## ---------
    survey <- subset(survey, HaulVal == "V" &
                             StdSpecRecCode == 1 &
                             !is.na(Lat) & !is.na(Lon))
## SpecVal %in% c(1,10,4,7,5,6) & ## HERE: if selecting species first this does not work


    ## Merge species list
    ## --------------------------------------------------------------
    if(specflag){

        ## Try to connect AphiaID that is NA by using TSN_code
        ## ----------
        ## In historical submissions TSN and NODC species codes were used, which is
        ## reflected in the SpecCodeTypes T and N in older data.
        length(is.na(survey$AphiaID))
        aphia_list <- unique(survey$AphiaID)
        aphia_list <- aphia_list[!duplicated(aphia_list)]
        length(aphia_list)
        ##

        ## Subset relevant columns from species list
        ## ----------
        ind <- which(is.na(specs$aphiaID))
        if(length(ind)>0) specs <- specs[-ind,]
        ind <- which(duplicated(specs$aphiaID))
        if(length(ind)>0) specs <- specs[-ind,]


        ## Merge species list to survey
        ## ----------
        survey <- merge(survey, specs, by.x="AphiaID",by.y="aphiaID",all.x = TRUE)

        ## Code to integrate from Anna on species bycatch corrections
        ## ----------
        if(any(colnames(survey) == "Species")){
            survey$Species[which(survey$Species == "Synaphobranchus kaupii")] <- "Synaphobranchus kaupi"
            survey$Species[which(survey$Species == "Dipturus batis")] <- "Dipturus spp"
            survey$Species[which(survey$Species == "Dipturus flossada")] <- "Dipturus spp"
            survey$Species[which(survey$Species == "Dipturus batis-complex")] <- "Dipturus spp"
            survey$Species[which(survey$Species == "Dipturus intermedia")] <- "Dipturus spp"
            survey$Species[which(survey$Species == "Dipturus")] <- "Dipturus spp"
            survey$Species[which(survey$Species == "Liparis montagui")] <- "Liparis spp"
            survey$Species[which(survey$Species == "Liparis liparis")] <- "Liparis spp"
            survey$Species[which(survey$Species == "Liparis liparis liparis")] <- "Liparis spp"
            survey$Species[which(survey$Species == "Chelon aurata")] <- "Chelon spp"
            survey$Species[which(survey$Species == "Chelon ramada")] <- "Chelon spp"
            survey$Species[which(survey$Species == "Mustelus mustelus/asterias")] <- "Mustelus spp"
            survey$Species[which(survey$Species == "Mustelus")] <- "Mustelus spp"
            survey$Species[which(survey$Species == "Mustelus mustelus")] <- "Mustelus spp"
            survey$Species[which(survey$Species == "Mustelus asterias")] <- "Mustelus spp"
            survey$Species[which(survey$Species == "Alosa")] <- "Alosa spp"
            survey$Species[which(survey$Species == "Alosa alosa")] <- "Alosa spp"
            survey$Species[which(survey$Species == "Alosa fallax")] <- "Alosa spp"
            survey$Species[which(survey$Species == "Argentina")] <- "Argentina spp"
            survey$Species[which(survey$Species == "Argentinidae")] <- "Argentina spp"
            survey$Species[which(survey$Species == "Argentina silus")] <- "Argentina spp"
            survey$Species[which(survey$Species == "Argentina sphyraena")] <- "Argentina spp"
            survey$Species[which(survey$Species == "Callionymus reticulatus")] <- "Callionymus spp"
            survey$Species[which(survey$Species == "Callionymus maculatus")] <- "Callionymus spp"
            survey$Species[which(survey$Species == "Ciliata mustela")] <- "Ciliata spp"
            survey$Species[which(survey$Species == "Ciliata septentrionalis")] <- "Ciliata spp"
            survey$Species[which(survey$Species == "Gaidropsarus")] <- "Gaidropsarus spp"
            survey$Species[which(survey$Species == "Gaidropsaurus macrophthalmus")] <- "Gaidropsarus spp"
            survey$Species[which(survey$Species == "Gaidropsaurus mediterraneus")] <- "Gaidropsarus spp"
            survey$Species[which(survey$Species == "Gaidropsaurus vulgaris")] <- "Gaidropsarus spp"
            survey$Species[which(survey$Species == "Sebastes")] <- "Sebastes spp"
            survey$Species[which(survey$Species == "Sebastes norvegicus")] <- "Sebastes spp"
            survey$Species[which(survey$Species == "Sebastes mentella")] <- "Sebastes spp"
            survey$Species[which(survey$Species == "Sebastes marinus")] <- "Sebastes spp"
            survey$Species[which(survey$Species == "Syngnathus")] <- "Syngnatus spp"
            survey$Species[which(survey$Species == "Syngnathus rostellatus")] <- "Syngnatus spp"
            survey$Species[which(survey$Species == "Syngnathus acus")] <- "Syngnatus spp"
            survey$Species[which(survey$Species == "Syngnathus typhle")] <- "Syngnatus spp"
            survey$Species[which(survey$Species == "Nerophis ophidion")] <- "Syngnatus spp"
            survey$Species[which(survey$Species == "Pomatoschistus")] <- "Pomatoschistus spp"
            survey$Species[which(survey$Species == "Pomatoschistus microps")] <- "Pomatoschistus spp"
            survey$Species[which(survey$Species == "Pomatoschistus minutus")] <- "Pomatoschistus spp"
            survey$Species[which(survey$Species == "Pomatoschistus pictus")] <- "Pomatoschistus spp"
            survey$Species[which(survey$Species == "Lesueurigobius")] <- "Gobius spp"
            survey$Species[which(survey$Species == "Gobius cobitis")] <- "Gobius spp"
            survey$Species[which(survey$Species == "Gobius niger")] <- "Gobius spp"
            survey$Species[which(survey$Species == "Leusueurigobius friesii")] <- "Gobius spp"
            survey$Species[which(survey$Species == "Neogobius melanostomus")] <- "Gobius spp"
            survey$Species[which(survey$Species == "Neogobius")] <- "Gobius spp"

            survey <- subset(survey, !(BySpecRecCode==0 & Survey == "BTS" &
                                       !Species %in% c("Chelidonichthys cuculus","Chelidonichthys lucerna","Eutrigla gurnardus",
                                                       "Gadus morhua","Limanda limanda","Lophius piscatorius",
                                                       "Merlangius merlangus","Microstomus kitt","Mullus surmuletus",
                                                       "Mustelus asterias","Pegusa lascaris","Platichthys flesus",
                                                       "Pleuronectes platessa","Raja brachyura","Raja clavata",
                                                       "Raja montagui","Scophthalmus maximus","Scophthalmus rhombus",
                                                       "Scyliorhinus canicula","Solea solea","Trispoterus luscus")))



            survey <- subset(survey, !(BySpecRecCode==0 & Survey == "SP-NORTH" &
                                       !Species %in% c("Chelidonichthys lucerna","Conger conger","Eutrigla gurnardus",
                                                       "Galeus melastomus","Helicolenus dactylopterus","Lepidorhombus boscii",
                                                       "Lepidorhombus whiffiagoni","Leucoraja circularis",
                                                       "Leucoraja naevus","Lophius budegassa","Lophius piscatorius","Merluccius merluccius",
                                                       "Micromesistius poutassou","Phycis blennoides", "Raja clavata","Raja montagui",
                                                       "Scomber scombrus","Scyliorhinus canicula","Trachurus trachurus",
                                                       "Trisopterus luscus","Zeus faber")))
            survey <- subset(survey, !(BySpecRecCode==0 & Survey == "SP-PORC" &
                                       !Species %in% c("Argentina silus","Chelidonichthys lucerna","Conger conger",
                                                       "Eutrigla gurnardus","Gadus morhua","Galeus melastomus",
                                                       "Glyptocephalus cynoglossu","Helicolenus dactylopterus","Hexanchus griseus",
                                                       "Hippoglossoides platessoi","Lepidorhombus boscii","Lepidorhombus whiffiagoni",
                                                       "Leucoraja circularis","Leucoraja naevus", "Lophius budegassa","Lophius piscatorius",
                                                       "Melanogrammus aeglefinus","Merluccius merluccius","Micromesistius poutassou",
                                                       "Molva dypterygia","Molva molva","Phycis blennoides","Raja clavata",
                                                       "Raja montagui","Scomber scombrus","Scyliorhinus canicula",
                                                       "Trachurus trachurus","Zeus faber")))
            survey <- subset(survey, !(BySpecRecCode==0 & Survey %in% c("NS-IBTS1","NS-IBTS3") &
                                       !Species %in% c("Clupea harengus","Sprattus sprattus","Scomber scombrus","Gadus morhua",
                                                       "Melanogrammus aeglefinus","Merlangius merlangus","Trisopterus esmarkii")))
            survey <- subset(survey, !(BySpecRecCode==2 &
                                       !Species %in% c("Ammodytidae","Anarhichas lupus","Argentina silus","Argentina sphyraena",
                                                       "Chelidonichthys cuculus","Callionymus lyra","Eutrigla gurnardus",
                                                       "Lumpenus lampretaeformis", "Mullus surmuletus","Squalus acanthias",
                                                       "Trachurus trachurus", "Platichthys flesus","Pleuronectes platessa","Limanda limanda",
                                                       "Lepidorhombus whiffiagoni","Hippoglossus hippoglossus","Hippoglossoides platessoi",
                                                       "Glyptocephalus cynoglossu","Microstomus kitt","Scophthalmus maximus",
                                                       "Scophthalmus rhombus","Solea solea", "Pollachius virens","Pollachius pollachius",
                                                       "Trisopterus luscus","Trisopterus minutus","Micromesistius poutassou","Molva molva",
                                                       "Merluccius merluccius","Brosme brosme", "Clupea harengus","Sprattus sprattus",
                                                       "Scomber scombrus","Gadus morhua","Melanogrammus aeglefinus" ,"Merlangius merlangus",
                                                       "Trisopterus esmarkii")))
            survey <- subset(survey, !(BySpecRecCode==3 &
                                       !Species %in% c("Pollachius virens","Pollachius pollachius","Trisopterus luscus","Trisopterus minutus",
                                                       "Micromesistius poutassou","Molva molva", "Merluccius merluccius","Brosme brosme",
                                                       "Clupea harengus","Sprattus sprattus","Scomber scombrus","Gadus morhua",
                                                       "Melanogrammus aeglefinus","Merlangius merlangus","Trisopterus esmarkii")))
            survey <- subset(survey, !(BySpecRecCode==4 &
                                       !Species %in% c("Platichthys flesus","Pleuronectes platessa","Limanda limanda",
                                                       "Lepidorhombus whiffiagoni","Hippoglossus hippoglossus","Hippoglossoides platessoi",
                                                       "Glyptocephalus cynoglossu","Microstomus kitt","Scophthalmus maximus","Scophthalmus rhombus",
                                                       "Solea solea", "Clupea harengus","Sprattus sprattus","Scomber scombrus","Gadus morhua",
                                                       "Melanogrammus aeglefinus","Merlangius merlangus","Trisopterus esmarkii")))
            survey <- subset(survey, !(BySpecRecCode==5 &
                                       !Species %in% c("Ammodytidae","Anarhichas lupus","Argentina silus","Argentina sphyraena",
                                                       "Chelidonichthys cuculus","Callionymus lyra","Eutrigla gurnardus",
                                                       "Lumpenus lampretaeformis", "Mullus surmuletus","Squalus acanthias","Trachurus trachurus",
                                                       "Clupea harengus","Sprattus sprattus","Scomber scombrus","Gadus morhua",
                                                       "Melanogrammus aeglefinus","Merlangius merlangus","Trisopterus esmarkii")))
        }


    }



    ## Some data transformation
    ## ---------
    survey$abstime <- survey$Year+(survey$Month-1)*1/12+(survey$Day-1)/365
    survey$timeOfYear <- (survey$Month-1)*1/12+(survey$Day-1)/365
    survey$ctime <- as.numeric(as.character(survey$Year)) + round(survey$timeOfYear,1)
    survey$TimeShotHour <- as.integer(survey$TimeShot/100) + (survey$TimeShot%%100)/60
    survey$Depth <- replace(survey$Depth, survey$Depth<0, NA)
    ## two different gear sizes are used in the Baltic -
    ##they cannot converted reliably for sensitives, so they are treated as separate surveys in my analysis.
    ## mutate(Survey = if_else(Survey=='BITS' & Gear=='TVS', 'BITSS', Survey)) %>%
    ## mutate(Survey = if_else(Survey=='BITS' & Gear=='TVL', 'BITSL', Survey)) %>%
    survey$Ship <- as.factor(survey$Ship)
    survey$Gear <- as.factor(survey$Gear)
    survey$ShipG <- factor(paste(survey$Ship, survey$Gear, sep=":"))


    ## Remove surveys with issues
    ## -------
    ## Remove NS-IBTS quarter 2 and 4
    survey <- subset(survey, Survey != "NS-IBTS" | (Survey == "NS-IBTS" & Quarter %in% c(1,3)))
    ## IGFS survey only sampled at depths greater than 200m from 2005 to present
    survey <- subset(survey, Survey != "NIGFS" | (Survey != "NIGFS" & Year >= 2006))

    ## TODO: apply these?
    ## !(Survey=='NS-IBTS' & Quarter==1 & Year<1967), ## 0
    ## !(Survey=='SWC-IBTS' & Quarter %in% c(2,3)), ## 3300
    ## !(Survey=='BITSS' & Quarter %in% c(2,3)),  ## 0
    ## !(Survey %in% c('BITS')), ## 667825  ## BITS and Gear not TVS or TVL ## before: removing BITSL, why?
    ## !(Survey=='BTS' & Year<1987),    ## 7618
    ## !(Survey=='IE-IGFS' & Year<2003),  ## 0



    ## Double-check key variables
    ## -------
    any(is.na(survey$Year))
    any(is.na(survey$Lat))
    any(is.na(survey$Lon))
    any(is.na(survey$timeOfYear))
    any(is.na(survey$Depth))
    any(is.na(survey$SweptArea))
    any(survey$SweptArea == 0)
    any(is.na(survey$DayNight))
    any(is.na(survey$DepthStratum))
    length(unique(survey$HaulID[is.na(survey$DepthStratum)])) ## many NAs (useful?)



    ## Zero data
    ## --------------------------------------------------------------
    ## Dummy survey with zeros (important so that all Survey * Year * Quarter combis are included (with 0)),
    ## even if there where no individuals caught
    if(!saflag) survey$SweptArea <- NA
    survey0 <- survey[,c("HaulID", "Survey", "Year", "Month", "Day", "Quarter", "StatRec", "Lat",
                         "Lon", "HaulDur", "Ship", "Gear", "Depth", "DayNight",
                         "SweptArea", "DepthStratum", "ShipG", "GearCat", "Ecoregion", "Area_27",
                         "BySpecRecCode", "TimeShotHour", "abstime", "timeOfYear","ctime")]
    survey0 <- survey0[!duplicated(survey0),]
    survey0$N <- 0
    colnames(survey0)[which(colnames(survey0) == "HaulID")] <- "haul.id"

    dat_sum <- survey0[,c("Survey","Quarter","Year","N")]
    dat_sum <- dat_sum[!duplicated(dat_sum),]
    dat_sum <- aggregate(list(Years=dat_sum$Year),
                         by = list(Survey = dat_sum$Survey, Quarter = dat_sum$Quarter),
                         range)
    dat_sum <- dat_sum[order(dat_sum$Survey),]
    dat_sum[,3] <- apply(dat_sum[,3],1,paste,collapse="-")

    dat_sum2 <- aggregate(list(StatRec=survey0$StatRec),
                          by = list(Survey = survey0$Survey, Quarter = survey0$Quarter),
                          function(x) length(unique(x)))

    print(cbind(dat_sum,StatRec=dat_sum2[,3]))


    ## Day and Night
    ## ---------
    unique(survey$DayNight)
    ## meaningful


    ## HaulDur
    ## ---------
    unique(survey$HaulDur)
    range(survey$HaulDur)
    ## meaningful


    ## Swept Area
    ## ---------
    range(survey$SweptArea)
    ## meaningful


    ## SpecVal
    ## ---------
    ## SpecVal (5,6) only useful for presence-absence
    survey <- subset(survey, SpecVal %in% c(1,10,4,7,5,6))
    unique(survey$SpecVal)
    ind <- which(survey$SpecVal %in% c(5,6))
    tmp <- survey[ind,]

    if(any(colnames(survey) == "Species")){
        tmp <- as.data.frame(table(tmp$Species))
        if(nrow(tmp)>0){
            colnames(tmp) <- c("Species", "Frequency")
            print(tmp)
            writeLines(paste0("Removing these for now."))
            if(length(ind) > 0) survey <- survey[-ind,]
        }
    }


    ## HLNoAtLngt
    ## -------------
    ind <- which(is.na(survey$HLNoAtLngt))
    writeLines(paste0("Entries with HLNoAtLngt == NA: ",length(ind)))
    survey$LngtClass[ind] ## None of them has LngtClass info
    survey$TotalNo[ind]  ## Most of them have still TotalNo info!
    survey$SubFactor[ind]

    tmp <- cbind(paste0(survey$HaulID[ind],"-",survey$AphiaID[ind]),
                 survey$CatIdentifier[ind], survey$TotalNo[ind],
                 survey$NoMeas[ind], survey$SubFactor[ind])

    ## DECISION: Using TotalNo when HLNoAtLngt == NA and setting subFactor to 1.
    ## only two duplicated but Sex different: => summing up okay
    survey[which(paste0(survey$HaulID, "-", survey$AphiaID) %in% tmp[which(duplicated(tmp[,1]))][1]),]
    survey[which(paste0(survey$HaulID, "-", survey$AphiaID) %in% tmp[which(duplicated(tmp[,1]))][2]),]

    survey$HLNoAtLngt[ind] <- survey$TotalNo[ind]
    survey$SubFactor[ind] <- 1


    ## SubFactor
    ## -------------
    ind <- which(is.na(survey$SubFactor))
    writeLines(paste0("Entries with SubFactor == NA: ",length(ind)))
    ind <- which(survey$SubFactor == 0)
    writeLines(paste0("Entries with SubFactor == 0: ",length(ind)))
    range(survey$SubFactor)  ## high values ... realistic? keeping them for now

    ##     unique(survey$Species[survey$SubFactor > 20])

    ## Using HLNoAgeLngt for now
    ## -------------------
    ## account for datatype
    ## https://www.ices.dk/data/Documents/DATRAS/DATRAS_FAQs.pdf:
    ## DataType R,S: TotalNo –report the total number of fish of one species, sex, and category in the given haul
    ## DataType C: TotalNo –report the total number of fish of one species and sex in the given haul, raised to 1 hour hauling;
    survey$multiplier <- ifelse(survey$DataType=="C", survey$HaulDur/60, survey$SubFactor)  ## not using SubFactor if DataType == "C"
    survey$N <- survey$HLNoAtLngt * survey$multiplier
    ## sum up counts
    survey[is.na(survey)] <- "NA"  ## needed because aggregate deletes all NA
    survey$N <- as.numeric(survey$N)

    dontUse <- c("N","multiplier","CatIdentifier","Subfactor","LngtClass","HLNoAtLngt")
    survey <- aggregate(as.formula(
        paste0("N ~ ",
               paste0(colnames(survey)[-which(colnames(survey)%in%dontUse)],
                      collapse=" + "))),
                        function(x) sum(x, na.rm = TRUE), data = survey, na.action = na.pass)
    survey$N <- round(survey$N)
    colnames(survey)[which(colnames(survey) == "HaulID")] <- "haul.id"


    ## check that all entries in survey have N > 0!
    ## if(!all(survey$N > 0)) stop("All surveys should have more than 0 observations. Check data and code!")

    ## Worms list
    ## -----------
    ## creating taxonomy tables for each species
    worms.rec <- try(worrms::wm_record(id = aphia_list[!is.na(aphia_list)]),
                     silent = TRUE)
    if(!inherits(worms.rec,"try-error")){
        str(worms.dat)
        worms.dat <- as.data.frame(worms.rec)[,c("AphiaID","scientificname","genus","family","order","class")]
        any(duplicated(worms.dat$AphiaID))
        survey <- merge(survey, worms.dat, by='AphiaID', all.x=TRUE)
    }

    ## Order
    survey0 <- survey0[order(survey0$Year, survey0$Month, survey0$Day),]
    survey <- survey[order(survey$Year, survey$Month, survey$Day),]


    ## Return
    return(list(survey0 = survey0, survey = survey))

}
