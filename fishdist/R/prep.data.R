#' @name prep.data
#'
#' @title Prepare data for distribution modelling
#'
#' @param data Data set
#' @param AphiaID Aphia IDs with species to select. If provided as column of data
#'     frame, all additional columns are added. If NULL (default), data is not
#'     subsetted for species.
#' @param datras.variables Variables of DATRAS data set to be used
#' @param verbose Print stuff? Default: TRUE
#'
#' @return List containing hh and hl data sets.
#'
#' @importFrom worrms wm_record
#' @importFrom plyr join
#'
#' @export
prep.data <- function(data, AphiaID = NULL,
                      datras.variables = list.datras.variables.req(),
                      verbose = TRUE){

    ## Check validity of data
    ## ------------------
    if(class(data) != "list" || length(data) != 2 || names(data) != c("HH","HL"))
        stop("Function requires data list with two DATRAS data sets 'HH' and 'HL' as elements.")
    hh <- data$HH
    hl <- data$HL


    ## Flags
    ## ------------------
    saflag <- ifelse(any("SweptAreaDSKM2" == colnames(hh)),1,0)
    specflag <- ifelse(is.null(AphiaID[1]) || is.na(AphiaID[1]) || AphiaID[1] %in% c("all","All","ALL"),0,1)



    ## Species subsetting for hl (hh sufficient for survey0)
    ## ------------------
    if(specflag){
        if(!inherits(AphiaID, "data.frame")) specs <- data.frame(AphiaID = AphiaID)
        if(!any(colnames(specs) == "AphiaID"))
            stop("At least one of the columns in AphiaID needs to indicate the Aphia ID and be called 'AphiaID'")

        ## Species that could not be matched
        ## ---------
        flag <- any(specs$AphiaID %in% hl$Valid_Aphia)
        if(!flag) stop("Provided Aphia IDs were not found in the data set. Please check data and/or IDs!")
        ind <- which(!(specs$AphiaID %in% hl$Valid_Aphia))
        if(length(ind) > 0){
            if(verbose){
                writeLines("These Aphia IDs could not be matched:")
                print(specs[ind,])
            }
        }

        ## Subset based on species list
        ## ---------
        hl <- subset(hl, Valid_Aphia %in% specs$AphiaID)
    }


    ## Subset data sets to reduce memory usage and prevent R collapse
    ## ------------------
    ind <- datras.variables[["HH"]]
    if(!saflag) ind <- ind[-which(ind %in% c("SweptAreaDSKM2", "SweptAreaWSKM2", "SweptAreaBWKM2"))]
    hh <- hh[,which(colnames(hh) %in% ind)]


    ## Create haul IDs (SLOW:)
    ## ------------------
    hl$HaulID <- paste(hl$Survey, hl$Year,hl$Quarter, hl$Country, hl$Ship, hl$Gear,
                       hl$StNo, hl$HaulNo, sep = ":")
    hh$HaulID <- paste(hh$Survey, hh$Year,hh$Quarter, hh$Country, hh$Ship, hh$Gear,
                       hh$StNo, hh$HaulNo, sep = ":")


    ## Remove duplicated haul IDs in hh
    ## ------------------
    tmp <- duplicated(hh$HaulID)
    ind <- which(tmp)
    if(length(ind) > 0){
        for(i in 1:length(ind)){
            dups <- hh[which(hh$HaulID==hh$HaulID[ind[i]]),]
            flag <- all(apply(dups,2,function(x) all(x == x[1],na.rm = TRUE)))
            ## writeLines(paste0(i, ": ", flag))
            if(!flag){
                for(j in 2:nrow(dups)){
                    if(verbose){
                        writeLines(paste0(colnames(dups)[which(dups[j,] != dups[1,])]))
                        writeLines(paste0(dups[c(1,j),which(dups[j,] != dups[1,])]))
                    }
                }
            }
        }
        ## Only differences between duplicated HaulIDs in WindSpeed -> delete duplicates
        hh <- hh[-ind,]
    }
    if(verbose) writeLines(paste("All haul IDs unique: ",
                                 checkmark(nrow(hh) == length(unique(hh$HaulID))),
                                 sep="\t\t\t\t\t\t\t\t\t\t\t\t\t\t"))


    ## Remove HaulIDs that are NA
    ## ------------------
    if(any(is.na(hh$HaulID))){
        hh <- hh[-is.na(hh$HaulID),]
    }
    if(any(is.na(hl$HaulID))){
        hl <- hl[-is.na(hl$HaulID),]
    }
    if(verbose) writeLines(paste("All haul IDs meaningful (no NA): ",
                                 checkmark(all(!is.na(hh$HaulID)) && all(!is.na(hl$HaulID))),
                                 sep="\t\t\t\t\t\t\t\t"))


    ## Subset HL, only keep hauls with a length composition
    ## ------------------
    hl <- subset(hl, hl$HaulID %in% hh$HaulID)
    if(verbose) writeLines(paste("All hauls in HL included in HH: ",
                                 checkmark(all(hl$HaulID %in% hh$HaulID)),
                                 sep="\t\t\t\t\t\t\t\t"))



    ## Subset HL (also to avoid double info in hh and hl after merging)
    ## ------------------
    hl <- hl[,c("HaulID","SpecCodeType","SpecCode","SpecVal",
                "TotalNo","CatIdentifier","SubFactor",
                "HLNoAtLngt","Valid_Aphia")] ## "LngtClass"


    ## Merge HH and HL into survey and clean up (SLOW:) alternatives:  plyr::join, sqldf::sqldf
    ## ------------------
    ## survey <- merge(hh, hl, by="HaulID", all = TRUE) ## 158s
    survey <- plyr::join(hh, hl, by="HaulID") ## 16s
    ## survey <- sqldf::sqldf(c("create index ix1 on hh(HaulID)",
    ##                          "select * from main.hh join hl using(HaulID)")) ## 74s
    rm(hh, hl, data)
    gc() ## pryr::mem_used()


    ## Add missing StatRec
    ## -------------
    survey <- predict.statrec(survey)
    if(verbose) writeLines(paste("All StatRec meaningful (no NA): ",
                                 checkmark(all(!is.na(survey$StatRec))),
                                 sep="\t\t\t\t\t\t\t\t"))


    ## Add Ecoregion and Area 27
    ## -------------
    data(ices.rectangles)
    tmp <- ices.rectangles[,c("ICESNAME","Ecoregion","Area_27")]
    colnames(tmp)[1] <- "StatRec"
    ## survey <- merge(survey, tmp, by="StatRec",all.x=TRUE) ## 148s
    survey <- plyr::join(survey, tmp, by="StatRec") ## 27s
    ind <- which(is.na(survey$Ecoregion)) ## StatRec 43E6 has no Ecoregion...


    ## Rename some surveys
    ## -------------
    survey$Survey[which(survey$Survey == "SCOWCGFS")] <- "SWC-IBTS"
    survey$Survey[which(survey$Survey == "SCOROC")] <- "ROCKALL"
    survey$Survey[which(survey$Survey == "BTS-VIII")] <- "BTS"


    ## Remove other gears than TVL and TVS from BITS
    ## -------------
    survey <- subset(survey, Survey != "BITS" |
                             (Survey == "BITS" & Gear %in% c("TVS","TVL")))


    ## Gear Categories
    ## -------------
    survey <- add.gear.categories(survey)
    ## Manual gear corrections
    ## 1. 27.7g and 27.7a
    unique(survey$Area_27)
    ind <- which(survey$Survey == "IE-IGFS" & survey$Gear == "GOV" &
                 survey$Area_27 %in% c("7.g","7.a"))
    if(length(ind) > 0) survey$GearCat[ind] <- "GOV_CL"
    ## 2.
    ind <- which(survey$Survey == "SWC-IBTS" & survey$Gear == "GOV" &
                 survey$ShootLat < 57.5)
    if(length(ind) > 0) survey$GearCat[ind] <- "GOV_CL"
    ## any gear categories NA?
    if(verbose) writeLines(paste("Gear categories succesfully assigned (no NA): ",
                                 checkmark(all(!is.na(survey$GearCat))), sep="\t"))


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

        if(verbose) writeLines(paste("All swept area entries meaningful (no NA): ",
                                     checkmark(all(!is.na(survey$SweptArea))),
                                               sep="\t\t\t"))
    }else{
        survey$SweptArea <- NA
    }
    survey$SweptAreaWSKM2 <- NULL
    survey$SweptAreaBWKM2 <- NULL
    survey$SweptAreaDSKM2 <- NULL

    ## CHECK: some (200000) entries without SweptArea! check in calc.swept.area!


    ## Remove invalid data and clean data set
    ## -----------------------

    ## Convert -9 to NA
    ## ---------
    survey <- minus9toNA(survey)

    ## DataType
    ## ---------
    ind <- which(is.na(survey$DataType))
    if(length(ind) > 0) survey$DataType[ind] <- "Z"
    ## EVHOE datatype is entered as ‘C’ in 2018. This is a mistake it should be ‘R’.
    ind <- which(survey$Survey == "EVHOE" & survey$Year == 2018)
    if(length(ind) > 0) survey$DataType[ind] <- "R"

    ## AphiaID
    ## ---------
    ind <- which(is.na(survey$AphiaID))

    ## HaulVal
    ## ---------
    ind <- which(survey$HaulVal != "V")

    ## SpecVal (1,4,7,10) see: http://vocab.ices.dk/?ref=5
    ## ---------
    ind <- which(!(survey$SpecVal %in% c(1,4,7,10)))

    ## StdSpecRecCode
    ## ---------
    ## http://vocab.ices.dk/?ref=88
    ## 0 	No standard species recorded
    ## 1 	All standard species recorded
    ## 2 	Pelagic standard species recorded
    ## 3 	Roundfish standard species recorded
    ## 4 	Individual standard species recorded
    ind <- which(survey$StdSpecRecCode != 1)
    ## only keep StdSpecRecCode == 1

    ## Lat and Lon
    ## ---------
    ind <- which(is.na(survey$Lat))
    ind <- which(is.na(survey$Lon))

    ## Apply selection
    ## ---------
    survey <- subset(survey, HaulVal == "V" &
                             StdSpecRecCode == 1 &
                             !is.na(Lat) & !is.na(Lon))

    ## Merge species list
    ## --------------------------------------------------------------
    ## Try to connect AphiaID that is NA by using TSN_code
    ## ----------
    ## In historical submissions TSN and NODC species codes were used, which is
    ## reflected in the SpecCodeTypes T and N in older data.
    aphia_list <- as.numeric(unique(survey$AphiaID))

    if(specflag){
        ## Subset relevant columns from species list
        ## ----------
        ind <- which(is.na(specs$AphiaID))
        if(length(ind) > 0) specs <- specs[-ind,]
        ind <- which(duplicated(specs$AphiaID))
        if(length(ind) > 0) specs <- specs[-ind,]

        ## Merge species list to survey
        ## ----------
        survey <- plyr::join(survey, specs, by="AphiaID")

        ## Species and bycatch corrections
        ## ------------
        survey <- correct.species(survey)
    }


    ## Some data transformation
    ## ---------
    survey$abstime <- survey$Year+(survey$Month-1)*1/12+(survey$Day-1)/365
    survey$timeOfYear <- (survey$Month-1)*1/12+(survey$Day-1)/365
    survey$ctime <- as.numeric(as.character(survey$Year)) + round(survey$timeOfYear,1)
    if("TimeShot" %in% colnames(survey))
        survey$TimeShotHour <- as.integer(survey$TimeShot/100) + (survey$TimeShot%%100)/60
    survey$Depth <- replace(survey$Depth, survey$Depth<0, NA)
    survey$Ship <- as.factor(survey$Ship)
    survey$Gear <- as.factor(survey$Gear)
    survey$ShipG <- factor(paste(survey$Ship, survey$Gear, sep=":"))


    ## Remove surveys with issues
    ## -------
    ## Remove NS-IBTS quarter 2 and 4
    ind <- which(survey$Survey == "NS-IBTS" & survey$Quarter %in% c(2,4))
    if(length(ind) > 0) survey <- survey[-ind,]
    ## IGFS survey only sampled at depths greater than 200m from 2005 to present
    ind <- which(survey$Survey == "NIGFS" & survey$Year < 2005)
    if(length(ind) > 0) survey <- survey[-ind,]
    ## CHECK: With survey experts
    ## length(which(survey$Survey=='SWC-IBTS' & survey$Quarter %in% c(2,3))) ## 3300
    ## length(which(survey$Survey=='BTS' & survey$Year<1987))    ## 7618


    ## Double-check key variables
    ## -------
    ## Key variables not NA
    flag <- all(!is.na(survey$Year)) && all(!is.na(survey$Lat)) &&
    all(!is.na(survey$Lon)) &&
    all(!is.na(survey$timeOfYear)) &&
    all(!is.na(survey$Depth))
    ## all(!is.na(survey$SweptArea)) && ## CHECK: why some Swept Area NA? pattern? remove?
    ## all(survey$SweptArea > 0))
    if(verbose) writeLines(paste("Meaningful key variables (no NA): ",
                                 checkmark(flag), sep=":\t\t\t\t\t\t\t"))


    ## Zero data (includes all hauls independent of caught species)
    ## --------------------------------------------------------------
    survey0 <- survey[,c("HaulID", "Survey", "Year", "Month", "Day", "Quarter",
                         "StatRec", "Lat", "Lon", "HaulDur", "Ship", "Gear",
                         "Depth", "SweptArea", "ShipG", "GearCat", "Ecoregion",
                         "Area_27", "BySpecRecCode", "abstime", "timeOfYear",
                         "ctime")]
    if("TimeShotHour" %in% colnames(survey)) survey0$TimeShotHour <- survey$TimeShotHour
    ind <- duplicated(survey0$HaulID)
    survey0 <- survey0[!ind,]
    survey0$N <- 0
    colnames(survey0)[which(colnames(survey0) == "HaulID")] <- "haul.id" ## required by surveyIdx package


    ## SpecVal (5,6) only useful for presence-absence
    ## ---------
    survey <- subset(survey, SpecVal %in% c(1,10,4,7,5,6)) ## REMOVE: 5,6?
    unique(survey$SpecVal)

    ## HERE: CHECK: this
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

    ## HERE: REMOVE:?
    if(FALSE){
    tmp <- cbind(paste0(survey$HaulID[ind],"-",survey$AphiaID[ind]),
                 survey$CatIdentifier[ind], survey$TotalNo[ind],
                 survey$NoMeas[ind], survey$SubFactor[ind])
    ## only two duplicated but Sex different: => summing up okay
    survey[which(paste0(survey$HaulID, "-", survey$AphiaID) %in% tmp[which(duplicated(tmp[,1]))][1]),]
    survey[which(paste0(survey$HaulID, "-", survey$AphiaID) %in% tmp[which(duplicated(tmp[,1]))][2]),]
    }

    ## DECISION: Using TotalNo when HLNoAtLngt == NA and setting subFactor to 1.
    if(verbose && length(ind) > 0){
        survey$HLNoAtLngt[ind] <- survey$TotalNo[ind]
        survey$SubFactor[ind] <- 1
        ind2 <- which(!is.na(survey$TotalNo[ind]))
        if(verbose) writeLines(paste0(paste("Number of entries with missing HLNoAtLngt: ",
                                            paste0(length(ind), " (",round(length(ind)/nrow(survey)*100,1),
                                                   "%)"), sep = "\t\t\t"),
                                      " Using TotalNo (with SubFactor = 1) for these hauls."))
        writeLines(paste("Number of meaningful TotalNo replacements: ",
                                 paste0(length(ind2), " (",round(length(ind2)/length(ind)*100,1),
                                 "%)"), sep = "\t\t\t"))
    }


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

    dontUse <- c("N","multiplier","CatIdentifier","SubFactor","LngtClass","HLNoAtLngt",
                 "TotalNo","DataType","SpecVal","Sex","HaulVal","StdSpecRecCode")
    survey <- aggregate(as.formula(
        paste0("N ~ ",
               paste0(colnames(survey)[-which(colnames(survey) %in% dontUse)],
                      collapse=" + "))),
                        function(x) sum(x, na.rm = TRUE), data = survey, na.action = na.pass)
    survey$N <- round(survey$N)
    colnames(survey)[which(colnames(survey) == "HaulID")] <- "haul.id" ## required by surveyIdx package

    dim(survey)

    flag <- all(!is.na(survey$N))
    if(verbose) writeLines(paste("Meaningful N estimated for all hauls: ",
                                 checkmark(flag),
                                 sep = "\t\t\t\t\t"))



    ## Worms list
    ## -----------
    ## creating taxonomy tables for each species
    aphia_list2 <- aphia_list[!is.na(aphia_list)]
    nspec <- length(aphia_list2)
    seqi <- seq(1, nspec + 100, 50)
    worms.rec <- NULL
    for(i in 1:ceiling(nspec/50)){  ## Seems that only 50 species can be downloaded at a time
        endind <- ifelse(seqi[i+1]-1 > nspec, nspec, seqi[i+1]-1)
        tmp <- try(worrms::wm_record(id = aphia_list2[seqi[i]:endind]),
                         silent = TRUE)
        if(!inherits(tmp,"try-error")){
            worms.rec <- rbind(worms.rec, tmp)
        }else{
            if(verbose) writeLines(paste("There was a problem downloading information from the Worms list."))
        }
    }
    if(!inherits(worms.rec,"try-error") && !is.null(worms.rec)){
        worms.dat <- as.data.frame(worms.rec)[,c("AphiaID","scientificname","genus","family","order","class")]
        survey <- plyr::join(survey, worms.dat, by='AphiaID')
    }
    ## save(worms.dat, file="worms.dat.rda", version = 2)
    ## TODO: load worms.dat and try to find aphiaID in there, then only download once that were not found!


    ## Order
    ## -----------
    survey0 <- survey0[order(survey0$Year, survey0$Month, survey0$Day),]
    survey <- survey[order(survey$Year, survey$Month, survey$Day),]


    ## Return
    ## -----------
    return(list(survey0 = survey0, survey = survey))
}
