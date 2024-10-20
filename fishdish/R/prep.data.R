#' @name prep.data
#'
#' @title Prepare data for distribution modelling
#'
#' @param data Data set
#' @param AphiaID Aphia IDs with species to select. If provided as column of data
#'     frame, all additional columns are added. If NULL (default), data is not
#'     subsetted for species.
#' @param datras.variables Variables of DATRAS data set to be used
#' #' @param verbose Print stuff? Default: TRUE
#'
#' @return List containing hh and hl data sets.
#'
#' @importFrom worrms wm_record
#' @importFrom plyr join
prep.data.internal <- function(data, AphiaID = NULL,
                               datras.variables = list.datras.variables.req(),
                               use.total.catch.w.and.n = TRUE,
                               verbose = TRUE){

    specflag <- ifelse(is.null(AphiaID[1]) || is.na(AphiaID[1]) || AphiaID[1] %in% c("all","All","ALL"),0,1)

    hh <- data$HH
    hl <- data$HL
    ca <- data$CA
    rm(data)

    ## Flags
    ## ------------------
    saflag <- ifelse(any("SweptAreaDSKM2" == colnames(hh)),1,0)

    ## Species subsetting for hl (full hh needed for survey0)
    ## ------------------
    if(as.logical(specflag)){
        if(!inherits(AphiaID, "data.frame")) specs <- data.frame(AphiaID = AphiaID)
        if(!any(colnames(specs) == "AphiaID"))
            stop("At least one of the columns in AphiaID needs to indicate the Aphia ID and be called 'AphiaID'")

        ## Species that could not be matched
        ## ---------
        flag <- any(specs$AphiaID %in% hl$AphiaID)
        unique(hl$AphiaID)
        if(!flag) stop("Provided Aphia IDs were not found in the data set. Please check data and/or IDs!")
        ind <- which(!(specs$AphiaID %in% hl$AphiaID))
        if(length(ind) > 0){
            if(verbose){
                writeLines("These Aphia IDs could not be matched:")
                print(specs[ind,])
            }
        }

        ind <- which(specs$AphiaID %in% hl$AphiaID)
        if(length(ind) > 0){
            specs.matched <- data.frame(AphiaID = specs[ind,])
        }else{
            stop()
        }

        ## Subset based on species list
        ## ---------
        hl <- subset(hl, AphiaID %in% specs.matched$AphiaID)
        if(!is.null(ca)) ca <- subset(ca, AphiaID %in% specs.matched$AphiaID)
    }else{
        specs.matched <- data.frame(AphiaID = unique(hl$AphiaID))
    }


    ## Subset data sets to reduce memory usage and prevent R collapse
    ## ------------------
    ind <- datras.variables[["HH"]]
    if(!saflag) ind <- ind[-which(ind %in% c("SweptAreaDSKM2",
                                             "SweptAreaWSKM2",
                                             "SweptAreaBWKM2"))]
    if(!"GearEx" %in% colnames(hh)){
        hh$GearEx <- hh$Gear
    }
    ind <- c(ind, "GearEx")
    hh <- hh[,which(colnames(hh) %in% ind)]


    ## Create haul IDs (SLOW:)
    ## ------------------
    hl$HaulID <- get.haul.id(hl)
    hh$HaulID <- get.haul.id(hh)
## paste(hl$Survey, hl$Year, hl$Quarter, hl$Country, hl$Ship, hl$Gear,
##                        hl$StNo, hl$HaulNo, sep = ":")

    if(!is.null(ca)){
        ca$HaulID <- paste(ca$Survey, ca$Year, ca$Quarter, ca$Country, ca$Ship, ca$Gear,
                           ca$StNo, ca$HaulNo, sep = ":")
    }

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
    if(verbose) writeLines(paste(checkmark(nrow(hh) == length(unique(hh$HaulID))),
                                 "All haul IDs unique",
                                 sep="\t\t\t"))


    ## Remove HaulIDs that are NA
    ## ------------------
    if(any(is.na(hh$HaulID))){
        hh <- hh[-is.na(hh$HaulID),]
    }
    if(any(is.na(hl$HaulID))){
        hl <- hl[-is.na(hl$HaulID),]
    }
    if(!is.null(ca)){
        if(any(is.na(ca$HaulID))){
            ca <- ca[-is.na(ca$HaulID),]
        }
    }
    if(verbose) writeLines(paste(checkmark(all(!is.na(hh$HaulID)) && all(!is.na(hl$HaulID))),
                                 "All haul IDs meaningful (no NA)",
                                 sep="\t\t\t"))


    ## Subset HL, only keep hauls that can be matched to HH later
    ## ------------------
    hl <- subset(hl, hl$HaulID %in% hh$HaulID)
    if(verbose) writeLines(paste(checkmark(all(hl$HaulID %in% hh$HaulID)),
                                 "All hauls in HL included in HH",
                                 sep="\t\t\t"))
    if(!is.null(ca)) ca <- subset(ca, ca$HaulID %in% hh$HaulID)

    ## Subset HL (also to avoid double info in hh and hl after merging)
    ## ------------------
    hl <- hl[,c("HaulID","SpecCodeType","SpecCode","SpecVal",
                "TotalNo", ## "CatCatchWgt",
                "CatIdentifier","SubFactor", "LngtCode",
                "LngtClass","HLNoAtLngt","AphiaID")]

    ## Subset CA (also to avoid double info in hh and ca after merging)
    ## ------------------
    if(!is.null(ca)){
        ca <- ca[,c("HaulID","LngtCode","LngtClass","Sex","Maturity",
                    "PlusGr","Age","CANoAtLngt","IndWgt","MaturityScale",
                    "AphiaID")]
    }


    ## Add StatRec, SubStatRec, Ecoregion and Area_27 (before: add missing StatRec)
    ## -------------
    oldStatRec <- hh$StatRec
    hh$StatRec <- NULL
    hh <- pred.statrec(hh, verbose = verbose)
    ind <- which(is.na(hh$StatRec))
    if(length(ind) > 0){
        if(verbose) writeLines("Removing entries that could not be matched to any statistical rectangle.")
        hh <- hh[-ind,]
    }
    if(verbose) writeLines(paste(checkmark(all(!is.na(hh$StatRec))),
                                 "All StatRec meaningful (no NA)",
                                 sep="\t\t\t"))

    ## REMOVE:
    if(FALSE){
        ## Add Ecoregion and Area 27
        ## -------------
        data(ices.rectangles)
        tmp <- ices.rectangles[,c("ICESNAME","Ecoregion","Area_27")]
        colnames(tmp)[1] <- "StatRec"
        ## survey <- merge(survey, tmp, by="StatRec",all.x=TRUE) ## 148s
        hh <- plyr::join(hh, tmp, by="StatRec") ## 27s
        ind <- which(is.na(hh$Ecoregion)) ## StatRec 43E6 has no Ecoregion...
    }


    ## Some data transformation
    ## ---------
    hh$abstime <- hh$Year+(hh$Month-1)*1/12+(hh$Day-1)/365
    hh$timeOfYear <- (hh$Month-1)*1/12+(hh$Day-1)/365
    hh$ctime <- as.numeric(as.character(hh$Year)) + round(hh$timeOfYear,1)
    hh$Depth <- replace(hh$Depth, hh$Depth<0, NA)
    hh$Ship <- as.factor(hh$Ship)
    hh$Gear <- as.factor(hh$Gear)
    hh$ShipG <- factor(paste(hh$Ship, hh$Gear, sep=":"))

    ## Convert TimeShot to time of day (ToD)
    ## Remove hauls with incorrect time of day entry
    ## ind <- which(nchar(hh$TimeShot) < 3)
    ## if(length(ind) > 0){
    ##     hh <- hh[-ind,]
    ## }
    ## Assume that first character is 0 if missing
    ind <- which(nchar(hh$TimeShot) == 3)
    if(length(ind) > 0){
        hh$TimeShot[ind] <- paste0(0, hh$TimeShot[ind])
    }
    ## Assume that first character is 0 if missing
    ind <- which(nchar(hh$TimeShot) == 2)
    if(length(ind) > 0){
        hh$TimeShot[ind] <- paste0(0, 0, hh$TimeShot[ind])
    }
    ## Assume that first character is 0 if missing
    ind <- which(nchar(hh$TimeShot) == 1)
    if(length(ind) > 0){
        hh$TimeShot[ind] <- paste0(0, 0, 0, hh$TimeShot[ind])
    }
    ## Convert to time
    hh$time <- strptime(hh$TimeShot, format = "%H%M")
    ## check
    head(cbind(hh$TimeShot, format(hh$time,"%H:%M")))
    range(hh$time)

    ## Create time of day (using local time zones)
    hh$ToD <- as.numeric(format(hh$time, format = "%H")) +
        (as.numeric(format(hh$time, format = "%M"))/60)

    ## check
    range(hh$ToD)

    if("TimeShot" %in% colnames(hh))
        hh$TimeShotHour <- as.integer(as.numeric(hh$TimeShot)/100) +
            (as.numeric(hh$TimeShot)%%100)/60

    ## Add 0 to months with one digit
    hh$Month_char <- hh$Month
    ind <- which(nchar(hh$Month) == 1)
    if(length(ind) > 0){
        hh$Month_char[ind] <- paste0(0, hh$Month[ind])
    }
    ## Add 0 to days with one digit
    hh$Day_char <- hh$Day
    ind <- which(nchar(hh$Day) == 1)
    if(length(ind) > 0){
        hh$Day_char[ind] <- paste0(0, hh$Day[ind])
    }

    ## Add time zone
    hh$timezone <- lutz::tz_lookup_coords(lat = hh$lat, lon = hh$lon,
                                          method = "accurate")

    ## Date columns
    hh$date <- paste0(hh$Year,"-",hh$Month_char,"-",hh$Day_char)
    ## CHECK: account for the fact that area covers different time zones?
    hh$datetime <- as.POSIXct(paste(hh$date, format(hh$time,"%H:%M")),
                              format = "%Y-%m-%d %H:%M")

    ## day of year
    hh$DoY <- as.numeric(format(hh$datetime, format = "%j"))
    ## check
    range(hh$DoY)


    ## CHECK: necessary to account for time zones?
    ## hh$datetime <- rep("", length(hh$timezone))
    ## hh$timezone2 <- sapply(strsplit(hh$timezone, ";"), "[[", 1)
    ## tzs <- unique(hh$timezone2)
    ## for(i in 1:length(tzs)){
    ##     ind <- which(hh$timezone2 == tzs[i])
    ##     hh$datetime[ind] <- as.character(as.POSIXct(paste(hh$date[ind], format(hh$time[ind],"%H:%M")),
    ##                           format = "%Y-%m-%d %H:%M", tz = tzs[i]))
    ## }
    ## test <- lubridate::with_tz(hh$datetime, hh$timezone2)
    ## hh$datetimeOneTZ <- purrr::map2(.x = hh$datetime,
    ##                                 .y = hh$timezone,
    ##                                 .f = function(x, y) {lubridate::with_tz(time = x, tzone = y)})


    ## Overwrite depth
    ## -------------
    if(any(colnames(hh) == "Depth_gam")) hh$Depth <- hh$Depth_gam
    hh$Depth_gam <- NULL


    ## Combine swept area
    ## -------------
    if(saflag){
        hh$SweptArea <- hh$SweptAreaBWKM2
        ## WKABSENS: use wing spread index! Don't use both!
        hh$SweptArea[is.na(hh$SweptArea)] <- hh$SweptAreaWSKM2[is.na(hh$SweptArea)]
        ## hh$SweptArea[is.na(hh$SweptArea)] <- hh$SweptAreaDSKM2[is.na(hh$SweptArea)]
        if(verbose) writeLines(paste(checkmark(all(!is.na(hh$SweptArea))),
                                     "All swept area entries meaningful (no NA)",
                                     sep="\t\t\t"))
    }else{
        hh$SweptArea <- NA
    }
    hh$SweptAreaWSKM2 <- NULL
    hh$SweptAreaBWKM2 <- NULL
    hh$SweptAreaDSKM2 <- NULL



    ## Remove invalid data and clean data set
    ## -----------------------

    ## Convert -9 to NA
    ## ---------
    hh <- minus9toNA(hh)
    hl <- minus9toNA(hl)
    if(!is.null(ca)) ca <- minus9toNA(ca)

    ## TODO: CHECK:
    ## Check SpecCodeType (hl)
    ## T 	TSN code - should not be used for data submissions
    ## W 	WoRMS AphiaID code


    ## DataType (http://vocab.ices.dk/?ref=9)
    ## ---------
    ## -9   Invalid hauls
    ##  C 	Data calculated as CPUE (number per hour)
    ##  P 	Pseudocategory sampling
    ##  R 	Data by haul
    ##  S 	Sub sampled data
    ## ---------
    ## Remove NA
    ind <- which(is.na(hh$DataType))
    if(length(ind) > 0) hh <- hh[-ind,]
    ## EVHOE datatype is entered as ‘C’ in 2018. This is a mistake it should be ‘R’.
    ind <- which(hh$Survey == "EVHOE" & hh$Year == 2018)
    if(length(ind) > 0) hh$DataType[ind] <- "R"
    if(any(hh$DataType == "S")){
        if(verbose) warning("DataType 'S' found in length data. These hauls will be interpreted as DataType 'R' wrt. total numbers caught.")
    }
    if(any(hh$DataType == "P")){
        if(verbose) warning("DataType 'P' found in length data. These hauls will be interpreted as DataType 'R' wrt. total numbers caught.")
    }
    ## ind <- which(hh$DataType == "S")
    ## ind2 <- which(hl$HaulID %in% hh$HaulID[ind])
    ## all(aggregate(hl$TotalNo[ind2], by = list(hl$HaulID[ind2]), unique) == aggregate(hl$HLNoAtLngt[ind2], by = list(hl$HaulID[ind2]), sum))
    ## Interpret all P and S as R! (write message)


    ## AphiaID
    ## ---------
    ind <- which(is.na(hl$AphiaID))

    ## HaulVal
    ## ---------
    ind <- which(!hl$HaulVal %in% c("A","V"))

    ## SpecVal (1,4,7,10) see: http://vocab.ices.dk/?ref=5
    ## ---------
    ind <- which(!(hl$SpecVal %in% c(1,4,7,10)))

    ## StdSpecRecCode
    ## ---------
    ## http://vocab.ices.dk/?ref=88
    ## 0 	No standard species recorded
    ## 1 	All standard species recorded
    ## 2 	Pelagic standard species recorded
    ## 3 	Roundfish standard species recorded
    ## 4 	Individual standard species recorded
    ind <- which(hh$StdSpecRecCode != 1)
    ## only keep StdSpecRecCode == 1

    ## lat and lon
    ## ---------
    ind <- which(is.na(hh$lat))
    ind <- which(is.na(hh$lon))


    ## Apply selection
    ## ---------
    hh <- subset(hh, HaulVal %in% c("A","V") &
                     StdSpecRecCode == 1 &
                     !is.na(lat) & !is.na(lon))
    ## if(saflag) hh <- subset(hh, !is.na(SweptArea))


    ## Surveys
    ## -------------
    hh <- correct.surveys(hh)

    ## Gear Categories
    ## -------------
    hh <- add.gear.categories(hh)
    ## any gear categories NA?
    if(verbose) writeLines(paste(checkmark(all(!is.na(hh$GearCat))),
                                 "Gear categories succesfully assigned (no NA)",
                                 sep="\t\t\t"))


    ## Double-check key variables
    ## -------
    ## Key variables not NA
    flag <- all(!is.na(hh$Year)) && all(!is.na(hh$lat)) &&
        all(!is.na(hh$lon)) &&
        all(!is.na(hh$timeOfYear)) &&
        all(!is.na(hh$Depth)) &&
        (!saflag || (all(!is.na(hh$SweptArea)) && all(hh$SweptArea > 0)))
    if(verbose) writeLines(paste(checkmark(flag),
                                 "Meaningful key variables (no NA)",
                                 sep="\t\t\t"))

    ## Zero data (includes all hauls independent of caught species)
    ## --------------------------------------------------------------
    survey0 <- hh[,c("HaulID", "Survey", "Year", "Month", "Day", "Quarter",
                     "StatRec", "lat", "lon", "HaulDur", "Ship", "Gear","Country",
                     "DayNight", "DataType",
                     "ToD","DoY",
                     "Depth", "SweptArea", "ShipG", "GearCat", "Ecoregion",
                     "Area_27", "BySpecRecCode", "abstime", "timeOfYear",
                     "ctime")]
    ## add optional variables
    if("TimeShotHour" %in% colnames(hh)) survey0$TimeShotHour <- hh$TimeShotHour
    ind <- duplicated(survey0$HaulID)
    survey0 <- survey0[!ind,]


    ## Data sets including species
    ## --------------------------------------------
    ## Merge variables from hh needed in hl
    hl <- plyr::join(hl, hh[,c("HaulID","BySpecRecCode","HaulDur","DataType",
                               "GearEx")],
                     by = "HaulID") ## 16s
    rm(hh)

    ## Check DataType in hl
    ## ---------
    ind <- which(is.na(hl$DataType))
    if(length(ind) > 0) hl <- hl[-ind,]


    ## Check HaulDur in hl
    ## ---------
    ind <- which(is.na(hl$HaulDur))
    if(length(ind) > 0) hl <- hl[-ind,]



    ## CHECK: write to ICES about this:
    if(FALSE){
        xtabs( ~ DataType =="C" & SubFactor>1,hl)
        ind <- which(hl$DataType == "C" & hl$SubFactor > 1)
        hl[ind,]
    }


    ## Merge species list
    ## --------------------------------------------------------------
    ## Try to connect AphiaID that is NA by using TSN_code
    ## ----------
    ## In historical submissions TSN and NODC species codes were used, which is
    ## reflected in the SpecCodeTypes T and N in older data.
    ## TODO: account for this

    if(specflag){
        ## Subset relevant columns from species list
        ## ----------
        ind <- which(is.na(specs.matched$AphiaID))
        if(length(ind) > 0) specs.matched <- specs.matched[-ind,]
        if(!inherits(specs.matched,"data.frame")) specs.matched <- data.frame(AphiaID = specs.matched)
        ind <- which(duplicated(specs.matched$AphiaID))
        if(length(ind) > 0) specs.matched <- specs.matched[-ind,]
        if(!inherits(specs.matched,"data.frame")) specs.matched <- data.frame(AphiaID = specs.matched)
        ## Merge species list to survey
        ## ----------
        hl <- plyr::join(hl, specs.matched, by="AphiaID")
        ## Species and bycatch corrections
        ## ------------
        hl <- correct.species(hl)
    }

    ## SpecVal (5,6) only useful for presence-absence
    ## ---------
    ## CHECK: REMOVE: 5,6?
    hl <- subset(hl, SpecVal %in% c(1,10,4,7,5,6))
    unique(hl$SpecVal)

    ## CHECK: this:
    ind <- which(hl$SpecVal %in% c(5,6))
    tmp <- hl[ind,]
    if(any(colnames(hl) == "Species")){
        tmp <- as.data.frame(table(tmp$Species))
        if(nrow(tmp)>0){
            colnames(tmp) <- c("Species", "Frequency")
            print(tmp)
            writeLines(paste0("Removing these for now."))
            if(length(ind) > 0) hl <- hl[-ind,]
        }
    }

    ## HLNoAtLngt
    ## -------------
    ind <- which(is.na(hl$HLNoAtLngt))
    if(!use.total.catch.w.and.n){
        ## NEW: DECISION: removing HLNoAtLngt = NA entries
        if(length(ind) > 0){
            hl <- hl[-ind,]
            writeLines(paste0(length(ind), " entries do not have HLNoAtlngt information are removed!"))
        }
    }else{
        hl$HLNoAtLngt[ind] <- hl$TotalNo[ind]
        hl$SubFactor[ind] <- 1
        ## all length NA for these entries, so if length is being used later than they are removed anyways
        ## TODO: keep CatCatchWgt also for est.bio!
    }

    ## Account for LngtCode (mm and cm)
    ## Remove LngtCode = NA (LngtClass also NA)
    ind <- which(is.na(hl$LngtCode))
    if(!use.total.catch.w.and.n){
        if(length(ind) > 0){
            hl <- hl[-ind,]
            writeLines(paste0(length(ind), " entries do not have LngtCod information are removed!"))
        }
    }


    hl$LngtCm <- NA
    ind <- which(!is.na(hl$LngtCode))
    ## DATRAS:::getAccuracyCM
    lngt2cm <- c("." = 0.1, "0" = 0.1, "1" = 1, "2" = 1, "5" = 1)[as.character(hl$LngtCode[ind])] ## 6,7 for NO shrimp survey
    hl$LngtCm[ind] <- lngt2cm * hl$LngtClass[ind]
    range(hl$LngtCm, na.rm = TRUE)
    ## DATRAS::addSpectrum

    ## https://www.ices.dk/data/Documents/DATRAS/DATRAS_FAQs.pdf:
    ## DataType R,S: TotalNo –report the total number of fish of one species, sex, and category in the given haul
    ## DataType C: TotalNo –report the total number of fish of one species and sex in the given haul, raised to 1 hour hauling;
    hl$multiplier <- ifelse(hl$DataType=="C", hl$HaulDur/60, hl$SubFactor)  ## not using SubFactor if DataType == "C"

    hl$Counts <- as.numeric(hl$HLNoAtLngt * hl$multiplier)

    ## Account for double beams
    if(any(colnames(hl) == "GearEx")){
        ind <- which(!is.na(hl$GearEx) & hl$GearEx == "DB")
        if(length(ind) > 0) hl$Counts[ind] <- hl$Counts[ind] * 2
    }

    ## TESTING: check if D has an effect (double sweeps)

    ## CHECK: this!

    ## Remove hauls where datatype = C and Subfactor != 1
    ind <- which(hl$DataType == "C" & hl$SubFactor != 1)
    if(length(ind) > 0){
        if(verbose) writeLines(paste0(length(ind), " hauls are data type 'C' and SubFactor is not equal to 1. This is contrary to the DATRAS manual. Removing these hauls!"))
        hl <- hl[-ind,]
    }
    ## HERE: send these to ICES


    ## Potential dangerous to remove large hauls!
    ## ## Considered outlier and removed!
    ## ## NEW: DECISION: CHECK:
    ## ## absolute number does not work, but quantile?
    ## quant.cut <- round(quantile(hl$SubFactor, prob = c(0.9999), na.rm = TRUE),2)
    ## ind <- which(hl$SubFactor > quant.cut)
    ## if(length(ind) > 0){
    ##     if(verbose) writeLines(paste0("Cutting SubFactor at 99.99% percentile which is equal to: ", quant.cut, ". This cuts ", length(ind)," data points."))
    ##     hl <- hl[-ind,]
    ## }

    ## ##  hist(hl$LngtCm)
    ## ## NEW: DECISION: CHECK:
    ## quant.cut <- round(quantile(hl$Counts, prob = c(0.9999), na.rm = TRUE),2)
    ## ind <- which(hl$Counts > quant.cut)
    ## if(length(ind) > 0){
    ##     if(verbose) writeLines(paste0("Cutting Counts at 99.99% percentile which is equal to: ", quant.cut, ". This cuts ", length(ind)," data points."))
    ##     hl <- hl[-ind,]
    ## }

    ## Remove columns that are not needed any longer
    hl$LngtClass <- hl$HLNoAtLngt <- hl$DataType <- NULL

    ## Correct specs.matched again after all these data cleaning steps
    ind.remove <- NULL
    for(i in 1:nrow(specs.matched)){
        if(sum(hl$AphiaID == specs.matched$AphiaID[i]) == 0){
            ind.remove <- c(ind.remove,i)
        }
    }
    if(length(ind.remove) > 0){
        specs.matched <- specs.matched[-ind.remove,]
        if(!inherits(specs.matched,"data.frame")) specs.matched <- data.frame(AphiaID = specs.matched)
    }

    res <- list(hl = hl,
                ca = ca,
                specs.matched = specs.matched,
                survey0 = survey0)

    return(res)
}



#' @name prep.data
#'
#' @title Prepare data for distribution modelling
#'
#' @param data Data set
#' @param AphiaID Aphia IDs with species to select. If provided as column of data
#'     frame, all additional columns are added. If NULL (default), data is not
#'     subsetted for species.
#' @param datras.variables Variables of DATRAS data set to be used
#' @param use.ca Use CA data for AphiaID to estimate bio?
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
                      est.n = TRUE, est.bio = FALSE,
                      split.juv.adults = FALSE,
                      split.length = FALSE,
                      use.ca = TRUE,
                      use.total.catch.w.and.n = TRUE,
                      select = NULL,
                      overlap = NULL,
                      verbose = TRUE){

    specflag <- ifelse(is.null(AphiaID[1]) || is.na(AphiaID[1]) || AphiaID[1] %in% c("all","All","ALL"),0,1)

    ## Check validity of data
    ## ------------------
    if((!inherits(data,"fdist.datras") && !inherits(data,"fdist.prepped")) ||
       (!all(c("HH","HL") %in% names(data)) && !all(c("survey0","survey") %in% names(data))))
        stop("Function requires list (of class fdist.datras) with two DATRAS data sets 'HH' and 'HL' as elements (see download.data) or list (of class fdist.prepped) with two survey data sets 'survey0' and 'survey' as elements (see prep.data).")

    if(inherits(data,"fdist.datras")){

        ## Option to subset data
        if(!is.null(select)){
            for(j in 1:length(names(select))){
                data$HH <- data$HH[data$HH[[names(select)[j]]] %in% select[[j]],]
                data$HL <- data$HL[data$HL[[names(select)[j]]] %in% select[[j]],]
                data$CA <- data$CA[data$CA[[names(select)[j]]] %in% select[[j]],]
            }
        }

        ## Prepare data set
        data.prepped <- prep.data.internal(data = data, AphiaID = AphiaID,
                                           datras.variables = datras.variables,
                                           use.total.catch.w.and.n =
                                               use.total.catch.w.and.n,
                                           verbose = verbose)

        ## Option to choose entries that overlap
        if(!is.null(overlap)){
            ## HL
            survi <- sapply(strsplit(data.prepped$hl$HaulID, ":"), function(x) x[[1]])
            spliti <- split(data.prepped$hl, survi)
            for(j in 1:length(overlap)){
                sel <- unlist(lapply(spliti, function(x) unique(x[[overlap[j]]])))
                sel <- as.character(sel[duplicated(sel)])
                for(k in 1:length(spliti)){
                    spliti[[k]] <- spliti[[k]][spliti[[k]][[overlap[j]]] %in% sel,]
                }
            }
            data.prepped$hl <- do.call(rbind, spliti)
            rownames(data.prepped$hl) <- NULL

            ## CA
            survi <- sapply(strsplit(data.prepped$ca$HaulID, ":"), function(x) x[[1]])
            spliti <- split(data.prepped$ca, survi)
            for(j in 1:length(overlap)){
                sel <- unlist(lapply(spliti, function(x) unique(x[[overlap[j]]])))
                sel <- as.character(sel[duplicated(sel)])
                for(k in 1:length(spliti)){
                    spliti[[k]] <- spliti[[k]][spliti[[k]][[overlap[j]]] %in% sel,]
                }
            }
            data.prepped$ca <- do.call(rbind, spliti)
            rownames(data.prepped$ca) <- NULL

        }

        hl <- data.prepped$hl
        ca <- data.prepped$ca
        specs.matched <- data.prepped$specs.matched
        survey0 <- data.prepped$survey0


        ## Estimate N & Bio (requires CA)
        ## -------------------
        if(est.n){

            survey <- NULL
            nspec <- nrow(specs.matched)
            for(i in 1:nspec){

                hlc <- subset(hl, AphiaID == specs.matched$AphiaID[i])
                unique(hlc$AphiaID)
                if(nrow(hlc) == 0){
                    stop(paste0("No entries in HL after data prep for aphia ID:",
                                specs.matched$AphiaID[i],". Cannot calc n!"))
                }


                ## NEW: account for some lengths = NA (Counts = TotalNo)
                ind <- which(!is.na(hlc$LngtCode))

                if(length(ind) == 0){
                    if(split.juv.adults || split.length){
                        stop(paste0("Species ",specs.matched$AphiaID[i],
                                    " does not have any length information. Cannot split into length classes! Run total N/bio or remove species!"))
                    }else{
                        tmpi <- hlc[, c("HaulID","AphiaID","Counts")]
                        survey.spec <- aggregate(list(N = tmpi$Counts),
                                           by = list(haul.id = tmpi$HaulID,
                                                     AphiaID = tmpi$AphiaID),
                                           FUN = sum)

                        ## TODO: remove NA?
                    }
                }else{

                    ## DATRAS::addSpectrum
                    ## DATRAS:::getAccuracyCM
                    by <- max(c("." = 0.1, "0" = 0.5, "1" = 1, "2" = 2, "5" = 5)[as.character(hlc$LngtCode[ind])],
                              na.rm = TRUE)
                    cm.breaks <- seq(min(hlc$LngtCm[ind], na.rm = TRUE),
                                     max(hlc$LngtCm[ind], na.rm = TRUE) + by,
                                     by = by)
                    midLengths <- cm.breaks[-1] - diff(cm.breaks)/2
                    names(midLengths) <- cm.breaks[-length(cm.breaks)]
                    ## TODO: make issue/PR for DATRAS (always assumes 1cm bins)
                    hlc$sizeGroup <- NA
                    hlc$sizeGroup[ind] <- cut(hlc$LngtCm[ind],
                                              breaks = cm.breaks,
                                              right = FALSE)
                    n.by.length <- round(xtabs(Counts ~ HaulID + sizeGroup,
                                               data = hlc)) ## round after summing up?

                    ## TODO: needed? but how to add missing species info to bio.pars? or how to pass specific Lm?
                    ## if(split.juv.adults){
                    ##     data("bio.pars")
                    ## }

                    if(split.juv.adults && any(bio.pars$AphiaID ==
                                               specs.matched$AphiaID[i]) &&
                       !split.length){
                        if(verbose){
                            print(paste0("Lm(",i,") = ",
                                         bio.pars$Lm[bio.pars$AphiaID ==
                                                     specs.matched$AphiaID[i]]))
                        }

                        ## TODO: this should use the limits of length classes rather than the midLengths or account for the sizebin!
                        ind.juv <- which(midLengths <
                                         bio.pars$Lm[bio.pars$AphiaID ==
                                                     specs.matched$AphiaID[i]])
                        ind.adult <- which(midLengths >=
                                           bio.pars$Lm[bio.pars$AphiaID ==
                                                       specs.matched$AphiaID[i]])
                        survey.spec <- data.frame(haul.id = rownames(n.by.length),
                                                  AphiaID = specs.matched$AphiaID[i])

                        if(length(ind.juv) > 0){
                            if(nrow(n.by.length) > 1 && length(ind.juv) > 1){
                                survey.spec$n.juv = unname(apply(n.by.length[,ind.juv],
                                                                 1, sum))
                            }else{
                                survey.spec$n.juv = sum(n.by.length[,ind.juv])
                            }
                        }else{
                            survey.spec$n.juv <- 0
                        }
                        if(length(ind.adult) > 0){
                            if(nrow(n.by.length) > 1 && length(ind.adult) > 1){
                                survey.spec$n.adult = unname(apply(n.by.length[,ind.adult],
                                                                   1, sum))
                            }else{
                                survey.spec$n.adult = sum(n.by.length[,ind.adult])
                            }
                        }else{
                            survey.spec$n.adult <- 0
                        }
                    }else if(split.length &&
                             any(bio.pars$AphiaID == specs.matched$AphiaID[i])){

                        ind.split <- grep("split",colnames(bio.pars))

                        if(verbose){
                            print(paste0("Lsplit(",i,") = ",bio.pars[bio.pars$AphiaID == specs.matched$AphiaID[i],ind.split]))
                        }

                        survey.spec <- data.frame(haul.id = rownames(n.by.length),
                                                  AphiaID = specs.matched$AphiaID[i])

                        inds <- as.numeric(cut(midLengths, c(-10,bio.pars[bio.pars$AphiaID ==
                                                                          specs.matched$AphiaID[i],
                                                                          ind.split],1e4)))

                        for(j in 1:length(unique(inds))){
                            tmp <- n.by.length[,inds == j]
                            if(ncol(tmp) > 0){
                                if(nrow(tmp) > 1){
                                    addi <- data.frame(apply(tmp, 1, sum))
                                }else{
                                    addi <- data.frame(rep(sum(tmp),nrow(survey.spec)))
                                }
                            }else{
                                addi <- data.frame(rep(0, nrow(survey.spec)))
                            }
                            colnames(addi) <- paste0("n",j)
                            survey.spec <- data.frame(survey.spec, addi)
                        }

                    }else{
                        survey.spec <- data.frame(haul.id = rownames(n.by.length),
                                                  AphiaID = specs.matched$AphiaID[i],
                                                  N = unname(apply(n.by.length, 1, sum)))
                        ## Add entries with missing length information
                        ind <- which(is.na(hlc$sizeGroup))
                        if(length(ind) > 0){
                            tmpi <- hlc[ind, c("HaulID","AphiaID","Counts")]
                            tmpi2 <- aggregate(list(N = tmpi$Counts),
                                               by = list(haul.id = tmpi$HaulID,
                                                         AphiaID = tmpi$AphiaID),
                                               FUN = sum)
                            survey.spec <- rbind(survey.spec, tmpi2)
                            survey.spec <- aggregate(list(N = survey.spec$N),
                                               by = list(haul.id = survey.spec$haul.id,
                                                         AphiaID = survey.spec$AphiaID),
                                               FUN = sum)
                            ## CHECK: NEEDED?
                            ## ## remove NA
                            ## ind <- which(is.na(survey.spec$N))
                            ## if(length(ind) > 0){
                            ##     survey.spec <- survey.spec[-ind,]
                            ## }
                        }
                    }
                }

                if(est.bio){

                    if(use.ca){
                        if(!is.null(ca) && any(ca$AphiaID ==
                                               specs.matched$AphiaID[i])){


                            cac <- subset(ca, AphiaID == specs.matched$AphiaID[i])

                            ## NEW: account for some lengths = NA (bio = CatCatchWgt)
                            ind <- which(!is.na(cac$LngtCode))

                            if(length(ind) == 0){
                                if(split.juv.adults || split.length){
                                    stop(paste0("Species ",specs.matched$AphiaID[i],
                                                " does not have any length information. Cannot split into length classes! Run total N/bio or remove species!"))
                                }else{
                                    tmpi <- hlc[, c("HaulID","AphiaID","Counts")]
                                    survey.spec <- aggregate(list(bio = tmpi$CatCatchWgt),
                                                             by = list(haul.id = tmpi$HaulID,
                                                                       AphiaID = tmpi$AphiaID),
                                                             FUN = sum)

                                    ## TODO: remove NA?
                                }
                            }else{

                                ## DATRAS::addWeightByHaul
                                cac$LngtCm <- NA
                                cac$LngtCm[ind] <- c("." = 0.1, "0" = 0.1, "1" = 1, "2" = 1, "5" = 1)[as.character(cac$LngtCode[ind])] * cac$LngtClass[ind]



                                ## For debugging length-weight relationship
                                if(FALSE){
                                    browser()

                                    range(cac$LngtCm, na.rm = TRUE)

                                    cac <- cac[-grep("BITS:2021:4:DE:06SL:TVS:24300:35",
                                                     cac$HaulID),]

                                    cac <- cac[-grep("BTS:2005:3:GB:74RY:BT4A",
                                                     cac$HaulID),]

                                    grep("NS-IBTS:2015:1:DE:06NI:GOV",
                                         cac$HaulID[cac$LngtCm > 3 &
                                        cac$IndWgt > 25000
                                        & !is.na(cac$IndWgt)])

                                    plot(cac$LngtCm, cac$IndWgt)
                                    indi <- grep(##"BTS:1999:3:GB:74RY:BT4A:102",
                                        "NS-IBTS:2005:3:GB:74E9:GOV:31:88",
                                        "BITS:2017:4:DK:26D4:TVL:160",
                                                 cac$HaulID)
                                    points(cac$LngtCm[indi], cac$IndWgt[indi],col=3,
                                           pch = 16,cex=1.5)

                                }

                                mod <- try(lm(log(IndWgt) ~ log(LngtCm),
                                              data = subset(cac, IndWgt > 0)))
                                if(!inherits(mod, "try-error")){
                                    LW <- exp(predict(mod,
                                                      newdata = data.frame(LngtCm = midLengths)))
                                }else{
                                    stop(paste0("Cannot fit length-weight model for: ",
                                                specs.matched$AphiaID[i],
                                                ". Conider removing species or use a and b in bio.pars and use.ca = FALSE."))
                                }

                                if(verbose){
                                    plot(IndWgt ~ LngtCm, data = subset(cac, IndWgt > 0))
                                    lines(midLengths, LW, lwd = 2, col = 4)
                                }
                                if(split.juv.adults &&
                                   any(bio.pars$AphiaID == specs.matched$AphiaID[i])){
                                    survey.spec$bio.juv <- unname(apply(n.by.length[,ind.juv], 1,
                                                                        function(x) x %*% LW[ind.juv]))
                                    survey.spec$bio.adult <- unname(apply(n.by.length[,ind.adult], 1,
                                                                          function(x) x %*% LW[ind.adult]))
                                }else{
                                    survey.spec$bio <- NA
                                    survey.spec$bio[survey.spec$haul.id %in% rownames(n.by.length)] <- unname(apply(n.by.length, 1,
                                                                                                                    function(x) x %*% LW[names(LW) %in% colnames(n.by.length)]))

                                    ## Add entries with missing length information
                                    ind <- which(is.na(hlc$sizeGroup))
                                    if(length(ind) > 0){
                                        tmpi <- hlc[ind, c("HaulID","AphiaID","CatCatchWgt")]
                                        tmpi2 <- aggregate(list(bio = tmpi$CatCatchWgt),
                                                           by = list(haul.id = tmpi$HaulID,
                                                                     AphiaID = tmpi$AphiaID),
                                                           FUN = sum, na.rm = TRUE)
                                        survey.spec$bio[match(tmpi2$haul.id,
                                                              survey.spec$haul.id)] <-
                                            tmpi2$bio

                                    }
                                }
                            }

                        }else{
                            ## Look up in downloaded fishbase data
                                    data(fishbase.dat)
                                    ind <- which(fishbase.dat$AphiaID == specs.matched$AphiaID[i])
                                    if(length(ind) > 0){
                                        LW <- fishbase.dat$LWa[ind] * midLengths ^ fishbase.dat$LWb[ind]
                                        if(split.juv.adults && any(bio.pars$AphiaID == specs.matched$AphiaID[i])){
                                            survey.spec$bio.juv <- unname(apply(n.by.length[,ind.juv], 1,
                                                                                function(x) x %*% LW[ind.juv]))
                                            survey.spec$bio.adult <- unname(apply(n.by.length[,ind.adult], 1,
                                                                                  function(x) x %*% LW[ind.adult]))
                                        }else{
                                            survey.spec$bio <- unname(apply(n.by.length, 1, function(x) x %*% LW))
                                        }
                                    }else{
                                        ## otherwise try to download
                                        tmp <- try(as.data.frame(rfishbase::length_weight(spec)), silent = TRUE)
                                        if(!inherits(tmp,"try-error")){
                                            LW <- median(na.omit(tmp$a)) * midLengths ^ median(na.omit(tmp$b))
                                            survey.spec$bio <- unname(apply(n.by.length, 1, function(x) x %*% LW))
                                        }else{
                                            warning("Cannot estimate biomass, because neither CA data available nor are the length-weight parameters available on fishbase.")
                                            survey.spec$bio <- NA
                                        }
                                    }
                                }
                    }else{

                        tmp <- try(get("bio.pars"))
                        if(inherits(tmp, "try-error")) data(bio.pars)

                        indii <- which(bio.pars$AphiaID == specs.matched$AphiaID[i])

                        if(length(indii) == 1){
                            if(any(colnames(bio.pars) == "a") &&
                               any(colnames(bio.pars) == "b")){
                                a <- bio.pars$a[indii]
                                b <- bio.pars$b[indii]
                            }else{
                                stop(paste0("No 'a' and 'b' found in bio.pars for AphiaID ",
                                            specs.matched$AphiaID[i]))
                            }
                        }else{
                            stop(paste0("No (or more than 1) match found in bio.pars for AphiaID ",
                                        specs.matched$AphiaID[i]))
                        }


                        ## There might be no length measurements (only CatCatchWgt)
                        if(any(!is.na(hlc$LngtCm))){

                            LW <- a * midLengths^b

                            if(split.juv.adults &&
                               any(bio.pars$AphiaID == specs.matched$AphiaID[i]) &&
                               !split.length){
                                survey.spec$bio.juv <- unname(apply(n.by.length[,ind.juv], 1,
                                                                    function(x) x %*% LW[ind.juv]))
                                survey.spec$bio.adult <- unname(apply(n.by.length[,ind.adult], 1,
                                                                      function(x) x %*% LW[ind.adult]))
                            }else if(split.length &&
                                     any(bio.pars$AphiaID == specs.matched$AphiaID[i])){

                                bios <- NULL
                                for(j in 1:length(unique(inds))){
                                    tmp <- n.by.length[,inds == j]
                                    bios <- cbind(bios,
                                                  unname(apply(tmp, 1,
                                                               function(x) x %*% LW[inds == j])))
                                }
                                colnames(bios) <- paste0("bio", 1:length(unique(inds)))

                                survey.spec <- data.frame(survey.spec, bios)

                            }else{
                                survey.spec$bio <- NA
                                bio <- unname(apply(n.by.length, 1,
                                                    function(x) x %*%
                                                                LW[match(names(midLengths)[
                                                                    as.numeric(colnames(
                                                                        n.by.length))],
                                                                    names(LW))]))
                                if(any(rownames(n.by.length) %in% survey.spec$haul.id)){
                                    indi <- which(survey.spec$haul.id %in% rownames(n.by.length))
                                    indi2 <- which(rownames(n.by.length) %in% survey.spec$haul.id)
                                    survey.spec$bio[indi] <- bio[indi2]
                                }else{
                                    indi <- indi2 <- NULL
                                }

                                if(any(!rownames(n.by.length) %in% survey.spec$haul.id)){
                                    if(!is.null(indi2)){
                                        hauli <- rownames(n.by.length)[-indi2]
                                        bioi <- bio[-indi2]
                                    }else{
                                        hauli <- rownames(n.by.length)
                                        bioi <- bio
                                    }
                                    tmpi <- data.frame(haul.id = hauli,
                                                       AphiaID = specs.matched$AphiaID[i],
                                                       N = NA,
                                                       bio = bioi)
                                    survey.spec <- rbind(survey.spec, tmpi)

                                }


                                ## Add entries with missing length information
                                ind <- which(is.na(hlc$sizeGroup))
                                ## TODO: CatCatchWgt not in hl/hlc any more? Why?
                                ## if(length(ind) > 0){
                                ##     tmpi <- hlc[ind, c("HaulID","AphiaID","CatCatchWgt")]
                                ##     tmpi2 <- aggregate(list(bio = tmpi$CatCatchWgt),
                                ##                        by = list(haul.id = tmpi$HaulID,
                                ##                                  AphiaID = tmpi$AphiaID),
                                ##                        FUN = sum, na.rm = TRUE)
                                ##     survey.spec$bio[match(tmpi2$haul.id,
                                ##                           survey.spec$haul.id)] <-
                                ##         tmpi2$bio

                                ## }
                            }

                        }else{

                            survey.spec$bio <- NA

                            ## Add entries with missing length information
                            ind <- which(is.na(hlc$LngtCm))
                            if(length(ind) > 0){
                                tmpi <- hlc[ind, c("HaulID","AphiaID","CatCatchWgt")]
                                tmpi2 <- aggregate(list(bio = tmpi$CatCatchWgt),
                                                   by = list(haul.id = tmpi$HaulID,
                                                             AphiaID = tmpi$AphiaID),
                                                   FUN = sum, na.rm = TRUE)
                                survey.spec$bio[match(tmpi2$haul.id,
                                                      survey.spec$haul.id)] <-
                                    tmpi2$bio

                            }


                        }


                    }
                }else{
                    survey.spec$bio <- NA
                }
                survey <- rbind(survey, survey.spec)
            }

            ## CHECK: put this in prep.data
            colnames(survey0)[colnames(survey0) == "HaulID"] <- "haul.id"
            ## merge other variables to survey
            survey <- plyr::join(survey, survey0, by="haul.id")
            if(split.juv.adults && any(colnames(survey) == "n.juv") &&
               any(colnames(survey) == "n.adult")){
                survey$n.juv[is.na(survey$n.juv)] <- 0
                survey$n.adult[is.na(survey$n.adult)] <- 0
                if(est.bio){
                    survey$bio.juv[is.na(survey$bio.juv)] <- 0
                    survey$bio.adult[is.na(survey$bio.adult)] <- 0
                }
            }else if(split.length && any(colnames(survey) == "n1")){
                for(j in 1:length(unique(inds))){
                    survey[is.na(survey[,grep(paste0("n",j),colnames(survey))]),
                           grep(paste0("n",j),colnames(survey))] <- 0
                    if(est.bio){
                        survey[is.na(survey[,grep(paste0("bio",j),colnames(survey))]),
                               grep(paste0("bio",j),colnames(survey))] <- 0
                    }
                }
            }

            ## HERE: just for checking! REMOVE:
            ## colnames(hl)[colnames(hl) == "HaulID"] <- "haul.id"
            ## survey <- plyr::join(survey, hl[,c("haul.id","multiplier","SubFactor")], by = "haul.id")



            ## Add species information
            if(specflag){
                ## Subset relevant columns from species list
                ## ----------
                ind <- which(is.na(specs.matched$AphiaID))
                if(length(ind) > 0) specs.matched <- specs.matched[-ind,]
                ind <- which(duplicated(specs.matched$AphiaID))
                if(length(ind) > 0) specs.matched <- specs.matched[-ind,]

                ## Merge species list to survey
                ## ----------
                survey <- plyr::join(survey, specs.matched, by="AphiaID")

                ## Species and bycatch corrections
                ## ------------
                survey <- correct.species(survey)
            }
        }else{
            ## CHECK: put this in prep.data
            colnames(survey0)[colnames(survey0) == "HaulID"] <- "haul.id"
            colnames(hl)[colnames(hl) == "HaulID"] <- "haul.id"
            survey <- plyr::join(hl, survey0, by="haul.id")
            survey$N <- NA
            survey$bio <- NA
        }
        survey0$N <- 0
        survey0$bio <- 0

        ## Worms list
        ## -----------
        ## Try to get info from worms.dat (data in package)
        data(worms.dat)
        survey <- plyr::join(survey, worms.dat, by='AphiaID')
        ## For species not matched: download info
        aphia_list <- as.numeric(unique(survey$AphiaID[is.na(survey$scientificname)]))
        if(length(aphia_list) > 0){
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
        }

        ## Order
        ## -----------
        survey0 <- survey0[order(survey0$Year, survey0$Month, survey0$Day),]
        survey <- survey[order(survey$Year, survey$Month, survey$Day),]
        ## required by surveyIdx package
        colnames(survey0)[which(colnames(survey0) == "HaulID")] <- "haul.id"
        colnames(survey)[which(colnames(survey) == "HaulID")] <- "haul.id"


    }else if(inherits(data,"fdist.prepped")){

        survey0 <- data$survey0
        surveyIn <- data$survey
        ca <- data$CA
        rm(data)

        ## Species subsetting for survey
        ## ------------------
        if(!is.null(AphiaID[1]) && !is.na(AphiaID[1]) && !(AphiaID[1] %in% c("all","All","ALL"))){
            if(!inherits(AphiaID, "data.frame")) specs <- data.frame(AphiaID = AphiaID)
            if(!any(colnames(specs) == "AphiaID"))
                stop("At least one of the columns in AphiaID needs to indicate the Aphia ID and be called 'AphiaID'")

            ## Species that could not be matched
            ## ---------
            flag <- any(specs$AphiaID %in% surveyIn$AphiaID)
            if(!flag) stop("Provided Aphia IDs were not found in the data set. Please check data and/or IDs!")
            ind <- which(!(specs$AphiaID %in% surveyIn$AphiaID))
            if(length(ind) > 0){
                if(verbose){
                    writeLines("These Aphia IDs could not be matched:")
                    print(specs[ind,])
                }
            }

            ## Subset based on species list
            ## ---------
            surveyIn <- subset(surveyIn, AphiaID %in% specs$AphiaID)
            if(!is.null(ca)) ca <- subset(ca, AphiaID %in% specs$AphiaID)
        }


        ## Estimate N & Bio (requires CA)
        ## -------------------
        if(est.n){

            if(split.juv.adults){
                data("bio.pars")
            }

            survey <- NULL
            nspec <- nrow(specs)
            for(i in 1:nspec){

                surveyc <- subset(surveyIn, AphiaID %in% specs$AphiaID[i])
                unique(surveyc$AphiaID)

                ## DATRAS::addSpectrum
                by <- max(c(. = 0.1, `0` = 0.5, `1` = 1, `2` = 2, `5` = 5)[as.character(surveyc$LngtCode)],
                          na.rm = TRUE)
                cm.breaks <- seq(min(surveyc$LngtCm, na.rm = TRUE),
                                 max(surveyc$LngtCm, na.rm = TRUE) + by,
                                 by = by)
                midLengths <- cm.breaks[-1] - diff(cm.breaks)/2 ## TODO: make issue/PR for DATRAS (always assumes 1cm bins)
                surveyc$sizeGroup <- cut(surveyc$LngtCm, breaks = cm.breaks, right = FALSE)
                n.by.length <- round(xtabs(Counts ~ haul.id + sizeGroup, data = surveyc))

                if(split.juv.adults && any(bio.pars$AphiaID == specs$AphiaID[i])){
                    ind.juv <- which(midLengths < bio.pars$Lm[bio.pars$AphiaID == specs$AphiaID[i]])
                    ind.adult <- which(midLengths >= bio.pars$Lm[bio.pars$AphiaID == specs$AphiaID[i]])
                    survey.spec <- data.frame(haul.id = rownames(n.by.length),
                                              AphiaID = specs$AphiaID[i],
                                              n.juv = unname(apply(n.by.length[,ind.juv], 1, sum)),
                                              n.adult = unname(apply(n.by.length[,ind.adult], 1, sum))
                                              )
                    survey.spec$N <- survey.spec$n.juv + survey.spec$n.adult
                }else{
                    survey.spec <- data.frame(haul.id = rownames(n.by.length),
                                              AphiaID = specs$AphiaID[i],
                                              N = unname(apply(n.by.length, 1, sum)))
                }

                if(est.bio){
                    if(!is.null(ca) && any(ca$AphiaID == specs$AphiaID[i])){
                        cac <- subset(ca, AphiaID == specs$AphiaID[i])
                        ## DATRAS::addWeightByHaul
                        cac$LngtCm <- c(. = 0.1, `0` = 0.5, `1` = 1, `2` = 2, `5` = 5)[as.character(cac$LngtCode)] * cac$LngtClass
                        mod <- lm(log(IndWgt) ~ log(LngtCm), data = subset(cac, IndWgt > 0))
                        LW <- exp(predict(mod, newdata = data.frame(LngtCm = midLengths)))
                        if(split.juv.adults && any(bio.pars$AphiaID == specs$AphiaID[i])){
                            survey.spec$bio.juv <- unname(apply(n.by.length[,ind.juv], 1,
                                                                function(x) x %*% LW[ind.juv]))
                            survey.spec$bio.adult <- unname(apply(n.by.length[,ind.adult], 1,
                                                                  function(x) x %*% LW[ind.adult]))
                            survey.spec$bio <- survey.spec$bio.juv + survey.spec$bio.adult
                        }else{
                            survey.spec$bio <- unname(apply(n.by.length, 1, function(x) x %*% LW))
                        }
                    }else{
                        ## Look up in downloaded fishbase data
                        if(verbose) writeLines("No CA data found! Using fishbased pars for L-W rel.")
                        data("fishbase.dat")
                        ind <- which(fishbase.dat$AphiaID == specs$AphiaID[i])
                        if(length(ind) > 0){
                            LW <- fishbase.dat$LWa[ind] * midLengths ^ fishbase.dat$LWb[ind]
                            if(split.juv.adults && any(bio.pars$AphiaID == specs$AphiaID[i])){
                                survey.spec$bio.juv <- unname(apply(n.by.length[,ind.juv], 1,
                                                                    function(x) x %*% LW[ind.juv]))
                                survey.spec$bio.adult <- unname(apply(n.by.length[,ind.adult], 1,
                                                                      function(x) x %*% LW[ind.adult]))
                                survey.spec$bio <- survey.spec$bio.juv + survey.spec$bio.adult
                            }else{
                                survey.spec$bio <- unname(apply(n.by.length, 1, function(x) x %*% LW))
                            }
                        }else{
                            ## otherwise try to download
                            tmp <- try(as.data.frame(rfishbase::length_weight(spec)), silent = TRUE)
                            if(!inherits(tmp,"try-error")){
                                LW <- median(na.omit(tmp$a)) * midLengths ^ median(na.omit(tmp$b))
                                survey.spec$bio <- unname(apply(n.by.length, 1, function(x) x %*% LW))
                            }else{
                                warning("Cannot estimate biomass, because neither CA data available nor are the length-weight parameters available on fishbase.")
                                survey.spec$bio <- NA
                            }
                        }
                    }
                }else{
                    survey.spec$bio <- NA
                }
                survey <- rbind(survey, survey.spec)
            }

            ## merge other variables to survey
            survey <- plyr::join(survey, survey0[,!(colnames(survey0) %in% c("N","bio"))], by="haul.id")
            if(split.juv.adults && any(colnames(survey) == "n.juv") &&
               any(colnames(survey) == "n.adult")){
                survey$n.juv[is.na(survey$n.juv)] <- 0
                survey$n.adult[is.na(survey$n.adult)] <- 0
            }

            ## Add species information
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

            ## Worms list
            ## -----------
            ## Try to get info from worms.dat (data in package)
            data(worms.dat)
            survey <- plyr::join(survey, worms.dat, by='AphiaID')
            ## For species not matched: download info
            aphia_list <- as.numeric(unique(survey$AphiaID[is.na(survey$scientificname)]))
            if(length(aphia_list) > 0){
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
            }
        }
    }


    ## Return
    ## -----------
    if(est.bio){
        res <- list(survey0 = survey0, survey = survey)
    }else{
        res <- list(survey0 = survey0, survey = survey, CA = ca)
    }
    class(res) <- c("fdist.prepped","list")
    return(res)
}
