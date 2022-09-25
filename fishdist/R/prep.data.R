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
                      est.n = TRUE, est.bio = FALSE,
                      split.juv.adults = FALSE,
                      verbose = TRUE){

    specflag <- ifelse(is.null(AphiaID[1]) || is.na(AphiaID[1]) || AphiaID[1] %in% c("all","All","ALL"),0,1)

    ## Check validity of data
    ## ------------------
    if((!inherits(data,"fdist.datras") && !inherits(data,"fdist.prepped")) ||
       (!all(c("HH","HL") %in% names(data)) && !all(c("survey0","survey") %in% names(data))))
        stop("Function requires list (of class fdist.datras) with two DATRAS data sets 'HH' and 'HL' as elements (see download.data) or list (of class fdist.prepped) with two survey data sets 'survey0' and 'survey' as elements (see prep.data).")

    if(inherits(data,"fdist.datras")){
        hh <- data$HH
        hl <- data$HL
        ca <- data$CA
        rm(data)


        ## Flags
        ## ------------------
        saflag <- ifelse(any("SweptAreaDSKM2" == colnames(hh)),1,0)


        ## Species subsetting for hl (full hh needed for survey0)
        ## ------------------
        if(specflag){
            if(!inherits(AphiaID, "data.frame")) specs <- data.frame(AphiaID = AphiaID)
            if(!any(colnames(specs) == "AphiaID"))
                stop("At least one of the columns in AphiaID needs to indicate the Aphia ID and be called 'AphiaID'")

            ## Species that could not be matched
            ## ---------
            flag <- any(specs$AphiaID %in% hl$AphiaID)
            if(!flag) stop("Provided Aphia IDs were not found in the data set. Please check data and/or IDs!")
            ind <- which(!(specs$AphiaID %in% hl$AphiaID))
            if(length(ind) > 0){
                if(verbose){
                    writeLines("These Aphia IDs could not be matched:")
                    print(specs[ind,])
                }
            }

            ## Subset based on species list
            ## ---------
            hl <- subset(hl, AphiaID %in% specs$AphiaID)
            if(!is.null(ca)) ca <- subset(ca, AphiaID %in% specs$AphiaID)
        }


        ## Subset data sets to reduce memory usage and prevent R collapse
        ## ------------------
        ind <- datras.variables[["HH"]]
        if(!saflag) ind <- ind[-which(ind %in% c("SweptAreaDSKM2",
                                                 "SweptAreaWSKM2",
                                                 "SweptAreaBWKM2"))]
        hh <- hh[,which(colnames(hh) %in% ind)]


        ## Create haul IDs (SLOW:)
        ## ------------------
        hl$HaulID <- paste(hl$Survey, hl$Year, hl$Quarter, hl$Country, hl$Ship, hl$Gear,
                           hl$StNo, hl$HaulNo, sep = ":")
        hh$HaulID <- paste(hh$Survey, hh$Year, hh$Quarter, hh$Country, hh$Ship, hh$Gear,
                           hh$StNo, hh$HaulNo, sep = ":")
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
                    "TotalNo","CatIdentifier","SubFactor", "LngtCode",
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
        if("TimeShot" %in% colnames(hh))
            hh$TimeShotHour <- as.integer(hh$TimeShot/100) + (hh$TimeShot%%100)/60
        hh$Depth <- replace(hh$Depth, hh$Depth<0, NA)
        hh$Ship <- as.factor(hh$Ship)
        hh$Gear <- as.factor(hh$Gear)
        hh$ShipG <- factor(paste(hh$Ship, hh$Gear, sep=":"))

        ## Convert TimeShot to time of day (ToD)
        ## Remove hauls with incorrect time of day entry
        ind <- which(nchar(hh$TimeShot) < 3)
        if(length(ind) > 0){
            hh <- hh[-ind,]
        }
        ## Assume that first character is 0 if missing
        ind <- which(nchar(hh$TimeShot) == 3)
        if(length(ind) > 0){
            hh$TimeShot[ind] <- paste0(0, hh$TimeShot[ind])
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
        hh$timezone <- lutz::tz_lookup_coords(lat = hh$lat, lon = hh$lon, method = "accurate")

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
        if(saflag) hh <- subset(hh, !is.na(SweptArea))


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
        hl <- plyr::join(hl, hh[,c("HaulID","BySpecRecCode","HaulDur","DataType")], by="HaulID") ## 16s
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
            ind <- which(is.na(specs$AphiaID))
            if(length(ind) > 0) specs <- specs[-ind,]
            ind <- which(duplicated(specs$AphiaID))
            if(length(ind) > 0) specs <- specs[-ind,]
            ## Merge species list to survey
            ## ----------
            hl <- plyr::join(hl, specs, by="AphiaID")
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

        ## NEW: DECISION: removing HLNoAtLngt = NA entries
        hl <- hl[-ind,]
        ## CHECK:
        ## DECISION: Using TotalNo when HLNoAtLngt == NA and setting subFactor to 1.
        ## this might still work, but only on TotalNo by haul id should be used! totalNo is repeated!
        if(FALSE){
            if(length(ind) > 0){
                tmp <- hl[ind,]
                any(duplicated(tmp$HaulID))
                tmp$DataType <- "R"
                hl$HLNoAtLngt[ind] <- hl$TotalNo[ind]
                hl$SubFactor[ind] <- 1
                ind2 <- which(!is.na(hl$TotalNo[ind]))
                if(verbose){
                    writeLines(paste0(paste("Number of entries with missing HLNoAtLngt: ",
                                            paste0(length(ind), " (",round(length(ind)/nrow(hl)*100,1),
                                                   "%)"),
                                            sep = "\t\t\t"),
                                      " Using TotalNo (with SubFactor = 1) for these hauls."))
                    writeLines(paste("Number of meaningful TotalNo replacements: ",
                                     paste0(length(ind2), " (",round(length(ind2)/length(ind)*100,1),
                                            "%)"),
                                     sep = "\t\t\t"))
                }
            }
        }


        ## Account for LngtCode (mm and cm)
        ## Remove LngtCode = NA (LngtClass also NA)
        ind <- which(is.na(hl$LngtCode))
        if(length(ind) > 0) hl <- hl[-ind,]
        ## DATRAS:::getAccuracyCM
        lngt2cm <- c("." = 0.1, "0" = 0.1, "1" = 1, "2" = 1, "5" = 1)[as.character(hl$LngtCode)]
        hl$LngtCm <- lngt2cm * hl$LngtClass
        range(hl$LngtCm)
        ## DATRAS::addSpectrum
        ## https://www.ices.dk/data/Documents/DATRAS/DATRAS_FAQs.pdf:
        ## DataType R,S: TotalNo –report the total number of fish of one species, sex, and category in the given haul
        ## DataType C: TotalNo –report the total number of fish of one species and sex in the given haul, raised to 1 hour hauling;
        hl$multiplier <- ifelse(hl$DataType=="C", hl$HaulDur/60, hl$SubFactor)  ## not using SubFactor if DataType == "C"
        hl$Counts <- as.numeric(hl$HLNoAtLngt * hl$multiplier)

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

        ## Estimate N & Bio (requires CA)
        ## -------------------
        if(est.n){

            if(split.juv.adults){
                data("bio.pars")
            }

            survey <- NULL
            nspec <- nrow(specs)
            for(i in 1:nspec){

                hlc <- subset(hl, AphiaID == specs$AphiaID[i])
                unique(hlc$AphiaID)

                ## DATRAS::addSpectrum
                by <- max(c("." = 0.1, "0" = 0.5, "1" = 1, "2" = 2, "5" = 5)[as.character(hlc$LngtCode)],
                          na.rm = TRUE)
                cm.breaks <- seq(min(hlc$LngtCm, na.rm = TRUE),
                                 max(hlc$LngtCm, na.rm = TRUE) + by,
                                 by = by)
                midLengths <- cm.breaks[-1] - diff(cm.breaks)/2 ## TODO: make issue/PR for DATRAS (always assumes 1cm bins)
                hlc$sizeGroup <- cut(hlc$LngtCm, breaks = cm.breaks, right = FALSE)
                n.by.length <- round(xtabs(Counts ~ HaulID + sizeGroup, data = hlc)) ## round after summing up?

                if(split.juv.adults && any(bio.pars$AphiaID == specs$AphiaID[i])){
                    ind.juv <- which(midLengths < bio.pars$Lm[bio.pars$AphiaID == specs$AphiaID[i]])
                    ind.adult <- which(midLengths >= bio.pars$Lm[bio.pars$AphiaID == specs$AphiaID[i]])
                    survey.spec <- data.frame(haul.id = rownames(n.by.length),
                                              AphiaID = specs$AphiaID[i],
                                              n.juv = unname(apply(n.by.length[,ind.juv], 1, sum)),
                                              n.adult = unname(apply(n.by.length[,ind.adult], 1, sum))
                                              )
                }else{
                    survey.spec <- data.frame(haul.id = rownames(n.by.length),
                                              AphiaID = specs$AphiaID[i],
                                              N = unname(apply(n.by.length, 1, sum)))
                }

                if(est.bio){
                    if(!is.null(ca) && any(ca$AphiaID == specs$AphiaID[i])){

                        cac <- subset(ca, AphiaID == specs$AphiaID[i])
                        ## DATRAS::addWeightByHaul
                        cac$LngtCm <- c("." = 0.1, "0" = 0.5, "1" = 1, "2" = 2, "5" = 5)[as.character(cac$LngtCode)] * cac$LngtClass
                        mod <- lm(log(IndWgt) ~ log(LngtCm), data = subset(cac, IndWgt > 0))
                        LW <- exp(predict(mod, newdata = data.frame(LngtCm = midLengths)))
                        if(split.juv.adults && any(bio.pars$AphiaID == specs$AphiaID[i])){
                            survey.spec$bio.juv <- unname(apply(n.by.length[,ind.juv], 1,
                                                                function(x) x %*% LW[ind.juv]))
                            survey.spec$bio.adult <- unname(apply(n.by.length[,ind.adult], 1,
                                                                  function(x) x %*% LW[ind.adult]))
                        }else{
                            survey.spec$bio <- unname(apply(n.by.length, 1, function(x) x %*% LW))
                        }
                    }else{
                        ## Look up in downloaded fishbase data
                        data(fishbase.dat)
                        ind <- which(fishbase.dat$AphiaID == specs$AphiaID[i])
                        if(length(ind) > 0){
                            LW <- fishbase.dat$LWa[ind] * midLengths ^ fishbase.dat$LWb[ind]
                            if(split.juv.adults && any(bio.pars$AphiaID == specs$AphiaID[i])){
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
            }

            ## HERE: just for checking! REMOVE:
            ## colnames(hl)[colnames(hl) == "HaulID"] <- "haul.id"
            ## survey <- plyr::join(survey, hl[,c("haul.id","multiplier","SubFactor")], by = "haul.id")



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

                surveyc <- subset(surveyIn, AphiaID == specs$AphiaID[i])
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
