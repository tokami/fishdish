#' @name minus9toNA
#' @title Minus 9 to NA
#' @return Vector with NA
#' @export
minus9toNA <- function(x){
    if(inherits(x,"matrix") || inherits(x, "data.frame")){
        for(i in 1:ncol(x)){
            if(is.numeric(x[,i])){
                is9 <- which(abs(x[,i]+9)<1e-14)
                if(length(is9)>0) x[,i][is9] <- NA
            }
        }
    }else{
        if(is.numeric(x)){
            is9 <- which(abs(x+9)<1e-14)
            if(length(is9)>0) x[is9] <- NA
        }
    }
    return(x)
}

#' @name list.surveys
#' @title List all survey names
#' @return Vector with all survey names
#' @export
list.surveys <- function(){
    all.surveys <- c("NS-IBTS","BITS","EVHOE","FR-CGFS",
                     "IE-IGFS", "NIGFS", "PT-IBTS", "ROCKALL",
                     "SCOROC", "SP-ARSA", "SP-NORTH", "SP-PORC",
                     "SNS", "SWC-IBTS","SCOWCGFS", "BTS",
                     "BTS-VIII", "DYFS")

    return(all.surveys)
}


#' @name get.info.surveys
#' @title Get some info about surveys
#' @param survey optional vector or single survey
#' @param statrec logical indicating whether to include statistical rectangles for each survey (Default: FALSE). If FALSE, returned object is a list.
#' @param plot logical shoul plot with survey distributions be drawn?
#' @importFrom maps map
#' @return Data frame with info
#' @export
get.info.surveys <- function(survey=NULL, statrec = FALSE, plot = TRUE){

    data("survey.info")

    all.surveys <- sapply(survey.info,function(x) x$survey)
    if(!is.null(survey[1])){
        survey.sel <- survey.info[which(all.surveys %in% survey)]
        if(!any(all.surveys %in% survey))
            stop("Provided survey could not be matched (remove the arguments or run list.surveys() to see all surveys).")
    }else{
        survey.sel <- survey.info
    }
    ns <- length(survey.sel)
    survs <- sapply(survey.sel,function(x) x$survey)

    if(statrec == FALSE){
        res <- as.data.frame(t(sapply(survey.sel,
                                      function(x) unlist(x[c("survey","first.year","last.year","quarters")]))))
    }else{
        res <- survey.sel
    }


    if(plot){
        data("ices.rectangles")
        lat.range <- c(35,63)
        lon.range <- c(-20,25)

        if(ns >= 9){
            mfrow = c(3,ceiling(ns/3))
        }else if(ns < 9 & ns >= 4){
            mfrow = c(2,ceiling(ns/2))
        }else if(ns < 4){
            mfrow = c(1,ns)
        }
        par(mfrow = mfrow, mar = c(3,3,2,1), oma = c(2.5,2.5,1,1))
        for(i in 1:ns){
            plot(lon.range, lat.range,
                 xlim = lon.range, ylim = lat.range,
                 ty='n',
                 xlab = "", ylab = "")
            ind <- which(ices.rectangles$ICESNAME %in% survey.sel[[i]]$StatRec)
            tmp <- ices.rectangles[ind,]
            for(j in 1:nrow(tmp)){
                tmpj <- tmp[j,]
                polygon(c(tmpj$stat_west, tmpj$stat_east, tmpj$stat_east, tmpj$stat_west),
                        c(tmpj$stat_south, tmpj$stat_south, tmpj$stat_north, tmpj$stat_north),
                        border = "goldenrod2", col = "goldenrod3")
                ## text((tmpj$WEST + tmpj$EAST)/2, (tmpj$SOUTH + tmpj$NORTH)/2,
                ##      labels = tmpj$ICESNAME)
            }
            maps::map("world", xlim = lon.range, ylim = lat.range,
                      fill = TRUE, plot = TRUE, add = TRUE,
                      col = grey(0.8),
                      border = grey(0.7)
                      )
            mtext(survs[i], 3, 0.5)
            box(lwd=1.5)
        }
        mtext("Latitude", 2, 1, outer = TRUE)
        mtext("Longitude", 1, 1, outer = TRUE)
    }

    return(res)
}



#' @name list.recom.models
#'
#' @title List all model recommended structures
#'
#' @param specdata Species data
#' @param use.toy Use time of year? (Caused problems for some species)
#' @param use.swept.area Use swept area? (Might not be available)
#' @param dim.lat.lon Dimensions of the basis for the lat - lon smooth term (1
#'     number)
#' @param dim.ctime.lat.lon Dimensions of the basis for the ctime - lat - lon
#'     smooth term (2 numbers)
#' @param dim.timeOfYear.lat.lon Dimensions of the basis for the timeOfYear -
#'     lat - lon smooth term (2 numbers)
#'
#' @return List with all model structures
#'
#' @export
list.recom.models <- function(specdata,
                              use.toy = TRUE,
                              use.swept.area = TRUE,
                              use.sqrt.depth = FALSE,
                              use.gear.as.fixed = FALSE,
                              use.random.ship = FALSE,
                              dim.lat.lon = 256,
                              dim.ctime = "nyears",
                              dim.ctime.lat.lon = c("nyears",10),
                              dim.timeOfYear.lat.lon = c(6,30)){

    ## Checks
    if(length(dim.ctime.lat.lon) != 2) stop("The variable dim.ctime.lat.lon has to have length equal to 2.")
    if(length(dim.timeOfYear.lat.lon) != 2) stop("The variable dim.timeOfYear.lat.lon has to have length equal to 2.")

    if(dim.ctime == "nyears" && !is.null(specdata)) dim.ctime <- length(unique(specdata$Year))
    if(dim.ctime.lat.lon[1] == "nyears" && !is.null(specdata))
        dim.ctime.lat.lon[1] <- length(unique(specdata$Year))

    latLon <- paste0("s(lon, lat, bs=c('ds'), k=",dim.lat.lon[1],", m=c(1,0.5))")
    ctime <- paste0("s(ctime, bs='ds', k=",dim.ctime[1],", m=c(1,0))")
    ctimeLatLon <- paste0("ti(ctime, lon, lat, d=c(1,2), bs=c('ds','ds'), k=c(",
                          dim.ctime.lat.lon[1], ",",
                          dim.ctime.lat.lon[2],"), m=list(c(1,0), c(1,0.5)))")
    timeOfYearLatLon <- paste0("te(timeOfYear, lon, lat, d=c(1,2), bs=c('cc','ds'), k=c(",
                               dim.timeOfYear.lat.lon[1],",",
                               dim.timeOfYear.lat.lon[2],"), m=list(c(1,0), c(1,0.5)))")
    if(use.sqrt.depth){
        depth <- "s(sqrt(Depth), bs='ds', k=5, m=c(1,0))"
    }else{
        depth <- "s(Depth, bs='ds', k=5, m=c(1,0))"
    }
    if(use.random.ship){
        ship <- "s(ShipG, bs='re')" ## might be dangerous to include, omitted for now! TEST:
    }else{
        ship <- ""
    }
    if(use.gear.as.fixed){
        gear <- "Gear"
    }else{
        gear <- "s(Gear, bs='re')"
    }

    offset.var <- ifelse(use.swept.area, "SweptArea", "HaulDur")
    offset <- paste0("offset(log(",offset.var,"))")

    ##        1        2        3               4           5     6      7,    8
    mm <- c(latLon, ctime, ctimeLatLon, timeOfYearLatLon, gear, ship, depth, offset)
    mSel <- rep(TRUE, length(mm))
    if(!use.random.ship) mSel[6] <- FALSE
    if (!use.toy) mSel[4] <- FALSE
    if(length(unique(specdata$Gear)) == 1)  mSel[5] <- FALSE
    ## if(length(unique(specdata$ShipG)) == 1) mSel[6] <- FALSE

    ## all
    mps <- list(paste(mm[mSel],collapse=' + '))
    ## keep all terms, reduce k
    if(mSel[3]){
        ctimeLatLon <- paste0("ti(ctime, lon, lat, d=c(1,2), bs=c('ds','ds'), k=c(",
                              dim.ctime.lat.lon[1], ",", 5, "), m=list(c(1,0), c(1,0.5)))")
        mps <- append(mps,paste(c(latLon, ctime, ctimeLatLon, timeOfYearLatLon, gear, ship, depth, offset)[mSel],
                                collapse=' + '))
    }
    if(mSel[1]){
        latLon <- paste0("s(lon, lat, bs=c('ds'), k=",128,", m=c(1,0.5))")
        mps <- append(mps,paste(c(latLon, ctime, ctimeLatLon, timeOfYearLatLon, gear, ship, depth, offset)[mSel],
                                collapse=' + '))
    }

    ## ## no ship, if exist
    ## if(mSel[6]){
    ##     mSel[6] <- FALSE
    ##     mps <- append(mps,paste(mm[mSel],collapse=' + '))
    ## }
    ## ## no gear, if exist
    ## if(mSel[5]){
    ##     mSel[5] <- FALSE
    ##     mps <- append(mps,paste(mm[mSel],collapse=' + '))
    ## }
    ## ## no timeOfYear, if exist
    ## if(mSel[3]){
    ##     mSel[3] <- FALSE
    ##     mps <- append(mps,paste(mm[mSel],collapse=' + '))
    ## }
    ## no toy, if exists
    ## if(mSel[4]){
    ##     mSel[4] <- FALSE
    ##     mps <- append(mps,paste(mm[mSel],collapse=' + '))
    ## }

    return(mps)
}




#' @name list.recom.models2
#'
#' @title List all model recommended structures
#'
#' @param specdata Species data
#' @param use.toy Use time of year? (Caused problems for some species)
#' @param use.swept.area Use swept area? (Might not be available)
#' @param dim.lat.lon Dimensions of the basis for the lat - lon smooth term (1
#'     number)
#' @param dim.ctime.lat.lon Dimensions of the basis for the ctime - lat - lon
#'     smooth term (2 numbers)
#' @param dim.timeOfYear.lat.lon Dimensions of the basis for the timeOfYear -
#'     lat - lon smooth term (2 numbers)
#'
#' @return List with all model structures
#'
#' @export
list.recom.models2 <- function(specdata,
                               dim.ToD = 5,
                               dim.DoY = 10,
                               dim.year = "nyears",
                               dim.lat.lon = 256,
                               dim.DoY.year = c(5, "nyears"),
                               dim.lat.lon.ToD = c(30, 5),
                               dim.lat.lon.DoY = c(30, 5),
                               dim.lat.lon.year = c(30, "nyears")){

    ## Checks
    if(length(dim.lat.lon.year) != 2) stop("The variable dim.ctime.lat.lon has to have length equal to 2.")
    if(length(dim.lat.lon.DoY) != 2) stop("The variable dim.timeOfYear.lat.lon has to have length equal to 2.")

    if(dim.year == "nyears" && !is.null(specdata)) dim.year <- length(unique(specdata$Year))
    if(dim.DoY.year[2] == "nyears" && !is.null(specdata))
        dim.DoY.year[2] <- length(unique(specdata$Year))
    if(dim.lat.lon.year[2] == "nyears" && !is.null(specdata))
        dim.lat.lon.year[2] <- length(unique(specdata$Year))

    ## simple model
    tod <- paste0("s(ToD, bs='cc', k=",dim.ToD[1],")")
    doy <- paste0("s(DoY, bs='cc', k=",dim.DoY[1],")")
    year <- paste0("s(Year, bs='ds', k=",dim.year[1],", m=c(1,0))")
    latLon <- paste0("s(lon, lat, bs='ds', k=",dim.lat.lon[1],", m=c(1,0.5))")
    doy.year <- paste0("ti(DoY, Year, bs=c('cc','ds'), k=c(",dim.DoY.year[1],",",dim.DoY.year[2],"), m=list(NA, c(1,0)))")
    depth <- "s(Depth, bs='ds', k=5, m=c(1,0))"
    gear <- "Gear"
    ship <- "s(ShipG, bs='re')" ## might be dangerous to include, omitted for now! TEST:

    ## additional for full model
    latLon.tod <- paste0("ti(lon, lat, ToD, d=c(2,1), bs=c('ds','cc'), k=c(",
                          dim.lat.lon.ToD[1], ",",
                         dim.lat.lon.ToD[2],"), m=list(c(1,0.5), NA))")
    latLon.doy <- paste0("ti(lon, lat, DoY, d=c(2,1), bs=c('ds','cc'), k=c(",
                          dim.lat.lon.DoY[1], ",",
                         dim.lat.lon.DoY[2],"), m=list(c(1,0.5), NA))")
    latLon.year <- paste0("ti(lon, lat, Year, d=c(2,1), bs=c('ds','tp'), k=c(",
                          dim.lat.lon.year[1], ",",
                          dim.lat.lon.year[2],"), m=list(c(1,0.5),NA))")

    ## offset
    offset.var <- "SweptArea"
    offset <- paste0("offset(log(",offset.var,"))")

    ##       1    2    3      4        5          6          7,           8,        9,    10,   11,     12
    mm <- c(tod, doy, year, latLon, doy.year, latLon.tod, latLon.doy, latLon.year, gear, ship, depth, offset)
    mSel <- rep(TRUE, length(mm))

    ## full model
    mps <- list(paste(mm[mSel],collapse=' + '))
    ## simple model
    mps <- c(full = mps,
             simple = list(paste(mm[c(rep(TRUE,5),rep(FALSE,3),rep(TRUE,4))],collapse=' + ')))

    return(mps)
}



#' @name list.datras.variables.req
#' @title List all DATRAS variables
#' @return List with all DATRAS variables for the HH and HL DATRAS data sets
#' @export
list.datras.variables.all <- function(){
    all.variables <- list()
    all.variables[["HH"]] <- c("RecordType","Survey","Quarter","Country","Ship",
                               "Gear","SweepLngt","GearEx","DoorType","StNo",
                               "HaulNo","Year","Month","Day","TimeShot",
                               "DepthStratum","HaulDur","DayNight","lat",
                               "lon","HaulLat","HaulLong","StatRec",
                               "Depth","HaulVal","HydroStNo","StdSpecRecCode",
                               "BySpecRecCode","DataType","Netopening",
                               "Rigging","Tickler","Distance","Warplngt",
                               "Warpdia","WarpDen","DoorSurface","DoorWgt",
                               "DoorSpread","WingSpread","Buoyancy","KiteDim",
                               "WgtGroundRope","TowDir","GroundSpeed",
                               "SpeedWater","SurCurDir","SurCurSpeed",
                               "BotCurDir","BotCurSpeed","WindDir","WindSpeed",
                               "SwellDir","SwellHeight","SurTemp","BotTemp",
                               "SurSal","BotSal","ThermoCline","ThClineDepth",
                               "CodendMesh","SecchiDepth","Turbidity",
                               "TidePhase","TideSpeed","PelSampType",
                               "MinTrawlDepth","MaxTrawlDepth",
                               "DateofCalculation")
    all.variables[["HL"]] <- c("RecordType","Survey","Quarter","Country","Ship",
                               "Gear","SweepLngt","GearEx","DoorType","StNo",
                               "HaulNo","Year","SpecCodeType","SpecCode",
                               "SpecVal","Sex","TotalNo","CatIdentifier",
                               "NoMeas","SubFactor","SubWgt","CatCatchWgt",
                               "LngtCode","LngtClass","HLNoAtLngt","DevStage",
                               "LenMeasType","DateofCalculation","Valid_Aphia","LngtCm")
    all.variables[["CA"]] <- c("RecordType","Survey","Quarter","Country","Ship",
                               "Gear","SweepLngt","GearEx","DoorType","StNo",
                               "HaulNo","Year","SpecCodeType","SpecCode","AreaType",
                               "AreaCode","LngtCode","LngtClass","Sex","Maturity",
                               "PlusGr","Age","CANoAtLngt","IndWgt","FishID",
                               "GenSamp","StomSamp","AgeSource","AgePrepMet","OtGrading",
                               "ParSamp","MaturityScale","DateofCalculation","AphiaID")
    return(all.variables)
}


#' @name list.datras.variables.req
#'
#' @title List required DATRAS variables
#'
#' @param swept.area.calculated Was swept area already calculated? Otherwise
#'     include Include DATRAS variables that are required for the swept area
#'     calculation. Default: TRUE
#'
#' @return List with the required DATRAS variables for the HH and HL DATRAS data sets
#'
#' @export
list.datras.variables.req <- function(swept.area.calculated = TRUE){
    all.variables <- list()
    all.variables[["HH"]] <- c("Survey","Year","Quarter","Country","Ship",
                               "Gear","StNo", "HaulNo","Month","Day","TimeShot",
                               "HaulDur","lat","lon","StatRec","DayNight",
                               "Depth","HaulVal", "StdSpecRecCode",
                               "BySpecRecCode","DataType", "SurTemp","BotTemp")
    if(swept.area.calculated){
        all.variables[["HH"]] <- c(all.variables[["HH"]],
                                   "SweptAreaDSKM2", "SweptAreaWSKM2", "SweptAreaBWKM2")
    }else{
        all.variables[["HH"]] <- c(all.variables[["HH"]], "HaulLat", "HaulLong",
                                   "DepthStratum", "SweepLngt","DoorType",
                                   "Distance","Warplngt", "DoorSpread",
                                   "WingSpread", "GroundSpeed")
    }
    all.variables[["HL"]] <- c("Survey","Year","Quarter","Country","Ship",
                               "Gear","StNo", "HaulNo","SpecCodeType",
                               "SpecCode","SpecVal","TotalNo","CatIdentifier",
                               "SubFactor","HLNoAtLngt","AphiaID")
    all.variables[["CA"]] <- c("Survey","Year","Quarter","Country","Ship",
                               "Gear","StNo", "HaulNo","SpecCodeType",
                               "SpecCode","AreaType",
                               "AreaCode","LngtCode","LngtClass","Sex","Maturity",
                               "PlusGr","Age","CANoAtLngt","IndWgt","FishID",
                               "GenSamp","StomSamp","AgeSource","AgePrepMet","OtGrading",
                               "ParSamp","MaturityScale","AphiaID")
    return(all.variables)
}



#' @name pred.statrec
#'
#' @title Predict Statistical rectangle based on latitude and longitude
#'
#' @param data Data set that includes variables: lat and lon
#'
#' @return Data set with predicted StatRec if NA.
#'
#' @export
pred.statrec <- function(data, tol = 0.00001, only.missing = TRUE,
                         fast.approach = TRUE,
                         verbose = TRUE, dbg = 0){

    datain <- data

    if(!inherits(data,"data.frame")){
        stop("Please provide a data.frame!")
    }

    if(!any(colnames(data) == "lat")) data$lat <- data$Lat
    if(is.null(data$lat)) data$lat <- data$ShootLat
    if(!any(colnames(data) == "lon")) data$lon <- data$Lon
    if(is.null(data$lon)) data$lon <- data$ShootLong
    if(is.null(data$lon) || is.null(data$lat)){
        stop("No lat and lon columns found in data set!")
    }

    data("ices.rectangles")

    if(fast.approach){
        lon.cuts <- sort(unique(round(c(ices.rectangles$sub_west,
                                        ices.rectangles$sub_east),5)))
        lat.cuts <- sort(unique(round(c(ices.rectangles$sub_south,
                                        ices.rectangles$sub_north),5)))
        ices.rectangles$lon_fac <- cut(ices.rectangles$sub_x, lon.cuts)
        ices.rectangles$lat_fac <- cut(ices.rectangles$sub_y, lat.cuts)
        data$lon_fac <- cut(data$lon, lon.cuts)
        data$lat_fac <- cut(data$lat, lat.cuts)
        cols.merge <- c("ICESNAME","sub_code","Area_27","Ecoregion","sub_area")
        if(dbg > 0) cols.merge <- c(cols.merge, "sub_x","sub_y","stat_x","stat_y")
        data <- plyr::join(x = data, y = ices.rectangles[,c("lon_fac","lat_fac",cols.merge)],
                           by = c("lon_fac","lat_fac"))
        datain <- cbind(datain, data[,cols.merge])
        colnames(datain)[colnames(datain) == "ICESNAME"] <- "StatRec"
        colnames(datain)[colnames(datain) == "sub_code"] <- "SubStatRec"
        colnames(datain)[colnames(datain) == "sub_area"] <- "SubStatRec_area"
        if(dbg > 0){
            colnames(datain)[colnames(datain) == "sub_x"] <- "sub_lon"
            colnames(datain)[colnames(datain) == "sub_y"] <- "sub_lat"
            colnames(datain)[colnames(datain) == "stat_x"] <- "stat_lon"
            colnames(datain)[colnames(datain) == "stat_y"] <- "stat_lat"
        }
    }else{
        if(!any(colnames(datain) == "StatRec")) datain$StatRec <- NA
        if(!any(colnames(datain) == "SubStatRec")) datain$SubStatRec <- NA
        if(!any(colnames(datain) == "Area_27")) datain$Area_27 <- NA
        if(!any(colnames(datain) == "Ecoregion")) datain$Ecoregion <- NA
        if(!any(colnames(datain) == "SubStatRec_area")) datain$SubStatRec_area <- NA
        if(dbg > 0){
            datain$sub_lon <- NA
            datain$sub_lat <- NA
            datain$stat_lon <- NA
            datain$stat_lat <- NA
        }

        if(only.missing){
            ind.na <- which(is.na(datain$StatRec))
        }else{
            ind.na <- 1:nrow(datain)
        }
        if(verbose) writeLines("Filling missing StatRec!")
        if(length(ind.na) > 0){
            for(i in 1:length(ind.na)){
                ind <- which(ices.rectangles$sub_south - data$lat[ind.na[i]] < tol &
                             ices.rectangles$sub_north - data$lat[ind.na[i]] > tol &
                             ices.rectangles$sub_west - data$lon[ind.na[i]]  < tol &
                             ices.rectangles$sub_east - data$lon[ind.na[i]]  > tol )
                if(length(ind) > 0){
                    datain$StatRec[ind.na[i]] <- ices.rectangles$ICESNAME[ind]
                    datain$SubStatRec[ind.na[i]] <- ices.rectangles$sub_code[ind]
                    datain$Area_27[ind.na[i]] <- ices.rectangles$Area_27[ind]
                    datain$Ecoregion[ind.na[i]] <- ices.rectangles$Ecoregion[ind]
                    datain$SubStatRec_area[ind.na[i]] <- ices.rectangles$sub_area[ind]
                    if(dbg > 0){
                        datain$sub_lon[ind.na[i]] <- ices.rectangles$sub_x[ind]
                        datain$sub_lat[ind.na[i]] <- ices.rectangles$sub_y[ind]
                        datain$stat_lon[ind.na[i]] <- ices.rectangles$stat_x[ind]
                        datain$stat_lat[ind.na[i]] <- ices.rectangles$stat_y[ind]
                    }
                }
            }
        }
    }

    return(datain)
}


#' @name add.gear.categories
#'
#' @title Assign gears to categories based on gear and survey
#'
#' @param data Data set that includes variables: Gear and Survey
#'
#' @details See data(gear.categories) for the assignment.
#'
#' @importFrom plyr join
#'
#' @return Data set with added GearCat column.
#'
#' @export
add.gear.categories <- function(data){

    flag <- ifelse(all(c("Survey","Gear","Area_27") %in% colnames(data)), 0, 1)
    if(flag) stop("Function requires variables Survey, Gear, Area_27. Please check your data.")

    data("gear.categories")

    ## data <- merge(data, gear.categories, by=c('Survey','Gear'), all.x = TRUE) ## 104s
    data <- plyr::join(data, gear.categories, by=c('Survey','Gear')) ## 30s

    ## Manual gear corrections
    ## 1. 27.7g and 27.7a
    unique(data$Area_27)
    ind <- which(data$Survey == "IE-IGFS" & data$Gear == "GOV" &
                 data$Area_27 %in% c("7.g","7.a"))
    if(length(ind) > 0) data$GearCat[ind] <- "GOV_CL"
    ## 2.
    ind <- which(data$Survey == "SWC-IBTS" & data$Gear == "GOV" &
                 data$lat < 57.5)
    if(length(ind) > 0) data$GearCat[ind] <- "GOV_CL"

    return(data)
}


#' @name correct.surveys
#'
#' @title Correct surveys
#'
#' @param data Data set that includes variables: Species and BySpecRecCode
#'
#' @return Data set with corrected Species column
#'
#' @export
correct.surveys <- function(data){

    flag <- ifelse(any(c("Survey","Year","Quarter","Gear") %in% colnames(data)), 0, 1)
    if(flag) stop("Function requires variables Survey, Year, Quarter, Gear. Please check your data.")

    ## Rename some surveys
    ## -------------
    data$Survey[which(data$Survey == "SCOWCGFS")] <- "SWC-IBTS"
    data$Survey[which(data$Survey == "SCOROC")] <- "ROCKALL"
    data$Survey[which(data$Survey == "BTS-VIII")] <- "BTS"


    ## Remove other gears than TVL and TVS from BITS
    ## -------------
    data <- subset(data, Survey != "BITS" |
                         (Survey == "BITS" & Gear %in% c("TVS","TVL")))

    ## Remove surveys with issues
    ## -------
    ## Remove NS-IBTS quarter 2 and 4
    ind <- which(data$Survey == "NS-IBTS" & data$Quarter %in% c(2,4))
    if(length(ind) > 0) data <- data[-ind,]
    ## IGFS survey only sampled at depths greater than 200m from 2005 to present
    ind <- which(data$Survey == "NIGFS" & data$Year < 2005)
    if(length(ind) > 0) data <- data[-ind,]

    ## CHECK: With survey experts
    ## length(which(data$Survey=='SWC-IBTS' & data$Quarter %in% c(2,3))) ## 3300
    ## length(which(data$Survey=='BTS' & data$Year<1987))    ## 7618

    return(data)
}


#' @name correct.species
#'
#' @title Correct species
#'
#' @param data Data set that includes variables: Species and BySpecRecCode
#'
#' @return Data set with corrected Species column
#'
#' @export
correct.species <- function(data){

    flag <- ifelse(any(c("Species","BySpecRecCode") %in% colnames(data)), 0, 1)
    if(flag) stop("Function requires variables Species and/or BySpecRecCode. Please check your data.")

    ## Code to integrate from Anna on species bycatch corrections
    if("Species" %in% colnames(data)){
        data$Species[which(data$Species == "Synaphobranchus kaupii")] <- "Synaphobranchus kaupi"
        data$Species[which(data$Species == "Dipturus batis")] <- "Dipturus spp"
        data$Species[which(data$Species == "Dipturus flossada")] <- "Dipturus spp"
        data$Species[which(data$Species == "Dipturus batis-complex")] <- "Dipturus spp"
        data$Species[which(data$Species == "Dipturus intermedia")] <- "Dipturus spp"
        data$Species[which(data$Species == "Dipturus")] <- "Dipturus spp"
        data$Species[which(data$Species == "Liparis montagui")] <- "Liparis spp"
        data$Species[which(data$Species == "Liparis liparis")] <- "Liparis spp"
        data$Species[which(data$Species == "Liparis liparis liparis")] <- "Liparis spp"
        data$Species[which(data$Species == "Chelon aurata")] <- "Chelon spp"
        data$Species[which(data$Species == "Chelon ramada")] <- "Chelon spp"
        data$Species[which(data$Species == "Mustelus mustelus/asterias")] <- "Mustelus spp"
        data$Species[which(data$Species == "Mustelus")] <- "Mustelus spp"
        data$Species[which(data$Species == "Mustelus mustelus")] <- "Mustelus spp"
        data$Species[which(data$Species == "Mustelus asterias")] <- "Mustelus spp"
        data$Species[which(data$Species == "Alosa")] <- "Alosa spp"
        data$Species[which(data$Species == "Alosa alosa")] <- "Alosa spp"
        data$Species[which(data$Species == "Alosa fallax")] <- "Alosa spp"
        data$Species[which(data$Species == "Argentina")] <- "Argentina spp"
        data$Species[which(data$Species == "Argentinidae")] <- "Argentina spp"
        data$Species[which(data$Species == "Argentina silus")] <- "Argentina spp"
        data$Species[which(data$Species == "Argentina sphyraena")] <- "Argentina spp"
        data$Species[which(data$Species == "Callionymus reticulatus")] <- "Callionymus spp"
        data$Species[which(data$Species == "Callionymus maculatus")] <- "Callionymus spp"
        data$Species[which(data$Species == "Ciliata mustela")] <- "Ciliata spp"
        data$Species[which(data$Species == "Ciliata septentrionalis")] <- "Ciliata spp"
        data$Species[which(data$Species == "Gaidropsarus")] <- "Gaidropsarus spp"
        data$Species[which(data$Species == "Gaidropsaurus macrophthalmus")] <- "Gaidropsarus spp"
        data$Species[which(data$Species == "Gaidropsaurus mediterraneus")] <- "Gaidropsarus spp"
        data$Species[which(data$Species == "Gaidropsaurus vulgaris")] <- "Gaidropsarus spp"
        data$Species[which(data$Species == "Sebastes")] <- "Sebastes spp"
        data$Species[which(data$Species == "Sebastes norvegicus")] <- "Sebastes spp"
        data$Species[which(data$Species == "Sebastes mentella")] <- "Sebastes spp"
        data$Species[which(data$Species == "Sebastes marinus")] <- "Sebastes spp"
        data$Species[which(data$Species == "Syngnathus")] <- "Syngnatus spp"
        data$Species[which(data$Species == "Syngnathus rostellatus")] <- "Syngnatus spp"
        data$Species[which(data$Species == "Syngnathus acus")] <- "Syngnatus spp"
        data$Species[which(data$Species == "Syngnathus typhle")] <- "Syngnatus spp"
        data$Species[which(data$Species == "Nerophis ophidion")] <- "Syngnatus spp"
        data$Species[which(data$Species == "Pomatoschistus")] <- "Pomatoschistus spp"
        data$Species[which(data$Species == "Pomatoschistus microps")] <- "Pomatoschistus spp"
        data$Species[which(data$Species == "Pomatoschistus minutus")] <- "Pomatoschistus spp"
        data$Species[which(data$Species == "Pomatoschistus pictus")] <- "Pomatoschistus spp"
        data$Species[which(data$Species == "Lesueurigobius")] <- "Gobius spp"
        data$Species[which(data$Species == "Gobius cobitis")] <- "Gobius spp"
        data$Species[which(data$Species == "Gobius niger")] <- "Gobius spp"
        data$Species[which(data$Species == "Leusueurigobius friesii")] <- "Gobius spp"
        data$Species[which(data$Species == "Neogobius melanostomus")] <- "Gobius spp"
        data$Species[which(data$Species == "Neogobius")] <- "Gobius spp"

        data <- subset(data, !(BySpecRecCode==0 & Data == "BTS" &
                               !Species %in% c("Chelidonichthys cuculus","Chelidonichthys lucerna","Eutrigla gurnardus",
                                               "Gadus morhua","Limanda limanda","Lophius piscatorius",
                                               "Merlangius merlangus","Microstomus kitt","Mullus surmuletus",
                                               "Mustelus asterias","Pegusa lascaris","Platichthys flesus",
                                               "Pleuronectes platessa","Raja brachyura","Raja clavata",
                                               "Raja montagui","Scophthalmus maximus","Scophthalmus rhombus",
                                               "Scyliorhinus canicula","Solea solea","Trispoterus luscus")))



        data <- subset(data, !(BySpecRecCode==0 & Data == "SP-NORTH" &
                               !Species %in% c("Chelidonichthys lucerna","Conger conger","Eutrigla gurnardus",
                                               "Galeus melastomus","Helicolenus dactylopterus","Lepidorhombus boscii",
                                               "Lepidorhombus whiffiagoni","Leucoraja circularis",
                                               "Leucoraja naevus","Lophius budegassa","Lophius piscatorius","Merluccius merluccius",
                                               "Micromesistius poutassou","Phycis blennoides", "Raja clavata","Raja montagui",
                                               "Scomber scombrus","Scyliorhinus canicula","Trachurus trachurus",
                                               "Trisopterus luscus","Zeus faber")))
        data <- subset(data, !(BySpecRecCode==0 & Data == "SP-PORC" &
                               !Species %in% c("Argentina silus","Chelidonichthys lucerna","Conger conger",
                                               "Eutrigla gurnardus","Gadus morhua","Galeus melastomus",
                                               "Glyptocephalus cynoglossu","Helicolenus dactylopterus","Hexanchus griseus",
                                               "Hippoglossoides platessoi","Lepidorhombus boscii","Lepidorhombus whiffiagoni",
                                               "Leucoraja circularis","Leucoraja naevus", "Lophius budegassa","Lophius piscatorius",
                                               "Melanogrammus aeglefinus","Merluccius merluccius","Micromesistius poutassou",
                                               "Molva dypterygia","Molva molva","Phycis blennoides","Raja clavata",
                                               "Raja montagui","Scomber scombrus","Scyliorhinus canicula",
                                               "Trachurus trachurus","Zeus faber")))
        data <- subset(data, !(BySpecRecCode==0 & Data %in% c("NS-IBTS1","NS-IBTS3") &
                               !Species %in% c("Clupea harengus","Sprattus sprattus","Scomber scombrus","Gadus morhua",
                                               "Melanogrammus aeglefinus","Merlangius merlangus","Trisopterus esmarkii")))
        data <- subset(data, !(BySpecRecCode==2 &
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
        data <- subset(data, !(BySpecRecCode==3 &
                               !Species %in% c("Pollachius virens","Pollachius pollachius","Trisopterus luscus","Trisopterus minutus",
                                               "Micromesistius poutassou","Molva molva", "Merluccius merluccius","Brosme brosme",
                                               "Clupea harengus","Sprattus sprattus","Scomber scombrus","Gadus morhua",
                                               "Melanogrammus aeglefinus","Merlangius merlangus","Trisopterus esmarkii")))
        data <- subset(data, !(BySpecRecCode==4 &
                               !Species %in% c("Platichthys flesus","Pleuronectes platessa","Limanda limanda",
                                               "Lepidorhombus whiffiagoni","Hippoglossus hippoglossus","Hippoglossoides platessoi",
                                               "Glyptocephalus cynoglossu","Microstomus kitt","Scophthalmus maximus","Scophthalmus rhombus",
                                               "Solea solea", "Clupea harengus","Sprattus sprattus","Scomber scombrus","Gadus morhua",
                                               "Melanogrammus aeglefinus","Merlangius merlangus","Trisopterus esmarkii")))
        data <- subset(data, !(BySpecRecCode==5 &
                               !Species %in% c("Ammodytidae","Anarhichas lupus","Argentina silus","Argentina sphyraena",
                                               "Chelidonichthys cuculus","Callionymus lyra","Eutrigla gurnardus",
                                               "Lumpenus lampretaeformis", "Mullus surmuletus","Squalus acanthias","Trachurus trachurus",
                                               "Clupea harengus","Sprattus sprattus","Scomber scombrus","Gadus morhua",
                                               "Melanogrammus aeglefinus","Merlangius merlangus","Trisopterus esmarkii")))
    }

    return(data)
}




#' @name checkmark
#'
#' @title Checkmark
#'
#' @param x TRUE or FALSE
#'
#' @return Checkmark or X
#'
#' @export
checkmark <- function(x) ifelse(x, '\u2714', '\u2613')



#' @name subset.fdist.datras
#'
#' @title Subset
#'
#' @param x object to be subsetted.
#' @param subset logical expression indicating elements or rows to keep: missing
#'     values are taken as false.
#'
#' @export
subset.fdist.datras <- function(x, subset){

    ## Check validity of data
    ## ------------------
    if(!inherits(x, "fdist.datras")){
        stop("Function requires a list of class fdist.datras with two DATRAS data sets 'HH' and 'HL' as elements (see function 'download.data').")
    }

    hh <- x$HH
    hl <- x$HL
    ca <- x$CA
    e <- substitute(subset)

    ## AphiaID only in hl and ca
    ind <- grepl("AphiaID", e, fixed = TRUE)
    if(any(ind)){
        if(length(e) > 2){
            e[[which(ind)]][[grep("AphiaID",e[[which(ind)]])]] <- substitute(Valid_Aphia)
        }else{
            e[[grep("AphiaID",e)]] <- substitute(Valid_Aphia)
        }
        if(any(grepl("&", e, fixed = TRUE))){
            ind1 <- which(grepl("&", e, fixed = TRUE))
            e1 <- e[!ind]
            if(ind1 == 1 || ind1 == length(e1)) e1 <- e1[-ind1]
            if(length(e1) == 1) e1 <- e1[[1]]
        }else if(any(grepl("|", e, fixed = TRUE))){
            ind1 <- which(grepl("|", e, fixed = TRUE))
            e1 <- e[!ind]
            if(ind1 == 1 || ind1 == length(e1)) e1 <- e1[-ind1]
            if(length(e1) == 1) e1 <- e1[[1]]
        }else{
            e1 <- NULL
        }
    }else{
        e1 <- e
    }


    ## TODO: Find better solution
    ## LngtCm only in hl and ca
    ## ind <- grepl("LngtCm", e, fixed = TRUE)
    ## if(any(ind)){
    ##     if(any(grepl("&", e, fixed = TRUE))){
    ##         ind1 <- which(grepl("&", e, fixed = TRUE))
    ##         e1 <- e[!ind]
    ##         if(ind1 == 1 || ind1 == length(e1)) e1 <- e1[-ind1]
    ##         if(length(e1) == 1) e1 <- e1[[1]]
    ##     }else if(any(grepl("|", e, fixed = TRUE))){
    ##         ind1 <- which(grepl("|", e, fixed = TRUE))
    ##         e1 <- e[!ind]
    ##         if(ind1 == 1 || ind1 == length(e1)) e1 <- e1[-ind1]
    ##         if(length(e1) == 1) e1 <- e1[[1]]
    ##     }else{
    ##         e1 <- NULL
    ##     }
    ## }else{
    ##     e1 <- e
    ## }


    if(!is.null(e1)) hh <- base::subset.data.frame(hh, eval(e1))
    hl <- base::subset.data.frame(hl, eval(e))
    if(!is.null(ca)) ca <- base::subset.data.frame(ca, eval(e))

    ## Return
    ## -----------
    res <- list(HH = hh, HL = hl, CA = ca)
    class(res) <- c("fdist.datras","list")
    return(res)
}


#' @name subset.fdist.prepped
#'
#' @title Subset
#'
#' @param x object to be subsetted.
#' @param subset logical expression indicating elements or rows to keep: missing
#'     values are taken as false.
#'
#' @export
subset.fdist.prepped <- function(x, subset){

    ## Check validity of data
    ## ------------------
    if(!inherits(x, "fdist.prepped")){
        stop("Function requires a list of class fdist.preppred with two data sets 'survey0' and 'survey' as elements (see function 'prep.data').")
    }

    survey0 <- x$survey0
    survey <- x$survey
    e <- substitute(subset)

    ## AphiaID only in survey
    ind <- grepl("AphiaID", e, fixed = TRUE)
    if(any(ind)){
        e1 <- e[!ind]
        ind1 <- which(grepl("&", e1, fixed = TRUE))
        if(ind1 == 1 || ind1 == length(e1)) e1 <- e1[-ind1]
        if(length(e1) == 1) e1 <- e1[[1]]
    }else{
        e1 <- e
    }

    survey0 <- base::subset.data.frame(survey0, eval(e1))
    survey <- base::subset.data.frame(survey, eval(e))


    ## Return
    ## -----------
    res <- list(survey0 = survey0, survey = survey)
    class(res) <- c("fdist.prepped","list")
    return(res)
}




#' @name get.gear.effect
#'
#' @title Subset
#'
#' @param fit fit
#' @param mod 1
#' @param CI 0.95
#'
#' @export
get.gear.effect <- function(fit, mod = 1, CI = 0.95, var = "Gear", exp = TRUE){

    ## zscore <- qnorm(CI + (1 - CI)/2)
    ## fe <- data.frame(summary(fit$fits[[1]]$pModels[[1]])$p.table)[,c(1,2)]
    ## colnames(fe) =  c('value', 'se')
    ## fe[1,] <- c(0,0)
    ## fe$lo <- fe$value - fe$se
    ## fe$up <- fe$value + fe$se
    ## fe <- exp(fe)

    sel <- grep(var,names(coef(fit$fits[[1]]$pModels[[1]])))
    vals <- coef(fit$fits[[1]]$pModels[[1]])[sel]
    if(exp) vals <- exp(vals)
    sds <- sqrt(diag(vcov(fit$fits[[1]]$pModels[[1]])[sel,sel]))
    ## HERE:
    if(length(levels(droplevels(fit$data[,var]))) > length(vals)){
        vals <- c(1, vals)
        sds <- c(NA, sds)
    }
    res <- as.data.frame(cbind(vals, sds))
    colnames(res) <- c("mean", "sd")
    rownames(res) <- levels(droplevels(fit$data[,var]))

    return(res)
}



#' @name remove.zeros
#'
#' @title Remove zeros from data based on presence-absence model with cutoff
#'
#' @param data data
#' @param cutat default: -3
#' @param plot default TRUE
#' @param verbose default TRUE
#' @param response default "bio"
#'
#' @author Casper W. Berg
#'
#' @export
remove.zeros <- function(d, cutat=-3, plot=TRUE, verbose=TRUE, response="bio"){

    if(verbose) cat("Doing ",d$AphiaID[1],"\n")
    if(plot) par(mfrow=c(3,4))

    ## Remove empty levels
    d$Gear = factor(d$Gear,levels=unique(d$Gear))
    d$ShipG = factor(d$ShipG,levels=unique(d$ShipG))


    d$isPos = d[,response] > 0
    d$dum = 1
    system.time( m <- gam( isPos ~ s(Gear,bs='re',by=dum) + s(ShipG,bs='re',by=dum) + s(Depth,bs='ds',m=c(1,0),k=10) + s(lon,bs='ds',m=c(1,0),k=30) + s(lat,bs='ds',m=c(1,0),k=30),data=d,family=binomial) )


    getLimits<-function(nam,cutat,plot){
        lims = range(d[,nam])
        pd = d[rep(1,200),]
        pd$lon = median(d$lon)
        pd$lat = median(d$lat)
        pd$Depth = median(d$Depth)
        pd$dum=0

        pd[,nam] = seq(lims[1],lims[2],length.out=200)

        ## Obs, vals includes mean -- need to centralize!
        vals = predict(m,newdata=pd)
        vals = vals - mean(vals)

        idxMin = 1
        while(vals[idxMin] < cutat) idxMin = idxMin +1

        idxMax = 200
        while(vals[idxMax] < cutat) idxMax = idxMax - 1

        if(plot){
            plot(pd[,nam],vals,type="l",xlab=nam)
            abline(v=pd[idxMin,nam],col=2)
            abline(v=pd[idxMax,nam],col=2)
            rug(d[,nam])
        }
        return(c(min=pd[idxMin,nam],max=pd[idxMax,nam]))

    }

    depthLim = getLimits("Depth",cutat=cutat,plot=plot)
    lonLim = getLimits("lon",cutat=cutat,plot=plot)
    latLim = getLimits("lat",cutat=cutat,plot=plot)

    getBadLevels<-function(nam,cutat=cutat,plot){

        sel = grep(nam,names(coef(m)))
        vals = coef(m)[sel]
        names(vals) <- levels(d[,nam])

        if(plot){
            plot(vals)
            axis(1,at=1:length(vals),labels=names(vals))
            abline(h=cutat,col=2)
        }
        return(names(vals[vals<cutat]))
    }

    badGears = getBadLevels("Gear",cutat=cutat,plot=plot)
    badShips = getBadLevels("ShipG",cutat=cutat,plot=plot)

    cat("Bad levels of gear: ",badGears,"\n")
    cat("Bad levels of ShipG: ",badShips,"\n")

    dsub = d
    dsub = dsub[ dsub$Depth > depthLim[1],]
    dsub = dsub[ dsub$Depth < depthLim[2],]
    dsub = dsub[ dsub$lon > lonLim[1],]
    dsub = dsub[ dsub$lon < lonLim[2],]
    dsub = dsub[ dsub$lat > latLim[1],]
    dsub = dsub[ dsub$lat < latLim[2],]
    dsub = dsub[ !dsub$Gear %in% badGears, ]
    dsub = dsub[ !dsub$ShipG %in% badShips, ]

    if(plot){
        if(length(badGears)>0){
            sel = which(d$Gear %in% badGears)
            plot(d$lon[sel],d$lat[sel],pch=16,main="Bad gears",xlim=range(d$lon),ylim=range(d$lat))
            maps::map("worldHires", fill = TRUE, plot = TRUE,
                      add = TRUE, col = grey(0.8))
        }


        if(length(badShips)>0){
            sel = which(d$ShipG %in% badShips)
            plot(d$lon[sel],d$lat[sel],pch=16,main="Bad ships",xlim=range(d$lon),ylim=range(d$lat))
            maps::map("worldHires", fill = TRUE, plot = TRUE,
                      add = TRUE, col = grey(0.8))
        }
    }

    cat("Before:",nrow(d), " After:",nrow(dsub),"\n")

    if(plot){
        avgb = max(sqrt(d$bio/d$SweptArea))
        plot(d$lon,d$lat,cex=sqrt(d$bio/d$SweptArea)/(avgb/10),main=paste("Before",nrow(d)))
        points(d$lon[!d$isPos],d$lat[!d$isPos],pch=".",col=2)
        maps::map("worldHires", fill = TRUE, plot = TRUE,
                  add = TRUE, col = grey(0.8))

        plot(dsub$lon,dsub$lat,cex=sqrt(dsub$bio/dsub$SweptArea)/(avgb/10),main=paste("After",nrow(dsub)))
        points(dsub$lon[!dsub$isPos],dsub$lat[!dsub$isPos],pch=".",col=2)
        maps::map("worldHires", fill = TRUE, plot = TRUE,
                  add = TRUE, col = grey(0.8))

        title(paste(d$genus[1],d$family[1]),outer=TRUE)
    }
    return(dsub)
}
