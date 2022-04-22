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
                polygon(c(tmpj$WEST, tmpj$EAST, tmpj$EAST, tmpj$WEST),
                        c(tmpj$SOUTH, tmpj$SOUTH, tmpj$NORTH, tmpj$NORTH),
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
#' @param dim.lat.lon Dimensions of the basis for the Lat - Lon smooth term (1
#'     number)
#' @param dim.ctime.lat.lon Dimensions of the basis for the ctime - Lat - Lon
#'     smooth term (2 numbers)
#' @param dim.timeOfYear.lat.lon Dimensions of the basis for the timeOfYear -
#'     Lat - Lon smooth term (2 numbers)
#'
#' @return List with all model structures
#'
#' @export
list.recom.models <- function(specdata, use.toy = TRUE,
                              use.swept.area = TRUE,
                              dim.lat.lon = 128,
                              dim.ctime.lat.lon = c(12,32),
                              dim.timeOfYear.lat.lon = c(6,30)){

    ## Checks
    if(length(dim.ctime.lat.lon) != 2) stop("The variable dim.ctime.lat.lon has to have length equal to 2.")
    if(length(dim.timeOfYear.lat.lon) != 2) stop("The variable dim.timeOfYear.lat.lon has to have length equal to 2.")

    LatLon <- paste0("s(Lon, Lat, bs=c('ds'), k=c(",dim.lat.lon[1],"), m=c(1,0.5))")
    ctimeLatLon <- paste0("te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(",
                          dim.ctime.lat.lon[1], ",",
                          dim.ctime.lat.lon[2],"), m=list(c(1,0), c(1,0.5)))")
    timeOfYearLatLon <- paste0("te(timeOfYear, Lon, Lat, d=c(1,2), bs=c('cc','ds'), k=c(",
                               dim.timeOfYear.lat.lon[1],",",
                               dim.timeOfYear.lat.lon[2],"), m=list(c(1,0), c(1,0.5)))")
    depth <- "s(Depth, bs='ds', k=5, m=c(1,0))"
    ship <- "s(ShipG, bs='re')"
    gear <- "Gear"
    offset.var <- ifelse(use.swept.area, "SweptArea", "HaulDur")
    offset <- paste0("offset(log(",offset.var,"))")

    ##        1        2            3                 4      5    6     7
    mm <- c(LatLon, ctimeLatLon, timeOfYearLatLon, depth, gear, ship, offset)
    mSel <- rep(TRUE, length(mm))

    if (!use.toy) mSel[3] <- FALSE
    if(length(unique(specdata$Gear)) == 1)  mSel[5] <- FALSE
    if(length(unique(specdata$ShipG)) == 1) mSel[6] <- FALSE

    ## all
    mps <- list(paste(mm,collapse=' + '))
    ## no ship, if exist
    if(mSel[6]){
        mSel[6] <- FALSE
        mps <- append(mps,paste(mm[mSel],collapse=' + '))
    }
    ## no gear, if exist
    if(mSel[5]){
        mSel[5] <- FALSE
        mps <- append(mps,paste(mm[mSel],collapse=' + '))
    }
    ## no timeOfYear, if exist
    if(mSel[3]){
        mSel[3] <- FALSE
        mps <- append(mps,paste(mm[mSel],collapse=' + '))
    }
    ## no depth, if exist
    if(mSel[4]){
        mSel[4] <- FALSE
        mps <- append(mps,paste(mm[mSel],collapse=' + '))
    }

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
                               "DepthStratum","HaulDur","DayNight","ShootLat",
                               "ShootLong","HaulLat","HaulLong","StatRec",
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
                               "LenMeasType","DateofCalculation","Valid_Aphia")
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
                               "HaulDur","ShootLat","ShootLong","StatRec",
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
                               "SubFactor","HLNoAtLngt","Valid_Aphia")
    return(all.variables)
}



#' @name predict.statrec
#'
#' @title Predict Statistical rectangle based on latitude and longitude
#'
#' @param data Data set that includes variables: StatRec, ShootLat, and ShootLong
#'
#' @return Data set with predicted StatRec if NA.
#'
#' @export
predict.statrec <- function(data){

    flag <- ifelse(all(c("StatRec","ShootLat","ShootLong") %in% colnames(data)), 0, 1)

    if(flag) stop("Function requires variables StatRec, ShootLat, and ShootLong. Please check your data.")

    data("ices.rectangles")

    StatRecNew <- rep(NA, length(data$StatRec))
    ind <- which(is.na(data$StatRec))
    if(length(ind) > 0){
        for(i in 1:length(ind)){
            tmp2 <- ices.rectangles$ICESNAME[
                                        which(data$ShootLat[ind[i]] >= ices.rectangles$SOUTH &
                                              data$ShootLat[ind[i]] < ices.rectangles$NORTH &
                                              data$ShootLong[ind[i]] >= ices.rectangles$WEST &
                                              data$ShootLong[ind[i]] < ices.rectangles$EAST)]
            if(length(tmp2) == 1) StatRecNew[ind[i]] <- tmp2
        }
        ind <- which(!is.na(StatRecNew) & is.na(data$StatRec))
        data$StatRec[ind] <- StatRecNew[ind]
    }

    return(data)
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

    flag <- ifelse(all(c("Survey","Gear") %in% colnames(data)), 0, 1)
    if(flag) stop("Function requires variables Survey and Gear. Please check your data.")

    data("gear.categories")

    ## data <- merge(data, gear.categories, by=c('Survey','Gear'), all.x = TRUE) ## 104s
    data <- plyr::join(data, gear.categories, by=c('Survey','Gear')) ## 30s

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

    hh <- base::subset.data.frame(hh, eval(e1))
    hl <- base::subset.data.frame(hl, eval(e))



    ## Return
    ## -----------
    res <- list(HH = hh, HL = hl)
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
