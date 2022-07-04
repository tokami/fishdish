#' @name prep.species
#'
#' @title Prepare species
#'
#' @param data Data set
#' @param aphiaID required (single species or vector of species)
#' @param use.gear.cat = TRUE
#' @param remove.fragmented.years = TRUE
#' @param max.depth Maximum depth to inclde (default 1000m)
#' @param min.gears = 1
#' @param min.surveys = 1
#' @param min.hauls = 1
#' @param min.ship.gear = 1
#' @param verbose Print stuff Default: TRUE
#'
#' @return List containing hh and hl data sets.
#'
#' @export
prep.species <- function(data, aphiaID = NULL,
                         use.gear.cat = TRUE,
                         remove.fragmented.years = TRUE,
                         max.depth = 1000,
                         min.gears = 0, min.surveys = 0, min.hauls = 0, min.ship.gear = 0,
                         verbose = TRUE){
    ## Check validity of data
    ## ------------------
    if(!inherits(data,"fdist.prepped") || !all(c("survey0","survey") %in% names(data)))
        stop("Function requires list with the data sets 'survey0' and 'survey' created by the function 'prep.data'.")
    survey0 <- data$survey0
    survey <- data$survey

    if(any(colnames(survey) == "N")){
    }else if(!any(colnames(survey) == "N") && any(colnames(survey) == "n.juv") && any(colnames(survey) == "n.adult")){
        survey$N <- survey$n.juv + survey$n.adult
    }else{
        stop("No N!")
    }
    if(!any(colnames(survey) == "bio") && any(colnames(survey) == "bio.juv") && any(colnames(survey) == "bio.adult")){
        survey$bio <- survey$bio.juv + survey$bio.adult
    }


    ## Subset all species for given species category
    ## --------------------------------------
    if(!is.null(aphiaID) && !is.na(aphiaID)){
        if(!any(aphiaID %in% unique(survey$AphiaID))) stop("Provided Aphia IDs are not included in the data set! Please check if the Aphia IDs were included in the data preparation step (prep.data).")
        survey.spp <- subset(survey, AphiaID %in% aphiaID)
    }else{
        survey.spp <- survey
    }


    ## Gears and surveys
    ## --------------------------------------
    ## Use Gear category rather than many individual gears
    if(use.gear.cat){
        survey.spp$GearOri <- factor(survey.spp$Gear)
        survey0$GearOri <- factor(survey0$Gear)
        survey.spp$Gear <- factor(survey.spp$GearCat)
        survey0$Gear <- factor(survey0$GearCat)
        survey.spp$ShipG <- factor(paste0(survey.spp$Ship,":",survey.spp$GearCat))
        survey0$ShipG <- factor(paste0(survey0$Ship,":",survey0$GearCat))
    }

    ## Gears
    ## --------------
    if(verbose){
        writeLines(paste("Number of gears (categories) in survey0: ",
                         length(unique(survey0$Gear)), sep = "\t\t"))
        writeLines(paste("Number of gears (categories) in survey:  ",
                         length(unique(survey.spp$Gear)), sep = "\t\t"))
    }
    ## Only keep gears with more or equal x individual hauls
    tmp <- aggregate(list(haul.id = survey.spp$haul.id[survey.spp$N > 0]),
                     by = list(Gear = survey.spp$Gear[survey.spp$N > 0]),
                     function(x) length(unique(x)))
    gears.keep <- as.character(tmp$Gear[tmp$haul.id >= min.gears])
    colnames(tmp) <- c("Gear","#hauls")
    if(verbose) print(tmp)

    ## Surveys
    ## --------------
    if(verbose){
        writeLines(paste("Number of surveys in survey0: ",
                          length(unique(survey0$Survey)), sep = "\t\t\t\t\t\t\t"))
        writeLines(paste("Number of surveys in survey:  ",
                          length(unique(survey.spp$Survey)), sep = "\t\t\t\t\t\t\t"))
    }
    ## Only keep surveys with more or equal 5 individual hauls
    tmp <- aggregate(list(haul.id = survey.spp$haul.id[survey.spp$N > 0]),
                     by = list(Survey = survey.spp$Survey[survey.spp$N > 0]),
                     function(x) length(unique(x)))
    surveys.keep <- as.character(tmp$Survey[tmp$haul.id >= min.surveys])
    colnames(tmp) <- c("Gear","#surveys")
    if(verbose) print(tmp)

    ## Ship-Gear
    ## --------------
    tmp <- aggregate(list(haul.id = survey.spp$haul.id[survey.spp$N > 0]),
                     by = list(ShipG = survey.spp$ShipG[survey.spp$N > 0]),
                     function(x) length(unique(x)))
    shipgear.keep <- as.character(tmp$ShipG[tmp$haul.id >= min.ship.gear])
    colnames(tmp) <- c("Gear","#shipG")
    if(verbose) print(tmp)

    ## Depth
    ## --------------
    if(is.null(max.depth) || is.na(max.depth)) max.depth <- max(c(survey0$Depth,survey.spp$Depth), na.rm = TRUE)

    ## Realized habitat of species category
    ## --------------------------------------
    ## only keep ices squares where species category is present
    ## TODO: make this an arugment or separate function that can be called to plot and select
    ices.keep <- sort(unique(survey.spp$StatRec))  ## CHECK: that no StatRec with N=0 are in survey.spp
    ## NEW: make argument or only use this?
    ## TODO: make check that Area_27 is included!!
    ices.keep <- sort(unique(survey.spp$Area_27))


    ## Apply selections to both data sets
    ## --------------------------------------
    survey.spp <- subset(survey.spp,
                         Area_27 %in% ices.keep &
                         Gear %in% gears.keep &
                         ShipG %in% shipgear.keep &
                         Survey %in% surveys.keep &
                         Depth <= max.depth)
    survey0 <- subset(survey0,
                      Area_27 %in% ices.keep &
                      Gear %in% gears.keep &
                      ShipG %in% shipgear.keep &
                      Survey %in% surveys.keep &
                      Depth <= max.depth)


    ## Remove years before first occurrence
    ## --------------------------------------
    first_occurence <- min(survey.spp$Year)
    survey0 <- subset(survey0, Year >= first_occurence)



    ## Missing / fragmented years
    ## --------------------------------------
    if(remove.fragmented.years){
        conti <- TRUE
        rmYears <- NULL
        while(conti){
            obs <- unique(survey.spp$Year)
            all <- seq(min(survey.spp$Year),
                       max(survey.spp$Year), 1)
            mis <- all[!all %in% obs]
            tmp <- merge(data.frame(Year=all,
                                    all = 0),
                         data.frame(Year=obs,
                                    obs = 1),
                         by = "Year", all.x = TRUE)
            tmp$obs[is.na(tmp$obs)] <- tmp$all[is.na(tmp$obs)]
            tmp2 <- rle(tmp$obs)
            if(nrow(tmp) > 1 &&
               (tmp2$lengths[1] == 1 && tmp2$lengths[2] >= 1)){## ||
               ## (tmp2$lengths[1] == 2 && tmp2$lengths[2] == 1)){  ## this for removing: 2years + next missing
                rmYears <- c(rmYears, min(tmp$Year))
                keepYears <- tmp$Year[min(which(tmp$obs == 1)[-1])]
                survey.spp <- subset(survey.spp, Year >= keepYears)
                survey0 <- subset(survey0, Year >= keepYears)
            }else{
                conti <- FALSE
            }
        }
        if(verbose && !is.null(rmYears)){
            writeLines(paste0("The years: ", paste(rmYears,collapse=","),
                              " are followed by one or more years without observations and are thus removed (remove.fragmented.years = TRUE)."))
        }
    }


    ## Number of years
    ## --------------------------------------
    ny <- length(unique(survey.spp$Year))
    if(verbose){
        if(ny == 1){
            writeLines(paste0("Note that the data only spans ", ny, " year."))
        }else if(ny <= 5){
            writeLines(paste0("Note that the data only spans ", ny, " years."))
        }
    }


    ## Years with minimal number of hauls
    ## --------------------------------------
    conti <- TRUE
    rmYears <- NULL
    while(conti){
        tmp <- cbind(
            with(survey.spp,
                 ## subset(survey.spp, N >= 1),
                 aggregate(list(StatRec = StatRec),
                           by = list(year = Year),
                           function(x) length(unique(x)))),
            Hauls = with(survey.spp,
                         ## subset(survey.spp, N >= 1),
                         aggregate(list(hauls = haul.id),
                                   by = list(year = Year),
                                   function(x) length(unique(x))))[,2],
            N = aggregate(list(N = survey.spp$N),
                          by = list(year = survey.spp$Year), sum)[,2]
        )
        if(ny > 1 && tmp[1,2] <= min.hauls){
            rmYears <- c(rmYears,min(tmp$year))
            keepYears <- min(tmp$year[-1])

            survey.spp <- subset(survey.spp, Year >= keepYears)
            survey0 <- subset(survey0, Year >= keepYears)

        }else{
            conti <- FALSE
        }
        if(verbose && !is.null(rmYears)){
            writeLines(paste0("The years: ", paste(rmYears,collapse=","),
                              " have less than ", min.hauls,
                              " hauls with observations and are thus removed (min.hauls = ",min.hauls,")."))
        }
    }


    ## Re-check gears and rectangles
    ## --------------------------------------
    ## Gears
    ## --------------
    ## Only keep gears with more or equal 5 individual hauls
    tmp <- aggregate(list(haul.id = survey.spp$haul.id[survey.spp$N > 0]),
                     by = list(Gear = survey.spp$Gear[survey.spp$N > 0]),
                     function(x) length(unique(x)))
    gears.keep <- as.character(tmp$Gear[tmp$haul.id >= min.gears])
    ## Surveys
    ## --------------
    ## Only keep surveys with more or equal 5 individual hauls
    tmp <- aggregate(list(haul.id = survey.spp$haul.id[survey.spp$N > 0]),
                     by = list(Survey = survey.spp$Survey[survey.spp$N > 0]),
                     function(x) length(unique(x)))
    surveys.keep <- as.character(tmp$Survey[tmp$haul.id >= min.surveys])
    ## Check Ship-Gear
    ## --------------
    tmp <- aggregate(list(haul.id = survey.spp$haul.id[survey.spp$N > 0]),
                     by = list(ShipG = survey.spp$ShipG[survey.spp$N > 0]),
                     function(x) length(unique(x)))
    shipgear.keep <- as.character(tmp$ShipG[tmp$haul.id >= min.ship.gear])
    ## Check Habitat
    ## --------------
    ## only keep ices squares where species is present
    ices.keep <- sort(unique(survey.spp$StatRec))
    ## NEW: make argument or only use this?
    ## TODO: make check that Area_27 is included!!
    ices.keep <- sort(unique(survey.spp$Area_27))
    ## Apply selections to both data sets
    ## -------------
    survey.spp <- subset(survey.spp,
                         Area_27 %in% ices.keep &
                         Gear %in% gears.keep &
                         ShipG %in% shipgear.keep &
                         Survey %in% surveys.keep &
                         Depth <= max.depth)
    survey0 <- subset(survey0,
                      Area_27 %in% ices.keep &
                      Gear %in% gears.keep &
                      ShipG %in% shipgear.keep &
                      Survey %in% surveys.keep &
                      Depth <= max.depth)

    ## print(cbind(aggregate(list(StatRec = survey.spp$StatRec),
    ##                       by = list(year = survey.spp$Year),
    ##                       function(x) length(unique(x))),
    ## N = aggregate(list(N = survey.spp$N),
    ##           by = list(year = survey.spp$Year), sum)[,2]))


    ## Combine surveys with observations and zeros
    ## --------------------------------------
    if(any(colnames(survey.spp) == "n.juv")) survey0$n.juv <- 0
    if(any(colnames(survey.spp) == "n.adult")) survey0$n.adult <- 0
    if(any(colnames(survey.spp) == "bio.juv")) survey0$bio.juv <- 0
    if(any(colnames(survey.spp) == "bio.adult")) survey0$bio.adult <- 0
    ind <- colnames(survey0)[colnames(survey0) %in% colnames(survey.spp)]
    ind2 <- c("haul.id","N","bio")
    if(any(colnames(survey.spp) == "n.juv")) ind2 <- c(ind2, "n.juv")
    if(any(colnames(survey.spp) == "n.adult")) ind2 <- c(ind2, "n.adult")
    if(any(colnames(survey.spp) == "bio.juv")) ind2 <- c(ind2, "bio.juv")
    if(any(colnames(survey.spp) == "bio.adult")) ind2 <- c(ind2, "bio.adult")
    ind <- !(colnames(survey.spp) %in% ind[!(ind %in% ind2)])
    survey.spp2 <- plyr::join(survey0, survey.spp[,ind], by="haul.id")
    if(is.null(aphiaID)){
        aphiaID <- unique(survey.spp$AphiaID)
    }
    survey.spp2$AphiaID <- paste0(aphiaID, collapse = ",")
    survey.spp2$scientificname <- paste0(unique(survey.spp$scientificname), collapse = ",")
    survey.spp2$genus <- paste0(unique(survey.spp$genus), collapse = ",")
    survey.spp2$family <- paste0(unique(survey.spp$family), collapse = ",")
    survey.spp2$order <- paste0(unique(survey.spp$order), collapse = ",")
    survey.spp2$class <- paste0(unique(survey.spp$class), collapse = ",")
    ## Combine N from survey0 and survey
    ind <- which(colnames(survey.spp2) == "N")
    N <- rowSums(survey.spp2[,ind], na.rm = TRUE)
    survey.spp2[,ind] <- NULL
    survey.spp2$N <- N
    if(any(colnames(survey.spp) == "n.juv")){
        ind <- which(colnames(survey.spp2) == "n.juv")
        N <- rowSums(survey.spp2[,ind], na.rm = TRUE)
        survey.spp2[,ind] <- NULL
        survey.spp2$n.juv <- N
    }
    if(any(colnames(survey.spp) == "n.adult")){
        ind <- which(colnames(survey.spp2) == "n.adult")
        N <- rowSums(survey.spp2[,ind], na.rm = TRUE)
        survey.spp2[,ind] <- NULL
        survey.spp2$n.adult <- N
    }
    ## Combine bio from survey0 and survey
    ind <- which(colnames(survey.spp2) == "bio")
    bio <- rowSums(survey.spp2[,ind], na.rm = TRUE)
    survey.spp2[,ind] <- NULL
    survey.spp2$bio <- bio
    if(any(colnames(survey.spp) == "bio.juv")){
        ind <- which(colnames(survey.spp2) == "bio.juv")
        bio <- rowSums(survey.spp2[,ind], na.rm = TRUE)
        survey.spp2[,ind] <- NULL
        survey.spp2$bio.juv <- bio
    }
    if(any(colnames(survey.spp) == "bio.adult")){
        ind <- which(colnames(survey.spp2) == "bio.adult")
        bio <- rowSums(survey.spp2[,ind], na.rm = TRUE)
        survey.spp2[,ind] <- NULL
        survey.spp2$bio.adult <- bio
    }
    ## overwrite
    survey.spp <- survey.spp2
    rm(survey0, survey.spp2)


    ## Some checks
    ## --------------------------------------
    ## - No NA in lat/lon?
    ind <- which(is.na(survey.spp$lat))
    if(length(ind) > 0){
        if(verbose) writeLines(paste0(length(ind)," hauls have lat == NA. Omitting them."))
        survey.spp <- survey.spp[-ind,]
    }
    ind <- which(is.na(survey.spp$lon))
    if(length(ind) > 0){
        if(verbose) writeLines(paste0(length(ind)," hauls have lon == NA. Omitting them."))
        survey.spp <- survey.spp[-ind,]
    }
    ## - Duplicates in data?
    ind <- which(duplicated(survey.spp$haul.id))
    if(length(ind) > 0){
        if(verbose) writeLines(paste0("Duplicated haul ID in data set. Omitting all duplicates."))
        survey.spp <- survey.spp[-ind,]
    }
    ## - N all integers?
    tmp <- unique(survey.spp$N)
    ind <- which(tmp %% 1 != 0)
    if(length(ind) > 0){
        if(verbose) writeLines(paste0("Following non-integers found in total numbers per haul: ",
                                      paste(tmp[ind],collapse=", "),
                                      ". Rounding to integers!"))
        survey.spp$N <- round(survey.spp$N)
    }
    ## - Print remaining hauls
    nHaulsPos <- length(unique(survey.spp$haul.id[which(survey.spp$N > 0)]))
    if(verbose) writeLines(paste("Number of hauls with N>0: ", nHaulsPos, sep = "\t\t\t\t\t\t\t\t\t"))
    nHauls0 <- length(unique(survey.spp$haul.id[which(survey.spp$N == 0)]))
    if(verbose) writeLines(paste("Number of hauls with N==0: ", nHauls0, sep = "\t\t\t\t\t\t\t\t\t"))
    nHaulsTot <- length(unique(survey.spp$haul.id))
    if(verbose) writeLines(paste("Number of hauls total: ", nHaulsTot, sep = "\t\t\t\t\t\t\t\t\t\t\t"))

    ## Check variables
    if(any(colnames(survey.spp) != "haul.id")) colnames(survey.spp)[colnames(survey.spp) == "HaulID"] <- "haul.id"
    if(!inherits(survey.spp$Year, "factor")) survey.spp$Year <- as.factor(survey.spp$Year)

    ## Drop levels for all factors
    ind <- which(sapply(survey.spp, is.factor))
    if(length(ind) > 0){
        for(i in 1:length(ind)){
            survey.spp[,ind[i]] <- droplevels(survey.spp[,ind[i]])
        }
    }

    ## Return
    ## --------------------------------------
    class(survey.spp) <- c("fdist.species","data.frame")
    return(survey.spp)
}
