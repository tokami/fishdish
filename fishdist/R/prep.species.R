#' @name prep.species
#'
#' @title Prepare species
#'
#' @param data Data set
#' @param aphiaID required (single species or vector of species)
#' @param use.gear.cat = TRUE
#' @param remove.fragmented.years = TRUE
#' @param min.gears = 1
#' @param min.surveys = 1
#' @param min.hauls = 1
#' @param min.ship.gear = 1
#' @param verbose Print stuff Default: TRUE
#'
#' @return List containing hh and hl data sets.
#'
#' @export
prep.species <- function(data, aphiaID,
                         use.gear.cat = TRUE,
                         remove.fragmented.years = TRUE,
                         min.gears = 1, min.surveys = 1, min.hauls = 1, min.ship.gear = 1,
                         verbose = TRUE){


    ## Check validity of data
    ## ------------------
    if(class(data) != "list" || length(data) != 2 || names(data) != c("survey0","survey"))
        stop("Function requires data list with the data sets 'survey0' and 'survey' created by the function 'prep.data'.")
    survey0 <- data$survey0
    survey <- data$survey


    ## Subset all species for given species category
    ## --------------------------------------
    if(!any(aphiaID %in% unique(survey$AphiaID))) stop("Provided Aphia IDs are not included in the data set! Please check if the Aphia IDs were included in the data preparation step (prep.data).")
    survey.spp <- subset(survey, AphiaID %in% aphiaID)


    ## Gears and surveys
    ## --------------------------------------
    ## Use Gear category rather than many individual gears
    if(use.gear.cat){
        survey.spp$Gear <- as.factor(survey.spp$GearCat)
        survey0$Gear <- as.factor(survey0$GearCat)
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
    ## Check Ship-Gear
    ## --------------
    tmp <- aggregate(list(haul.id = survey.spp$haul.id[survey.spp$N > 0]),
                     by = list(ShipG = survey.spp$ShipG[survey.spp$N > 0]),
                     function(x) length(unique(x)))
    shipgear.keep <- as.character(tmp$ShipG[tmp$haul.id >= min.ship.gear])

    ## Realized habitat of species category
    ## --------------------------------------
    ## only keep ices squares where species category is present
    ices.keep <- sort(unique(survey.spp$StatRec))  ## CHECK: that no StatRec with N=0 are in survey.spp


    ## Apply selections to both data sets
    ## --------------------------------------
    survey.spp <- subset(survey.spp, StatRec %in% ices.keep &
                                     Gear %in% gears.keep &
                                     ShipG %in% shipgear.keep &
                                     Survey %in% surveys.keep)
    survey0 <- subset(survey0, StatRec %in% ices.keep &
                               Gear %in% gears.keep &
                               ShipG %in% shipgear.keep &
                               Survey %in% surveys.keep)


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
            if(nrow(tmp) > 1 && tmp2$lengths[1] == 1 && tmp2$lengths[2] >= 1){
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
                              " are followed by one more years without observations and are thus removed (remove.fragmented.years = TRUE)."))
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

            browser()

            survey.spp <- survey.spp %>%
                filter(Year >= keepYears)
            survey0 <- survey0 %>%
                filter(Year >= keepYears)

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
    ## Apply selections to both data sets
    ## -------------
    survey.spp <- subset(survey.spp, StatRec %in% ices.keep &
                                     Gear %in% gears.keep &
                                     ShipG %in% shipgear.keep &
                                     Survey %in% surveys.keep)
    survey0 <- subset(survey0, StatRec %in% ices.keep &
                               Gear %in% gears.keep &
                               ShipG %in% shipgear.keep &
                               Survey %in% surveys.keep)

    ## print(cbind(aggregate(list(StatRec = survey.spp$StatRec),
    ##                       by = list(year = survey.spp$Year),
    ##                       function(x) length(unique(x))),
    ## N = aggregate(list(N = survey.spp$N),
    ##           by = list(year = survey.spp$Year), sum)[,2]))


    ## Combine surveys with observations and zeros
    ## --------------------------------------
    ind <- colnames(survey0)[colnames(survey0) %in% colnames(survey.spp)]
    ind <- !(colnames(survey.spp) %in% ind[!(ind %in% c("haul.id","N"))])
    survey.spp2 <- plyr::join(survey0, survey.spp[,ind], by="haul.id")
    survey.spp2$AphiaID <- paste0(aphiaID, collapse = ",")
    ## Combine Ns from survey0 and survey
    ind <- which(colnames(survey.spp2) == "N")
    N <- rowSums(survey.spp2[,ind], na.rm = TRUE)
    survey.spp2[,ind] <- NULL
    survey.spp2$N <- N
    survey.spp <- survey.spp2
    rm(survey0, survey.spp2)


    ## Some checks
    ## --------------------------------------
    ## - No NA in Lat/Lon?
    ind <- which(is.na(survey.spp$Lat))
    if(length(ind) > 0){
        if(verbose) writeLines(paste0(length(ind)," hauls have Lat == NA. Omitting them."))
        survey.spp <- survey.spp[-ind,]
    }
    ind <- which(is.na(survey.spp$Lon))
    if(length(ind) > 0){
        if(verbose) writeLines(paste0(length(ind)," hauls have Lon == NA. Omitting them."))
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


    ## Required Nage
    ## --------------------------------------
    Nage <- as.matrix(survey.spp$N, na.rm=TRUE)
    colnames(Nage) <- "1"
    survey.spp$Nage <- Nage
    ages <- 1
    ## withweight (download CA)  d<-addWeightByHaul(d,to1min=FALSE) but not clear if enough info
    ## maybe future project


    ## Return
    ## --------------------------------------
    return(survey.spp)
}
