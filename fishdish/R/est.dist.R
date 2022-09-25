#' @name est.dist.one
#'
#' @title Estimate distribution of one species
#'
#' @param specdata Data set of one species
#' @param mods NULL default
#' @param nBoot number of bootrstrapping samples
#' @param use.bathy Use bathymetry for distribution. Default: FALSE
#' @param resolution Resolution for bathymetry map. Default: 20
#' @param max.dist Maximum distance to sample locations for bathymetry map. Default: 3
#' @param var.grid Use variable grid by year. Default: TRUE.
#' @param use.obs.for.pred Logical, Only use observed hauls for prediction values? Default: TRUE
#' @param predfix Prediction data. Default: NULL
#' @param verbose Print stuff? Default: TRUE
#'
#' @return List containing model results
#'
#' @export
est.dist.one <- function(specdata, mods = NULL, n.lon = 20,
                         nBoot = 0,
                         grid = NULL,
                         use.bathy = FALSE,
                         resolution = 20,
                         max.dist = 3,
                         var.grid = TRUE,
                         use.obs.for.pred = TRUE,
                         predfix = NULL,
                         model.bio = FALSE,
                         fam = NULL,
                         verbose = TRUE, ...){

    saflag <- ifelse(all(is.na(specdata$SweptArea)),0,1)

    if(is.null(mods)) mods <- list.recom.models(specdata,
                                                use.toy = TRUE,
                                                use.swept.area = as.logical(saflag))[1]
    nmods <- length(mods)

    ## surveyIndex package requirements
    ## if(!inherits(specdata$Year, "factor")) specdata$Year <- as.factor(specdata$Year)
    mods <- lapply(mods, function(x) gsub("Lat", "lat", x))
    mods <- lapply(mods, function(x) gsub("Lon", "lon", x))
    if(any(colnames(specdata) != "haul.id")) colnames(specdata)[colnames(specdata) == "HaulID"] <- "haul.id"
    if(any(colnames(specdata) != "lon")) colnames(specdata)[colnames(specdata) == "Lon"] <- "lon"
    if(any(colnames(specdata) != "lat")) colnames(specdata)[colnames(specdata) == "Lat"] <- "lat"

    ## Required Nage
    ## --------------------------------------
    if(model.bio){
        if(!any(colnames(specdata) == "bio") || all(is.na(specdata$bio))) stop("The bio model requires 'bio' variables in the data set. The variable is either missing or all NA. Please check!")
        Nage <- as.matrix(specdata$bio, na.rm=TRUE)
        colnames(Nage) <- "1"
        specdata$Nage <- Nage
        if(is.null(fam)) fam <- "Tweedie"
    }else{
        Nage <- as.matrix(specdata$N, na.rm=TRUE)
        colnames(Nage) <- "1"
        specdata$Nage <- Nage
        if(is.null(fam)) fam <- "negbin"
    }


    ## Stratified mean
    ## --------------------------------------
    stratMean <- try(
        surveyIndex::getSurveyIdxStratMean(specdata, 1),
        silent = TRUE
    )
    if(inherits(stratMean,"try-error")) print(paste0("Error in stratMean"))


    ## Define grid if not provided
    ## --------------------------------------
    ## years <- sort(unique(as.numeric(levels(droplevels(specdata$Year)))))
    years <- sort(unique(specdata$Year))
    ny <- length(years)
    if(is.null(grid) || is.na(grid)){
        gridflag <- FALSE
        if(!verbose) sink("/dev/null")  ## CHECK: this might be platform dependent? sink("NUL")
        if(use.bathy){
            if(var.grid){
                grid <- vector("list", ny)
                for(y in 1:ny){
                    grid[[y]] <- surveyIndex::getBathyGrid(subset(specdata, Year == years[y]),
                                                           resolution = resolution,
                                                           maxDist = max.dist)
                }
                names(grid) <- years
            }else{
                grid <- surveyIndex::getBathyGrid(specdata,
                                                  resolution = resolution,
                                                  maxDist = max.dist)
            }
            predD <- grid
            myids <- NULL
        }else{
            grid <- surveyIndex::getGrid(specdata, nLon = n.lon)
            predD <- NULL
            myids <- grid[[3]]
        }
        if(!verbose) sink()
    }else{
        gridflag <- TRUE
        if(inherits(grid, "list")){
            if(!all(years %in% as.numeric(names(grid)))) stop("Not all years in grid!")
            grid <- grid[as.numeric(names(grid)) >= min(years)]
        }
        predD <- grid
        myids <- NULL
    }


    ## Prediction data
    ## --------------------
    ## Median time of year
    if(is.null(predfix)){

        if(use.obs.for.pred){
            pred.data <- subset(specdata, N > 0)
        }else{
            pred.data <- specdata
        }

        ## Empty list
        predfix <- list()

        ## Time of year
        flag <- any(sapply(mods, function(x) grepl("timeOfYear", x)))
        if(any(colnames(pred.data) == "timeOfYear") && flag){
            if(any(colnames(pred.data) == "Quarter")){
                if(any(pred.data$Quarter=="1")){
                    toyPred <- median(pred.data$timeOfYear[pred.data$Quarter=="1"])
                }else if(any(pred.data$Quarter=="4")){
                    toyPred <- median(pred.data$timeOfYear[pred.data$Quarter=="4"])
                }else{
                    toyPred <- median(pred.data$timeOfYear)
                }
            }else{
                toyPred <- median(pred.data$timeOfYear)
            }
            predfix$timeOfYear <- toyPred
        }

        ## Swept area
        flag <- any(sapply(mods, function(x) grepl("SweptArea", x)))
        if(any(colnames(pred.data) == "SweptArea") && flag){
            predfix$SweptArea <- median(pred.data$SweptArea)
        }

        ## Haul duration
        flag <- any(sapply(mods, function(x) grepl("HaulDur", x)))
        if(any(colnames(pred.data) == "HaulDur") && flag){
            predfix$HaulDur <- median(pred.data$HaulDur)
        }

        ## Depth
        ## flag <- any(sapply(mods, function(x) grepl("Depth", x)))
        ## if(any(colnames(pred.data) == "Depth") && flag){
        ##     predfix$Depth <- median(pred.data$Depth)
        ## }

        ## ShipG
        flag <- any(sapply(mods, function(x) grepl("ShipG", x)))
        if(any(colnames(pred.data) == "ShipG") && flag){
            predfix$ShipG <- names(which.max(summary(pred.data$ShipG)))
        }

    }else{

        pot.vars <- colnames(specdata)

        ## TODO: include checks that check if all variables in model formula are included in predfix (or fill prediction values for missing variables)
    }

    ## knots (don't use)
    ## ---------------------
    ## knots=list(timeOfYear=seq(0,1,length=6))

    ## Loop through models (saves directory for each)
    ## ---------------------
    resList <- vector("list",nmods)
    for(j in 1:nmods){
        if(verbose){
            if(nmods > 1){
                writeLines(paste0("Fitting model ",j))
            }else{
                writeLines("Fitting model.")
            }
        }


        ## CHECK: find way to report original error messages from getSurveyIndex
        ## run gams
        if(!verbose) sink("/dev/null")  ## CHECK: this might be platform dependent? sink("NUL")
        resList[[j]] <- try(
            surveyIndex::getSurveyIdx(
                specdata,
                ages = 1,
                myids = myids,
                predD = predD,
                cutOff = 0,
                modelP = mods[[j]],
                ## knotsP = list(DoY = c(0.5,366.5), ToD = c(0.5,24.5)),  ## HERE:
                fam = fam,
                nBoot = nBoot,
                CIlevel = 0.95,
                mc.cores = 1, ## only parallel over ages
                predfix = predfix,
                control = list(trace = verbose,
                               maxit = 15),
                ## select = TRUE,
                ## use.bam = TRUE, ## HERE:
                ## method = "fREML", ## HERE:
                ## discrete = TRUE,
                ## nthreads = c(4,1),
                ...
            ),
            silent = TRUE
        )
        if(!verbose) sink()
    }
    names(resList) <- paste0("mod",1:nmods)

    ## TODO: add stratmean to resList!


    ## ## Save info
    ## modInfo <- data.frame(varInfo, modType,
    ##                       min(specdata$Year),
    ##                       max(specdata$Year),
    ##                       length(unique(specdata$haul.id)),
    ##                       length(unique(specdata$haul.id[which(specdata$N > 0)])),
    ##                       length(unique(specdata$haul.id[which(specdata$N == 0)])))
    ## colnames(modInfo) = c("varInfo","modType",
    ##                       "minYear","maxYear",
    ##                       "nHaulsTot","nHaulsPos","nHauls0")
    ## rownames(modInfo) <- species


    ## ## Estimates
    ## ## -------------
    ## res <- data.frame(surveyIdx$idx, surveyIdx$lo, surveyIdx$up,
    ##                   surveyIdx$sd, surveyIdx$sd/surveyIdx$idx,
    ##                   stratMean$stratMean)
    ## colnames(res) = c("gam","gam_lo","gam_up","gam_sd","gam_cv","stratMean")

    ## ## Summary
    ## ## -------------
    ## sumi <- summary(surveyIdx$pModels[[1]])

    ## nHauls
    ## nHaulsTot
    ## nZeroObs
    ## nNonZeroObs
    ## Deviance explained
    ## round(sumi$dev.expl * 100,2)
    ## sumi$s.table

    ## if(!inherits(specdata$Year, "factor")) specdata$Year <- as.factor(specdata$Year)

    ## Return
    ## -----------------
    ret <- list()
    ret$id <- paste(unique(specdata$AphiaID), collapse = ", ")
    ret$data <- specdata
    ret$grid <- grid
    ret$fits <- resList
    ret$pred.by.haul <- !use.bathy && !gridflag
    class(ret) <- c("fdist.fit","list")
    return(ret)
}




#' @name est.dist
#'
#' @title Estimate distribution
#'
#' @param data Data set
#' @param mods NULL default
#' @param nBoot number of bootrstrapping samples
#' @param use.bathy Use bathymetry for distribution. Default: FALSE
#' @param resolution Resolution for bathymetry map
#' @param verbose Print stuff? Default: TRUE
#'
#' @importFrom parallel mclapply
#'
#' @return List containing model results
#'
#' @export
est.dist <- function(data, mods = NULL, n.lon = 20, nBoot = 0,
                         use.bathy = FALSE,
                         resolution = 20, mc.cores = 1, verbose = TRUE){

    specs <- sort(unique(data$survey$AphiaID))
    nspec <- length(specs)


    if(mc.cores > 1){
        ## try(setMKLthreads(1), silent = TRUE)
        res <- parallel::mclapply(1:nspec,
                                  function(x){
                                      est.dist.one(
                                          prep.species(data, specs[x], verbose = FALSE),
                                          mods = mods,
                                          n.lon = n.lon,
                                          nBoot = nBoot,
                                          use.bathy = use.bathy,
                                          resolution = resolution,
                                          verbose = FALSE)
                                  })
    }else{
        res <- vector("list", length(nspec))
        for (i in 1:nspec){
            species <- specs[i]
            if(verbose) writeLines(paste0("\nSpecies ",i,": ",species,"\n"))
            specdata <- prep.species(data, species)
            if(verbose) cat("\n")
            res[[i]] <- est.dist.one(specdata, mods = mods,
                                     n.lon = n.lon, nBoot = nBoot,
                                     use.bathy = use.bathy,
                                     resolution = resolution, verbose = verbose)
        }
    }
    names(res) <- gsub(" ", "_",specs)


    ## Return
    ## -----------------
    class(res) <- c("fdist.multifit","list")
    return(res)
}






#' @name pred.dist.one
#'
#' @title Predict distribution for given grid or variables
#'
#' @param specdata Data set of one species
#' @param mods NULL default
#' @param nBoot number of bootrstrapping samples
#' @param use.bathy Use bathymetry for distribution. Default: FALSE
#' @param resolution Resolution for bathymetry map. Default: 20
#' @param max.dist Maximum distance to sample locations for bathymetry map. Default: 3
#' @param var.grid Use variable grid by year. Default: TRUE.
#' @param use.obs.for.pred Logical, Only use observed hauls for prediction values? Default: TRUE
#' @param predfix Prediction data. Default: NULL
#' @param verbose Print stuff? Default: TRUE
#'
#' @return List containing model results
#'
#' @export
pred.dist.one <- function(fit,
                          nBoot = 0,
                          grid = NULL,
                          use.obs.for.pred = TRUE,
                          predfix = NULL,
                          verbose = TRUE,
                          save.grid = FALSE,
                          save.data = FALSE,
                          ...){

    if(!inherits(fit, "fdist.fit")) stop("fit has to be an object fitted with 'est.dist.one'")


    specdata <- fit$data
    ## if(!inherits(specdata$Year, "factor")) specdata$Year <- as.factor(specdata$Year)
    nmods <- length(fit$fits)

    ## Define grid if not provided
    ## --------------------------------------
    ## years <- sort(unique(as.numeric(levels(droplevels(specdata$Year)))))
    years <- sort(unique(specdata$Year))
    ny <- length(years)
    if(is.null(grid) || is.na(grid)){
        grid <- fit$grid
        if(fit$pred.by.haul){
            predD <- NULL
            myids <- grid[[3]]
        }else{
            predD <- grid
            myids <- NULL
        }
    }else{
        if(inherits(grid, "list")){
            if(!all(years %in% as.numeric(names(grid)))) stop("Not all years in grid!")
            grid <- grid[as.numeric(names(grid)) >= min(years)]
        }
        predD <- grid
        myids <- NULL
    }

    ## Prediction data
    ## --------------------
    ## Median time of year
    if(is.null(predfix)){

        if(use.obs.for.pred){
            pred.data <- subset(specdata, N > 0)
        }else{
            pred.data <- specdata
        }

        ## Empty list
        predfix <- list()

        ## Time of year
        flag <- any(sapply(mods, function(x) grepl("timeOfYear", x)))
        if(any(colnames(pred.data) == "timeOfYear") && flag){
            if(any(colnames(pred.data) == "Quarter")){
                if(any(pred.data$Quarter=="1")){
                    toyPred <- median(pred.data$timeOfYear[pred.data$Quarter=="1"])
                }else if(any(pred.data$Quarter=="4")){
                    toyPred <- median(pred.data$timeOfYear[pred.data$Quarter=="4"])
                }else{
                    toyPred <- median(pred.data$timeOfYear)
                }
            }else{
                toyPred <- median(pred.data$timeOfYear)
            }
            predfix$timeOfYear <- toyPred
        }

        ## Swept area
        flag <- any(sapply(mods, function(x) grepl("SweptArea", x)))
        if(any(colnames(pred.data) == "SweptArea") && flag){
            predfix$SweptArea <- median(pred.data$SweptArea)
        }

        ## Haul duration
        flag <- any(sapply(mods, function(x) grepl("HaulDur", x)))
        if(any(colnames(pred.data) == "HaulDur") && flag){
            predfix$HaulDur <- median(pred.data$HaulDur)
        }

        ## Depth
        ## flag <- any(sapply(mods, function(x) grepl("Depth", x)))
        ## if(any(colnames(pred.data) == "Depth") && flag){
        ##     predfix$Depth <- median(pred.data$Depth)
        ## }

        ## ShipG
        flag <- any(sapply(mods, function(x) grepl("ShipG", x)))
        if(any(colnames(pred.data) == "ShipG") && flag){
            predfix$ShipG <- names(which.max(summary(pred.data$ShipG)))
        }

    }


    ## Loop through models (saves directory for each)
    ## ---------------------
    resList <- vector("list",nmods)
    for(j in 1:nmods){
        if(verbose){
            if(nmods > 1){
                writeLines(paste0("Predicting for model ",j))
            }else{
                writeLines("Predicting.")
            }
        }

        ## run gams
        if(!verbose) sink("/dev/null")  ## CHECK: this might be platform dependent? sink("NUL")
        resList[[j]] <- try(
            surveyIndex::redoSurveyIndex(x = specdata,
                                         model = fit$fits[[j]],
                                         predD = predD,
                                         myids = myids,
                                         nBoot = nBoot,
                                         predfix = predfix,
                                         mc.cores = 1,
                                         verbose = verbose),
            silent = TRUE
        )
        if(!verbose) sink()
    }
    names(resList) <- paste0("mod",1:nmods)


    ## Return
    ## -----------------
    ret <- list()
    ret$id <- paste(unique(specdata$AphiaID), collapse = ", ")
    if(save.data){
        ret$data <- specdata
    }
    if(save.grid){
        ret$grid <- grid
    }
    ret$predfix <- predfix
    ret$fits <- resList
    ret$pred.by.haul <- fit$pred.by.haul
    class(ret) <- c("fdist.fit","list")
    return(ret)
}
