#' @name est.dist.one
#' @title Estimated distribution
#' @param specdata Data set of one species
#' @param mods NULL default
#' @param nBoot number of bootrstrapping samples
#' @return List containing hh and hl data sets.
#' @export
est.dist.one <- function(specdata, mods = NULL, n.lon = 20, mc.cores = 1, nBoot = 0){

    setMKLthreads(1)

    saflag <- ifelse(all(is.na(specdata$SweptArea)),0,1)

    if(is.null(mods)) mods <- list.recom.models(specdata,
                                                use.toy = TRUE,
                                                use.swept.area = as.logical(saflag))[1]
    nmods <- length(mods)


    ## Stratified mean
    ## --------------------------------------
    stratMean <- try(
        surveyIndex::getSurveyIdxStratMean(specdata, 1),
        silent = TRUE
    )
    if(inherits(stratMean,"try-error")) print(paste0("Error in stratMean"))


    ## Fit GAMs
    ## --------------------------------------
    ## Get grid (doesn't matter too much for this approach)
    ## TODO: make option to use bathymetry function or provide grid directly
    grid <- surveyIndex::getGrid(specdata, nLon = n.lon) ## TODO: keep the optimise option?

    ## set max basis dim for spatial smooths, P=positive and Z=zero/absence. (as high as possible, but computation speed)
    kvP <- 100

    ## prediction
    ## --------------------
    ## Median time of year
    unique(specdata$Quarter)
    flag <- any(specdata$Quarter=="1")
    print(paste0("Includes Q1: ", flag))
    if(flag){
        toyPred <- median(specdata$timeOfYear[specdata$Quarter=="1"])
    }else{
        toyPred <- median(specdata$timeOfYear[specdata$Quarter=="4"])
    }
    ## Median SweptArea
    sweptAreaPred <- median(specdata$SweptArea)
    ## Median depth
    depthPred <- median(specdata$Depth)

    ## knots (don't use)
    ## ---------------------
    ## knots=list(timeOfYear=seq(0,1,length=6))

    ## Loop through models (saves directory for each)
    ## ---------------------
    resList <- vector("list",nmods)
    for(j in 1:nmods){
        if(nmods > 1){
            writeLines(paste0("Fitting model ",j))
        }else{
            writeLines("Fitting model.")
        }
        ## run gams
        resList[[j]] <- try(
            surveyIndex::getSurveyIdx(
                specdata,
                ages = 1,
                myids = grid[[3]],
                cutOff = 0.01,
                kvecP = kvP,
                modelP = mods[[j]],
                fam = "negbin",
                nBoot = nBoot,
                CIlevel = 0.95,
                mc.cores = mc.cores,
                predfix = list(timeOfYear = toyPred,
                               SweptArea = sweptAreaPred,
                               Depth = depthPred),
                control = list(trace=FALSE,
                               maxit=10)
            ),
            silent = TRUE
        )
    }


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
    return(resList)
}




#' @name est.dist
#' @title Estimate distribution
#' @param data Data set
#' @return List containing hh and hl data sets.
#' @export
est.dist <- function(data, mc.cores = 1){

    specs <- sort(unique(data$survey$AphiaID))
    nspec <- length(specs)

    res <- vector("list", length(nspec))
    for (i in 1:nspec){
        species <- specs[i]
        writeLines(paste0("\nSpecies ",i,": ",species,"\n"))
        specdata <- prep.species(data, species)
        res[[i]] <- est.dist.one(specdata)
    }
    names(res) <- gsub(" ", "_",specs)

    return(res)
}
