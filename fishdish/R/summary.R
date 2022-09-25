#' @name summary.fdist.datras
#'
#' @title Summary of data
#'
#' @param data Data set
#'
#' @return NULL
#'
#' @export
summary.fdist.datras <- function(data){

    ## Check validity of data
    ## ------------------
    if(!inherits(data, "fdist.datras")){
        stop("Function requires a list of class fdist.datras with two DATRAS data sets 'HH' and 'HL' as elements (see function 'download.data').")
    }

    dat1 <- data$HH
    dat2 <- data$HL

    ## Surveys
    tmp <- unique(dat1$Survey)
    ntmp <- length(tmp)
    writeLines(paste("Surveys: ", ntmp, sep="\t\t\t\t"))

    ## Years
    tmp <- unique(as.numeric(as.character(dat1$Year)))
    ntmp <- length(tmp)
    tmp1 <- range(tmp, na.rm = TRUE)
    writeLines(paste(paste("Years: ", ntmp, sep="\t\t\t\t\t"),
               paste0("(",paste0(tmp1,collapse = " - "),")"), sep="\t"))

    ## Species
    tmp <- unique(dat2$AphiaID)
    ntmp <- length(tmp)
    writeLines(paste("Species: ", ntmp, sep="\t\t\t\t"))

    ## Hauls
    ntmp <- nrow(dat1)
    writeLines(paste("Hauls: ", ntmp, sep="\t\t\t\t\t"))

    ## Depth range
    tmp <- range(dat1$Depth, na.rm = TRUE)
    writeLines(paste("Depth range: ", paste0(paste0(round(tmp,1), collapse = " - "),"m"), sep="\t\t"))

}


#' @name summary.fdist.prepped
#'
#' @title Summary of data
#'
#' @param data Data set
#'
#' @return NULL
#'
#' @export
summary.fdist.prepped <- function(data){

    ## Check validity of data
    ## ------------------
    if(!inherits(data, "fdist.prepped")){
        stop("Function requires a list of class fdist.prepped with two data sets 'survey0' and 'survey' as elements (see function 'prep.data').")
    }

    dat1 <- data$survey0
    dat2 <- data$survey


    ## General
    ## ----------------
    writeLines("General information\n")

    ## Surveys
    tmp <- unique(dat1$Survey)
    ntmp <- length(tmp)
    writeLines(paste("Surveys: ", ntmp, sep="\t\t\t\t"))

    ## Years
    tmp <- unique(as.numeric(as.character(dat1$Year)))
    ntmp <- length(tmp)
    tmp1 <- range(tmp, na.rm = TRUE)
    writeLines(paste(paste("Years: ", ntmp, sep="\t\t\t\t\t"),
               paste0("(",paste0(tmp1,collapse = " - "),")"), sep="\t"))

    ## Species
    tmp <- unique(dat2$AphiaID)
    ntmp <- length(tmp)
    writeLines(paste("Species: ", ntmp, sep="\t\t\t\t"))

    ## Hauls
    tmp <- unique(dat1$haul.id)
    ntmp <- length(tmp)
    writeLines(paste("Hauls: ", ntmp, sep="\t\t\t\t\t"))

    ## Depth range
    tmp <- range(dat1$Depth, na.rm = TRUE)
    writeLines(paste("Depth range: ", paste0(paste0(round(tmp,1), collapse = " - "),"m"), sep="\t\t"))



    ## More detailed
    ## ----------------
    cat("\n\n")
    writeLines("Detailed information\n")

    dat_sum <- dat1[,c("Survey","Quarter","Year","N")]
    dat_sum <- dat_sum[!duplicated(dat_sum),]
    dat_sum <- aggregate(list(Years=as.numeric(as.character(dat_sum$Year))),
                         by = list(Survey = dat_sum$Survey, Quarter = dat_sum$Quarter),
                         range)
    dat_sum <- dat_sum[order(dat_sum$Survey),]
    dat_sum[,3] <- apply(dat_sum[,3],1,paste,collapse="-")

    dat_sum2 <- aggregate(list(StatRec=dat1$StatRec),
                          by = list(Survey = dat1$Survey, Quarter = dat1$Quarter),
                          function(x) length(unique(x)))

    tmp <- cbind(dat_sum,StatRec=dat_sum2[,3])
    rownames(tmp) <- 1:nrow(tmp)

}


#' @name summary.fdist.species
#'
#' @title Summary of data
#'
#' @param data Data set
#'
#' @return NULL
#'
#' @export
summary.fdist.species <- function(data){

    ## Check validity of data
    ## ------------------
    if(!inherits(data, "fdist.species")){
        stop("Function requires a data set as returned by the function 'prep.species'.")
    }

    dat1 <- data

    ## General
    ## ----------------
    writeLines("General information\n")

    ## Surveys
    tmp <- unique(data$Survey)
    ntmp <- length(tmp)
    writeLines(paste("Surveys: ", ntmp, sep="\t\t\t\t\t"))

    ## Years
    tmp <- unique(as.numeric(as.character(dat1$Year)))
    ntmp <- length(tmp)
    tmp1 <- range(tmp, na.rm = TRUE)
    writeLines(paste(paste("Years: ", ntmp, sep="\t\t\t\t\t\t"),
               paste0("(",paste0(tmp1,collapse = " - "),")"), sep="\t"))

    ## Species
    tmp <- unique(dat1$AphiaID)
    ntmp <- length(tmp)
    if(ntmp <= 3){
    writeLines(paste(paste("Species: ", ntmp, sep="\t\t\t\t\t"),
               paste0("(IDs: ",paste0(tmp,collapse = ", "),")"), sep="\t"))
    }else{
        writeLines(paste("Species: ", ntmp, sep="\t\t\t\t\t"))
    }

    ## Depth range
    tmp <- range(dat1$Depth, na.rm = TRUE)
    writeLines(paste("Depth range: ",
                     paste0(paste0(round(tmp,1), collapse = " - "),"m"), sep="\t\t\t"))

    ## HaulDur range
    tmp <- range(dat1$HaulDur, na.rm = TRUE)
    writeLines(paste("Haul duration: ",
                     paste0(paste0(round(tmp/60,2), collapse = " - "),"h"), sep="\t\t"))

    ## Gear
    tmp <- unique(dat1$Gear)
    ntmp <- length(tmp)
    writeLines(paste("Gears: ", ntmp, sep="\t\t\t\t\t\t"))

    ## Gear categories
    tmp <- unique(dat1$GearCat)
    ntmp <- length(tmp)
    writeLines(paste("Gear cat.: ", ntmp, sep="\t\t\t\t"))

    ## Ship
    tmp <- unique(dat1$Ship)
    ntmp <- length(tmp)
    writeLines(paste("Ships: ", ntmp, sep="\t\t\t\t\t\t"))

    ## Gear categories
    tmp <- unique(dat1$ShipG)
    ntmp <- length(tmp)
    writeLines(paste("Ship-Gear int.: ", ntmp, sep="\t"))

    ## Hauls
    tmp <- unique(dat1$haul.id)
    ntmp <- length(tmp)
    writeLines(paste("Hauls: ", ntmp, sep="\t\t\t\t\t\t"))

    ## N
    tmp <- sum(dat1$N, na.rm = TRUE)
    writeLines(paste("Individuals: ", tmp, sep="\t\t\t"))


    ## More detailed
    ## ----------------
    cat("\n\n")
    writeLines("Detailed information\n\n")

    writeLines("By survey\n")
    ## --------------
    dat_sum <- dat1[,c("Survey","Quarter","Year","N")]
    dat_sum <- dat_sum[!duplicated(dat_sum),]
    dat_sum <- aggregate(list(Years=as.numeric(as.character(dat_sum$Year))),
                         by = list(Survey = dat_sum$Survey, Quarter = dat_sum$Quarter),
                         range)
    dat_sum <- dat_sum[order(dat_sum$Survey),]
    dat_sum[,3] <- apply(dat_sum[,3],1,paste,collapse="-")
    dat_sum2 <- aggregate(list(dat1$StatRec),
                          by = list(Survey = dat1$Survey, Quarter = dat1$Quarter),
                          function(x) length(unique(x)))
    dat_sum3 <- aggregate(list(dat1$haul.id),
                          by = list(Survey = dat1$Survey, Quarter = dat1$Quarter),
                          function(x) length(unique(x)))
    dat_sum4 <- aggregate(list(dat1$N),
                          by = list(Survey = dat1$Survey, Quarter = dat1$Quarter),
                          function(x) sum(x, na.rm = TRUE))
    dat_sum5 <- aggregate(list(dat1$Area_27),
                          by = list(Survey = dat1$Survey, Quarter = dat1$Quarter),
                          function(x) paste0(unique(sapply(strsplit(x,".", fixed = TRUE),"[[",1)),
                                             collapse = ","))
    tmp <- cbind(dat_sum,
                 `Stat. Rect.` = dat_sum2[,3],
                 Hauls = dat_sum3[,3],
                 Individuals = dat_sum4[,3],
                 `ICES Area` = dat_sum5[,3]
                 )
    rownames(tmp) <- NULL
    print(tmp, row.names = FALSE)


    cat("\n\n")
    writeLines("By year\n")
    ## --------------------
    dat_sum <- aggregate(list(Surveys = dat1$Survey, StatRec = dat1$StatRec, Hauls = dat1$haul.id),
                         by = list(Year = as.numeric(as.character(dat1$Year))),
                         function(x) length(unique(x)))
    dat_sum <- dat_sum[order(as.numeric(as.character(dat_sum$Year))),]
    dat_sum2 <- aggregate(list(dat1$N),
                          by = list(Year = as.numeric(as.character(dat1$Year))),
                          function(x) sum(x, na.rm = TRUE))
    dat_sum3 <- aggregate(list(dat1$Area_27),
                          by = list(Year = as.numeric(as.character(dat1$Year))),
                          function(x) paste0(unique(sapply(strsplit(x,".", fixed = TRUE),"[[",1)),
                                             collapse = ","))
    tmp <- cbind(dat_sum, dat_sum2[,2], dat_sum3[,2])
    colnames(tmp) <- c("Year", "Surveys", "Stat. Rec.", "Hauls", "Individuals", "ICES Area")
    rownames(tmp) <- NULL
    print(tmp, row.names = FALSE)

}




#' @name summary.fdist.fit
#'
#' @title Summary of Fit
#'
#' @param Fit A fit of est.dist or est.dist.one function
#'
#' @return NULL
#'
#' @export
summary.fdist.fit <- function(fit){

    ## Check validity of data
    ## ------------------
    if(!inherits(fit, "fdist.fit")){
        stop("Function requires the object as returned by the function 'est.dist.one'.")
    }

    fit <- fit$fit

    ## List with different models for single species
    nmods <- length(fit)

    ## General
    ## ----------------
    ## TODO: Print species name (or AphiaID) but before: add info to est.dist.one output
    ## TODO: number of years
    ## TODO: number of surveys, etc.


    ## TODO: account for model not-convergence (if try-error?)


    ## Model formulas
    ## ----------------
    for(i in 1:nmods){
        if(i > 1) cat("\n\n")
        writeLines(paste0("Model ", i))
        writeLines("---------------------------")
        print(fit[[i]]$pModels[[1]])
    }

    ## Abundance Index
    ## ----------------
    abund <- as.numeric(rownames(fit[[1]]$idx))
    uncert.flag <- all(fit[[i]]$lo == 0) && all(fit[[i]]$up == 0)
    for(i in 1:nmods){
        if(uncert.flag){
            abund <- cbind(abund, round(fit[[i]]$idx,2))
        }else{
            abund <- cbind(abund, round(fit[[i]]$idx,2), round(fit[[i]]$lo,2), round(fit[[i]]$up,2))
        }
    }
    if(uncert.flag){
        colnames(abund) <- c("Year",paste0("Index_mod",1:nmods))
    }else{
        colnames(abund) <- c("Year",paste0(c("Index_mod","lower_CI_mod","upper_CI_mod"),
                                           rep(1:nmods, each = 3)))
    }
    rownames(abund) <- NULL

    cat("\n\n")
    writeLines("Abundance Index")
    writeLines("---------------------------\n")
    print(abund, row.names = FALSE)


    ## Residuals


    ## Distribution
    ## ----------------
    ## TODO: possible to print anything?
}



#' @name summary.fdist.multifit
#'
#' @title Summary of Fit
#'
#' @param Fit A fit of est.dist or est.dist.one function
#'
#' @return NULL
#'
#' @export
summary.fdist.multifit <- function(fit){

    ## Check validity of data
    ## ------------------
    if(!inherits(fit, "fdist.multifit")){
        stop("Function requires the object as returned by the function 'est.dist'.")
    }


}
