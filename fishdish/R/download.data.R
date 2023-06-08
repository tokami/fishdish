#' @name download.data
#'
#' @title Download all surveys
#'
#' @param first.year First year (default: 1967)
#' @param last.year Last year (default: 2020)
#' @param surveys (default: "all")
#' @param quarters (default: "all")
#' @param aphiaID Downloads all species anyways, but early subsetting for
#'     species optional and can help with memory issues.
#' @param datasets (default: "HH","HL")
#' @param calc.swept.area Calculate swept area?
#' @param datras.variables DATRAS variables to use (Only used if reduce.file.size = TRUE).
#' @param reduce.file.size Reduce file size
#' @param verbose Print information? Default: TRUE
#' @param save.files Save individual files for each year? Default: TRUE
#'
#' @return List containing hh and hl data sets.
#'
#' @importFrom icesDatras getDATRAS
#'
#' @export
download.data <- function(first.year = 1967,
                          last.year = 2020,
                          surveys = "all",
                          quarters = "all",
                          aphiaID = "all",
                          datasets = c("HH","HL"),
                          calc.swept.area = TRUE,
                          datras.variables = list.datras.variables.req(swept.area.calculated = FALSE),
                          reduce.file.size = TRUE,
                          verbose = TRUE,
                          save.files = TRUE,
                          file.dir = "files"){

    ## Check surveys
    all.surveys <- list.surveys()
    if(surveys == "all" || surveys == "All" || surveys == "ALL") surveys <- all.surveys
    surveys.sel <- surveys[which(surveys %in% all.surveys)]
    if(any(!surveys %in% all.surveys))
        if(verbose) writeLines(paste0("Following surveys could not be matched (use list.surveys): ",
                          surveys[which(!surveys %in% all.surveys)]))
    ns <- length(surveys.sel)

    ## Check quarters
    all.quarters <- 1:4
    survs <- get.info.surveys(plot = FALSE)
    quarters.sel <- vector("list", ns)
    if(quarters[1] == "all" || quarters[1] == "All" || quarters[1] == "ALL"){
        for(i in 1:ns){
            quarters.sel[[i]] <- as.numeric(strsplit(survs$quarters[survs$survey %in% surveys.sel[i]],",")[[1]])
        }
    }else if(inherits(quarters,"list")){
        if(length(quarters) != ns) stop("Length of list with quarters does not match number of surveys.")
        if(!all(sapply(quarters, is.numeric))) stop("All elements of quarters have to be numeric.")
        quarters.pre <- quarters
        for(i in 1:ns){
            tmp <- as.numeric(strsplit(survs$quarters[survs$survey %in% surveys.sel[i]],",")[[1]])
            quarters.sel[[i]] <-  tmp[which(tmp %in% quarters.pre[[i]])]
            if(length(quarters.sel[[i]]) < 1) stop(paste0("Selected quarter(s) (",quarters.pre[[i]],") is/are not present in Survey ",surveys.sel[i],". Please select different quarters, survey-specific quarters, or omit survey. See get.info.surveys() for more information."))
        }
        quarters.sel <- quarters
    }else{
        quarters.pre <- quarters[which(as.numeric(quarters) %in% all.quarters)]
        if(any(!quarters.pre %in% all.quarters))
            if(verbose) writeLines(paste0("Following quarters could not be matched (1:4): ",
                              quarters.pre[which(!quarters.pre %in% all.quarters)]))
        for(i in 1:ns){
            tmp <- as.numeric(strsplit(survs$quarters[survs$survey %in% surveys.sel[i]],",")[[1]])
            quarters.sel[[i]] <-  tmp[which(tmp %in% quarters.pre)]
            if(length(quarters.sel[[i]]) < 1) stop(paste0("Selected quarter(s) (",quarters.pre,") is/are not present in Survey ",surveys.sel[i],". Please select different quarters, survey-specific quarters, or omit survey. See get.info.surveys() for more information."))
        }
    }

    dir.create(file.dir, showWarnings = FALSE)

    ## Haul info from Datras
    hl <- hh <- ca <- file.names <- NULL
    for(dt in 1:length(datasets)){
        dat.type <- datasets[dt]
        dat <- vector("list",ns)
        for(i in 1:ns){
            surv <- surveys.sel[i]
            if(verbose) writeLines(paste0("Downloading data set '",dat.type,"' of: ", surv))
            dat[[i]] <- try(icesDatras::getDATRAS(record = dat.type,
                                              survey = surv,
                                              years = first.year:last.year,
                                              quarters = quarters.sel[[i]]), silent = TRUE)
            ## suppressMessages

            if(inherits(dat[[i]], "data.frame")){
                ## CHECK: Remove this option as it would get rid of the zero hauls?
                ## Better: e.g. only keep surveys were species was present at least once.
                ## Subset Species if provided
                if(dat.type == "HL" && !(aphiaID[1] %in% c("all","All","ALL")) &&
                   !is.null(aphiaID[1]) && !is.na(aphiaID[1])){
                    ## if(verbose) writeLines(paste0("Subsetting downloaded data for following Aphia ID(s): ",
                    ##                   paste0(aphiaID, collapse = ", ")))
                    dat[[i]] <- subset(dat[[i]], Valid_Aphia %in% aphiaID)
                }
                ## Rename some variables
                if(dat.type == "HH"){
                    colnames(dat[[i]])[which(colnames(dat[[i]]) == "ShootLong")] <- "lon"
                    colnames(dat[[i]])[which(colnames(dat[[i]]) == "ShootLat")] <- "lat"
                }else if(dat.type %in% c("HL","CA")){
                    colnames(dat[[i]])[which(colnames(dat[[i]]) == "Valid_Aphia")] <- "AphiaID"
                    lngt2cm <- c("." = 0.1, "0" = 0.1, "1" = 1, "2" = 1, "5" = 1)[as.character(dat[[i]]$LngtCode)]
                    dat[[i]]$LngtCm <- lngt2cm * dat[[i]]$LngtClass
                }
                ## Subset required variables
                if(reduce.file.size){
                    ind <- datras.variables[[dat.type]]
                    ## if(verbose) writeLines(paste0("Subsetting downloaded data for following variables: ",
                    ##                   paste0(ind, collapse = ", ")))
                    dat[[i]] <- dat[[i]][ind]
                }

                if(save.files){
                    file.lab <- paste0(dat.type,"_",surv,"_",first.year,"-", last.year,"_",
                                       paste(quarters.sel[[i]], collapse = "-", sep = ""),
                                       "_", format(Sys.time(),"%Y%m%d%H%M"))
                    file.names <- c(file.names, file.lab)
                    write.table(dat[[i]], file = file.path(file.dir, paste0(file.lab,".csv")))
                    dat[[i]] <- NULL
                }
            }
        }

        if(!save.files){
            ## Combine data from surveys
            if(dat.type == "HH"){
                hh <- try(do.call(rbind, dat), silent = TRUE)
                if(inherits(hh, "try-error")){
                    stop(paste0("Cannot merge data sets. Please contact the package maintainer. This might be due to a misfit in DATRAS variables: ",lapply(dat, colnames)))
                }
                if(calc.swept.area){
                    if(verbose) writeLines("Calculating the swept area for downloaded Data.")
                    hh <- calc.swept.area(list(HH=hh))$HH
                }
            }else if(dat.type == "HL"){
                hl <- try(do.call(rbind, dat), silent = TRUE)
                if(inherits(hl, "try-error")){
                    stop(paste0("Cannot merge data sets. Please contact the package maintainer. This might be due to a misfit in DATRAS variables: ",lapply(dat, colnames)))
                }
            }else if(dat.type == "CA"){
                ca <- try(do.call(rbind, dat), silent = TRUE)
                if(inherits(ca, "try-error")){
                    stop(paste0("Cannot merge data sets. Please contact the package maintainer. This might be due to a misfit in DATRAS variables: ",lapply(dat, colnames)))
                }
            }
        }
    }

    ## Return
    ## -----------
    if(!save.files){
        res <- list(HH = hh, HL = hl, CA = ca)
        class(res) <- c("fdist.datras","list")
        return(res)
    }else{
        writeLines(paste0("Files were saved to ", file.dir, ". Run load.data next."))
        return(file.names)
    }
}


#' @name load.data
#'
#' @title Download all surveys
#'
#' @param files Optional list with names of files to load. If NULL all files in
#'     folder 'file.dir' will be loaded
#' @param file.dir Directory with saved files, default: 'files'
#' @param first.year First year (default: 1967)
#' @param last.year Last year (default: 2020)
#' @param surveys (default: "all")
#' @param quarters (default: "all")
#' @param aphiaID Downloads all species anyways, but early subsetting for
#'     species optional and can help with memory issues.
#' @param datasets (default: "HH","HL")
#' @param calc.swept.area Calculate swept area?
#' @param datras.variables DATRAS variables to use (Only used if
#'     reduce.file.size = TRUE).
#' @param reduce.file.size Reduce file size
#' @param verbose Print information? Default: TRUE
#'
#' @return List containing hh and hl data sets.
#'
#' @importFrom icesDatras getDATRAS
#'
#' @export
load.data <- function(file.dir = "files",
                      files = NULL,
                      first.year = 1967,
                      last.year = 2020,
                      surveys = "all",
                      quarters = "all",
                      aphiaID = "all",
                      datasets = c("HH","HL"),
                      calc.swept.area = TRUE,
                      datras.variables = list.datras.variables.req(swept.area.calculated = !calc.swept.area),
                      reduce.file.size = TRUE,
                      verbose = TRUE
                      ){## Check surveys
    all.surveys <- list.surveys()
    if(surveys == "all" || surveys == "All" || surveys == "ALL") surveys <- all.surveys
    surveys.sel <- surveys[which(surveys %in% all.surveys)]
    if(any(!surveys %in% all.surveys))
        if(verbose) writeLines(paste0("Following surveys could not be matched (use list.surveys): ",
                                      surveys[which(!surveys %in% all.surveys)]))
    ns <- length(surveys.sel)

    ## Check quarters
    all.quarters <- 1:4
    survs <- get.info.surveys(plot = FALSE)
    quarters.sel <- vector("list", ns)
    if(quarters[1] == "all" || quarters[1] == "All" || quarters[1] == "ALL"){
        for(i in 1:ns){
            quarters.sel[[i]] <- as.numeric(strsplit(survs$quarters[survs$survey %in% surveys.sel[i]],",")[[1]])
        }
    }else if(inherits(quarters,"list")){
        if(length(quarters) != ns) stop("Length of list with quarters does not match number of surveys.")
        if(!all(sapply(quarters, is.numeric))) stop("All elements of quarters have to be numeric.")
        quarters.pre <- quarters
        for(i in 1:ns){
            tmp <- as.numeric(strsplit(survs$quarters[survs$survey %in% surveys.sel[i]],",")[[1]])
            quarters.sel[[i]] <-  tmp[which(tmp %in% quarters.pre[[i]])]
            if(length(quarters.sel[[i]]) < 1) stop(paste0("Selected quarter(s) (",quarters.pre[[i]],") is/are not present in Survey ",surveys.sel[i],". Please select different quarters, survey-specific quarters, or omit survey. See get.info.surveys() for more information."))
        }
        quarters.sel <- quarters
    }else{
        quarters.pre <- quarters[which(as.numeric(quarters) %in% all.quarters)]
        if(any(!quarters.pre %in% all.quarters))
            if(verbose) writeLines(paste0("Following quarters could not be matched (1:4): ",
                              quarters.pre[which(!quarters.pre %in% all.quarters)]))
        for(i in 1:ns){
            tmp <- as.numeric(strsplit(survs$quarters[survs$survey %in% surveys.sel[i]],",")[[1]])
            quarters.sel[[i]] <-  tmp[which(tmp %in% quarters.pre)]
            if(length(quarters.sel[[i]]) < 1) stop(paste0("Selected quarter(s) (",quarters.pre,") is/are not present in Survey ",surveys.sel[i],". Please select different quarters, survey-specific quarters, or omit survey. See get.info.surveys() for more information."))
        }
    }

    if(is.null(files)){
        files2 <- dir(file.dir)
    }else{
        files2 <- dir(file.dir)[which(dir(file.dir) %in% files)]
    }
    nfiles <- length(files2)

    ## Haul info from Datras
    hl <- hh <- ca <- file.names <- NULL
    for(i in 1:nfiles){
        dat <- read.table(file.path(file.dir, files2[i]))
        dat.type <- strsplit(files2[i],"_")[[1]][1]

        if(inherits(dat, "data.frame")){
            ## CHECK: Remove this option as it would get rid of the zero hauls?
            ## Better: e.g. only keep surveys were species was present at least once.
            ## Subset Species if provided
            if(dat.type == "HL" && !(aphiaID[1] %in% c("all","All","ALL")) &&
               !is.null(aphiaID[1]) && !is.na(aphiaID[1])){
                ## if(verbose) writeLines(paste0("Subsetting downloaded data for following Aphia ID(s): ",
                ##                   paste0(aphiaID, collapse = ", ")))
                dat <- subset(dat, Valid_Aphia %in% aphiaID)
            }
            ## Rename some variables
            if(dat.type == "HH"){
                colnames(dat)[which(colnames(dat) == "ShootLong")] <- "lon"
                colnames(dat)[which(colnames(dat) == "ShootLat")] <- "lat"

            }else if(dat.type %in% c("HL","CA")){
                colnames(dat)[which(colnames(dat) == "Valid_Aphia")] <- "AphiaID"
                lngt2cm <- c("." = 0.1, "0" = 0.1, "1" = 1, "2" = 1, "5" = 1)[as.character(dat$LngtCode)]
                dat$LngtCm <- lngt2cm * dat$LngtClass
            }

            ## Subset required variables
            if(reduce.file.size){
                ind <- datras.variables[[dat.type]]
                ## if(verbose) writeLines(paste0("Subsetting downloaded data for following variables: ",
                ##                   paste0(ind, collapse = ", ")))
                dat <- dat[,ind]
            }
        }

        ## Combine data from surveys
        if(dat.type == "HH"){
            hh <- try(rbind(hh, dat), silent = TRUE)
            if(inherits(hh, "try-error")){
                stop(paste0("Cannot merge data sets. Please contact the package maintainer. This might be due to a misfit in DATRAS variables: ",lapply(dat, colnames)))
            }
        }else if(dat.type == "HL"){
            hl <- try(rbind(hl, dat), silent = TRUE)
            if(inherits(hl, "try-error")){
                stop(paste0("Cannot merge data sets. Please contact the package maintainer. This might be due to a misfit in DATRAS variables: ",lapply(dat, colnames)))
            }
        }else if(dat.type == "CA"){
            ca <- try(rbind(ca, dat), silent = TRUE)
            if(inherits(ca, "try-error")){
                stop(paste0("Cannot merge data sets. Please contact the package maintainer. This might be due to a misfit in DATRAS variables: ",lapply(dat, colnames)))
            }
        }
    }


    if(calc.swept.area){
        hh <- calc.swept.area(list(HH=hh))$HH
    }

    ## Return
    ## -----------
    res <- list(HH = hh, HL = hl, CA = ca)
    class(res) <- c("fdist.datras","list")
    return(res)
}
