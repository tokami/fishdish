#' @name download.data
#' @title Download all surveys
#' @param first.year First year (default: 1967)
#' @param last.year Last year (default: 2020)
#' @param surveys (default: "all")
#' @param quarters (default: "all")
#' @param datasets (default: "HH","HL")
#' @return List containing hh and hl data sets.
#' @importFrom icesDatras getDATRAS
#' @export
download.data <- function(first.year = 1967, last.year = 2020,
                          surveys = "all", quarters = "all", datasets = c("HH","HL")){

    ## Check surveys
    all.surveys <- list.surveys()
    if(surveys == "all" || surveys == "All" || surveys == "ALL") surveys <- all.surveys
    surveys.sel <- surveys[which(surveys %in% all.surveys)]
    if(any(!surveys %in% all.surveys))
        writeLines(paste0("Following surveys could not be matched (use list.surveys): ",
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
            writeLines(paste0("Following quarters could not be matched (1:4): ",
                              quarters.pre[which(!quarters.pre %in% all.quarters)]))
        for(i in 1:ns){
            tmp <- as.numeric(strsplit(survs$quarters[survs$survey %in% surveys.sel[i]],",")[[1]])
            quarters.sel[[i]] <-  tmp[which(tmp %in% quarters.pre)]
            if(length(quarters.sel[[i]]) < 1) stop(paste0("Selected quarter(s) (",quarters.pre,") is/are not present in Survey ",surveys.sel[i],". Please select different quarters, survey-specific quarters, or omit survey. See get.info.surveys() for more information."))
        }
    }

    writeLines("Downloading data sets. This might take some time.")


    ## Haul info from Datras
    hl <- hh <- NULL
    for(dt in 1:length(datasets)){
        dat.type <- datasets[dt]
        dat <- vector("list",ns)
        for(i in 1:ns){
            surv <- surveys.sel[i]
            writeLines(paste0("Downloading data set '",dat.type,"' of: ", surv))
            dat[[i]] <- suppressMessages(icesDatras::getDATRAS(record = dat.type,
                                                               survey = surv,
                                                               years = first.year:last.year,
                                                               quarters = quarters.sel[[i]]))
        }
        ## Combine data from surveys
        if(dat.type == "HH"){
            hh <- do.call(rbind, dat)
        }else if(dat.type == "HL"){
            hl <- do.call(rbind, dat)
        }
    }

    return(list(hh=hh,hl=hl))
}
