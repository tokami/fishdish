#' @name download.data
#' @title Download all surveys
#' @param first.year First year (default: 1967)
#' @param last.year Last year (default: 2020)
#' @param surveys (default: "all")
#' @param quarters (default: "all")
#' @return List containing hh and hl data sets.
#' @importFrom icesDatras getDATRAS
#' @export
download.data <- function(first.year = 1967, last.year = 2020, surveys = "all", quarters = "all"){

    ## TODO: don't use quarters 1:4 for every survey, some surveys might have had some quarters in the past, but then stopped, e.g. BITS, make helper function to get the quarters for each survey!

    all.surveys <- list.surveys()
    if(surveys == "all" || surveys == "All" || surveys == "ALL") surveys <- all.surveys
    surveys.sel <- surveys[which(surveys %in% all.surveys)]
    if(any(!surveys %in% all.surveys))
        writeLines(paste0("Following surveys could not be matched (use list.surveys): ",
                          surveys[which(!surveys %in% all.surveys)]))
    ns <- length(surveys.sel)

    all.quarters <- 1:4
    if(quarters == "all" || quarters == "All" || quarters == "ALL") quarters <- all.quarters
    quarters.sel <- quarters[which(quarters %in% all.quarters)]
    if(any(!quarters %in% all.quarters))
        writeLines(paste0("Following quarters could not be matched (1:4): ",
                          quarters[which(!quarters %in% all.quarters)]))

    writeLines("Downloading data sets. This might take some time.")

    ## Haul info from Datras
    for(dt in 1:2){
        if(dt==1) dat.type <- "HH" else if(dt==2) dat.type <- "HL"
        dat <- vector("list",ns)
        for(i in 1:ns){
            surv <- surveys.sel[i]
            if(dat.type == "HH"){
                writeLines(paste0("Downloading 1st data set (HH) of: ",surv))
            }else if(dat.type == "HL"){
                writeLines(paste0("Downloading 2nd data set (HL) of: ",surv))
            }
            dat[[i]] <- suppressMessages(icesDatras::getDATRAS(record = dat.type,
                                                               survey = surv,
                                                               years = first.year:last.year,
                                                               quarters = quarters.sel))
        }

        if(dt == 1){
            hh <- do.call(rbind, dat)
        }else if(dt == 2){
            hl <- do.call(rbind, dat)
        }
    }

    return(list(hh=hh,hl=hl))
}
