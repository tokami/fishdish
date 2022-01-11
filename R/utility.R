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
#' @return Data frame with info
#' @export
get.info.surveys <- function(survey=NULL){

    cur.year <- format(Sys.time(),"%Y")

    survey.info <- data.frame(survey = list.surveys(),
    first.year = c(1967,1991,1997,1998,2003,2005,2002,1999,2011,1996,1990,2001,2002,1985,2011,1985,2011,2002),
    last.year = c(rep(cur.year,7),2009,rep(cur.year,10)),
    quarters = c(paste0(c(1,3),collapse=","),paste0(c(1,4),collapse=","),4,4,4,paste0(c(1:4),collapse=","),
                 paste0(c(3,4),collapse=","),3,3,paste0(c(1:4),collapse=","),paste0(c(3,4),collapse=","),
                 paste0(c(3,4),collapse=","),paste0(c(3,4),collapse=","),paste0(c(1:4),collapse=","),
                 paste0(c(1:4),collapse=","),paste0(c(1:4),collapse=","),4,paste0(c(3,4),collapse=","))
    )
    ## TODO: add all statistical rectangles? or as included data? (pot. long vectors)

    if(!is.null(survey[1])){
        res <- survey.info[which(survey.info$survey %in% survey),]
        if(!any(survey.info$survey %in% survey))
            stop("Provided survey could not be matched (remove the arguments or run list.surveys() to see all surveys).")
    }else{
        res <- survey.info
    }

    return(res)
}





#' @name list.recom.models
#' @title List all model recommended structures
#' @param specdata Species data
#' @param use.toy Use time of year? (Caused problems for some species)
#' @param use.swept.area Use swept area? (Might not be available)
#' @return List with all model structures
#' @export
list.recom.models <- function(specdata, use.toy = TRUE, use.swept.area = TRUE){

    offset.var <- ifelse(use.swept.area, "SweptArea", "HaulDur")

    if(use.toy){

        ## models
        if(length(unique(specdata$Gear)) > 1 && length(unique(specdata$ShipG)) > 1){

            mps <- list(
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + te(timeOfYear, Lon, Lat, d=c(1,2), bs=c('cc','ds'), k=c(6,30), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + Gear + s(ShipG, bs='re') + offset(log(",offset.var,"))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + te(timeOfYear, Lon, Lat, d=c(1,2), bs=c('cc','ds'), k=c(6,30), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + Gear + offset(log(",offset.var,"))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + te(timeOfYear, Lon, Lat, d=c(1,2), bs=c('cc','ds'), k=c(6,30), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(",offset.var,"))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(SweptArea))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + offset(log(",offset.var,"))")
            )

        }else if(length(unique(specdata$ShipG)) > 1){

            writeLines(paste0("Not using Gear as variable - only one gear present in data set."))

            mps <- list(
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + te(timeOfYear, Lon, Lat, d=c(1,2), bs=c('cc','ds'), k=c(6,30), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + s(ShipG, bs='re') + offset(log(",offset.var,"))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + te(timeOfYear, Lon, Lat, d=c(1,2), bs=c('cc','ds'), k=c(6,30), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(",offset.var,"))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + te(timeOfYear, Lon, Lat, d=c(1,2), bs=c('cc','ds'), k=c(6,30), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(",offset.var,"))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(",offset.var,"))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + offset(log(",offset.var,"))")
            )

        }else{

            writeLines(paste0("Not using Gear nor ShipG as variable - only one gear and ShipG present in data set."))

            mps <- list(
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + te(timeOfYear, Lon, Lat, d=c(1,2), bs=c('cc','ds'), k=c(6,30), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(",offset.var,"))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + te(timeOfYear, Lon, Lat, d=c(1,2), bs=c('cc','ds'), k=c(6,30), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(",offset.var,"))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + te(timeOfYear, Lon, Lat, d=c(1,2), bs=c('cc','ds'), k=c(6,30), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(",offset.var,"))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(",offset.var,"))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + offset(log(",offset.var,"))")
            )

        }


    }else{
        ## models
        if(length(unique(specdata$Gear)) > 1 && length(unique(specdata$ShipG)) > 1){

            mps <- list(
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + Gear + s(ShipG, bs='re') + offset(log(",offset.var,"))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + Gear + offset(log(",offset.var,"))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(",offset.var,"))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(",offset.var,"))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + offset(log(",offset.var,"))")
            )

        }else if(length(unique(specdata$ShipG)) > 1){

            writeLines(paste0("Not using Gear as variable - only one gear present."))

            mps <- list(
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + s(ShipG, bs='re') + offset(log(",offset.var,"))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(",offset.var,"))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(",offset.var,"))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(",offset.var,"))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + offset(log(",offset.var,"))")
            )

        }else{

            writeLines(paste0("Not using Gear nor ShipG as variable - only one gear and ShipG present."))

            mps <- list(
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(",offset.var,"))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(",offset.var,"))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(",offset.var,"))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + s(Depth, bs='ds', k=5, m=c(1,0)) + offset(log(",offset.var,"))"),
                ##
                paste0("s(Lon, Lat, bs=c('ds'), k=c(128), m=c(1,0.5)) + te(ctime, Lon, Lat, d=c(1,2), bs=c('ds','ds'), k=c(12,32), m=list(c(1,0), c(1,0.5))) + offset(log(",offset.var,"))")
            )

        }
    }

    return(mps)

}
