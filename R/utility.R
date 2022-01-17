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
        res <- as.data.frame(t(sapply(survey.sel, function(x) x[c("survey","first.year","last.year","quarters")])))
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
