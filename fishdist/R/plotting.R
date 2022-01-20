#' @name plotdist
#' @title plot dist
#' @param data data
#' @importFrom maps map
#' @return Nothing
#' @export
plotdist <- function(data, plot.survey.dist = TRUE){

    if(inherits(data, "list")){
        if(any(names(data[[2]]) == "AphiaID")){
            aphia.name <- "AphiaID"
        }else{
            aphia.name <- "Valid_Aphia"
            stop("Not yet implemented. Run prep.data first.")
        }
        specs <- unique(data[[2]][,aphia.name])
        ns <- length(specs)

        statrec.all <- statrec.obs <- statrec.nonobs <- vector("list", ns)
        for(i in 1:ns){
            sub0 <- data[[1]]
            sub <- data[[2]][data[[2]][,aphia.name] == specs[i],]
            statrec.all[[i]] <- unique(c(sub0$StatRec, sub$StatRec))
            statrec.obs[[i]] <- unique(sub$StatRec[which(sub$N > 0)])
            statrec.nonobs[[i]] <- statrec.all[[i]][!statrec.all[[i]] %in% statrec.obs[[i]]]
        }
    }else{
        specs <- unique(data$AphiaID)
        ns <- length(specs)

        statrec.all <- statrec.obs <- statrec.nonobs <- vector("list", ns)
        for(i in 1:ns){
            sub <- subset(data, AphiaID == specs[i])
            statrec.all[[i]] <- unique(sub$StatRec)
            statrec.obs[[i]] <- unique(sub$StatRec[which(sub$N > 0)])
            statrec.nonobs[[i]] <- statrec.all[[i]][!statrec.all[[i]] %in% statrec.obs[[i]]]
        }
    }


    data("ices.rectangles")
    lat.range <- c(35,63)
    lon.range <- c(-20,25)

    if(ns >= 19){
        mfrow = c(4,ceiling(ns/4))
    }else if(ns >= 9){
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
        if(plot.survey.dist & length(statrec.nonobs[[i]]) > 0){
            ind <- which(ices.rectangles$ICESNAME %in% statrec.nonobs[[i]])
            tmp <- ices.rectangles[ind,]
            for(j in 1:nrow(tmp)){
                tmpj <- tmp[j,]
                polygon(c(tmpj$WEST, tmpj$EAST, tmpj$EAST, tmpj$WEST),
                        c(tmpj$SOUTH, tmpj$SOUTH, tmpj$NORTH, tmpj$NORTH),
                        border = "goldenrod2", col = "goldenrod3")
            }
        }
        ind <- which(ices.rectangles$ICESNAME %in% statrec.obs[[i]])
        tmp <- ices.rectangles[ind,]
        for(j in 1:nrow(tmp)){
            tmpj <- tmp[j,]
            polygon(c(tmpj$WEST, tmpj$EAST, tmpj$EAST, tmpj$WEST),
                    c(tmpj$SOUTH, tmpj$SOUTH, tmpj$NORTH, tmpj$NORTH),
                    border = "dodgerblue2", col = "dodgerblue3")
        }
        maps::map("world", xlim = lon.range, ylim = lat.range,
                  fill = TRUE, plot = TRUE, add = TRUE,
                  col = grey(0.8),
                  border = grey(0.7)
                  )
        mtext(paste0("AphiaID: ", specs[i]), 3, 0.5)
        box(lwd=1.5)
    }
    mtext("Latitude", 2, 1, outer = TRUE)
    mtext("Longitude", 1, 1, outer = TRUE)

}




#' @name plotfit
#' @title plot fit
#' @param data data
#' @importFrom maps map
#' @return Nothing
#' @export
plotfit <- function(data){

    ns <- length(data)


    ## if(ns >= 19){
    ##     mfrow = c(4,ceiling(ns/4))
    ## }else if(ns >= 9){
    ##     mfrow = c(3,ceiling(ns/3))
    ## }else if(ns < 9 & ns >= 4){
    ##     mfrow = c(2,ceiling(ns/2))
    ## }else if(ns < 4){
    ##     mfrow = c(1,ns)
    ## }
    ## par(mfrow = mfrow, mar = c(3,3,2,1), oma = c(2.5,2.5,1,1))

    ylim <- range(sapply(data, function(x) x$idx))

    plot(rownames(data[[1]]$idx), data[[1]]$idx, ty='n',
         xlim = range(as.numeric(rownames(data[[1]]$idx))),
         ylim = ylim)
    for(i in 1:ns){
        lines(rownames(data[[i]]$idx), data[[i]]$idx, ty='b', col = i)
    }
    box(lwd=1.5)
    mtext("Abundance index", 2, 3)
    mtext("Year", 1, 3)
    if(ns > 1) legend("topright",
                      legend = paste0("model ",1:ns),
                      col = 1:ns, lwd = 1, lty = 1,
                      bg = "white")
}
