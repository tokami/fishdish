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




#' @name plotfdist.abund
#'
#' @title plot fit
#'
#' @param fit fit
#'
#' @importFrom maps map
#'
#' @return Nothing
#'
#' @export
plotfdist.abund <- function(fit){

    fit <- fit$fit

    ## CHECK: TODO: might be species if fitted with est.dist
    ns <- length(fit)


    ylim <- range(sapply(fit, function(x) x$idx))

    plot(rownames(fit[[1]]$idx), fit[[1]]$idx, ty='n',
         xlim = range(as.numeric(rownames(fit[[1]]$idx))),
         ylim = ylim)
    for(i in 1:ns){
        lines(rownames(fit[[i]]$idx), fit[[i]]$idx, ty='b', col = i)
    }
    box(lwd=1.5)
    mtext("Abundance index", 2, 3)
    mtext("Year", 1, 3)
    if(ns > 1) legend("topright",
                      legend = paste0("model ",1:ns),
                      col = 1:ns, lwd = 1, lty = 1,
                      bg = "white")
}



#' @name plotfdist.dist
#'
#' @title plot fit
#'
#' @param fit fit
#' @param mod Select one of the models. Default: NULL
#'
#' @importFrom maps map
#'
#' @return Nothing
#'
#' @export
plotfdist.dist <- function(fit, mod = NULL){

    nmods <- length(fit$fit)
    if(is.null(mod)){
        if(nmods == 1){
            mod <- 1
        }else{
            mod <- 1:nmods
        }
    }
    nmods <- length(fit$fit[mod])
    use.bathy <- fit$use.bathy


    if(use.bathy){
        predD <- fit$grid
        myids <- NULL
    }else{
        predD <- NULL
        myids <- fit$grid[[3]]
    }

    if(nmods >= 19){
        mfrow = c(4,ceiling(nmods/4))
    }else if(nmods >= 9){
        mfrow = c(3,ceiling(nmods/3))
    }else if(nmods < 9 & nmods >= 4){
        mfrow = c(2,ceiling(nmods/2))
    }else if(nmods < 4){
        mfrow = c(1,nmods)
    }
    if(nmods > 1){
        lt <- layout(matrix(c(1:(mfrow[1]*mfrow[2]),rep(mfrow[1]*mfrow[2]+1,3)),
                            mfrow[1]+1,mfrow[2],byrow = TRUE), heights = c(rep(1,mfrow[1]),0.2))
        par(mar = c(1,1,2,1), oma = c(4,3,2,1))
    }



    for(i in mod){

        x <- fit$fit[[i]]
        dat <- fit$data
        cols=1
        alt.idx=NULL
        myids = myids
        predD = predD
        par=NULL
        legend=TRUE
        map.cex = 1.5
        main = paste0("Model ",i)
        colors=rev(heat.colors(8))
        select="map"
        plotByAge=FALSE
        xlims = range(dat$lon, na.rm = TRUE)
        ylims = range(dat$lat, na.rm = TRUE)
        mapvals = NULL
        year = NULL
        a = 1
        if (is.null(predD)) {
            tmp = subset(dat, haul.id %in% myids)
        } else {
            tmp = predD
        }
        if(!any(names(tmp) == "lat")) stop("Seems that yearly variable Bathy grid is used. Overall map not implemented yet. Plot maps by year!")
        if (is.null(year)) {
            concT = surveyIndex:::concTransform(log(x$gPreds[[a]]))
            mapvals = x$gPreds[[a]]
        } else {
            y = which(as.numeric(as.character(names(x$gPreds2[[a]]))) ==
                      year)
            if (length(y) == 0)
                stop(paste("Year", year, "age group", a, "not found."))
            concT = surveyIndex:::concTransform(log(x$gPreds2[[a]][[y]]))
            mapvals = x$gPreds2[[a]][[y]]
        }
        if (length(colors) > 1){
            zFac = cut(concT, 0:length(colors)/length(colors))
        }else zFac = 1
        if (length(map.cex) > 1){
            sFac = cut(log(x$gPreds[[a]]), length(map.cex))
        }else sFac = 1
        myCols = colors
        plot(tmp$lon, y = tmp$lat, col = 1, pch = 1, cex = map.cex[sFac],
             xlim = xlims, ylim = ylims, xlab = "Longitude",
             ylab = "Latitude", main = main)
        points(tmp$lon, y = tmp$lat, col = myCols[zFac],
               pch = 16, cex = map.cex[sFac])
        ## ## REMOVE:
        ## sp:::plot.SpatialPolygons(sandeel_areas, xlim = xlims, ylim = ylims,add=TRUE,
        ##                           border = rgb(t(col2rgb("grey10"))/255,alpha=0.4))
        maps::map("worldHires", xlim = xlims, ylim = ylims,
                  fill = TRUE, plot = TRUE, add = TRUE, col = grey(0.5))
        ## ## REMOVE:
        ## sp:::plot.SpatialPolygons(tobisbanker_wgs84, xlim = xlims, ylim = ylims, add=TRUE,
        ##                           col=rgb(t(col2rgb("darkgoldenrod4"))/255,alpha=0.4),
        ##                           border=rgb(t(col2rgb("darkgoldenrod4"))/255,alpha=0.4))
        box(lwd=1.5)
        if (legend){
            maxcuts = aggregate(mapvals ~ zFac, FUN=max)
            mincuts = aggregate(mapvals ~ zFac, FUN=min)
            mm = mean(mapvals)
            ml = signif(mincuts[,2]/mm,3)
            ml[1] = 0
            leg = paste0("[",ml,",",signif(maxcuts[,2]/mm,3),"]")
            legend("bottomright", legend = leg, pch = 16, col = colors, bg = "white")
        }



    }

    ## surveyIndex::surveyIdxPlots(fit$fit[[i]], fit$data,
    ##                             cols=1, alt.idx=NULL,
    ##                             myids = myids, predD = predD,
    ##                             par=NULL,legend=FALSE,
    ##                             map.cex = 1.5, main = paste0("Model ",i),
    ##                             colors=rev(heat.colors(8)),
    ##                             select="map",plotByAge=FALSE)

}



#' @name plotfdist.dist.year
#'
#' @title plot fit
#'
#' @param fit fit
#' @param mod Select one of the models. Default: NULL
#' @param var.lim variable axis limits? Default: FALSE
#' @param all.years Plot all years? Otherwise plot 3 years: first or 1991, intermediate,
#'     and last year. Default: TRUE
#'
#' @importFrom maps map
#'
#' @return Nothing
#'
#' @export
plotfdist.dist.year <- function(fit, mod = NULL, var.lim = FALSE, all.years = TRUE, sandeel_areas = NULL, tobisbanker_wgs84 = NULL){

    nmods <- length(fit$fit)
    if(nmods == 1){
        mod <- 1
    }else if(is.null(mod)){
        stop("Please use argument 'mod' to select one of the models, e.g. 'mod=1' for the first model!")
    }
    use.bathy <- fit$use.bathy


    if(use.bathy){
        predD <- fit$grid
        myids <- NULL
    }else{
        predD <- NULL
        myids <- fit$grid[[3]]
    }

    fitx <- fit$fit[[mod]]
    data <- fit$data
    map.cex <- 1
    years0 <- sort(unique(fitx$yearNum))

    if(all.years){

        years <- years0
        ny <- length(years)
        ny1 <- ny + 1
        if(ny1 >= 19){
            mfrow = c(4,ceiling(ny1/4))
        }else if(ny1 >= 9){
            mfrow = c(3,ceiling(ny1/3))
        }else if(ny1 < 9 & ny1 >= 4){
            mfrow = c(2,ceiling(ny1/2))
        }else if(ny1 < 4){
            mfrow = c(1,ny1)
        }
        if(ny1 > 1){
            ## lt <- layout(matrix(c(1:(mfrow[1]*mfrow[2]),rep(mfrow[1]*mfrow[2]+1,3)),
            ##                     mfrow[1]+1,mfrow[2],byrow = TRUE),
            ##              heights = c(rep(1,mfrow[1]),0.2))
            par(mfrow = mfrow, mar = c(1,1,2,1), oma = c(4,3,2,1))
        }else{
            par(mar = c(1,1,2,1), oma = c(4,3,2,1))
        }

        for(j in 1:ny){
            year <- years[j]
            if(is.na(year)) break()
            colors=rev(heat.colors(8))
            a <- 1
            if(var.lim){
                xlims <- NULL
                ylims <- NULL
            }else{
                xlims = range(data$lon, na.rm = TRUE)
                ylims = range(data$lat, na.rm = TRUE)
            }
            if (is.null(predD)) {
                tmp = subset(data, haul.id %in% myids)
            }else {
                tmp = predD
            }
            ally = data.frame(val = fitx$gPreds2[[a]][[1]],
                              year = as.character(levels(as.factor(data$Year))[1]))
            cc = 0
            for (y in names(fitx$gPreds2[[a]])) {
                cc = cc + 1
                ally = rbind(ally, data.frame(val = fitx$gPreds2[[a]][[cc]],
                                              year = as.character(levels(as.factor(data$Year))[cc])))
            }
            ally$conc = surveyIndex:::concTransform(log(ally$val))
            ally$zFac = cut(ally$conc, 0:length(colors)/length(colors))
            for (yy in year) {
                if(inherits(tmp, "list")){
                    tmpx <- tmp[[as.character(yy)]]
                }else tmpx <- tmp
                plot(tmpx$lon, y = tmpx$lat, col = 1, pch = 1,
                     ty = "n",
                     xlim = xlims, ylim = ylims,
                     cex = map.cex, xlab = "", ylab = "",
                     axes = FALSE)
                points(tmpx$lon, y = tmpx$lat, col = 1, pch = 1,
                     cex = map.cex)
                title(yy, line = 1)
                sel = which(ally$year == yy)
                points(tmpx$lon, y = tmpx$lat, col = colors[as.numeric(ally$zFac[sel])],
                       pch = 16, cex = map.cex)
                ## ## REMOVE:
                ## sp:::plot.SpatialPolygons(sandeel_areas, xlim = xlims, ylim = ylims,add=TRUE,
                ##      border = rgb(t(col2rgb("grey10"))/255,alpha=0.4))
                maps::map("worldHires", xlim = xlims, ylim = ylims,
                          fill = TRUE, plot = TRUE, add = TRUE, col = grey(0.5))
                ## ## REMOVE:
                ## sp:::plot.SpatialPolygons(tobisbanker_wgs84, xlim = xlims, ylim = ylims, add=TRUE,
                ##      col=rgb(t(col2rgb("darkgoldenrod4"))/255,alpha=0.4),
                ##      border=rgb(t(col2rgb("darkgoldenrod4"))/255,alpha=0.4))
                box(lwd=1.5)
            }
        }
        plot.new()
        maxcuts = aggregate(val ~ zFac, data = ally,
                            FUN = max)
        mincuts = aggregate(val ~ zFac, data = ally,
                            FUN = min)
        mm = mean(ally$val)
        ml = signif(mincuts[, 2]/mm, 3)
        ml[1] = 0
        leg = paste0("[", ml, ",", signif(maxcuts[,2]/mm, 3), "]")
        legend("center",
               ## ncol = length(leg),
               legend = leg, pch = 16,
               col = colors, bg = "white")

    }else{

        years <- c(max(min(years0),1991), floor(mean(years0)), 2020)
        ny <- length(years)
        lt <- layout(matrix(c(1:3,rep(4,3)),2,3,byrow = TRUE), heights = c(1,0.2))
        par(mar = c(1,1,2,1), oma = c(4,3,2,1))
        for(j in 1:ny){
            year <- years[j]
            if(is.na(year)) break()
            colors=rev(heat.colors(8))
            a <- 1
            if(var.lim){
                xlims <- NULL
                ylims <- NULL
            }else{
                xlims = range(data$lon, na.rm = TRUE)
                ylims = range(data$lat, na.rm = TRUE)
            }
            if (is.null(predD)) {
                tmp = subset(data, haul.id %in% myids)
            }else {
                tmp = predD
            }
            ally = data.frame(val = fitx$gPreds2[[a]][[1]],
                              year = as.character(levels(as.factor(data$Year))[1]))
            cc = 0
            for (y in names(fitx$gPreds2[[a]])) {
                cc = cc + 1
                ally = rbind(ally, data.frame(val = fitx$gPreds2[[a]][[cc]],
                                              year = as.character(levels(as.factor(data$Year))[cc])))
            }
            ally$conc = surveyIndex:::concTransform(log(ally$val))
            ally$zFac = cut(ally$conc, 0:length(colors)/length(colors))
            for (yy in year) {
                if(is.list(tmp)){
                    tmpx <- tmp[[as.character(yy)]]
                }else tmpx <- tmp
                plot(tmpx$lon, y = tmpx$lat, col = 1, pch = 1,
                     xlim = xlims, ylim = ylims,
                     cex = map.cex, xlab = "", ylab = "",
                     axes = FALSE)
                title(yy, line = 1)
                sel = which(ally$year == yy)
                points(tmpx$lon, y = tmpx$lat, col = colors[as.numeric(ally$zFac[sel])],
                       pch = 16, cex = map.cex)
                maps::map("worldHires", xlim = xlims, ylim = ylims,
                          fill = TRUE, plot = TRUE, add = TRUE, col = grey(0.5))
                box(lwd=1.5)
            }
        }
        plot.new()
        maxcuts = aggregate(val ~ zFac, data = ally,
                            FUN = max)
        mincuts = aggregate(val ~ zFac, data = ally,
                            FUN = min)
        mm = mean(ally$val)
        ml = signif(mincuts[, 2]/mm, 3)
        ml[1] = 0
        leg = paste0("[", ml, ",", signif(maxcuts[,2]/mm, 3), "]")
        legend("center",
               ## ncol = length(leg),
               legend = leg, pch = 16,
               col = colors, bg = "white")
    }

}
