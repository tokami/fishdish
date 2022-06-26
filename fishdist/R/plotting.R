
#' @name plotfdist.cog
#' @title plot cog
#' @param cog cog
#' @return Nothing
#' @export
plotfdist.cog <- function(cog){
    par(mfrow = c(2,1), mar = c(1,5,2,1), oma = c(4,0,1,1))
    plot(cog$year, cog$cog.lon, ty = 'b',
         ylab = "Longitude", xlab = "")
    plot(cog$year, cog$cog.lat, ty = 'b',
         ylab = "Latitude", xlab = "")
    mtext("Year", 1, 2, outer = TRUE)
    mtext("Center of gravity", 3, -1, outer = TRUE, font = 2)
}


#' @name plotdist
#' @title plot dist
#' @param data data
#' @importFrom maps map
#' @return Nothing
#' @export
plotdist <- function(data, plot.survey.dist = TRUE){

    if(inherits(data, "list")){
        aphia.name <- "AphiaID"
        specs <- unique(data[[2]][,aphia.name])
        ns <- length(specs)
##         ns <- 1

        statrec.all <- statrec.obs <- statrec.nonobs <- vector("list", ns)
        for(i in 1:ns){
            sub0 <- data[[1]]
           sub <- data[[2]][data[[2]][,aphia.name] == specs[i],]
            statrec.all[[i]] <- unique(c(sub0$StatRec, sub$StatRec))
            statrec.all[[i]] <- unique(sub0$StatRec)
           statrec.obs[[i]] <- unique(sub$StatRec[which(sub$N > 0)])
           statrec.nonobs[[i]] <- statrec.all[[i]][!statrec.all[[i]] %in% statrec.obs[[i]]]
        }
    }else{
        specs <- unique(data$AphiaID)
        ns <- length(specs)

        statrec.all <- statrec.obs <- statrec.nonobs <- sub <- vector("list", ns)
        for(i in 1:ns){
            sub[[i]] <- subset(data, AphiaID == specs[i])
            statrec.all[[i]] <- unique(sub[[i]]$StatRec)
            statrec.obs[[i]] <- unique(sub[[i]]$StatRec[which(sub[[i]]$N > 0)])
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
        ## HERE:
        if(plot.survey.dist & length(statrec.nonobs[[i]]) > 0){
            ind <- which(ices.rectangles$ICESNAME %in% statrec.nonobs[[i]])
            tmp <- ices.rectangles[ind,]
            for(j in 1:nrow(tmp)){
                tmpj <- tmp[j,]
                polygon(c(tmpj$stat_west, tmpj$stat_east, tmpj$stat_east, tmpj$stat_west),
                        c(tmpj$stat_south, tmpj$stat_south, tmpj$stat_north, tmpj$stat_north),
                        border = "goldenrod2", col = "goldenrod3")
            }
        }
        ind <- which(ices.rectangles$ICESNAME %in% statrec.obs[[i]]) ## obs
        tmp <- ices.rectangles[ind,]
        for(j in 1:nrow(tmp)){
            tmpj <- tmp[j,]
                polygon(c(tmpj$stat_west, tmpj$stat_east, tmpj$stat_east, tmpj$stat_west),
                        c(tmpj$stat_south, tmpj$stat_south, tmpj$stat_north, tmpj$stat_north),
                    border = "dodgerblue2", col = "dodgerblue3")
        }
        ## maps::map("world", xlim = lon.range, ylim = lat.range,
        ##           fill = TRUE, plot = TRUE, add = TRUE,
        ##           col = grey(0.8),
        ##           border = grey(0.7)
        ##           )
        ## points(sub[[i]]$lon[sub[[i]]$N > 0], sub[[i]]$lat[sub[[i]]$N > 0], pch = 16, col = 1, cex = 0.8)
        ## points(sub[[i]]$lon, sub[[i]]$lat, pch = 16, col = 1, cex = 0.8)
        mtext(paste0("AphiaID: ", specs[i]), 3, 0.5)
        box(lwd=1.5)
    }
    mtext("Latitude", 2, 1, outer = TRUE)
    mtext("Longitude", 1, 1, outer = TRUE)

}




#' @name plotfdist.abun
#'
#' @title plot fit
#'
#' @param fit fit
#' @param by by
#'
#'
#' @return Nothing
#'
#' @importFrom RColorBrewer brewer.pal
#'
#' @export
plotfdist.abun <- function(fit, by.area = FALSE,
                           ylab = "Abundance index",
                           y.scale = 1){
    if(by.area){

        cols <- c(RColorBrewer::brewer.pal(n = 8, "Dark2"), RColorBrewer::brewer.pal(n = 8, "Accent"))

        areas <- sapply(fit, function(x) unique(x$grid[[1]]$Area_27))
        ## tail needed because some ices areas spread over multiple ecoregions (e.g. 4a = Faroe, NS, Celtic)
        ecoregions <- sapply(fit, function(x) tail(unique(x$grid[[1]]$Ecoregion),1))
        ecoregions.uni <- unique(ecoregions)
        nareas <- length(areas)
        neco <- length(ecoregions.uni)

        if(neco >= 19){
            mfrow = c(4,ceiling(neco/4))
        }else if(neco >= 9){
            mfrow = c(3,ceiling(neco/3))
        }else if(neco < 9 & neco >= 4){
            mfrow = c(2,ceiling(neco/2))
        }else if(neco < 4){
            mfrow = c(1,neco)
        }
        if(neco > 1){
            ## par(mfrow = mfrow, mar = c(0.5,0.5,0.5,0.5), oma = c(4,3,2,1))
            par(mfrow = mfrow, mar = c(2,2,1,1), oma = c(3,3,2,1))
        }
        ## par(mfrow = c(neco,1), mar = c(2,2,1,1), oma = c(2,3,2,1))


        xlim <- range(as.numeric(unlist(lapply(fit, function(x){
            if(!inherits(x$fits[[1]],"try-error")){
                rownames(x$fits[[1]]$idx)
            }else{
                NA
            }}))), na.rm = TRUE)
        xaxt.ind <- c((prod(mfrow) - mfrow[2] + 1):prod(mfrow), ((mfrow[1] - 1) * mfrow[2]):((mfrow[1] - 1) * mfrow[2] - (prod(mfrow) - neco) + 1))
        yaxt.ind <- seq(1, prod(mfrow), mfrow[2])
        for(i in 1:neco){
            ias <- which(ecoregions %in% ecoregions.uni[i])
            nia <- length(ias)
            xaxt <- ifelse(i %in% xaxt.ind, "s", "n")
##            yaxt <- ifelse(i %in% yaxt.ind, "s", "n")
            ylim <- range(unlist(lapply(fit[ias], function(x){
                if(!inherits(x$fits[[1]],"try-error")){
                    c(x$fits[[1]]$idx, x$fits[[1]]$lo, x$fits[[1]]$up)
                }else{
                    NA
                }})), na.rm = TRUE) / y.scale
            plot(xlim, c(1,1), ty = "n",
                 xaxt = xaxt, yaxt = "s",
                 ylim = ylim, xlim = xlim,
                 xlab = "", ylab = "")
            for(j in 1:nia){
                if(!inherits(fit[[ias[j]]]$fits[[1]], "try-error")){
                    years <- rownames(fit[[ias[j]]]$fits[[1]]$idx)
                    idx <- fit[[ias[j]]]$fits[[1]]$idx / y.scale
                    lo <- fit[[ias[j]]]$fits[[1]]$lo / y.scale
                    up <- fit[[ias[j]]]$fits[[1]]$up / y.scale
                    polygon(c(years,rev(years)), c(lo, rev(up)), border = NA,
                            col = rgb(t(col2rgb(cols[j]))/255, alpha = 0.2))
                    lines(years, idx, col = cols[j], lwd = 2)
                }
            }
            mtext(ecoregions.uni[i], 3, 0.5, font = 2, cex = 0.9)
            ## legend("topleft",
            ##        legend = ecoregions.uni[i],
            ##        pch = NA,
            ##        x.intersp = 0.1,
            ##        bg = "white")
            legend("topright",
                   legend = areas[ias],
                   lwd = 2,
                   col = cols[1:nia],
                   bg = "white")
            box(lwd=1.5)
        }
        mtext("Year", 1, 1, outer = TRUE)
        mtext(ylab, 2, 1, outer = TRUE)


    }else{

        ## TODO: plot relative (to what?) or absolute
        alpha <- 0.3
        cols <- c("dodgerblue","darkorange","darkgreen","goldenrod")

        fiti <- fit$fits
        ## CHECK: TODO: might be species if fitted with est.dist
        ns <- length(fiti)
        years <- as.numeric(rownames(fiti[[1]]$idx))
        ylim <- range(lapply(fiti, function(x) c(x$idx, x$up, x$lo) / y.scale))
        xlim <- range(years)
        plot(years, fiti[[1]]$idx/y.scale, ty='n',
             xlim = xlim, ylim = ylim,
             xlab = "", ylab = "")
        for(i in 1:ns){
            polygon(c(years,rev(years)), c(fiti[[i]]$lo,rev(fiti[[i]]$up))/y.scale,
                    border = NA, col = rgb(t(col2rgb(cols[i]))/255, alpha = alpha))
        }
        for(i in 1:ns){
            lines(years, fiti[[i]]$idx/y.scale, ty='b', col = cols[i])
        }
        box(lwd=1.5)
        mtext(ylab, 2, 3)
        mtext("Year", 1, 3)
        if(ns > 1) legend("topright",
                          legend = paste0("model ",1:ns),
                          col = cols[1:ns], lwd = 1, lty = 1,
                          bg = "white")

    }
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
plotfdist.dist <- function(fit, mod = NULL, cex = 1){

    nmods <- length(fit$fit)
    if(is.null(mod)){
        if(nmods == 1){
            mod <- 1
        }else{
            mod <- 1:nmods
        }
    }
    nmods <- length(fit$fit[mod])
    pred.by.haul <- fit$pred.by.haul

    if(pred.by.haul){
        predD <- NULL
        myids <- fit$grid[[3]]
    }else{
        predD <- fit$grid
        myids <- NULL
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
        map.cex = cex
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
        if(!any(names(tmp) == "lat")){
            tmp <- predD[[1]]
            warning("Seems that yearly variable grid is used. Using only the grid for the first year!")
        }
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
        maps::map("world", xlim = xlims, ylim = ylims,
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
plotfdist.dist.year <- function(fit, mod = NULL, var.lim = FALSE, all.years = TRUE,
                                sandeel_areas = NULL, tobisbanker_wgs84 = NULL,
                                xlim = NULL, ylim = NULL, cex = 1){

    nmods <- length(fit$fits)
    if(nmods == 1){
        mod <- 1
    }else if(is.null(mod)){
        stop("Please use argument 'mod' to select one of the models, e.g. 'mod=1' for the first model!")
    }
    pred.by.haul <- fit$pred.by.haul

    if(pred.by.haul){
        predD <- NULL
        myids <- fit$grid[[3]]
    }else{
        predD <- fit$grid
        myids <- NULL
    }

    fitx <- fit$fit[[mod]]
    data <- fit$data
    map.cex <- cex
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
            par(mfrow = mfrow, mar = c(1,1,2,1), oma = c(2,2,2,1))
        }else{
            par(mar = c(1,1,2,1), oma = c(4,3,2,1))
        }

        for(j in 1:ny){
            year <- years[j]
            if(is.na(year)) break()
            colors=rev(heat.colors(8))
            a <- 1
            if(is.null(xlim) || is.null(ylim)){
                if(var.lim){
                    xlims <- NULL
                    ylims <- NULL
                }else{
                    xlims = range(data$lon, na.rm = TRUE)
                    ylims = range(data$lat, na.rm = TRUE)
                }
            }else{
                xlims <- xlim
                ylims <- ylim
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
                for(i in 1:nlevels(ally$zFac[sel])){
                    ind <- which(ally$zFac[sel] == levels(ally$zFac[sel])[i])
                    points(tmpx$lon[ind], y = tmpx$lat[ind],
                           col = colors[i], ## col = colors[as.numeric(ally$zFac[sel])],
                           pch = 16, cex = map.cex)
                }
                ## ## REMOVE:
                ## sp:::plot.SpatialPolygons(sandeel_areas, xlim = xlims, ylim = ylims,add=TRUE,
                ##      border = rgb(t(col2rgb("grey10"))/255,alpha=0.4))
                maps::map("world", xlim = xlims, ylim = ylims,
                          fill = TRUE, plot = TRUE, add = TRUE,
                          col = grey(0.95), border = grey(0.5))
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
               col = colors, bg = "white",
               cex = 1.2)

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
                maps::map("world;", xlim = xlims, ylim = ylims,
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



#' @name plotfdist.gam.effects
#'
#' @title plot gam effects
#'
#' @param fit fit
#' @param mod Select one of the models. Default: 1
#'
#' @importFrom mgcv plot.gam
#'
#' @return Nothing
#'
#' @export
plotfdist.gam.effects <- function(fit, mod = 1, xlim = NULL, ylim = NULL){

    plotInfo <- mgcv::plot.gam(fit$fits[[mod]]$pModels[[1]], select = 0, residuals = TRUE)

    gamTerms <- unlist(lapply(plotInfo, function(x) ifelse(!is.null(x$xlab), x$xlab, NA)))

    cols <- c("dodgerblue", "goldenrod3")
    par(mfrow = c(1,3))

    ind <- which(gamTerms == "lon")
    if(length(ind) > 0){
        tmp <- plotInfo[[ind]]
        tmp$lo <- tmp$fit - 1.95 * tmp$se
        tmp$up <- tmp$fit + 1.95 * tmp$se
        fitmat <- matrix(tmp$fit, nrow = length(tmp$x), ncol = length(tmp$y))
        if(is.null(xlim)) xlim <- range(tmp$x)
        if(is.null(ylim)) ylim <- range(tmp$y)
        image(tmp$x, tmp$y, fitmat,
              xlab = tmp$xlab, ylab = tmp$ylab,
              xlim = xlim, ylim = ylim)
        contour(tmp$x, tmp$y, fitmat, add = TRUE)
        maps::map('world',xlim=xlim,ylim=ylim,
                  fill=TRUE,plot=TRUE,add=TRUE,
                  col=rgb(t(col2rgb("grey70"))/255, alpha = 0.3),
                  border=rgb(t(col2rgb("white"))/255, alpha = 0.3))
        ## points(tmp$raw$x, tmp$raw$y, pch = 16, cex = 0.1,
        ##        col = rgb(t(col2rgb("grey80"))/255, alpha = 0.6))
        box(lwd=1.5)
    }

    ind <- which(gamTerms == "ctime")
    if(length(ind) > 0){
        tmp <- plotInfo[[2]]
        tmp$lo <- tmp$fit - 1.95 * tmp$se
        tmp$up <- tmp$fit + 1.95 * tmp$se
        plot(tmp$x, tmp$fit, ty = "n",
             ylim = range(tmp$fit, tmp$lo, tmp$up), ## tmp$p.resid
             xlab = tmp$xlab, ylab = tmp$ylab)
        polygon(c(tmp$x, rev(tmp$x)), c(tmp$lo, rev(tmp$up)),
                border = NA, col = rgb(t(col2rgb(cols[1]))/255, alpha = 0.3))
        ## points(tmp$raw, tmp$p.resid, pch = 16, cex = 0.1,
        ##        col = rgb(t(col2rgb("grey80"))/255, alpha = 0.6))
        lines(tmp$x, tmp$fit, col = cols[1], lwd = 2)
        rug(tmp$raw)
        box(lwd=1.5)
    }

    ind <- which(gamTerms == "Depth")
    if(length(ind) > 0){
        tmp <- plotInfo[[5]]
        tmp$lo <- tmp$fit - 1.95 * tmp$se
        tmp$up <- tmp$fit + 1.95 * tmp$se
        plot(tmp$x, tmp$fit, ty = "n",
             ylim = range(tmp$fit, tmp$lo, tmp$up), ## tmp$p.resid
             xlab = tmp$xlab, ylab = tmp$ylab)
        polygon(c(tmp$x, rev(tmp$x)), c(tmp$lo, rev(tmp$up)),
                border = NA, col = rgb(t(col2rgb(cols[2]))/255, alpha = 0.3))
        ## points(tmp$raw, tmp$p.resid, pch = 16, cex = 0.1,
        ##        col = rgb(t(col2rgb("grey80"))/255, alpha = 0.6))
        lines(tmp$x, tmp$fit, col = cols[2], lwd = 2)
        rug(tmp$raw)
        box(lwd=1.5)
    }
}


#' @name plotfdist.diag
#'
#' @title plot diagnostics
#'
#' @param fit fit
#' @param mod Select one of the models. Default: 1
#'
#' @importFrom mgcv plot.gam
#'
#' @return Nothing
#'
#' @export
plotfdist.diag <- function(fit, mod = 1){
    opar <- par()
    par(mfrow = c(2,3), mar = c(5,5,4,2), oma = c(0,0,0,0))
    on.exit(par(opar))
    surveyIndex::surveyIdxPlots(fit$fits[[mod]],
                                fit$data,
                                select = "residuals",
                                par = NULL,
                                main = "Residuals")
    surveyIndex::surveyIdxPlots(fit$fits[[mod]],
                                fit$data,
                                select = "fitVsRes",
                                par = NULL,
                                main = "Residuals vs. Fitted")
    surveyIndex::surveyIdxPlots(fit$fits[[mod]],
                                fit$data,
                                select = "resVsYear",
                                par = NULL,
                                main = "Residuals vs. year")
    surveyIndex::surveyIdxPlots(fit$fits[[mod]],
                                fit$data,
                                select = "resVsShip",
                                par = NULL,
                                main = "Residuals vs. Ship")
    surveyIndex::surveyIdxPlots(fit$fits[[mod]],
                                fit$data,
                                select = "resVsGear",
                                par = NULL,
                                main = "Residuals vs. Gear")
    surveyIndex::surveyIdxPlots(fit$fits[[mod]],
                                fit$data,
                                select = "resVsDepth",
                                par = NULL,
                                main = "Residuals vs. Depth")
    return(NULL)
}


#' @name plotfdist.diag.spatial
#'
#' @title plot spatial diagnostics
#'
#' @param fit fit
#' @param mod Select one of the models. Default: 1
#'
#' @importFrom mgcv plot.gam
#'
#' @return Nothing
#'
#' @export
plotfdist.diag.spatial <- function(fit, mod = 1, year = NULL){
    mfrow <- par$mfrow
    xaxt.ind <- (prod(mfrow) - mfrow[2] + 1):prod(mfrow)
    yaxt.ind <- seq(1, prod(mfrow), mfrow[2])
    for(y in 1:length(year)){
        xaxt <- ifelse(y %in% xaxt.ind, "s", "n")
        yaxt <- ifelse(y %in% yaxt.ind, "s", "n")
        surveyIndex::surveyIdxPlots(fit$fits[[mod]],
                                    fit$data,
                                    select = "spatialResiduals",
                                    year = year[y],
                                    par = NULL,
                                    xaxt = xaxt, yaxt = yaxt,
                                    main = year[y])
    }
    return(NULL)
}
