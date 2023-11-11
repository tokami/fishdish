
#' @name plotfishdish.cog
#' @title plot cog
#' @param cog cog
#' @return Nothing
#' @export
plotfishdish.cog <- function(cog){
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




#' @name plotfishdish.abun
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
plotfishdish.abun <- function(fit, by.area = FALSE, by.eco = FALSE,
                           ylab = "Abundance index",
                           y.scale = 1, fixed.scale = FALSE,
                           mean.one = FALSE){

    cols <- rep(c(RColorBrewer::brewer.pal(n = 8, "Dark2"),
                  RColorBrewer::brewer.pal(n = 8, "Accent")),50)

    if(by.area && by.eco){

        if(inherits(fit, "data.frame")){

            fit$year <- as.numeric(as.character(fit$year))
            fit$idx <- as.numeric(as.character(fit$idx))
            fit$lo <- as.numeric(as.character(fit$lo))
            fit$up <- as.numeric(as.character(fit$up))

            ecoregions <- fit$ecoregion
            ecoregions.uni <- unique(ecoregions)
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

            xlim <- range(fit$year)
            if(fixed.scale) ylim <- c(0.99,1.01) * range(c(fit$idx, fit$lo, fit$up), na.rm = TRUE) / y.scale
            xaxt.ind <- (prod(mfrow) - mfrow[2] + 1):prod(mfrow)
            if(prod(mfrow) - neco > 0) xaxt.ind <- (min(xaxt.ind) - (prod(mfrow) - neco)):max(xaxt.ind)
            yaxt.ind <- seq(1, prod(mfrow), mfrow[2])
            for(i in 1:neco){
                eco.ind <- which(ecoregions %in% ecoregions.uni[i])
                areas <- fit$ices.area[eco.ind]
                areas.uni <- unique(areas)
                nia <- length(areas.uni)
                xaxt <- ifelse(i %in% xaxt.ind, "s", "n")
                ##            yaxt <- ifelse(i %in% yaxt.ind, "s", "n")
                if(!fixed.scale) ylim <- c(0.99,1.01) * range(c(fit$idx[eco.ind], fit$lo[eco.ind], fit$up[eco.ind]), na.rm = TRUE) / y.scale
                plot(xlim, c(1,1), ty = "n",
                     xaxt = xaxt, yaxt = "s",
                     ylim = ylim, xlim = xlim,
                     xlab = "", ylab = "")
                for(j in 1:nia){
                    ia.ind <- which(areas %in% areas.uni[j])
                    years <- fit$year[eco.ind][ia.ind]
                    idx <- fit$idx[eco.ind][ia.ind] / y.scale
                    lo <- fit$lo[eco.ind][ia.ind] / y.scale
                    up <- fit$up[eco.ind][ia.ind] / y.scale
                    polygon(c(years,rev(years)), c(lo, rev(up)), border = NA,
                            col = rgb(t(col2rgb(cols[j]))/255, alpha = 0.2))
                    lines(years, idx, col = cols[j], lwd = 2)
                }
                mtext(ecoregions.uni[i], 3, 0.5, font = 2, cex = 0.9)
                ## legend("topleft",
                ##        legend = ecoregions.uni[i],
                ##        pch = NA,
                ##        x.intersp = 0.1,
                ##        bg = "white")
                legend("topright",
                       legend = areas.uni,
                       lwd = 2,
                       col = cols[1:nia],
                       bg = "white")
                box(lwd=1.5)
            }
            mtext("Year", 1, 1, outer = TRUE)
            mtext(ylab, 2, 1, outer = TRUE)

        }else{

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
            xaxt.ind <- (prod(mfrow) - mfrow[2] + 1):prod(mfrow)
            if(prod(mfrow) - neco > 0) xaxt.ind <- (min(xaxt.ind) - (prod(mfrow) - neco)):max(xaxt.ind)
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

        }

        }else if(by.area){

        if(inherits(fit, "data.frame")){

            fit$year <- as.numeric(as.character(fit$year))
            fit$idx <- as.numeric(as.character(fit$idx))
            fit$lo <- as.numeric(as.character(fit$lo))
            fit$up <- as.numeric(as.character(fit$up))

            areas <- fit$ices.area
            areas.uni <- unique(areas)
            nareas <- length(areas.uni)

            if(nareas >= 42){
                mfrow = c(6,ceiling(nareas/6))
            }else if(nareas >= 30){
                mfrow = c(5,ceiling(nareas/5))
            }else if(nareas >= 19){
                mfrow = c(4,ceiling(nareas/4))
            }else if(nareas >= 9){
                mfrow = c(3,ceiling(nareas/3))
            }else if(nareas < 9 & nareas >= 4){
                mfrow = c(2,ceiling(nareas/2))
            }else if(nareas < 4){
                mfrow = c(1,nareas)
            }
            if(nareas > 1){
                ## par(mfrow = mfrow, mar = c(0.5,0.5,0.5,0.5), oma = c(4,3,2,1))
                par(mfrow = mfrow, mar = c(2,2,1,1), oma = c(3,3,2,1))
            }
            ## par(mfrow = c(nareas,1), mar = c(2,2,1,1), oma = c(2,3,2,1))

            xlim <- range(fit$year)
            if(fixed.scale) ylim <- c(0.9,1.1) * range(c(fit$idx, fit$lo, fit$up)) / y.scale
            xaxt.ind <- (prod(mfrow) - mfrow[2] + 1):prod(mfrow)
            if(prod(mfrow) - nareas > 0) xaxt.ind <- (min(xaxt.ind) - (prod(mfrow) - nareas)):max(xaxt.ind)
            yaxt.ind <- seq(1, prod(mfrow), mfrow[2])
            for(i in 1:nareas){
                ia.ind <- which(areas %in% areas.uni[i])
                xaxt <- ifelse(i %in% xaxt.ind, "s", "n")
                ##            yaxt <- ifelse(i %in% yaxt.ind, "s", "n")
                if(!fixed.scale) ylim <- c(0.9,1.1) * range(c(fit$idx[ia.ind], fit$lo[ia.ind], fit$up[ia.ind])) / y.scale
                plot(xlim, c(1,1), ty = "n",
                     xaxt = xaxt, yaxt = "s",
                     ylim = ylim, xlim = xlim,
                     xlab = "", ylab = "")
                years <- fit$year[ia.ind]
                idx <- fit$idx[ia.ind] / y.scale
                lo <- fit$lo[ia.ind] / y.scale
                up <- fit$up[ia.ind] / y.scale
                polygon(c(years,rev(years)), c(lo, rev(up)), border = NA,
                        col = rgb(t(col2rgb(cols[i]))/255, alpha = 0.2))
                lines(years, idx, col = cols[i], lwd = 2)
                box(lwd=1.5)
                mtext(areas.uni[i], 3, 0.5, font = 2, cex = 0.9)
            }

            mtext("Year", 1, 1, outer = TRUE)
            mtext(ylab, 2, 1, outer = TRUE)

        }else{
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
            xaxt.ind <- (prod(mfrow) - mfrow[2] + 1):prod(mfrow)
            if(prod(mfrow) - neco > 0) xaxt.ind <- (min(xaxt.ind) - (prod(mfrow) - neco)):max(xaxt.ind)
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

        }



    }else{

        ## TODO: plot relative (to what?) or absolute
        alpha <- 0.3
        cols <- c("dodgerblue","darkorange","darkgreen","goldenrod")

        fiti <- fit$fits
        if(mean.one) y.scale <- mean(fiti[[1]]$idx[,1])
        ## CHECK: TODO: might be species if fitted with est.dist
        ns <- length(fiti)
        years <- as.numeric(rownames(fiti[[1]]$idx))
        ylim <- range(lapply(fiti, function(x) c(x$idx, x$up, x$lo) / y.scale), na.rm = TRUE)
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




#' @name plotfishdish.dist
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
plotfishdish.dist <- function(fit, mod = NULL, year = NULL,
                              grid.all = NULL,
                              xlim = NULL, ylim = NULL,
                              legend = TRUE, fixed.scale = TRUE,
                              title = NULL, xlab = NULL, ylab = NULL,
                              xaxt = NULL, yaxt = NULL,
                              fixed.lims = TRUE,
                              cols = rev(heat.colors(8)),
                              min.val = NA,
                              cut.cv = NULL, asp = 2,
                              plot.land = TRUE,
                              plot.obs = FALSE,
                              average = FALSE,
                              mfrow = NULL
                              ){
    xaxt0 <- xaxt
    yaxt0 <- yaxt

    ## TODO: not ideal fit$grid might be a list!
    if(is.null(grid.all)){
        grid.all <- fit$grid
    }

    if(is.null(year)){
        year <- tail(rownames(fit$fits[[1]]$idx),1)
        writeLines(paste0("No year selected. Plotting year: ",year))
    }
    if(is.null(mod)){
        mod <- 1
    }

    if(year[1] == "all"){
        year <- names(fit$fits[[mod]]$gPreds2[[1]])
    }

    names(fit$fits[[mod]]$gPreds2[[1]])
    ny <- length(year)
    pred <- fit$fits[[mod]]$gPreds2[[1]][as.character(year)]
    cv <- fit$fits[[mod]]$gPreds2.CV[[1]][as.character(year)]
    if(inherits(fit$grid, "list")){
        grid <- fit$grid[as.character(year)]
    }else{
        grid <- lapply(1:ny, function(x) fit$grid)
    }

    if(ny > 1 && !average && is.null(mfrow)){
        mfrow <- n2mfrow(ny, asp = asp)
        par(mfrow = mfrow, mar = c(0,0,2,0), oma = c(5,5,2,1))
    }else if(!is.null(mfrow)){
        par(mfrow = mfrow, mar = c(0,0,2,0), oma = c(5,5,2,1))
    }

    if(!is.null(mfrow)){
        xaxt.ind <- (prod(mfrow) - mfrow[2] + 1):prod(mfrow)
        yaxt.ind <- seq(1, prod(mfrow), mfrow[2])
    }else{
        xaxt.ind <- yaxt.ind <- 1
    }

    ##TODO: include error when first year not in pred!

    ## length(pred)

    nyx <- ifelse(average, 1, ny)
    if(average){
        grid <- list(collapse.grid(grid))
        nrowi <- sapply(pred, nrow)
        if(!is.na(sd(unlist(nrowi)[!sapply(nrowi,is.null)])) &&
           sd(unlist(nrowi)[!sapply(nrowi,is.null)]) > 0.1){
            stop("Did you use the same prediction grid for the years that you want to combine?")
        }
        tmp <- do.call(cbind, pred)
        pred <- list(data.frame(apply(tmp, 1, mean, na.rm = TRUE)))
    }

    if(fixed.scale){
        lastmax <- NULL
        prediAll <- unlist(lapply(pred, function(x) x[,1]))
        ind <- vector("list",length(pred))
        for(i in 1:length(pred)){
            if(i == 1){
                starti <- 1
            }else{
                starti <- (lastmax + 1)
            }
            if(!is.null(pred[[i]])){
                ind[[i]] <- starti:(starti + nrow(pred[[i]])-1)
                lastmax <- max(ind[[i]])
            }else{
                ind[[i]] <- NULL
            }
        }
        concT <- surveyIndex:::concTransform(log(prediAll))
        if(is.null(min.val) || is.na(min.val)){
            zFac <- cut(concT, 0:length(cols)/length(cols))
        }else{
            zFac <- cut(concT, sort(c(min.val,seq(0,1, length.out = length(cols)-1))))
        }
        for(i in 1:length(grid)){
            if(!is.null(ind[[i]])){
                grid[[i]]$pred <- as.numeric(zFac[ind[[i]]])
            }else{
                grid[[i]]$pred <- NA
            }
        }
    }

    if(fixed.lims){
        if(is.null(xlim)) xlim <- extendrange(r = range(unlist(lapply(grid, function(x) range(if(!is.null(x$lon)) x$lon else NA))),na.rm = TRUE), f = 0.1)
        if(is.null(ylim)) ylim <- extendrange(r = range(unlist(lapply(grid, function(x) range(if(!is.null(x$lat)) x$lat else NA))),na.rm = TRUE), f = 0.1)
    }


    for(i in 1:nyx){
        if(!fixed.scale){
            predi <- pred[[i]][,1]
            if(!is.null(predi)){
                concT <- surveyIndex:::concTransform(log(predi))
                zFac <- cut(concT, 0:length(cols)/length(cols))
                grid[[i]]$pred <- as.numeric(zFac)
            }else{
                grid[[i]]$pred <- NA
            }
        }
        if(!is.null(cut.cv)){
            grid[[i]]$pred[which(cv[[i]] > cut.cv)] <- NA
        }
        if(!is.null(grid[[i]]$lon)){
            ## tmp <- reshape2::acast(grid[[i]], lon~lat, value.var = "pred")
            ## Needed because grid[[i]] could have gaps which are then filled with long cells
            ## TODO: not ideal to have to provide another grid! how to fix?
            grid.dum <- merge(grid.all[,c("lon","lat")],
                              grid[[i]][,c("lon","lat","pred")],
                              by = c("lon","lat"), all.x = TRUE)
            tmp <- reshape2::acast(grid.dum, lon~lat, value.var = "pred")
        }
        if(is.null(xlim)) xlimi <- extendrange(r = range(as.numeric(rownames(tmp))), f = 0.1) else xlimi <- xlim
        if(is.null(ylim)) ylimi <- extendrange(r = range(as.numeric(colnames(tmp))), f = 0.1) else ylimi <- ylim
        if(is.null(xaxt0)) xaxt <- ifelse(i %in% xaxt.ind, "s", "n")
        if(is.null(yaxt0)) yaxt <- ifelse(i %in% yaxt.ind, "s", "n")
        plot(1,1, xlim = xlimi, ylim = ylimi,
             xaxt = xaxt, yaxt = yaxt,
             ty = "n",
             xlab = "", ylab = "")
        if(!is.null(grid[[i]]$lon)){
        image(as.numeric(rownames(tmp)), as.numeric(colnames(tmp)), tmp,
              add = TRUE,
              xlab = "", ylab = "", col = cols,
              breaks = seq(0.5,length(cols)+0.5,1))
        }
        if(plot.land){
        maps::map("world", xlim = xlimi,
                  ylim = ylimi,
                  fill = TRUE, plot = TRUE, add = TRUE,
                  col = grey(0.95), border = grey(0.8))
        }
        if(plot.obs){
            dat <- subset(fit$data, Year == year[i] & N > 0)
            points(dat$lon, dat$lat, pch = 1,
                   col = adjustcolor(1, 1)) ##, cex = dat$N)  ## HERE: cex too high
        }
        if(is.null(title)){
            mtext(year[i], 3, 0.3, font = 2, cex = 0.8)
        }else{
            mtext(title, 3, 0.3, font = 2, cex = 0.8)
        }
        if ((legend && fixed.scale && i == ny) || (legend && average)){
            maxcuts = aggregate(prediAll ~ zFac, FUN=max)
            mincuts = aggregate(prediAll ~ zFac, FUN=min)
            mm = mean(prediAll)
            ml = signif(mincuts[,2]/mm,3)
            ml[1] = 0
            leg = paste0("[",ml,",",signif(maxcuts[,2]/mm,3),"]")
            legend("bottomright", legend = leg, pch = 16, col = cols, bg = "white", ncol = 1, cex = 0.6) ## HERE:
        }else if (legend && !fixed.scale){
            prediAll <- pred[[i]][,1]
            if(!is.null(prediAll)){
            zFac <- grid[[i]]$pred
            maxcuts = aggregate(prediAll ~ zFac, FUN=max)
            mincuts = aggregate(prediAll ~ zFac, FUN=min)
            mm = mean(prediAll)
            ml = signif(mincuts[,2]/mm,3)
            ml[1] = 0
            leg = paste0("[",ml,",",signif(maxcuts[,2]/mm,3),"]")
            legend("bottomright", legend = leg, pch = 16, col = cols, bg = "white", ncol = 1, cex = 0.6) ## HERE:
            }
        }
        box(lwd = 1.5)
    }
    if(is.null(xlab)){
        mtext("Latitude", 2, 3, outer = TRUE)
    }else{
        mtext(xlab, 2, 3, outer = TRUE)
    }
    if(is.null(ylab)){
        mtext("Longitude", 1, 3, outer = TRUE)
    }else{
        mtext(ylab, 1, 3, outer = TRUE)
    }


    ## OLD: pre-wkfishdis II
    if(FALSE){
        cex <- 1

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
}

#' @name plotfishdish.dist.cv
#'
#' @title plot cv of fit
#'
#' @param fit fit
#' @param mod Select one of the models. Default: NULL
#'
#' @importFrom maps map
#'
#' @return Nothing
#'
#' @export
plotfishdish.dist.cv <- function(fit, mod = NULL, year = NULL,
                                 grid.all = NULL,
                                 xlim = NULL, ylim = NULL,
                                 legend = TRUE, fixed.scale = TRUE,
                                 title = NULL, xlab = NULL, ylab = NULL,
                                 xaxt = NULL, yaxt = NULL,
                                 fixed.lims = TRUE,
                                 cols = cm.colors(7)[-1], breaks = NULL,
                                 min.val = NA,
                                 cut.cv = NULL, asp = 2,
                                 plot.land = TRUE,
                                 plot.obs = 2,   ## 0 = no hauls, 1 = all hauls, 2 = all pos hauls
                                 average = FALSE,
                                 mfrow = NULL
                                 ){
    xaxt0 <- xaxt
    yaxt0 <- yaxt

    ## TODO: not ideal fit$grid might be a list!
    if(is.null(grid.all)){
        grid.all <- fit$grid
    }

    if(is.null(year)){
        year <- tail(rownames(fit$fits[[1]]$idx),1)
        writeLines(paste0("No year selected. Plotting year: ",year))
    }
    if(is.null(mod)){
        mod <- 1
    }

    if(year[1] == "all"){
        year <- names(fit$fits[[mod]]$gPreds2[[1]])
    }

    names(fit$fits[[mod]]$gPreds2[[1]])
    ny <- length(year)
    pred <- fit$fits[[mod]]$gPreds2.CV[[1]][as.character(year)]
    names(fit$fits[[mod]]$gPreds2.CV[[1]])
    if(inherits(fit$grid, "list")){
        grid <- fit$grid[as.character(year)]
    }else{
        grid <- lapply(1:ny, function(x) fit$grid)
    }

    if(ny > 1 && !average && is.null(mfrow)){
        mfrow <- n2mfrow(ny, asp = asp)
        par(mfrow = mfrow, mar = c(0,0,2,0), oma = c(5,5,2,1))
    }else if(!is.null(mfrow)){
        par(mfrow = mfrow, mar = c(0,0,2,0), oma = c(5,5,2,1))
    }


    if(!is.null(mfrow)){
        xaxt.ind <- (prod(mfrow) - mfrow[2] + 1):prod(mfrow)
        yaxt.ind <- seq(1, prod(mfrow), mfrow[2])
    }else{
        xaxt.ind <- yaxt.ind <- 1
    }


    nyx <- ifelse(average, 1, ny)
    if(average){
        grid <- list(collapse.grid(grid))
        if(sd(sapply(pred, nrow)) > 0.1){
            stop("Did you use the same prediction grid for the years that you want to combine?")
        }
        tmp <- do.call(cbind, pred)
        pred <- list(data.frame(apply(tmp, 1, mean, na.rm = TRUE)))
    }

    if(fixed.scale){
        lastmax <- NULL
        predi <- unlist(lapply(pred, function(x) x[,1]))
        ind <- vector("list",length(pred))
        for(i in 1:length(pred)){
            if(i == 1){
                starti <- 1
            }else{
                starti <- (lastmax + 1) ## max(ind[[i-1]])
            }
            if(!is.null(pred[[i]])){
                ind[[i]] <- starti:(starti + nrow(pred[[i]])-1)
                lastmax <- max(ind[[i]])
            }else{
                ind[[i]] <- NULL
            }
        }
        concT <- surveyIndex:::concTransform(log(predi))
        if(is.null(min.val) || is.na(min.val)){
            zFac <- cut(concT, 0:length(cols)/length(cols))
        }else{
            zFac <- cut(concT, sort(c(min.val,seq(0,1, length.out = length(cols)-1))))
        }
        if(is.null(breaks)){
            zFac <- cut(concT, 0:length(cols)/length(cols))
        }else{
            zFac <- cut(predi, breaks)
        }
        for(i in 1:length(grid)){
            if(!is.null(ind[[i]])){
                grid[[i]]$pred <- as.numeric(zFac[ind[[i]]])
            }else{
                grid[[i]]$pred <- NA
            }
        }
    }

    if(fixed.lims){
        if(is.null(xlim)) xlim <- extendrange(r = range(unlist(lapply(grid, function(x) range(if(!is.null(x$lon)) x$lon else NA))),na.rm = TRUE), f = 0.1)
        if(is.null(ylim)) ylim <- extendrange(r = range(unlist(lapply(grid, function(x) range(if(!is.null(x$lat)) x$lat else NA))),na.rm = TRUE), f = 0.1)
    }

    if(plot.obs == 2){
        obs <- fit$data[which(fit$data$bio > 0),]
    }else{
        obs <- fit$data
    }
    for(i in 1:nyx){
        if(!fixed.scale){
            predi <- pred[[i]][,1]
            if(!is.null(predi)){
                concT <- surveyIndex:::concTransform(log(predi))
                zFac <- cut(concT, 0:length(cols)/length(cols))
                if(is.null(breaks)){
                    zFac <- cut(concT, 0:length(cols)/length(cols))
                }else{
                    zFac <- cut(predi, breaks)
                }
                grid[[i]]$pred <- as.numeric(zFac)
            }else{
                grid[[i]]$pred <- NA
            }
        }
        tmp <- reshape2::acast(grid[[i]], lon~lat, value.var = "pred")
        ## if(is.null(xlim)) xlimi <- range(as.numeric(rownames(tmp))) else xlimi <- xlim
        ## if(is.null(ylim)) ylimi <- range(as.numeric(rownames(tmp))) else ylimi <- ylim
        if(is.null(xlim)) xlimi <- extendrange(r = range(as.numeric(rownames(tmp))), f = 0.1) else xlimi <- xlim
        if(is.null(ylim)) ylimi <- extendrange(r = range(as.numeric(colnames(tmp))), f = 0.1) else ylimi <- ylim
        if(is.null(xaxt0)) xaxt <- ifelse(i %in% xaxt.ind, "s", "n")
        if(is.null(yaxt0)) yaxt <- ifelse(i %in% yaxt.ind, "s", "n")
        plot(1,1, xlim = xlimi, ylim = ylimi,
             xaxt = xaxt, yaxt = yaxt,
             ty = "n",
             xlab = "", ylab = "")
        image(as.numeric(rownames(tmp)), as.numeric(colnames(tmp)), tmp,
              add = TRUE,
              xlab = "", ylab = "", col = cols, breaks = seq(0.5,length(cols)+0.5,1))
        maps::map("world", xlim = xlimi,
                  ylim = ylimi,
                  fill = TRUE, plot = TRUE, add = TRUE,
                  col = grey(0.95), border = grey(0.8))
        ind <- which(as.character(obs$Year) == year[i])
        if(plot.obs %in% c(1,2)) points(obs$lon[ind], obs$lat[ind], col = rgb(t(col2rgb("black"))/255,alpha = 1),
                            cex = 0.3, pch = 16)
        mtext(year[i], 3, 0.3, font = 2, cex = 0.8)
        if (is.null(breaks) && ((legend && fixed.scale && i == ny) || legend && !fixed.scale)){
            maxcuts = aggregate(predi ~ zFac, FUN=max)
            mincuts = aggregate(predi ~ zFac, FUN=min)
            mm = mean(predi)
            ml = signif(mincuts[,2]/mm,3)
            ml[1] = 0
            leg = paste0("[",ml,",",signif(maxcuts[,2]/mm,3),"]")
            legend("bottomright", legend = leg, pch = 16, col = cols, bg = "white")
        }
        if (!is.null(breaks) && ((legend && fixed.scale && i == ny) || legend && !fixed.scale)){
            legend("bottomright", legend = levels(zFac), pch = 16, col = cols, bg = "white")
        }
        box(lwd = 1.5)
    }

    mtext("Longitude", 1, 3, outer = TRUE)
    mtext("Latitude", 2, 3, outer = TRUE)
}



#' @name plotfishdish.dist.year
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
plotfishdish.dist.year <- function(fit, mod = NULL, var.lim = FALSE, all.years = TRUE,
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
        print(ny1)
        if(ny1 >= 30){
            mfrow = c(5,ceiling(ny1/5))
        }else if(ny1 >= 19){
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
                ## Account for missing year)
                if(!is.null(fitx$gPreds2[[a]][[cc]])){
                    ally = rbind(ally, data.frame(val = fitx$gPreds2[[a]][[cc]],
                                                  year = as.character(levels(as.factor(data$Year))[cc])))
                }else{
                }
            }
            ally$conc = surveyIndex:::concTransform(log(ally$val))
            ally$zFac = cut(ally$conc, 0:length(colors)/length(colors))

            yy = 2015
            for (yy in year) {
                yy
                length(tmp)
                names(tmpx)
                browser()
                if(inherits(tmp, "list")){
                    tmpx <- tmp[[as.character(yy)]]
                }else tmpx <- tmp
                plot(tmpx$lon, y = tmpx$lat, col = 1, pch = 1,
                     ty = "n",
                     xlim = xlims, ylim = ylims,
                     cex = map.cex, xlab = "", ylab = "",
                     axes = FALSE)


                cols = rev(heat.colors(8))
                cols
                tmpx

                image(as.numeric(rownames(tmpx)), as.numeric(colnames(tmpx)), tmpx,
                      add = TRUE,
                      xlab = "", ylab = "", col = cols,
                      breaks = seq(0.5,length(cols)+0.5,1))

                ## OLD:
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



#' @name plotfishdish.gam.effects
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
plotfishdish.gam.effects <- function(fit, mod = 1, xlim = NULL, ylim = NULL,
                                     add = FALSE){

    cols <- c(RColorBrewer::brewer.pal(n = 8, "Dark2"),
              RColorBrewer::brewer.pal(n = 8, "Accent"))

    plotInfo <- mgcv::plot.gam(fit$fits[[mod]]$pModels[[1]], select = 0, residuals = TRUE)

    gamTerms <- unlist(lapply(plotInfo, function(x) ifelse(!is.null(x$xlab), x$xlab, NA)))

    if(!add) par(mfrow = c(1,1))  ## HERE:

    ## ind <- which(gamTerms == "lon")
    ## if(length(ind) > 0){
    ##     tmp <- plotInfo[[ind]]
    ##     tmp$lo <- tmp$fit - 1.95 * tmp$se
    ##     tmp$up <- tmp$fit + 1.95 * tmp$se
    ##     fitmat <- matrix(tmp$fit, nrow = length(tmp$x), ncol = length(tmp$y))
    ##     if(is.null(xlim)) xlim <- range(tmp$x)
    ##     if(is.null(ylim)) ylim <- range(tmp$y)
    ##     image(tmp$x, tmp$y, fitmat,
    ##           xlab = tmp$xlab, ylab = tmp$ylab,
    ##           xlim = xlim, ylim = ylim)
    ##     contour(tmp$x, tmp$y, fitmat, add = TRUE)
    ##     maps::map('world',xlim=xlim,ylim=ylim,
    ##               fill=TRUE,plot=TRUE,add=TRUE,
    ##               col=rgb(t(col2rgb("grey70"))/255, alpha = 0.3),
    ##               border=rgb(t(col2rgb("white"))/255, alpha = 0.3))
    ##     ## points(tmp$raw$x, tmp$raw$y, pch = 16, cex = 0.1,
    ##     ##        col = rgb(t(col2rgb("grey80"))/255, alpha = 0.6))
    ##     box(lwd=1.5)
    ## }

    ## ind <- which(gamTerms == "ctime")
    ## if(length(ind) > 0){
    ##     tmp <- plotInfo[[ind]]
    ##     tmp$lo <- tmp$fit - 1.95 * tmp$se
    ##     tmp$up <- tmp$fit + 1.95 * tmp$se
    ##     plot(tmp$x, tmp$fit, ty = "n",
    ##          ylim = range(tmp$fit, tmp$lo, tmp$up), ## tmp$p.resid
    ##          xlab = tmp$xlab, ylab = tmp$ylab)
    ##     polygon(c(tmp$x, rev(tmp$x)), c(tmp$lo, rev(tmp$up)),
    ##             border = NA, col = rgb(t(col2rgb(cols[1]))/255, alpha = 0.3))
    ##     ## points(tmp$raw, tmp$p.resid, pch = 16, cex = 0.1,
    ##     ##        col = rgb(t(col2rgb("grey80"))/255, alpha = 0.6))
    ##     lines(tmp$x, tmp$fit, col = cols[1], lwd = 2)
    ##     rug(tmp$raw)
    ##     box(lwd=1.5)
    ## }

    ind <- which(gamTerms == "Depth" | gamTerms == "sqrt(Depth)")
    if(length(ind) > 0){
        tmp <- plotInfo[[ind]]
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


#' @name plotfishdish.gam.effects.gear
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
plotfishdish.gam.effects.gear <- function(fit, mod = 1, xlim = NULL, ylim = NULL,
                                       CI = 0.95, var = "Gear", exp = TRUE){

    cols <- rep(c(RColorBrewer::brewer.pal(n = 8, "Dark2"),
                  RColorBrewer::brewer.pal(n = 8, "Accent")),50)

    gear.effs <- get.gear.effect(fit, mod = mod, CI = CI, var = var, exp = exp)
    vals <- gear.effs[,"est"]
    ll <- gear.effs[,"ll"]
    ul <- gear.effs[,"ul"]
    sds <- gear.effs[,"sd"]
    labs <- rownames(gear.effs)
    if(var == "ShipG"){
        ships <- sapply(strsplit(labs,":"),"[[",1)
        gears <- sapply(strsplit(labs,":"),"[[",2)
        ## get original gear (not category)
        tmp <- as.data.frame(cbind(country = as.character(fit$data$Country),
                                   ship = sapply(strsplit(fit$data$haul.id, ":"), "[[", 5),
                                   gearcat = as.character(fit$data$GearCat),
                                   gear = as.character(fit$data$GearOri)))
        tmp <- tmp[!duplicated(tmp),]
        ## group by gears
        gears.ori <- countries <- rep(NA, length(ships))
        all.info <- vector("list", length(ships))
        for(i in 1:length(ships)){
            ind <- which(tmp$ship == ships[i] & tmp$gearcat == gears[i])
            all.info[[i]]  <- tmp[ind,]
            gears.ori[i] <- paste(unique(tmp$gear[ind]), collapse = "&")
            countries[i] <- paste(unique(tmp$country[ind]), collapse = "&")
        }
        print(all.info[vals > 2])
        ordi <- order(factor(gears))
        gears <- gears[ordi]
        ships <- ships[ordi]
        gears.ori <- gears.ori[ordi]
        labs <- paste0(ships," - ",gears.ori)
        ## paste0(countries,":",gears.ori, ":", ships)
        vals <- vals[ordi]
        sds <- sds[ordi]
        ll <- ll[ordi]
        ul <- ul[ordi]
        cols <- cols[factor(gears)]
    }else{
        cols <- cols
    }


    if(is.null(ylim)){
        ylim <- extendrange(r = range(vals, ll, ul, na.rm = TRUE), f = 0.1)
        ## if(ylim[1] < 0) ylim <- c(1.05,1.05) * ylim else ylim <- c(0.95,1.05) * ylim
    }
    plot(1:length(vals), vals, ty = "n",
         xlim = extendrange(r = c(1,length(vals)), f = 0.02),
         xlab = "", ylab = "",
         xaxt="n",ylim=ylim)
    if(exp) abline(h=1, lty=2, lwd = 1.5, col="grey50") else abline(h=0, lty=2, lwd = 1.5, col="grey50")
    for(i in 1:length(vals)){
        arrows(i, ll[i], i, ul[i], col = cols[i], angle=90, code=3, lengt = 0.1)
    }
    points(1:length(vals), vals, pch = 16,
           cex = 1.5,
           col = cols[1:length(vals)])
    axis(1, at = 1:length(vals), labels = labs, las = 2)
##    mtext(var, 3, 0.5, font = 2)
    if(exp) ylab <- "Effect" else ylab <- "Effect (log)"
    mtext(ylab, 2, 3)
    ##    if(var == "ShipG") legend("topright", legend = unique(gears), col = unique(cols), pch = 16, ncol = 2)
    box(lwd=1.5)

    ## This only works for fixed gear effect (not random)
    ## zscore <- qnorm(CI + (1 - CI)/2)
    ## fe <- data.frame(summary(fit$fits[[1]]$pModels[[1]])$p.table)[,c(1,2)]
    ## colnames(fe) =  c('value', 'se')
    ## fe[1,] <- c(0,0)
    ## fe$lo <- fe$value - fe$se
    ## fe$up <- fe$value + fe$se
    ## fe <- exp(fe)

    ## ## par(mfrow = c(1,1), mar = c(5,5,4,2))
    ## labs <- sapply(strsplit(rownames(fe)[-1],"Gear"),"[[",2)
    ## all.gears <- unique(fit$data$Gear)
    ## labs <- c(as.character(all.gears[!all.gears %in% labs]), labs)
    ## ylim <- range(0,fe)
    ## if(ylim[1] < 0) ylim <- c(1.1,1.1) * ylim else ylim <- c(0.9,1.1) * ylim
    ## bp <- barplot(fe$value, ylim = ylim, col = cols[1:nrow(fe)])
    ## arrows(bp,fe$lo, bp, fe$up , angle=90, code=3, lengt = 0.1)
    ## axis(1, at = bp, labels = labs, las = 2)
    ## mtext("Gears", 3, 0.5, font = 2)
    ## mtext("Effect", 2, 3)

}



#' @name plotfishdish.diag
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
plotfishdish.diag <- function(fit, mod = 1){
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
        abline(h=0, lty=1, col = "grey30")
    surveyIndex::surveyIdxPlots(fit$fits[[mod]],
                                fit$data,
                                select = "resVsShip",
                                par = NULL,
                                main = "Residuals vs. Ship")
        abline(h=0, lty=1, col = "grey30")
    surveyIndex::surveyIdxPlots(fit$fits[[mod]],
                                fit$data,
                                select = "resVsGear",
                                par = NULL,
                                main = "Residuals vs. Gear")
        abline(h=0, lty=1, col = "grey30")
    surveyIndex::surveyIdxPlots(fit$fits[[mod]],
                                fit$data,
                                select = "resVsDepth",
                                par = NULL,
                                main = "Residuals vs. Depth")
    abline(h=0, lty=1, col = "grey70")
}


#' @name plotfishdish.diag.spatial
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
plotfishdish.diag.spatial <- function(fit, mod = 1, year = NULL){
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
}




#' @name plotfishdish.dist.year2
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
plotfishdish.dist.year2 <- function(fit, mod = NULL, var.lim = FALSE, all.years = TRUE,
                                    sandeel_areas = NULL, tobisbanker_wgs84 = NULL,
                                    fixed.scale = TRUE,
                                    cols = rev(heat.colors(8)),
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
        print(ny1)
        if(ny1 >= 30){
            mfrow = c(5,ceiling(ny1/5))
        }else if(ny1 >= 19){
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

        ## browser()
        ## j = 1


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

    pred <- fit$fits[[mod]]$gPreds2[[1]][as.character(year)]
    cv <- fit$fits[[mod]]$gPreds2.CV[[1]][as.character(year)]
    grid <- fit$grid[as.character(year)]

            yy = 2015
            for (yy in year) {
                if(inherits(tmp, "list")){
                    tmpx <- tmp[[as.character(yy)]]
                }else tmpx <- tmp
                plot(tmpx$lon, y = tmpx$lat, col = 1, pch = 1,
                     ty = "n",
                     xlim = xlims, ylim = ylims,
                     cex = map.cex, xlab = "", ylab = "",
                     axes = FALSE)


                cols = rev(heat.colors(8))
                cols
                tmpx

                image(as.numeric(rownames(tmpx)), as.numeric(colnames(tmpx)), tmpx,
                      add = TRUE,
                      xlab = "", ylab = "", col = cols,
                      breaks = seq(0.5,length(cols)+0.5,1))

                ## OLD:
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



#' @name plotfishdish.overlapp
#'
#' @title plot overlapp
#'
#' @param x data
#'
#' @importFrom maps map
#'
#' @return Nothing
#'
#' @export
plotfishdish.overlap <- function(x, var = "Gear",
                                  xlim = NULL, ylim = NULL,
                                 combine = TRUE, cex = 2,
                                 cols = NULL,
                                 legend = TRUE){

    if(is.null(cols)){
        cols <- c("dodgerblue3","darkorange","forestgreen",
                  "purple","firebrick2","salmon3","chartreuse2","brown",
                  RColorBrewer::brewer.pal(n = 8, "Dark2"),
                  RColorBrewer::brewer.pal(n = 8, "Set1")
                  )
    }
    if(is.null(names(cols))){
        cols <- cols[1:length(unique(x[,var]))]
        names(cols) <- unique(x[,var])
    }

    if(is.null(xlim)) xlim <- range(x$lon)
    if(is.null(ylim)) ylim <- range(x$lat)

    if(combine){
        plot(1, 1, ty = "n",
             xlim = xlim,
             ylim = ylim,
             xlab = "", ylab = "")
        maps::map("world", xlim = xlim,
                  ylim = ylim,
                  fill = TRUE, plot = TRUE, add = TRUE,
                  col = grey(0.95), border = grey(0.8))
        ## hauls by var
        uni <- unique(x[,var])
        n <- length(uni)
        for(i in 1:n){
            xi <- x[x[,var] == uni[i],c("lat","lon","StatRec")]
            tmp <- merge(xi, ices.rectangles[,c("ICESNAME","sub_x","sub_y")],
                         by.x = "StatRec", by.y = "ICESNAME", all.x = TRUE)
            tmp2 <- tmp[!duplicated(tmp$StatRec),]
            points(tmp2$lon, tmp2$lat,
                   col = adjustcolor(cols[names(cols) == uni[i]], 0.4),
                   cex = cex, pch = 16)
        }
        if(legend){
            legend("bottomright", legend = names(cols)[names(cols) %in% uni],
                   col = adjustcolor(cols[names(cols) %in% uni], 0.4),
                   pch = 16, box.lwd = 1.5, bg = "white")
        }
        box(lwd = 1.5)
        mtext("Latitude", 2, 2)
        mtext("Longitude", 1, 2)
    }else{
        ## hauls by var
        uni <- unique(x[,var])
        n <- length(uni)
        par(mfrow = n2mfrow(n), mar = c(2,2,1,1), oma = c(3,3,1,1))
        for(i in 1:n){
            plot(1, 1, ty = "n",
                 xlim = xlim,
                 ylim = ylim,
                 xlab = "", ylab = "")
            maps::map("world", xlim = xlim,
                      ylim = ylim,
                      fill = TRUE, plot = TRUE, add = TRUE,
                      col = grey(0.95), border = grey(0.8))
            xi <- x[x[,var] == uni[i],c("lat","lon","StatRec")]
            tmp <- merge(xi, ices.rectangles[,c("ICESNAME","sub_x","sub_y")],
                         by.x = "StatRec", by.y = "ICESNAME", all.x = TRUE)
            tmp2 <- tmp[!duplicated(tmp$StatRec),]
            points(tmp2$lon, tmp2$lat,
                   col = adjustcolor(cols[i], 0.4),
                   cex = cex, pch = 16)
            legend("topleft", legend = uni[i],
                   pch = NA,
                   x.intersp = 0.6,
                   bg = "white")
            box(lwd = 1.5)
        }
        mtext("Latitude", 2, 1.4, outer = TRUE)
        mtext("Longitude", 1, 1.4, outer = TRUE)
    }
}
