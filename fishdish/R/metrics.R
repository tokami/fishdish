


#' @name est.cog
#'
#' @title Estimate abudance-weighted centre of gravity
#'
#' @param fit fitted object
#' @param grid prediction grid
#' @param mod Model (Default: 1)
#' @param cols ages, if multiple ages were modelled. Default: 1
#'
#' @return data frame with cog in lat and lon for each year
#'
#' @export
est.cog <- function(fit, grid, mod = 1, cols = 1){
    gpred <- fit$fits[[mod]]$gPreds2[[cols]]
    years <- attributes(gpred)$names
    pred.grid <- grid[years]
    cog.lon <- sapply(1:length(years), function(x) (t(pred.grid[[x]]$lon) %*% gpred[[x]])/sum(gpred[[x]]))
    cog.lat <- sapply(1:length(years), function(x) (t(pred.grid[[x]]$lat) %*% gpred[[x]])/sum(gpred[[x]]))
    res <- as.data.frame(cbind(year = as.numeric(years), cog.lon = cog.lon, cog.lat = cog.lat))
    return(res)
}



#' @name est.abun.by.area
#'
#' @title Estimate abudance by ICES Areas
#'
#' @param fit fitted object
#' @param grid prediction grid
#' @param mod Model (Default: 1)
#' @param cols ages, if multiple ages were modelled. Default: 1
#' @param by area Default: "Area_27"
#'
#' @return data frame with cog in lat and lon for each year
#'
#' @export
est.abun.by.area <- function(fit, grid, mod = 1, cols = 1, by = "Area_27"){
    gpred <- fit$fits[[mod]]$gPreds2[[cols]]
    years <- attributes(gpred)$names
    pred.grid <- grid[years]
    ## CHECK:
    ## if(!any(colnames(pred.grid[[1]]) == "Area_27")) stop("No ICES subarea (Area_27) found in grid!")
    tmp <- do.call(rbind, lapply(years, function(x){cbind(year = x, aggregate(list(idx=gpred[[x]]),
                                                                              by = list(area=pred.grid[[x]][,by]),
                                                                              sum))}))
    res <- as.data.frame(reshape2::dcast(tmp, year ~ area, value.var = "idx"))
    return(res)
}


#' @name est.var.by.area
#'
#' @title Estimate abudance by ICES Areas
#'
#' @param fit fitted object
#' @param grid prediction grid
#' @param mod Model (Default: 1)
#' @param cols ages, if multiple ages were modelled. Default: 1
#' @param by area Default: "Area_27"
#'
#' @return data frame with cog in lat and lon for each year
#'
#' @export
est.var.by.area <- function(fit, grid, mod = 1, cols = 1, by = "Area_27"){
    gpred <- fit$fits[[mod]]$gPreds2[[cols]]
    years <- attributes(gpred)$names
    pred.grid <- grid[years]
    ## CHECK:
    ## if(!any(colnames(pred.grid[[1]]) == "Area_27")) stop("No ICES subarea (Area_27) found in grid!")
    tmp <- do.call(rbind, lapply(years, function(x){cbind(year = x, aggregate(list(idx=gpred[[x]]),
                                                                              by = list(area=pred.grid[[x]][,by]),
                                                                              var))}))
    res <- as.data.frame(reshape2::dcast(tmp, year ~ area, value.var = "idx"))
    return(res)
}
