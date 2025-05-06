


## likely errors because of https
## readDatras.fd <- function (url) {
##     tmp <- tempfile()
##     on.exit(unlink(tmp))
##     ret <- if (icesDatras:::os.type("windows")) {
##     } else if (icesDatras:::os.type("unix") & Sys.which("wget") != "") {
##         download.file(url, destfile = tmp, quiet = TRUE, method = "wget")
##     } else if (icesDatras:::os.type("unix") & Sys.which("curl") != "") {
##         download.file(url, destfile = tmp, quiet = TRUE, method = "curl")
##     } else {
##         127
##     }
##     if (ret == 0) {
##         scan(tmp, what = "", sep = "\n", quiet = TRUE)
##     } else {
##         message("Unable to download file so using slower method url().\n",
##             "Try setting an appropriate value via\n\t", "options(download.file.method = ...)\n",
##             "see ?download.file for more information.")
##         con <- url(url)
##         on.exit(close(con))
##         scan(con, what = "", sep = "\n", quiet = TRUE)
##     }
## }


## better for https

#' @name readDatras.fd
#'
#' @title custom readDatras function
#'
#' @importFrom httr GET http_error status_code content
#'
readDatras.fd <- function(url) {
    res <- httr::GET(url)
    if (httr::http_error(res)) {
        stop("HTTP error: ", httr::status_code(res))
    }
    ret <- httr::content(res, as = "text", encoding = "UTF-8")
    strsplit(ret, "\n")[[1]]
}


#' @name getDATRAS.fd
#'
#' @title custom getDATRAS function
#'
#' @importFrom icesDatras checkSurveyOK getSurveyYearList getSurveyYearQuarterList
#'
getDATRAS.fd <- function (record = "HH", survey, years, quarters) {
    if (!record %in% c("HH", "HL", "CA")) {
        message("Please specify record type:", "\n\t\tHH (haul data)",
            "\n\t\tHL (length-based data)", "\n\t\tCA (age-based data)")
        return(FALSE)
    }
    if (!icesDatras:::checkSurveyOK(survey))
        return(FALSE)
    available_years <- icesDatras:::getSurveyYearList(survey)
    available_years_req <- intersect(years, available_years)
    if (length(available_years_req) == 0) {
        message("Supplied years (", paste(years, collapse = ", "),
            ") are not available.\n  Available options are:\n",
            paste(capture.output(print(available_years)), collapse = "\n"))
        return(FALSE)
    }
    else if (length(available_years_req) < length(years)) {
        message("Some supplied years (", paste(setdiff(years,
            available_years), collapse = ", "), ") are not available.")
    }
    mat <- sapply(as.character(available_years_req), function(y) icesDatras:::getSurveyYearQuarterList(survey,
        as.integer(y)), simplify = FALSE)
    mat <- sapply(mat, function(x) as.integer(1:4 %in% x))
    row.names(mat) <- 1:4
    if (sum(mat[quarters, ]) == 0) {
        message("Supplied quarters (", paste(quarters, collapse = ", "),
            ") are not available.\n  Available options are:\n",
            paste(capture.output(print(mat)), collapse = "\n"))
        return(FALSE)
    }
    else if (sum(mat[quarters, ] == 0) > 0) {
        message("Some supplied quarter and year combinations are not available.")
    }
    amat <- mat[quarters, , drop = FALSE]
    qvec <- quarters[row(amat)[amat == 1]]
    yvec <- available_years_req[col(amat)[amat == 1]]
    message("Data being extracted for:\n", paste(capture.output(print(cbind.data.frame(survey = survey,
        year = yvec, quarter = qvec))), collapse = "\n"))
    url <- sprintf("https://datras.ices.dk/WebServices/DATRASWebService.asmx/get%sdata?survey=%s&year=%i&quarter=%i", record, survey, yvec, qvec)
    out <- lapply(url, function(x) {
        x <- readDatras.fd(x)
        icesDatras:::parseDatras(x)
    })
    out <- do.call(rbind, out)
    out
}
