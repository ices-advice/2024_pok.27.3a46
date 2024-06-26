#-*- coding: utf-8 -*-

### File: cdd_survey_scripts.R
### Time-stamp: <2024-04-25 12:52:21 a23579>
###
### Created: 2016??
### Author: Anders Nielsen, adapted by Jennifer Devine, Yves Reecht
###
####################################################################################################
### Description:
###
### Survey plot functions
####################################################################################################

## Script to validate data to match the state-space assessment model
##
## Anders Nielsen <anders@nielsensweb.org> Jul. 2012, ...
## library(stockassessment)

formals(readLines)$warn <- FALSE

## read.table.nowarn <- function(...){
## tryCatch.W.E <- function(expr)
## {
## W <- NULL
## w.handler <- function(w){ # warning handler
## if(!grepl('incomplete final line', w))W <<- w
## invokeRestart("muffleWarning")
## }
## list(value = withCallingHandlers(tryCatch(expr, error = function(e) e), warning = w.handler), warning = W)
## }
## lis <- tryCatch.W.E(read.table(...))
## if(!is.null(lis$warning))warning(lis$warning)
## lis$value
## }

## is.whole.positive.number <- function(x, tol = .Machine$double.eps^0.5){
## (abs(x - round(x)) < tol) & (x >= 0)
## }



test.filenames <- function(path,
                           filelist = c('cn.dat', 'survey.dat',
                                        'nm.dat', 'mo.dat',
                                        'sw.dat', 'cw.dat', 'lw.dat', 'dw.dat',
                                        'lf.dat', 'pf.dat', 'pm.dat')
                           ){
    ## Function to test if specified filenames exist
    owd <- getwd()
    setwd(path)
    ret <- file.exists(filelist)
    setwd(owd)
    return(data.frame(file = filelist, exists = ret))
}

check.one <- function(filen){
    dummy <- read.ices(filen)
}

check.all <- function(path,
                      filen = c('cn.dat', 'cw.dat', 'lw.dat', 'dw.dat', 'lf.dat',
                                'survey.dat', 'nm.dat', 'mo.dat',
                                'sw.dat', 'pf.dat', 'pm.dat')
                      ){

    owd <- getwd()
    setwd(path)
    out <- test.filenames('.', filen)

    ret <- list()
    self <- rep(NA, length(filen))
    message <- rep("", length(filen))
    wrap <- function(expr){
        tryCatch(expr, error = function(e){E <<- e})
    }
    for(i in 1:length(filen)){
        if(out[i, 2]){
            fil <- filen[i]
            E <- NULL
            dummy <- wrap(read.ices(fil))
            if(is.null(E)){
                self[i] <- TRUE
            }else{
                self[i] <- FALSE
                message[i] <- E$message
            }
        }
    }

    out$self <- self

    cross <- rep(NA, length(filen))
    getAgeRange <- function(x)range(as.numeric(colnames(x)))
    getYearRange <- function(x)range(as.numeric(rownames(x)))
    ageRangeOK <- function(x, compare){
        message <- ""
        ran <- getAgeRange(x)
        ret <- NA
        if(identical(ran, compare)){
            ret <- TRUE
        }else{
            ret <- FALSE
            message <- "Age range mismatch"
        }
        return(list(ret, message))
    }
    yearRangeOK <- function(x, compare){
        message <- ""
        ran <- getYearRange(x)
        ret <- NA
        if(identical(ran, compare)){
            ret <- TRUE
        }else{
            ret <- FALSE
            message <- "year range mismatch"
        }
        return(list(ret, message))
    }

    check.local <- function(fil, ar, yr, ageAlso = c('cn.dat'), yearAlso = c('cn.dat')){
        test <- read.ices(fil)
        retA <- ageRangeOK(test, ar)
        retY <- yearRangeOK(test, yr)
        idx <- which(out[, 1] == fil)
        if(retA[[1]] && retY[[1]]){
            cross[idx] <<- TRUE
        }else{
            cross[idx] <<- FALSE
            message[idx] <<- paste("In file", fil, ":", retA[[2]], retY[[2]])
            if(!retA[[1]])cross[out[, 1]%in%ageAlso] <<- FALSE
            if(!retY[[1]])cross[out[, 1]%in%yearAlso] <<- FALSE
        }
    }

    if(!any(is.na(self)) & all(self)){
        cross[out[, 1] == 'cn.dat'] <- TRUE
        cross[out[, 1] == 'survey.dat'] <- TRUE

        catch.no <- read.ices('cn.dat')
        ageRangeCatch <- getAgeRange(catch.no)
        yearRangeCatch <- getYearRange(catch.no)
        surveys <- read.ices('survey.dat')
        ageRangeSur <- do.call(range, lapply(surveys, getAgeRange))
        yearRangeSur <- do.call(range, lapply(surveys, getYearRange))
        ageRangeTotal <- range(c(ageRangeCatch, ageRangeSur))
        yearRangeTotal <- range(c(yearRangeCatch, yearRangeSur))
        check.local('cw.dat', ageRangeTotal, yearRangeCatch, ageAlso = c('survey.dat', 'cn.dat'), yearAlso = c('cn.dat'))
        check.local('dw.dat', ageRangeTotal, yearRangeCatch, ageAlso = c('survey.dat', 'cn.dat'), yearAlso = c('cn.dat'))
        check.local('lw.dat', ageRangeTotal, yearRangeCatch, ageAlso = c('survey.dat', 'cn.dat'), yearAlso = c('cn.dat'))
        check.local('lf.dat', ageRangeTotal, yearRangeCatch, ageAlso = c('survey.dat', 'cn.dat'), yearAlso = c('cn.dat'))
        check.local('pf.dat', ageRangeTotal, yearRangeTotal, ageAlso = c('survey.dat', 'cn.dat'), yearAlso = c('survey.dat', 'cn.dat'))
        check.local('pm.dat', ageRangeTotal, yearRangeTotal, ageAlso = c('survey.dat', 'cn.dat'), yearAlso = c('survey.dat', 'cn.dat'))
        check.local('sw.dat', ageRangeTotal, yearRangeTotal, ageAlso = c('survey.dat', 'cn.dat'), yearAlso = c('survey.dat', 'cn.dat'))
        check.local('mo.dat', ageRangeTotal, yearRangeTotal, ageAlso = c('survey.dat', 'cn.dat'), yearAlso = c('survey.dat', 'cn.dat'))
        check.local('nm.dat', ageRangeTotal, yearRangeTotal, ageAlso = c('survey.dat', 'cn.dat'), yearAlso = c('survey.dat', 'cn.dat'))
    }

    out$cross <- cross
    out$message <- message

    setwd(owd)
    return(out)
}


gen.default <- function(x, outfile){
    if(!file.exists('cn.dat')){
        stop("File cn.dat does not exsist")
    }
    if(!file.exists('survey.dat')){
        stop("File survey.dat does not exsist")
    }
    s <- read.surveys('survey.dat')
    cn <- read.ices('cn.dat')
    ## calculate overall age range
    ager <- range(as.vector(unlist(lapply(s, function(m)as.numeric(colnames(m))))), as.numeric(colnames(cn)))
    ## calculate overall year range
    yearr <- range(as.vector(unlist(lapply(s, function(m)as.numeric(rownames(m))))), as.numeric(rownames(cn)))
    mat <- matrix(x, nrow = yearr[2] - yearr[1] + 1, ncol = ager[2] - ager[1] + 1)
    cat("Auto generated default file\n1 1\n", file = outfile)
    cat(yearr, "\n", file = outfile, append = TRUE)
    cat(ager, "\n", file = outfile, append = TRUE)
    cat(1, "\n", file = outfile, append = TRUE)
    write.table(mat, file = outfile, append = TRUE, row.names = FALSE, col.names = FALSE, quote = FALSE)
}



gen.default.catchdim <- function(x, outfile){
    if(!file.exists('cn.dat')){
        stop("File cn.dat does not exsist")
    }
    cn <- read.ices('cn.dat')
    ## calculate overall age range
    ager <- range(as.numeric(colnames(cn)))
    ## calculate overall year range
    yearr <- range(as.numeric(rownames(cn)))
    mat <- matrix(x, nrow = yearr[2] - yearr[1] + 1, ncol = ager[2] - ager[1] + 1)
    cat("Auto generated default file\n1 1\n", file = outfile)
    cat(yearr, "\n", file = outfile, append = TRUE)
    cat(ager, "\n", file = outfile, append = TRUE)
    cat(1, "\n", file = outfile, append = TRUE)
    write.table(mat, file = outfile, append = TRUE,
                row.names = FALSE, col.names = FALSE, quote = FALSE)
}

### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
