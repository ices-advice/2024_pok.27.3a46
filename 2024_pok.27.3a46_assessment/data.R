#-*- coding: utf-8 -*-

### File: data.R
### Time-stamp: <2024-04-25 12:51:05 a23579>
###
### Created: 2016??
### Author: Jennifer Devine, Yves Reecht
###
####################################################################################################
### Description:
###
### Pok.27.3a46 - TAF compliant data script.
####################################################################################################

## Preprocess data, write TAF data tables

## Before: cn.dat, cw.dat, dw.dat, lw.dat, sw.dat, mo.dat, nm.dat,
##  survey.dat, lf.dat, pf.dat, pm.dat
## After: cn.csv, cw.csv, dw.csv, lw.csv, sw.csv, mo.csv, nm.csv,
##  survey.csv, lf.csv, pf.csv, pm.csv,
##  check.all_log.txt, data.RData

library(icesTAF)
library(stockassessment)


## 0. Global settings for all scripts --------------------------------------
source("init.R")
mkdir(file.path(dataDir))


## 1. Read original data ------------------------------------------------------

cn <- read.ices("bootstrap/data/cn.dat")
cw <- read.ices("bootstrap/data/cw.dat")
dw <- read.ices("bootstrap/data/dw.dat")
lw <- read.ices("bootstrap/data/lw.dat")
mo <- read.ices("bootstrap/data/mo.dat") # time varying ogive; non-smoothed.
nm <- read.ices("bootstrap/data/nm.dat") # age varying M.
pf <- read.ices("bootstrap/data/pf.dat")
pm <- read.ices("bootstrap/data/pm.dat")
sw <- read.ices("bootstrap/data/sw.dat") # model based stock weights.
lf <- read.ices("bootstrap/data/lf.dat")
surveys <- read.ices("bootstrap/data/survey.dat") # Tuning series.

## Inverse of suvey variance at age as relative observation weights:
CV <- as.matrix(read.csv(file = "bootstrap/data/survey_index_CV.csv")[, -1])

W <- 1 / log(CV^2 + 1)
dimnames(W) <- dimnames(surveys[[1]]) # Helps making sure the ranges match.
attr(surveys[[1]], "weight") <- W

## mean(1 / log(CV^2 + 1))

## 2. validate data -----------------------------------------------------------

### IMPORTANT ###
## The following files must have the same terminal year:
## stock-related files: mo, nm, pf, pm, sw, survey1(IBTS)
## catch-related files: cn, cw, dw, lw, lf, survey2(industrial CPUE)
## In the case of reopening (in fall), stock-felated files are 1 year longer
## than catch-related files
#################

extent <- vector("list", 11)
names(extent) <- c("cn", "cw", "dw", "lw", "mo", "nm", "pf", "pm", "sw", "lf", "surveys")
for(i in seq(extent)){
    if(names(extent)[i] != "surveys"){
        extent[[i]] <- range(as.numeric(rownames(get(names(extent)[i]))))
    }else{
        extent[[i]] <- do.call("rbind",
                               lapply(surveys,
                                      FUN = function(x){range(as.numeric(rownames(x)))}))
    }
}
extent <- as.data.frame(do.call("rbind", extent))
colnames(extent) <- c("yearmin", "yearmax")
extent$filename <- rownames(extent)
extent$filetype <- ifelse(extent$filename %in%
                          c("mo", "nm", "pf", "pm", "sw",
                            "Survey index Q3-4"),
                          "stock", "catch")
extent <- extent[order(extent$filetype), ]
if(
    length(unique(subset(extent, filetype == "catch")$yearmax)) != 1 |
    length(unique(subset(extent, filetype == "stock")$yearmax)) != 1){
    "Warning: year extent of files not consistent"
} else {
    "Year extent of files is consistent"
}


source("utilities_datavalidator.R")
sink(file = file.path(dataDir, "check.all_log.txt"))
  check.all(path = "bootstrap/data")
sink()
check.all(path = "bootstrap/data")

## 3. create SAM setup file ---------------------------------------------------

dat <- setup.sam.data(surveys = surveys,
  residual.fleet = cn,
  prop.mature = mo,
  stock.mean.weight = sw,
  catch.mean.weight = cw,
  dis.mean.weight = dw,
  land.mean.weight = lw,
  prop.f = pf,
  prop.m = pm,
  natural.mortality = nm,
  land.frac = lf)

save(dat, file = file.path(dataDir, "data.RData"))


## 4. Make pre-process data (.csv) -----------------------------------------------------

catage <- xtab2taf(cn)
wcatch <- xtab2taf(cw)
wdiscards <- xtab2taf(dw)
wlandings <- xtab2taf(lw)
maturity <- xtab2taf(mo)
natmort <- xtab2taf(nm)
propf <- xtab2taf(pf)
propm <- xtab2taf(pm)
wstock <- xtab2taf(sw)
landfrac <- xtab2taf(lf)
survey_ibts_q3 <- xtab2taf(surveys[[1]])
survey_commercial_cpue <- xtab2taf(surveys[[2]])


## 5. Write TAF tables -----------------------------------------------------
write.taf(
  x = c("catage", "wcatch", "wdiscards", "wlandings", "maturity",
    "natmort", "propf", "propm", "wstock", "landfrac",
    "survey_ibts_q3", "survey_commercial_cpue"),
  dir = dataDir)



### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
