#-*- coding: utf-8 -*-

### File: model.R
### Time-stamp: <2024-04-25 14:01:50 a23579>
###
### Created: 2016??
### Author: Jennifer Devine, Yves Reecht
###
####################################################################################################
### Description:
###
### Pok.27.3a46 - TAF compliant model script.
####################################################################################################

### Time-stamp: <2024-04-25 12:04:46 a23579>

## Run analysis, write model results

## Before: data.RData
## After: fit.RData, forecast.RData", retro.RData, r
##  residuals.RData, residuals.RData, stk.Rdata, idx.Rdata

source("init.R")

library(icesTAF)
taf.library(FLfse)
library(stockassessment)

mkdir(modelDir)
## "model\(\(/\)\([^"]+\)\)?" → file.path(modelDir, "\3")


## 1. fit model ---------------------------------------------------------------

load(file.path(dataDir, "data.RData"))

conf <- loadConf(dat, "bootstrap/data/model.cfg", patch = TRUE)
par <- defpar(dat, conf)
fit <- sam.fit(dat, conf, par)
if(fit$opt$convergence != 0) stop("Model did not converge.")
save(fit, file = file.path(modelDir, "fit.RData"))



## 2. leave-one-out -----------------------------------------------------------

LO <- leaveout(fit)
save(LO, file = file.path(modelDir, "leaveout.RData"))



## 3. forecast ----------------------------------------------------------------


if (NsimForecast < 1e4)
{
    message("\n## Nsim = ", NsimForecast,
            "... remember to increase the number of iterations for final run!\n\n")
}

## Recruitment years
## Ry <- 1998:max(fit$data$years) # includes 2018
Ry <- tail(fit$data$years, NRy) # last 10 years


## Selectivity / biological vars years
## use last 3 years for fishing selectivity (Sy)
## and for average weight, maturity, M, etc. (Ay)
## This is what is specified in annex
Sy <- assess_year + c(-3:-1)
Ay <- assess_year + c(-3:-1)

## Reference points:
refPts <- list(
  Fmsy = Fmsy,
  Flim = Flim,
  Fpa = Fpa,
  Fp.05 = Fp.05,
  Bpa = Bpa,
  Blim = Blim,
  Btrigger = Btrigger,
  MAP.FMSY.lower = MAP.FMSY.lower,
  MAP.FMSY.upper = MAP.FMSY.upper)
save(refPts, file = file.path(modelDir, "refPts.RData"))

## save other objects needed for utilities_report.Rmd
save(assess_year, advice_year, reopening,
     Ry, Sy, Ay, file = file.path(modelDir, "settings.RData"))

## Fsq reference - Fbar from final year of catch data
FbarCols <- as.character(seq(conf$fbarRange[1], conf$fbarRange[2]))
Flast <- unname(rowMeans(faytable(fit)[, FbarCols])[as.character(assess_year - 1)])

set.seed(1112)
tmp <- stockassessment::forecast(fit = fit, ave.years = Ay, rec.years = Ry,
                                 nosim = NsimForecast,
                                 fval = c(Flast, Flast, Flast, Flast),
                                 label = "Fsq", overwriteSelYears = Sy, splitLD = TRUE)

SSB_advice_year <- attr(tmp, "tab")[as.character(advice_year), "ssb:median"]

## Estimates SSB at beginning of advice year in an intermediate TAC catch scenario:
set.seed(1112)
tmp2 <- stockassessment::forecast(fit = fit, ave.years = Ay, rec.years = Ry,
                                  nosim = NsimForecast,
                                  fval = c(Flast, NA, Flast, Flast),
                                  catchval = c(NA, TAC_assess_year, NA, NA),
                                  label = "TAC2020, Fmsy", overwriteSelYears = Sy, splitLD = TRUE)

SSB_advice_year_TAC <- attr(tmp2, "tab")[as.character(advice_year), "ssb:median"]

set.seed(1112)
tmp3 <- stockassessment::forecast(fit = fit, ave.years = Ay, rec.years = Ry,
                                  nosim = NsimForecast,
                                  fval = c(Flast, NA, rep(Fmsy * min(c(1, SSB_advice_year_TAC / Btrigger)), 2)),
                                  catchval = c(NA, TAC_assess_year, NA, NA),
                                  label = "TAC2020, Fmsy", overwriteSelYears = Sy, splitLD = TRUE)

F_advice_year_TAC <- attr(tmp3, "tab")[as.character(advice_year), "fbar:median"]
F_assess_year_TAC <- attr(tmp3, "tab")[as.character(assess_year), "fbar:median"]

## management scenarios
MS <- list(
  "A*" = data.frame(Ftarg = Fmsy, Btrig = Btrigger),
  "A" = data.frame(Ftarg = 0.35, Btrig = 250000),
  "B" = data.frame(Ftarg = 0.39, Btrig = 200000),
  "C" = data.frame(Ftarg = 0.35, Btrig = 250000),
  "A+D" = data.frame(Ftarg = 0.41, Btrig = 210000),
  "B+E" = data.frame(Ftarg = 0.39, Btrig = 220000),
  "C+E" = data.frame(Ftarg = 0.36, Btrig = 230000),
  "A+D1" = data.frame(Ftarg = 0.36, Btrig = 230000),
  "A*+D" = data.frame(Ftarg = Fmsy, Btrig = Btrigger)
)
MS <- do.call("rbind", MS)
MS$SSB <- SSB_advice_year

MS$Fadv <- ifelse(MS$SSB > MS$Btrig, MS$Ftarg, MS$Ftarg * (MS$SSB / MS$Btrig))
MS
save(MS, file = file.path(modelDir, "MS.RData"))

## Similar management scenarios with intermediate year catch = TAC:
MStac <- MS
## row.names(MStac) <- paste0(row.names(MStac), " (TAC ", assess_year, ")")
MStac$SSB <- SSB_advice_year_TAC

MStac$Fadv <- ifelse(MStac$SSB > MStac$Btrig, MStac$Ftarg, MStac$Ftarg * (MStac$SSB / MStac$Btrig))
MStac
save(MStac, file = file.path(modelDir, "MStac.RData"))


## main scenarios:
### NOTE: previous years have used the argument fscale = 1 for last data and intermediate years
### We should discuss returning to this, and what possible difference in parameterization there might be
scen1 <- list()

## Assume TAC taken on interim year rather than stable F:
scen1[[paste0("TAC (", assess_year, "), then Fmsy")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, NA, Fmsy, Fmsy),
         catchval = c(NA, TAC_assess_year, NA, NA))


scen1[[paste0("TAC (", assess_year, "), then MSY")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, NA, rep(Fmsy * min(c(1, SSB_advice_year_TAC / Btrigger)), 2)),
         catchval = c(NA, TAC_assess_year, NA, NA))

## F for interim (assessment) year same as F estimated for previous year (status quo):
scen1[["Fsq, then Fmsy"]] <- list(fscale = c(NA, NA, NA, NA),
                                  fval = c(Flast, Flast,
                                           rep(Fmsy, 2)))

scen1[["Fsq, then MSY"]] <- list(fscale = c(NA, NA, NA, NA),
                                  fval = c(Flast, Flast,
                                           rep(Fmsy * min(c(1, SSB_advice_year / Btrigger)), 2)))


## Alternative scenarios with F status quo for interim year:
scen1[[paste0("Fsq, then MAP FMSY lower")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, MAP.FMSY.lower, MAP.FMSY.lower),
         catchval = c(NA, NA, NA, NA))

scen1[[paste0("Fsq, then MAP FMSY upper")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, MAP.FMSY.upper, MAP.FMSY.upper),
         catchval = c(NA, NA, NA, NA))

scen1[[paste0("Fsq, then zero")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, 0.000001, 0.000001),
         catchval = c(NA, NA, NA, NA))

scen1[[paste0("Fsq, then Fpa")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, Fpa, Fpa),
         catchval = c(NA, NA, NA, NA))

scen1[[paste0("Fsq, then Fp.05")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, Fp.05, Fp.05),
         catchval = c(NA, NA, NA, NA))

scen1[[paste0("Fsq, then Flim")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, Flim, Flim),
         catchval = c(NA, NA, NA, NA))

scen1[[paste0("Fsq, then SSB(", advice_year + 1, ") = Blim")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, NA, NA),
         nextssb = c(NA, NA, Blim, Blim),
         catchval = c(NA, NA, NA, NA))

scen1[[paste0("Fsq, then SSB(", advice_year + 1, ") = Bpa")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, NA, NA),
         nextssb = c(NA, NA, Bpa, Bpa),
         catchval = c(NA, NA, NA, NA))

scen1[[paste0("Fsq, then SSB(", advice_year + 1, ") = MSY Btrigger")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, NA, NA),
         nextssb = c(NA, NA, Btrigger, Btrigger),
         catchval = c(NA, NA, NA, NA))

scen1[["Fsq"]] <- list(fscale = c(NA, NA, NA, NA), fval = c(Flast, Flast, Flast, Flast))

## TAC stability scenarios:
scen1[[paste0("Fsq, then TAC(", assess_year, ")")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, NA, NA),
         catchval = c(NA, NA, TAC_assess_year, TAC_assess_year))

scen1[[paste0("Fsq, then TAC(", assess_year, ") -15%")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, NA, NA),
         catchval = c(NA, NA, TAC_assess_year * 0.85, TAC_assess_year * 0.85))

scen1[[paste0("Fsq, then TAC(", assess_year, ") +15%")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, NA, NA),
         catchval = c(NA, NA, TAC_assess_year * 1.15, TAC_assess_year * 1.15))

scen1[[paste0("Fsq, then TAC(", assess_year, ") -20%")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, NA, NA),
         catchval = c(NA, NA, TAC_assess_year * 0.80, TAC_assess_year * 0.80))

scen1[[paste0("Fsq, then TAC(", assess_year, ") +25%")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, NA, NA),
         catchval = c(NA, NA, TAC_assess_year * 1.25, TAC_assess_year * 1.25))

## MAP scenarios with HCR
scen1[[paste0("Fsq, then MAP FMSY lower HCR")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast,
                  rep(MAP.FMSY.lower * min(c(1, SSB_advice_year / Btrigger)), 2)),
         catchval = c(NA, NA, NA, NA))

scen1[[paste0("Fsq, then MAP FMSY upper HCR")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast,
                  rep(MAP.FMSY.upper * min(c(1, SSB_advice_year / Btrigger)), 2)),
         catchval = c(NA, NA, NA, NA))

## Fyear scenarios:
scen1[[paste0("Fsq, then F(", assess_year - 1, ")")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, Flast, Flast),
         catchval = c(NA, NA, NA, NA))

scen1[[paste0("Fsq, then F(", assess_year, ")")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, F_assess_year_TAC, F_assess_year_TAC),
         catchval = c(NA, NA, NA, NA))


scen1[["Fsq"]] <- list(fscale = c(NA, NA, NA, NA), fval = c(Flast, Flast, Flast, Flast))


## 2019 Special request
scen1[[paste0("Fsq, then MS A")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, MS["A", ]$Fadv, MS["A", ]$Fadv),
         catchval = c(NA, NA, NA, NA))

scen1[[paste0("Fsq, then MS B")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, MS["B", ]$Fadv, MS["B", ]$Fadv),
         catchval = c(NA, NA, NA, NA))

scen1[[paste0("Fsq, then MS C")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, MS["C", ]$Fadv, MS["C", ]$Fadv),
         catchval = c(NA, NA, NA, NA))

scen1[[paste0("Fsq, then MS A+D")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, MS["A+D", ]$Fadv, MS["A+D", ]$Fadv),
         catchval = c(NA, NA, NA, NA))

scen1[[paste0("Fsq, then MS B+E")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, MS["B+E", ]$Fadv, MS["B+E", ]$Fadv),
         catchval = c(NA, NA, NA, NA))

scen1[[paste0("Fsq, then MS C+E")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, MS["C+E", ]$Fadv, MS["C+E", ]$Fadv),
         catchval = c(NA, NA, NA, NA))

scen1[[paste0("Fsq, then MS A+D1")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, MS["A+D1", ]$Fadv, MS["A+D1", ]$Fadv),
         catchval = c(NA, NA, NA, NA))

scen1[[paste0("Fsq, then MS A*+D")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, Flast, MS["A*+D", ]$Fadv, MS["A*+D", ]$Fadv),
         catchval = c(NA, NA, NA, NA))

## 2019 Special request (Alternative to Fsq: TAC interim year):
scen1[[paste0("TAC(", assess_year, "), then MS A")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, NA, MStac["A", ]$Fadv, MStac["A", ]$Fadv),
         catchval = c(NA, TAC_assess_year, NA, NA))
scen1[[paste0("TAC(", assess_year, "), then MS B")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, NA, MStac["B", ]$Fadv, MStac["B", ]$Fadv),
         catchval = c(NA, TAC_assess_year, NA, NA))
scen1[[paste0("TAC(", assess_year, "), then MS C")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, NA, MStac["C", ]$Fadv, MStac["C", ]$Fadv),
         catchval = c(NA, TAC_assess_year, NA, NA))
scen1[[paste0("TAC(", assess_year, "), then MS A+D")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, NA, MStac["A+D", ]$Fadv, MStac["A+D", ]$Fadv),
         catchval = c(NA, TAC_assess_year, NA, NA))
scen1[[paste0("TAC(", assess_year, "), then MS B+E")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, NA, MStac["B+E", ]$Fadv, MStac["B+E", ]$Fadv),
         catchval = c(NA, TAC_assess_year, NA, NA))
scen1[[paste0("TAC(", assess_year, "), then MS C+E")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, NA, MStac["C+E", ]$Fadv, MStac["C+E", ]$Fadv),
         catchval = c(NA, TAC_assess_year, NA, NA))
scen1[[paste0("TAC(", assess_year, "), then MS A+D1")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, NA, MStac["A+D1", ]$Fadv, MStac["A+D1", ]$Fadv),
         catchval = c(NA, TAC_assess_year, NA, NA))
scen1[[paste0("TAC(", assess_year, "), then MS A*+D")]] <-
    list(fscale = c(NA, NA, NA, NA),
         fval = c(Flast, NA, MStac["A*+D", ]$Fadv, MStac["A*+D", ]$Fadv),
         catchval = c(NA, TAC_assess_year, NA, NA))


## F-step scenarios (0.01 steps between Flower and Fupper)
MAP.FMSYs <- seq(round(MAP.FMSY.lower, 2), round(MAP.FMSY.upper, 2), 0.01)
scen2 <- vector("list", length(MAP.FMSYs))
names(scen2) <- paste0("Fsq, then MAP FMSY = ", sprintf(fmt = "%.2f", MAP.FMSYs) )
for(i in seq(scen2)){
    scen2[[i]] <- list(fval = c(Flast, Flast, rep(MAP.FMSYs[i], 2)),
                       catchval = c(NA, NA, NA, NA))
}

## Combine all scenarios, and give year names for clarity
scen <- c(scen1, scen2)
names(scen)
argNames <- if(reopening){
  argNames <- assess_year + (0:3)
} else {
  argNames <- assess_year + (-1:2)
}
for(i in seq(scen)){
  for(j in seq(scen[[i]])){
    names(scen[[i]][[j]]) <- argNames
  }
}


## If reopening, arguments must be shifted to correctly reflect
##   intermediate and forecast years.
## First value of argument vectors reflects last data year.
## In a normal assessment year (i.e. without new IBTS Q3 index), this is:
##   c(assess_year - 1, assess_year, advice_year, advice_year + 1)
## For a reopening assessment, this is:
##   c(assess_year, tac_yr, advice_year + 1, advice_year + 2)
## Thus, a reopening assessment needs to copy forecast values to the left:
if(reopening){
  for(i in seq(scen)){
    for(j in seq(scen[[i]])){
      scen[[i]][[j]][2] <- scen[[i]][[j]][3]
    }
  }
}


## perform forecasts
FC <- vector("list", length(scen))
names(FC) <- names(scen)

## ## trim number of forecasts for model selection:
## scen <- scen[c(1:4,
##                which(names(scen) %in% "Fsq"))]
## FC <- FC[names(scen)]

for(i in seq(scen)){
  set.seed(1112)

  ARGS <- scen[[i]]
  ARGS <- c(ARGS,
            list(fit = fit, ave.years = Ay, rec.years = Ry, nosim = NsimForecast,
                 label = names(scen)[i], overwriteSelYears = Sy,
                 splitLD = TRUE, savesim = TRUE))

  FC[[i]] <- do.call(stockassessment::forecast, ARGS)
  ## attr(FC[[i]], "tab")[, 4] # check consistency of med rec for forecast years

  print(paste0("forecast : ", "'", names(scen)[i], "'", " is complete"))
}


{ # Optimization to reach precise SSB targets:
    timeSaved <- Sys.time()
    message("## starting optimization at ", timeSaved)

    ## Additional solver for more precise matching of median value for SSB targets
    scen_num <- which(grepl(paste0("then SSB(", advice_year + 1, ") = "),
                            names(scen), fixed = TRUE))

    FC2 <- vector("list", length(scen_num))
    names(FC2) <- names(scen)[scen_num]
    for(i in seq(FC2)){

        ARGS <- scen[[scen_num[i]]]
        ARGS <- c(ARGS,
                  list(fit = fit, ave.years = Ay, rec.years = Ry, nosim = NsimForecast,
                       label = names(scen)[scen_num[i]], overwriteSelYears = Sy,
                       splitLD = TRUE, savesim = TRUE))

        fun <- function(fval = 0.35, ARGS){
            set.seed(12345)
            ARGS2 <- ARGS
            ARGS2$nextssb <- c(NA, NA, NA, NA)
            ARGS2$fval[which(as.numeric(names(ARGS2$fval)) > (assess_year))] <- fval

            fc <- do.call(stockassessment::forecast, ARGS2)
            fc_tab <- attr(fc, "tab")
            ssbmed <- fc_tab[rownames(fc_tab) == as.character(assess_year + 2),
                             colnames(fc_tab) == "ssb:median"]
            fitness <- sqrt((ssbmed - ARGS$nextssb[4])^2)
            return(fitness)
        }

        message("\n## Optimization for scenario \"",
                names(scen)[scen_num][i], "\"...")
        ## Non optimized scenario should constitute a safe starting point:
        Fstart <- attr(FC[[scen_num[i]]],
                       "tab")[as.character(assess_year + 1),
                              "fbar:median"]

        Frange <- Fstart * c(0.9, 1.1) # -/+ 10%

        system.time(
            res <- optim(par = Fstart, fn = fun, ARGS = ARGS,
                         lower = Frange[1], upper = Frange[2],
                         method = "Brent",
                         control = list(## trace = 4,
                                       ## factr = 1e-3,
                                       abstol = 0.49
                                   )))

        set.seed(12345)
        ARGS2 <- ARGS
        ARGS2$nextssb <- c(NA, NA, NA, NA)
        ARGS2$fval[which(as.numeric(names(ARGS2$fval)) > (assess_year))] <- res$par

        FC2[[i]] <- do.call(stockassessment::forecast, ARGS2)
        attr(FC2[[i]], "tab")

    }

    timeEnd <- Sys.time()
    timeDiff <- timeEnd - timeSaved

    message("## Finishing optimization at ", timeEnd,
            "\n## Elapsed time: ", round(timeDiff, 1), " ", attr(timeDiff, "unit"))
}


save(FC, FC2, file = file.path(modelDir, "forecast.RData"))



## 4. retrospective -----------------------------------------------------------

RETRO <- retro(fit, year = 5)
save(RETRO, file = file.path(modelDir, "retro.RData"))


## 5. residuals ---------------------------------------------------------------

RES <- residuals(fit)
RESP <- procres(fit)
save(RES, RESP, file = file.path(modelDir, "residuals.RData"))



## 6. FLStock and FLIndices objects ----------------------------------------------------------
## library(FLfse)

stk <- FLfse::SAM2FLStock(fit)
## range(stk)["plusgroup"] <- range(stk)["max"]
save(stk, file = file.path(modelDir, "stk.RData"))

idx <- FLCore::readFLIndices("bootstrap/data/survey.dat")
save(idx, file = file.path(modelDir, "idx.RData"))

## Exporting stock objects for WGMIXFISH:
source("./WGMIXFISH_exports.R")


### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
