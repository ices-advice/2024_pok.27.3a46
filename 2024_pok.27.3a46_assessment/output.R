#-*- coding: utf-8 -*-

### File: output.R
### Time-stamp: <2024-04-25 12:51:36 a23579>
###
### Created: 2016??
### Author: Jennifer Devine, Yves Reecht
###
####################################################################################################
### Description:
###
### Pok.27.3a46 - TAF compliant output script.
####################################################################################################

## Extract results of interest, write TAF output tables

## Before: fit.RData, forecast.RData, retro.RData, r
##  residuals.RData, residuals.RData
## After: diagnostic plots and tables

library(tidyverse)
library(icesTAF)
library(stockassessment)
library(FLCore)
library(ggplotFL)

source("init.R")

mkdir(outputDir)

## "output\(\(/\)\([^"]+\)\)?" → file.path(outputDir, "\3")


## Report tables -----------------------------------------------------------


load(file.path(modelDir, "fit.RData"))


## summary table =====
tsb <- tsbtable(fit)
colnames(tsb) <- c("TSB", "Low", "High")
tab_summary <- cbind(summary(fit), tsb)
tab_summary <- xtab2taf(tab_summary)
write.taf(tab_summary, file.path(outputDir, "tab_summary.csv"))

## F at age table =====
tab_fay <- faytable(fit)
tab_fay <- xtab2taf(tab_fay)
write.taf(tab_fay, file.path(outputDir, "tab_fay.csv"))

## catch table =====
tab_catch <- catchtable(fit)
colnames(tab_catch) <- c("Catch", "Low", "High")
tab_catch <- xtab2taf(tab_catch)
write.taf(tab_catch, file.path(outputDir, "tab_catch.csv"))


## numbers table =====
tab_numbers <- ntable(fit)
tab_numbers <- xtab2taf(tab_numbers)
write.taf(tab_numbers, file.path(outputDir, "tab_numbers.csv"))


## discards numbers table
load(file.path(modelDir, "stk.RData"))
df <- as.data.frame(stk@discards.n)
df <- reshape(df, idvar = "year", timevar = "age", v.names = "data", direction = "wide")
write.taf(df, file.path(outputDir, "tab_discards.n.csv"))

## landings numbers table
load(file.path(modelDir, "stk.RData"))
df <- as.data.frame(stk@landings.n)
df <- reshape(df, idvar = "year", timevar = "age", v.names = "data", direction = "wide")
write.taf(df, file.path(outputDir, "tab_landings.n.csv"))

## catch numbers table
load(file.path(modelDir, "stk.RData"))
df <- as.data.frame(stk@catch.n)
df <- reshape(df, idvar = "year", timevar = "age", v.names = "data", direction = "wide")
write.taf(df, file.path(outputDir, "tab_catch.n.csv"))


## catch, landings and discards at age for standard graphs =====

cn <- read.ices("bootstrap/data/cn.dat")
cw <- read.ices("bootstrap/data/cw.dat")
dw <- read.ices("bootstrap/data/dw.dat")
lw <- read.ices("bootstrap/data/lw.dat")
lf <- read.ices("bootstrap/data/lf.dat")

CA <- cn * cw
CA <- xtab2taf(CA)
CA$total <- rowSums(CA[, -1])
write.taf(CA, file.path(outputDir, "CA.csv"))

LA <- cn * lf * lw
LA <- xtab2taf(LA)
LA$total <- rowSums(LA[, -1])
write.taf(LA, file.path(outputDir, "LA.csv"))

DI <- cn * (1 - lf) * dw
DI <- xtab2taf(DI)
DI$total <- rowSums(DI[, -1])
write.taf(DI, file.path(outputDir, "DI.csv"))


## Index table =======
load(file.path(modelDir, "idx.RData"))
idx1 <- idx[[1]]@index
df1 <- as.data.frame(idx1)
df1 <- reshape(df1, idvar = "year", timevar = "age", v.names = "data", direction = "wide")

idx2 <- idx[[2]]@index
df2 <- as.data.frame(idx2)
df2 <- reshape(df2, idvar = "year", timevar = "age", v.names = "data", direction = "wide")

df <- merge(x = df1, y = df2, all = TRUE)
names(df) <- gsub(pattern = "data.", replacement = "", x = names(df), fixed = TRUE)
names(df) <- gsub(pattern = "year", replacement = "Year", x = names(df), fixed = TRUE)
names(df) <- gsub(pattern = "-1", replacement = "CPUE", x = names(df), fixed = TRUE)

df <- df[, c("Year", ac(3:8), "CPUE")]
write.taf(df, file.path(outputDir, "tab_indices.csv"))


## parameter table =====
tab_par <- partable(fit)
tab_par <- cbind(data.frame("Parameter name" = rownames(tab_par), check.names = FALSE), tab_par)
write.taf(tab_par, file.path(outputDir, "tab_pars.csv"))

## model table =====
basefit <- NULL
tab_model <- modeltable(c(Current = fit, base = basefit))
tab_model <- cbind(data.frame(model = rownames(tab_model)), tab_model)
write.taf(tab_model, file.path(outputDir, "tab_model.csv"))

## parameter sd table =====
sdState <- function(fit, y = max(fit$data$years) - 1:0)
{
    idx <- names(fit$sdrep$value) == "logR"
    sdLogR <- fit$sdrep$sd[idx][fit$data$years%in%y]
    idx <- names(fit$sdrep$value) == "logssb"
    sdLogSSB <- fit$sdrep$sd[idx][fit$data$years %in% y]
    idx <- names(fit$sdrep$value) == "logfbar"
    sdLogF <- fit$sdrep$sd[idx][fit$data$years%in%y]
    ret <- cbind(sdLogR, sdLogSSB, sdLogF)
    rownames(ret) <- y
    colnames(ret) <- c("sd(log(R))", "sd(log(SSB))", "sd(log(Fbar))")
    return(ret)
}
tab_sd <- sdState(fit)
tab_sd <- xtab2taf(tab_sd)
write.taf(tab_sd, file.path(outputDir, "tab_sd.csv"))

sdSeries <- matrix(exp(fit$pl$logSdLogObs),
                   nrow = 1)

## ###########################################################
## Correction SD for Survey (account for observation weights):

## Extraction weights (might need to subset ages if uncoupled SDs):
Wi <- fit$data$weight[fit$data$aux[, "fleet"] %in% 2]

## correct SD according to Anders: sqrt(mean(exp(2 * logSd) * (1/W))):
sdSeries[4] <- sqrt(mean(exp(2 * fit$pl$logSdLogObs[4]) * (1 / Wi), na.rm = TRUE))

colnames(sdSeries) <- c("Catches a3", "Catches a4-5", "Catches a6-10+", "Survey Q3-4", "Commercial CPUE")
row.names(sdSeries) <- "sd"
write.taf(cbind(data.frame("." = "sd"), sdSeries), file.path(outputDir, "tab_obs_sd.csv"))


## forecasts table =====
load(file.path(modelDir, "forecast.RData"))

## update scenarios that hit SSB refs
scen_num <- which(grepl(paste0("then SSB(", advice_year + 1, ") = "),
                        names(FC), fixed = TRUE))
FC[scen_num] <- FC2

writeLines("", con = file.path(outputDir, "tab_forecasts.txt"), sep = "\t")
FC_df <- vector("list", length(FC))
for(i in seq(FC)){
  f <- FC[[i]]
  fc_tab <- attr(f, "tab")
  fc_lab <- attr(f, "label")
  tmp <- as.data.frame(fc_tab)
  tmp <- cbind(data.frame("scenario" = fc_lab), tmp)
  tmp <- xtab2taf(tmp)
  FC_df[[i]] <- tmp

  fc_tab <- xtab2taf(fc_tab)
  fc_lab <- gsub(pattern = '*', replacement = "star", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = ", ", replacement = "", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = "+", replacement = "plus", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = "-", replacement = "minus", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = "=", replacement = "equals", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = "%", replacement = "perc", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = " ", replacement = "_", x = fc_lab, fixed = TRUE)
  fname <- paste0("tab_fc_", fc_lab, ".csv")

  write.taf(fc_tab, file.path(file.path(outputDir, ""), fname))

  write.table(x = paste("\n", attr(f, "label")), file = file.path(outputDir, "tab_forecasts.txt"), append = TRUE,
    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  write.table(x = fc_tab, file = file.path(outputDir, "tab_forecasts.txt"), append = TRUE,
    row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
}
FC_df <- do.call("rbind", FC_df)
save(FC_df, file = file.path(outputDir, "FC_df.Rdata"))

## Export forecast results for MAP with step 0.1:
mapIdx <- grepl(pattern = paste0("^Fsq, then MAP FMSY = [.[:digit:]]+$"),
                names(FC))

res <- lapply(FC[mapIdx],
              function(x)
       {
           fc_tab <- attr(x, "tab")
           fc_lab <- attr(x, "label")
           tmp <- as.data.frame(fc_tab)
           tmp <- cbind(data.frame("scenario" = fc_lab),
                        year = row.names(tmp),
                        tmp)
           row.names(tmp) <- NULL
           return(tmp)
       })#, simplify = FALSE, USE.NAMES = FALSE)
resFmap <- do.call(rbind, res)

row.names(resFmap) <- NULL

write.csv(resFmap,
          file = file.path(outputDir, "F-MAP_range_forecast_all_years.csv"))

write.csv(resFmap[resFmap$year %in% advice_year, ],
          file = file.path(outputDir, paste0("F-MAP_range_forecast_", advice_year, ".csv")))


## ##################################################
## Contribution of age classes to TAC year catch

## Select scenario:
i <- which(names(FC) %in% "Fsq, then MSY")

## Index of advice year:
idxTY <- which(row.names(attributes(FC[[i]])$tab) %in% advice_year)

contrib <- round(100 * apply(FC[[i]][[idxTY]]$catchatage, 1, median) /
                 sum(apply(FC[[i]][[idxTY]]$catchatage, 1, median)), 1)

yClass <- advice_year -
    as.numeric(row.names(FC[[i]][[idxTY]]$catchatage))

## Contribution in weight:
##  - weight at age in catches (forecast):
cmw <- apply(tail(attributes(FC[[i]])$fit$data$catchMeanWeight,
                  3), # Use mean over last three years of weight at age
             2, mean)

##  - landings mean weights-at-age (last 3 years):
lmw <- apply(tail(attributes(FC[[i]])$fit$data$landMeanWeight,
                  3), # Use mean over last three years of weight at age
             2, mean)

##  - mean landing fraction at age (last 3 years):
lmf <- apply(tail(attributes(FC[[i]])$fit$data$landFrac,
                  3), # Use mean over last three years of weight at age
             2, mean)

##  - simulated catch weights at age in TAC year:
catchWatAgeSim <- sweep(FC[[i]][[idxTY]]$catchatage, 1, cmw, "*")

## Simulated landings numbers at age:
landingNatAgeSim <- sweep(FC[[i]][[idxTY]]$catchatage, 1, lmf, "*")

## Simulated landing weights at age:
landingWatAgeSim <- sweep(landingNatAgeSim, 1, lmw, "*")


## Test (if not TRUE, possibly different number of mean years):
all.equal(apply(catchWatAgeSim, 2, sum), FC[[i]][[idxTY]]$catch)

contribW <- round(100 * apply(catchWatAgeSim, 1, median) /
                  sum(apply(catchWatAgeSim, 1, median)), 1)

contribL <- round(100 * apply(landingNatAgeSim, 1, median) /
                  sum(apply(landingNatAgeSim, 1, median)), 1)

contribWL <- round(100 * apply(landingWatAgeSim, 1, median) /
                   sum(apply(landingWatAgeSim, 1, median)), 1)



catchContrib <- data.frame(yearClass = yClass,
                           C.percentN = contrib,
                           C.percentWg = contribW,
                           L.percentN = contribL,
                           L.percentWg = contribWL)

write.taf(x = catchContrib, file = file.path(outputDir, "year-class_contrib_TAC_year.csv"))


## Mohn's rho =====
load(file.path(modelDir, "retro.RData"))
MOHN <- stockassessment::mohn(RETRO, what = NULL)
MOHN <- data.frame(par = names(MOHN), rho = c(MOHN))
write.taf(x = MOHN, file = file.path(outputDir, "tab_mohn.csv"))

## ##################################################
## Landing and discards 3-10+ (as used in the assessment):
lan <- as.data.frame(stk@landings)
names(lan)[names(lan) %in% "data"] <- "Landings_t"
dis <- as.data.frame(stk@discards)
names(dis)[names(dis) %in% "data"] <- "Discards_t"
df <- merge(lan, dis, all = TRUE)

write.taf(x = df[, c("age", "season", "area",
                      "year", "Discards_t", "Landings_t")],
          file = file.path(outputDir, "tab_landings+discard_3-10+_tonnes.csv"))



## ###########################################################################
## Catches and proportions per age group and year classes:
cat(NULL, file = file.path(outputDir, "tabs_landing_per_age_group.csv"), append = FALSE)
cat(NULL, file = file.path(outputDir, "tabs_landing_percent_per_age_group.csv"), append = FALSE)
cat(NULL, file = file.path(outputDir, "tabs_landing_per_cohort.csv"), append = FALSE)
cat(NULL, file = file.path(outputDir, "tabs_landing_percent_per_cohort.csv"), append = FALSE)

for (i in names(FC))
{
    Years <- as.character(sapply(FC[[i]], function(x) x$year))
    projLy <- sapply(FC[[i]],
                     function(x)
              {
                  apply(x$catchatage, 1, median) *
                      apply(tail(lf, 3), 2, mean) * apply(tail(lw, 3), 2, mean)
              }, simplify = FALSE) %>%
        bind_rows() %>%
        rename_with(~paste0("Age_", .x)) %>%
        as.data.frame()

    rownames(projLy) <- Years
    projLyY <- cbind(Year = Years, round(projLy))

    cat("\n\"", i, "\"\n",
        file = file.path(outputDir, "tabs_landing_per_age_group.csv"), append = TRUE)
    write.table(projLyY,
                row.names = FALSE, dec = ".", sep = ", ",
                file = file.path(outputDir, "tabs_landing_per_age_group.csv"), append = TRUE)

    cat("\n\"", i, "\"\n",
        file = file.path(outputDir, "tabs_landing_percent_per_age_group.csv"), append = TRUE)
    write.table(cbind(Year = Years,
                      round(100 * sweep(projLy, 1,
                                        apply(projLy, 1, sum, na.rm = TRUE),
                                        "/"),
                            1)),
                row.names = FALSE, dec = ".", sep = ", ", na = "",
                file = file.path(outputDir, "tabs_landing_percent_per_age_group.csv"), append = TRUE)

    projLyc <- sapply(FC[[i]],
                      function(x)
               {
                   res <- apply(x$catchatage, 1, median) *
                       apply(tail(lf, 3), 2, mean) * apply(tail(lw, 3), 2, mean)
                   ##
                   names(res) <- seq(from = x$year - 3, by = -1, along.with = res)
                   round(res)
               }, simplify = FALSE) %>%
        bind_rows() %>%
        select(., any_of(sort(colnames(.)))) %>%
        rename_with(~paste0("YCl_", .x)) %>%
        as.data.frame() %>%
        cbind(Year = Years,
              .)

    rownames(projLyc) <- Years

    cat("\n\"", i, "\"\n",
        file = file.path(outputDir, "tabs_landing_per_cohort.csv"), append = TRUE)
    write.table(projLyc,
                row.names = FALSE, dec = ".", sep = ", ",
                file = file.path(outputDir, "tabs_landing_per_cohort.csv"), append = TRUE)


    pcLyc <- sapply(FC[[i]],
                    function(x)
             {
                 res <- apply(x$catchatage, 1, median) *
                     apply(tail(lf, 3), 2, mean) * apply(tail(lw, 3), 2, mean)
                 ##
                 names(res) <- seq(from = x$year - 3, by = -1, along.with = res)
                 ##
                 head(round(100 * res / sum(res, na.rm = TRUE), 1), -1)
             }, simplify = FALSE) %>%
        bind_rows() %>%
        select(., any_of(sort(colnames(.)))) %>%
        rename_with(~paste0("YCl_", .x)) %>%
        as.data.frame() %>%
        cbind(Year = Years,
              .)

    rownames(pcLyc) <- Years

    cat("\n\"", i, "\"\n",
        file = file.path(outputDir, "tabs_landing_percent_per_cohort.csv"), append = TRUE)
    write.table(pcLyc,
                row.names = FALSE, dec = ".", sep = ", ", na = "",
                file = file.path(outputDir, "tabs_landing_percent_per_cohort.csv"), append = TRUE)
}




## ## Compare to icesAdvice::mohn (same)
## L <- vector("list", 3)
## names(L) <- c("R(age 3)", "SSB", "Fbar(4-7)")
## base <- summary(attr(RETRO, "fit"))
## df <- xtab2taf(base)
## for(i in seq(L)){
## df.i <- data.frame(Year = df$Year, base = c(df[, c(names(L)[i])]))
## ## merge peels
## for(j in seq(RETRO)){
## df.ij <- xtab2taf(summary(RETRO[[j]]))
## df.ij <- data.frame(Year = df.ij$Year, tmp = c(df.ij[, c(names(L)[i])]))
## names(df.ij)[2] <- paste0("-", j)
## df.i <- merge(x = df.i, y = df.ij, all.x = TRUE)
## }
## L[[i]] <- taf2xtab(df.i)
## }
## lapply(L, icesAdvice::mohn, details = TRUE)
##
## (tmp <- icesAdvice::mohn(L[[i]], details = TRUE, plot = TRUE))
## mean(tmp$compare$relbias)

### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
