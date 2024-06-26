#-*- coding: utf-8 -*-

### File: report.R
### Time-stamp: <2024-04-25 12:52:03 a23579>
###
### Created: 2016??
### Author: Jennifer Devine, Yves Reecht
###
####################################################################################################
### Description:
###
### Pok.27.3a46 - TAF compliant report script.
####################################################################################################

### Time-stamp: <2024-04-25 12:06:07 a23579>

source("init.R")

## Prepare plots/tables for report

## Before:
## After:

library(tidyverse)
library(icesTAF)
library(stockassessment)
library(FLCore)
library(ggplotFL)
library(icesAdvice)
library(rmarkdown)
library(FLash)
library(viridis)
library(forcats)

theme_set(theme_bw())

mkdir(reportDir)
## "report\(\(/\)\([^"]+\)\)?" → file.path(reportDir, "\3")
## taf.png("\([^"]+\)?" → taf.png(file.path(reportDir, "\1")

## functions with more plotting control of SAM objects
source("bootstrap/software/functions/plotit.sam2.R")


## 1. FLStock plot -----------------------------------
load(file = file.path(modelDir, "stk.RData"))
load(file = file.path(modelDir, "refPts.RData"))

if (! exists("Ry"))
{
    ## Get recruitment years if not available:
    Ry <- tail(dimnames(rec(stk))$year, NRy)
}

taf.png(file.path(reportDir, "FLStock"), width = 5, height = 6, units = "in", res = 400)
{
    refs <- FLPar(F = refPts$Fmsy,
                  Rec = c(#mean(rec(stk)[, ac(Ry)]),
                            gmean(rec(stk)[, ac(Ry)])),
                  SSB = c(refPts$Bpa, refPts$Blim))
}
print(plot(stk, refs) +
      ylim(0, NA))
dev.off()

## Same with uncertainty and a nicer finish:
resEst <- ssbtable(fit) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Year") %>%
    mutate(Variable = "SSB (t)") %>%
    bind_rows(fbartable(fit) %>%
              as.data.frame() %>%
              tibble::rownames_to_column(var = "Year") %>%
              mutate(Variable = "Fbar")) %>%
    bind_rows(rectable(fit) %>%
              as.data.frame() %>%
              tibble::rownames_to_column(var = "Year") %>%
              mutate(Variable = "Rec (age 3)")) %>%
    ## bind_rows(rectable(fit) %>%
    ##           as.data.frame() %>%
    ##           tibble::rownames_to_column(var = "Year") %>%
    ##           mutate(Variable = "R age 3 (1000')")) %>%
    mutate(Model = as.character(assess_year)) %>%
    mutate(Year = as.numeric(Year),
           Model = fct_inorder(factor(Model)))


refP <- data.frame(Variable = c("Fbar", "Fbar", "SSB (t)", "SSB (t)", "Rec (age 3)"),
                   orderP = c(1, 1, 2, 2, 3),
                   Reference_point = c("F[MSY]", "F[pa]", "B[lim]", "B[trigger]", "'R (10y gmean)'"),
                   order = c(1, 2, 4, 5, 3),
                   value = c(refPts$Fmsy, refPts$Fpa, refPts$Blim, refPts$Btrigger, gmean(rec(stk)[, ac(Ry)]))) %>%
    mutate(Reference_point = fct_reorder(factor(Reference_point), order),
           Variable = fct_reorder(Variable, orderP))


ggAllEst <- ggplot(resEst,
                   aes(x = Year, y = Estimate)) +
    geom_hline(data = refP,
               aes(yintercept = value, linetype = Reference_point
                   ##, colour = Variable
                   ),
               colour = "red"
               ) +
    geom_line() +
    geom_ribbon(aes(ymin = Low, ymax = High),
                alpha = 0.4) +
    ##ggtitle(unique(resEst$Model)) +
    ylim(0, NA) +
    scale_linetype_discrete(label = label_parse) +
    scale_colour_discrete(name = "Reference_point", guide = FALSE) +
    facet_wrap(~Variable, scales = "free_y", ncol = 1) +
    theme_bw()

ggsave(plot = ggAllEst,
       filename = file.path(reportDir, "Estimates_and_refpoints.png"),
       width = 6, height = 7, dpi = 400)


## 2. fit plots ---------------------------------------------------------------
WIDTH <- 6
HEIGHT <- 5
UNITS <- "in"
MAR <- c(2, 3.5, 0.5, 0.5)
MGP <- c(2, 0.5, 0)
PS <- 12
RESO <- 400


load(file = file.path(modelDir, "fit.RData"))

taf.png(file.path(reportDir, "summary"), width = WIDTH, height = HEIGHT, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
plot(fit, partial = FALSE, las = 0, xlab = "")
dev.off()

taf.png(file.path(reportDir, "ssb"), width = WIDTH, height = HEIGHT, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
ssbplot(fit, ci = TRUE, las = 0, xlab = "")
dev.off()

taf.png(file.path(reportDir, "fbar"), width = WIDTH, height = HEIGHT, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
fbarplot(fit, partial = FALSE, las = 0, xlab = "")
dev.off()

taf.png(file.path(reportDir, "rec"), width = WIDTH, height = HEIGHT, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
recplot(fit, las = 0, xlab = "")
dev.off()

taf.png(file.path(reportDir, "landings"), width = WIDTH, height = HEIGHT, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
tryCatch(catchplot(fit, las = 0, xlab = "",
                   ylim = c(0, max(apply(cn * cw, 1, sum,
                                         na.rm = TRUE),
                                   na.rm = TRUE))),
         error = function(e) catchplot(fit, las = 0, xlab = "",
                                       ylim = c(0, 4.3e5)))
dev.off()


taf.png(file.path(reportDir, "sr"), width = WIDTH, height = HEIGHT, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
srplot(fit, las = 0)
dev.off()

taf.png(file.path(reportDir, "sdplot"), width = WIDTH, height = HEIGHT * 1.5, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
sdplot(fit)
dev.off()

## Estimates SD series:
sddf <- sdState(fit, fit$data$years) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Year") %>%
    mutate(Year = as.numeric(as.character(Year)))

sddfL <- sddf %>%
    pivot_longer(`sd(log(R))`:`sd(log(Fbar))`,
                 names_to = "Variable",
                 values_to = "sd")

ggSD <- ggplot(data = sddfL) +
    geom_line(aes(x = Year, y = sd)) +
    ylim(c(0, NA)) +
    facet_wrap(~Variable, strip.position = "right", scales = "fixed",
               ncol = 1)

taf.png(file.path(reportDir, "sd_est_plot_series"), width = WIDTH, height = HEIGHT, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
print(ggSD)
dev.off()

## Improved SR plots:
SRdata <- extractSRinfo(fit) %>%
    mutate(year = Year,
           Year = as.numeric(Year),
           decade = Year %/% 10 * 10,
           Decade = factor(paste0(decade, "-", decade + 9)))

ggSR <- ggplot(data = SRdata, aes(x = SSB, y = `R(age 3)`)) +
    geom_path() +
    geom_point(aes(color = Year), size = 2) +
    lims(x = c(0, NA), y = c(0, NA)) +
    scale_color_viridis()

ggsave(ggSR,
       filename = file.path(reportDir, "SR2.png"),
       width = 7, height = 5, units = "cm", scale = 2)


ggSRd <- ggplot(data = SRdata, aes(x = SSB, y = `R(age 3)`)) +
    geom_path() +
    geom_point(aes(color = Decade), size = 2) +
    lims(x = c(0, NA), y = c(0, NA)) +
    scale_color_viridis_d()

ggsave(ggSRd,
       filename = file.path(reportDir, "SR3.png"),
       width = 7, height = 5, units = "cm", scale = 2)


## 3. leave-one-put plot ---------------------------------------------------

load(file = file.path(modelDir, "leaveout.RData"))
names(LO) <- paste("w.o.", c("IBTS", "CPUE"))

taf.png(file.path(reportDir, "leaveout"), width = 2000, height = 1400, res = 200)
par(mar = MAR, mgp = MGP, ps = PS, mfcol = c(2, 2))
ssbplot(LO, las = 0, ci = TRUE, xlab = "")
fbarplot(LO, ci = TRUE, partial = FALSE, las = 0, xlab = "")
recplot(LO, ci = TRUE, las = 0, xlab = "")
catchplot(LO, ci = TRUE, las = 0, xlab = "", obs.show = FALSE)
dev.off()


## 4. forecast plots -------------------------------------------------------

load(file = file.path(modelDir, "forecast.RData"))

## plot Fmsy forecast
taf.png(file.path(reportDir, "forecast_Fmsy"),
        width = 1000, height = 1400, res = 200)
par(mar = MAR, mgp = MGP, ps = PS, oma = c(0, 0,1.5, 0))
plot(FC[[1]], las = 0, xlab = "", ci = TRUE)
mtext(text = names(FC)[1], side = 3, line = 0.25, outer = TRUE)

## # logssb, logR, logfbar
## plotit.sam2(fit, "logssb", trans = exp,
##   xlim = range(fit$data$years) + c(0, length(FC[[1]])),
##   valcol = 2, vallty = 1, vallwd = 2,
##   cicol = adjustcolor(2, 0.2), cilty = 2, cilwd = 1, ciborder = 2,
##   gridcol = 8, ylab = "SSB")
## addforecast2(FC[[1]], "ssb", arrowcol = 2, arrowlength = 0.05, dotcol = 2, dotcex = 0.8)

dev.off()

## plot TAC taken, then Fmsy:
taf.png(file.path(reportDir, "forecast_MSY"), width = 1000, height = 1400)
par(mar = MAR, mgp = MGP, ps = PS, oma = c(0, 0,1.5, 0))
plot(FC[[2]], las = 0, xlab = "", ci = TRUE)
mtext(text = names(FC)[2], side = 3, line = 0.25, outer = TRUE)

## # logssb, logR, logfbar
## plotit.sam2(fit, "logssb", trans = exp,
## xlim = range(fit$data$years) + c(0, length(FC[[1]])),
## valcol = 2, vallty = 1, vallwd = 2,
## cicol = adjustcolor(2, 0.2), cilty = 2, cilwd = 1, ciborder = 2,
## gridcol = 8, ylab = "SSB")
## addforecast2(FC[[1]], "ssb", arrowcol = 2, arrowlength = 0.05, dotcol = 2, dotcex = 0.8)

dev.off()

idxFsq1 <- which(names(FC) %in% "Fsq, then Fmsy")
## plot TAC taken, then Fmsy:
taf.png(file.path(reportDir, "forecast_Fsq_Fmsy"), width = 1000, height = 1400)
par(mar = MAR, mgp = MGP, ps = PS, oma = c(0, 0,1.5, 0))
plot(FC[[idxFsq1]], las = 0, xlab = "", ci = TRUE)
mtext(text = names(FC)[idxFsq1], side = 3, line = 0.25, outer = TRUE)
dev.off()

idxFsq2 <- which(names(FC) %in% "Fsq, then MSY")
## plot TAC taken, then Fmsy:
taf.png(file.path(reportDir, "forecast_Fsq_MSY"), width = 1000, height = 1400)
par(mar = MAR, mgp = MGP, ps = PS, oma = c(0, 0,1.5, 0))
plot(FC[[idxFsq2]], las = 0, xlab = "", ci = TRUE)
mtext(text = names(FC)[idxFsq2], side = 3, line = 0.25, outer = TRUE)
dev.off()

idxTAC1 <- which(grepl(pattern = paste0("TAC .?", assess_year, ".?, then MSY"),
                       names(FC)))
if (length(idxTAC1)){
    ## plot TAC taken, then MS A*+D:
    taf.png(file.path(reportDir, "forecast_TAC-MSY"), width = 1000, height = 1400)
    par(mar = MAR, mgp = MGP, ps = PS, oma = c(0, 0,1.5, 0))
    plot(FC[[idxTAC1]], las = 0, xlab = "", ci = TRUE)
    mtext(text = names(FC)[idxTAC1], side = 3, line = 0.25, outer = TRUE)
    dev.off()
}

idxTAC2 <- which(grepl(pattern = paste0("TAC .?", assess_year, ".?, then Fmsy"),
                       names(FC)))
if (length(idxTAC2))
{
    ## plot TAC taken, then MS A*+D:
    taf.png(file.path(reportDir, "forecast_TAC-Fmsy"), width = 1000, height = 1400)
    par(mar = MAR, mgp = MGP, ps = PS, oma = c(0, 0,1.5, 0))
    plot(FC[[idxTAC2]], las = 0, xlab = "", ci = TRUE)
    mtext(text = names(FC)[idxTAC2], side = 3, line = 0.25, outer = TRUE)
    dev.off()
}

## plot all together
pdf(file.path(reportDir, "forecast.pdf"), width = WIDTH, height = HEIGHT, onefile = TRUE)
for(i in seq(FC)){
    par(mar = MAR, mgp = MGP, ps = PS, oma = c(0, 0,1.5, 0))
    plot(FC[[i]], las = 0, xlab = "")
    mtext(text = names(FC)[i], side = 3, line = 0.25, outer = TRUE)
}
dev.off()


## 5. retrospective ---------------------------------------------------

load(file = file.path(modelDir, "retro.RData"))

taf.png(file.path(reportDir, "retro"), width = 2000, height = 1400)
par(mar = MAR, mgp = MGP, ps = PS, mfcol = c(2, 2))
ssbplot(RETRO, las = 0, ci = TRUE, xlab = "")
fbarplot(RETRO, ci = TRUE, partial = FALSE, las = 0, xlab = "")
recplot(RETRO, ci = TRUE, las = 0, xlab = "")
catchplot(RETRO, ci = TRUE, las = 0, xlab = "", obs.show = FALSE)
dev.off()



## 6. residuals ------------------------------------------------------------

load(file = file.path(modelDir, "residuals.RData"))

taf.png(file.path(reportDir, "residuals"), width = 1400, height = 1400)
par(ps = PS)
plot(RES)
dev.off()

taf.png(file.path(reportDir, "procres"), width = 1800, height = 1400)
par(ps = PS)
plot(RESP)
dev.off()

taf.png(file.path(reportDir, "residuals_alt"), width = 1500, height = 1400)
par(ps = PS)
print(plot.SAMres(RES))
dev.off()

taf.png(file.path(reportDir, "procres_alt"), width = 1000, height = 1400)
par(ps = PS)
print(plot.SAMres(RESP))
dev.off()

taf.png(file.path(reportDir, "residuals_fl1_alt"), width = 1500 / 2.5, height = 1400 * 9 / 10)
par(ps = PS)
print(plot.SAMres(RES, fleet = c(1)))
dev.off()
taf.png(file.path(reportDir, "residuals_fl2_alt"), width = 1500 / 2.5, height = 1400 * 7 / 10)
par(ps = PS)
print(plot.SAMres(RES, fleet = c(2)))
dev.off()
taf.png(file.path(reportDir, "residuals_fl3_alt"), width = 1500 / 2., height = 1400 * 2.5 / 10)
par(ps = PS)
print(plot.SAMres(RES, fleet = c(3)))
dev.off()

## 7. other diagnostics ----------------------------------------------------
taf.png(file.path(reportDir, "sr"), width = WIDTH, height = HEIGHT, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
srplot(fit)
dev.off()

taf.png(file.path(reportDir, "ypr"), width = 800, height = 600)
## par(ps = PS)
plot(ypr(fit))
dev.off()


if(!all(fit$conf$obsCorStruct == "ID")){
    taf.png(file.path(reportDir, "cor"), width = 1600, height = 1600)
    par(mar = MAR, mgp = MGP, ps = PS)
    corplot(fit)
    dev.off()
}


for(f in 1:fit$data$noFleets){
    taf.png(paste0(file.path(reportDir, "fleet"), f),
            width = 1600, height = 1200, pointsize = PS)
    fitplot(fit, fleets = f)
    dev.off()
}



## median recruitment assumptions
taf.png(file.path(reportDir, "rec_assumptions.png"), width = 5, height = 4, units = "in", res = 400)
dftmp <- data.frame(x = c(assess_year - 20, assess_year - 10), xend = rep(assess_year - 1, 2))
dftmp$y <- dftmp$yend <- c(mean(rec(stk)[, ac((assess_year - 20):(assess_year - 1))]),
                           mean(rec(stk)[, ac((assess_year - 10):(assess_year - 1))]))
dftmp$span <- c("20-yr med.", "10-yr med.")
plot(rec(stk)) +
    geom_segment(data = dftmp, aes(x = x, y = y, xend = xend, yend = yend,
                                   group = span, col = span), lty = 2)
dev.off()

## hasMethod(f = "plot", signature = signature(x = "FLIndex", y = "missing"))

## findMethod(f = plot, signature = signature(x = "FLIndex", y = "missing"))
## selectMethod(f = plot, signature = signature(x = "FLIndex", y = "missing"))

## getMethod(f = plot, signature = signature(x = "FLIndex", y = "missing"))

## internal consistency DATRAS
taf.png(file.path(reportDir, "internal_consistency_DATRAS_Q3.png"), width = 5, height = 5.5,
        units = "in", res = 400)
i <- which(grepl("survey", names(idx), ignore.case = TRUE))
tryCatch(plot(idx[[i]],
              type = "internal",
              main = name(idx[[i]])),
         error = function(e)
{
    ## Finding functions:
    pic <- getAnywhere(plotInternalConsistency)
    pwc <- getAnywhere(pairwiseConsistency)
    pin <- getAnywhere(plotinternal)
    pts <- getAnywhere(plotts)

    ## Method overriden?
    setMethod("plot", signature(x = "FLIndex", y = "missing"),
              function(x, type = c("splom"), ...)
    {
        ## The body of the plot method
        validObject(x)
        type <- type[1]

        res <- switch(type[1],
                      "splom" = pin$objs[[grepl(":FLCore", pin$where)]](x = x, ... ),
                      "ts" = pts$objs[[grepl(":FLCore", pts$where)]](x = x, ... ),
                      "pairwise" = pwc$objs[[grepl(":FLCore", pwc$where)]](idx = x, ...),
                      "internal" = pic$objs[[grepl(":FLCore", pic$where)]](idx = x, ...),
                      stop("type must be 'splom', 'ts', 'pairwise' or 'internal'!\n"))
        ## Return result invisibly
        invisible(res)
    })



    plot(idx[[i]],
         type = "internal",
         main = name(idx[[i]]))
})
## plotInternalConsistency(idx[["DATRAS Q3 3-8"]])
dev.off()


## catch composition by year (landings and discards)
lan <- as.data.frame(stk@landings)
lan$cat <- "landings"
dis <- as.data.frame(stk@discards)
dis$cat <- "discards"
df <- merge(lan, dis, all = TRUE)

## stacked absolute
png(file.path(reportDir, "landings+discards~year.png"), width = 6, height = 5, units = "in", res = 400)
p <- ggplot(data = df) + aes(x = year, y = data, fill = cat) +
    geom_bar( stat = "identity") # +
## scale_fill_brewer(palette = "Set1")
print(p)
dev.off()

## stacked relative
png(file.path(reportDir, "rel_landings+rel_discards~year.png"), width = 6, height = 5, units = "in", res = 400)
p <- ggplot(data = df) + aes(x = year, y = data, fill = cat) +
    geom_bar( stat = "identity", position = "fill")
print(p)
dev.off()

## Same with TAC overlaid:
TACs <- read.csv("bootstrap/data/TAC_series.csv")

## stacked absolute
png(file.path(reportDir, "landings+discards~year_TAC.png"), width = 6, height = 5, units = "in", res = 400)
p <- ggplot(data = df) +
    geom_bar( stat = "identity", aes(x = year, y = data, fill = cat)) +
    geom_point(data = TACs, aes(x = Year, y = TotCatchTAC, colour = "")) +
    geom_line(data = TACs, aes(x = Year, y = TotCatchTAC, colour = "")) +
    scale_linetype_manual(name = "TAC", values = 1) +
    scale_colour_manual(name = "TAC", values = "black") +
    scale_shape_manual(name = "TAC", values = 1)# +
print(p)
dev.off()


## landings.n at age #####
taf.png(file.path(reportDir, "landings_at_age.png"), width = 4, height = 6,
        units = "in", res = 400)
plot(stk@landings.n)
dev.off()

## bubble
df <- subset(as.data.frame(stk@landings.n), year > 1990)
png(file.path(reportDir, "landings_at_age_2.png"), width = 6, height = 5,
    units = "in", res = 400)
p <- ggplot(df, aes(x = year, y = age, size = data)) +
    geom_point(alpha = 0.3) +
    scale_size_continuous(trans = "identity", range = c(0, 10),
                          breaks = pretty(df$data, n = 10)) +
    labs( size = "Numbers" ) +
    ggtitle(label = "Landings at age [n]") +
    theme_bw()
print(p)
dev.off()

## discards.n at age #####
taf.png(file.path(reportDir, "discards_at_age.png"), width = 4, height = 6,
        units = "in", res = 400)
plot(stk@discards.n)
dev.off()

## bubble
df <- subset(as.data.frame(stk@discards.n), year > 1990)
png(file.path(reportDir, "discards_at_age_2.png"), width = 6, height = 5,
    units = "in", res = 400)
p <- ggplot(df, aes(x = year, y = age, size = data)) +
    geom_point(alpha = 0.3) +
    scale_size_continuous(trans = "identity", range = c(0, 10),
                          breaks = pretty(df$data, n = 10)) +
    labs( size = "Numbers" ) +
    ggtitle(label = "Discards at age [n]") +
    theme_bw()
print(p)
dev.off()


## landings.wt at age #####
df <- subset(as.data.frame(stk@landings.wt))
df$age <- factor(df$age, levels = 10:3)
png(file.path(reportDir, "landings.wt~year.png"), width = 6, height = 5,
    units = "in", res = 400)
p <- ggplot(df, aes(x = year, y = data, color = age)) +
    ## geom_line(aes(linetype = as.factor(age)), lwd = 1.25) +
    geom_line(lwd = 1) +
    scale_color_brewer(palette = "Set1") +
    ggtitle(label = "landings.wt at age") +
    ylim(c(0, NA)) +
    ylab("kg")
print(p)
dev.off()

## discards.wt at age #####
df <- as.data.frame(stk@discards.wt)
df$age <- factor(df$age, levels = 10:3)
png(file.path(reportDir, "discards.wt~age+year.png"), width = 6, height = 5,
    units = "in", res = 400)
p <- ggplot(df, aes(x = year, y = data, color = age)) +
    ## geom_line(aes(linetype = as.factor(age)), lwd = 1.25) +
    geom_line(lwd = 1) +
    scale_color_brewer(palette = "Set1") +
    ggtitle(label = "discards.wt at age") +
    ylim(c(0, NA)) +
    ylab("kg")
print(p)
dev.off()

## catch.wt at age #####
df <- as.data.frame(stk@catch.wt)
df$age <- factor(df$age, levels = 10:3)
png(file.path(reportDir, "catch.wt~age+year.png"), width = 6, height = 5,
    units = "in", res = 400)
p <- ggplot(df, aes(x = year, y = data, color = age)) +
    ## geom_line(aes(linetype = as.factor(age)), lwd = 1.25) +
    geom_line(lwd = 1) +
    scale_color_brewer(palette = "Set1") +
    ggtitle(label = "catch.wt at age") +
    ylim(c(0, NA)) +
    ylab("kg")
print(p)
dev.off()

## stock.wt at age #####
df <- as.data.frame(stk@stock.wt)
df$age <- factor(df$age, levels = 10:3)
png(file.path(reportDir, "stock.wt~age+year.png"), width = 6, height = 5,
    units = "in", res = 400)
p <- ggplot(df, aes(x = year, y = data, color = age)) +
    ## geom_line(aes(linetype = as.factor(age)), lwd = 1.25) +
    geom_line(lwd = 1) +
    scale_color_brewer(palette = "Set1") +
    ggtitle(label = "stock.wt at age") +
    ylim(c(0, NA)) +
    ylab("kg")
print(p)
dev.off()



## stock at age (lines) #####
df <- as.data.frame(stk@stock.wt * stk@stock.n)
df$age <- factor(df$age, levels = 10:3)
png(file.path(reportDir, "stock~year.png"), width = 7, height = 5,
    units = "in", res = 400)
p <- ggplot(df, aes(x = year, y = data, color = age)) +
    ## geom_line(aes(linetype = as.factor(age)), lwd = 1.25) +
    geom_line(lwd = 1) +
    scale_color_brewer(palette = "Set1") +
    ggtitle(label = "stock at age") +
    ylab("tonnes")
print(p)
dev.off()

## stock at age w/ ssb (stacked) #####
df <- as.data.frame(stk@stock.wt * stk@stock.n)
df$age <- factor(df$age, levels = 3:10)
df2 <- as.data.frame(ssb(stk))
df2$age <- factor(df2$age)
levels(df2$age) <- levels(df$age)[1]
png(file.path(reportDir, "stock~year_2.png"), width = 7, height = 5,
    units = "in", res = 400)
p <- ggplot(df, aes(x = year, y = data, fill = age)) +
    ## geom_line(aes(linetype = as.factor(age)), lwd = 1.25) +
    geom_area() +
    scale_fill_brewer(palette = "Spectral") +
    ggtitle(label = "Stock at age (solid line = SSB)") +
    ylab("tonnes") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .05))) +
    geom_line(data = df2, mapping = aes(x = year, y = data)) +
    geom_hline(yintercept = refPts$Btrigger, linetype = 2, color = "red")
print(p)
dev.off()

## index at age #####
df <- as.data.frame(idx[[1]]@index)
png(file.path(reportDir, "index_at_age.png"), width = 6, height = 5,
    units = "in", res = 400)
p <- ggplot(df, aes(x = year, y = data, color = as.factor(age))) +
    geom_line() +
    ## scale_color_brewer(palette = "Set1") +
    ggtitle(label = "DATRAS Q3 index at age")
print(p)
dev.off()


## index by age (facet) #####
taf.png(file.path(reportDir, "datras_by_age.png"), width = 5, height = 4, units = "in", res = 400)
plot(idx[[1]]@index)
dev.off()

## F at age #####
df <- as.data.frame(stk@harvest)
df$age <- factor(df$age, levels = 10:3)
png(file.path(reportDir, "F_at_age.png"), width = 6, height = 5, units = "in", res = 400)
p <- ggplot(df, aes(x = year, y = data, color = age)) +
    ## geom_line(aes(linetype = as.factor(age)), lwd = 1.25) +
    geom_line(lwd = 1) +
    scale_color_brewer(palette = "Set1") +
    geom_abline(intercept = 0.363, slope = 0, lty = 2) +
    ggtitle(label = "F at age")
print(p)
dev.off()


## Fscaled at age by year #####
df <- as.data.frame(apply(stk@harvest, 2, function(x){x / mean(x[2:5])}))
df <- subset(df, year >= assess_year - 10)
df$year <- factor(df$year)
png(file.path(reportDir, "Fscaled_at_age.png"), width = 6, height = 5, units = "in", res = 400)
p <- ggplot(df, aes(x = age, y = data, color = year)) +
    ## geom_line(aes(linetype = as.factor(age)), lwd = 1.25) +
    geom_line(lwd = 1) +
    ## scale_color_brewer(palette = "Set1") +
    ## geom_abline(intercept = 1, slope = 0, lty = 2) +
    ggtitle(label = "Scaled F at age")
print(p)
dev.off()



## survey plots #####
source("bootstrap/software/functions/cdd_survey_scripts.R")

tmp1 <- suppressWarnings(FLIndices(idx[[1]]))
tmp2 <- suppressWarnings(FLIndices(idx[grepl("Exploitable biomass", names(idx))]))

names(idx)

png(file.path(reportDir, "catchcurve_index.png"), width = 7, height = 6, units = "in", res = 400)
op <- par(mar = c(4, 4,2, 1), mfrow = c(2, 2))
single_cpue_plot_year(tmp1)
single_cpue_plot_cohort(tmp1)
single_catchcurve_index(tmp1)
single_cpue_plot_year(tmp2)
## single_catchcurve_index(tmp2)
par(op)
dev.off()

## single_cpue_plot_cohort(tmp)
## agerange <- lapply(idx, FUN = function(x){c(unlist(dims(x@index)[c("min", "max")]))})
## single_catchcurvegrad_index(idx, agerange)


## forecasted biomass by age #####
fwd_yrs <- assess_year:(assess_year + 2)
stk2 <- stf(stk, nyears = length(fwd_yrs), wts.nyears = 3, na.rm = TRUE)
plot(stk2)

fc <- FC[[1]] # Fsq, then Fmsy
cwbase <- c(tail(fit$data$catchMeanWeight, 1)) # weight of last assessment year
cwmean <- apply(fit$data$catchMeanWeight[tail(seq(nrow(fit$data$catchMeanWeight)), 3), , ], 2, mean) # mean of last 3 years
stk2@catch.wt[, ac(fwd_yrs)] <- cwmean

fc_fwd_yrs <- which(rownames(attr(fc, "tab")) %in% ac(fwd_yrs))

### get numbers at age for all forecast years
numbers <- lapply(fc_fwd_yrs, function(x)
           {
               ## index for numbers at age
               idx <- seq(length(fit$conf$keyLogFsta[1, ]))
               ## get simulated numbers
               n <- exp(fc[[x]]$sim[, idx])
               ## median
               apply(n, 2, median)
           })
numbers <- do.call(cbind, numbers)
stk2@stock.n[, ac(fwd_yrs)] <- numbers


for(y in seq(fwd_yrs)){
    yr <- ac(fwd_yrs[y])
    ## stk2@harvest[, yr] <- apply(fc[[fc_fwd_yrs[y]]]$catchatage, 1,median)
    stk2@catch.n[, yr] <- apply(fc[[fc_fwd_yrs[y]]]$catchatage, 1,median)
    stk2@landings.n[, yr] <- stk2@catch.n[, yr] * stk2@landings.n[, yr]
    stk2@discards.n[, yr] <- stk2@catch.n[, yr] * stk2@discards.n[, yr]
}

stk2@harvest[, ac(fwd_yrs)] <- computeHarvest(stk2[, ac(fwd_yrs)])
stk2@catch[, ac(fwd_yrs)] <- computeCatch(stk2[, ac(fwd_yrs)])
stk2@discards[, ac(fwd_yrs)] <- computeDiscards(stk2[, ac(fwd_yrs)])
stk2@landings[, ac(fwd_yrs)] <- computeLandings(stk2[, ac(fwd_yrs)])


## forecast stock at age w/ ssb (stacked) #####
df <- as.data.frame(stk2@stock.wt * stk2@stock.n)
df$age <- factor(df$age, levels = 3:10)
df2 <- as.data.frame(ssb(stk2))
df2$age <- factor(df2$age)
levels(df2$age) <- levels(df$age)[1]
png(file.path(reportDir, "stock~year_2+FC_Fmsy.png"), width = 7, height = 5,
    units = "in", res = 400)
p <- ggplot(df, aes(x = year, y = data, fill = age)) +
    ## geom_line(aes(linetype = as.factor(age)), lwd = 1.25) +
    geom_area() +
    scale_fill_brewer(palette = "Spectral") +
    ggtitle(label = "Stock at age with Fmsy forecast (solid line = SSB)") +
    ylab("tonnes") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .05))) +
    geom_line(data = df2, mapping = aes(x = year, y = data)) +
    geom_vline(xintercept = assess_year, linetype = 2) +
    geom_hline(yintercept = refPts$Btrigger, linetype = 2, color = "red")
print(p)
dev.off()

## ###########################################################################
## Data presentation plots:

## Maturity ogive:
matData <- maturity %>%
    tail(1) %>%
    select(-Year) %>%
    gather("Age", "Maturity") %>%
    mutate(Age = as.numeric(Age))

ggMat <- ggplot(data = matData, aes(x = Age, y = Maturity)) +
    geom_smooth(method = "glm",
                method.args = list(family = "binomial"),
                se = FALSE) +
    geom_point() +
    ylab("Prop. mature")

ggsave(ggMat,
       filename = file.path(reportDir, "maturity_model.png"),
       width = 5, height = 5, units = "cm", scale = 1.5)


## 8. Rmarkdown report ------------------------------------------------

rmarkdown::render(
               input = "utilities_report.Rmd",
               output_file = file.path(reportDir, "report.docx")
           )

## Few extra outputs:
tryCatch(source("Post_analyses.R"),
         error = function(e)
{
    message("No model output for previous year")
})

## Save session info:
sink(file = file.path(outputDir, "SessionInfo.txt"), type = c("output", "message"))
sessionInfo()
sink(file = NULL, type = c("output", "message"))


### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
