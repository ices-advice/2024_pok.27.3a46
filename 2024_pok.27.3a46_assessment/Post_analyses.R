#-*- coding: utf-8 -*-

### File: tmp.R
### Time-stamp: <2024-05-08 15:38:13 a23579>
###
### Created: 22/09/2022	09:09:20
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

source("init.R")

## source("./bootstrap/software/functions/misc_functions.R")

if (! exists("assess_year"))
{
    assess_year <- 2024
}


## Extract forecasted numbers at age:
load(file.path(modelDir, "forecast.RData"))
load(file.path(modelDir, "fit.RData"))

## ##################################################
## Load old SA results:

files <- c("./SA_y-1/forecast.RData", "./SA_y-1/fit.RData")

old <- lapply(files, function(x) mget(load(x)))

fitOld <- old[[2]]$fit
FCold <- old[[1]]$FC

rm(old)

## ## Years (est. + forecasts)
## fitYears <- fit$data$years
## fitOldYears <- fitOld$data$years

## fcYears <- sapply(FC[[3]], function(x) x$year)
## fcOldYears <- sapply(FCold[[3]], function(x) x$year)

## Extract old SA numbers at age:
## forecastPrev <- "TAC (2023), then MSY"
FC_old <- FCold[[forecastPrev]]

## Extract new SA numbers at age:
## forecastNew <- "Fsq, then Fmsy"
FC_new <- FC[[forecastNew]]

NatAgeOld <- get_natage(FC_old)
NatAgeNew <- get_natage(FC_new)

WatAgeOld <- get_watage(FC_old, type = "stock")
WatAgeNew <- get_watage(FC_new, type = "stock")

FatAgeOld <- get_fatage(FC_old)
FatAgeNew <- get_fatage(FC_new)

MOatAgeOld <- get_moatage(FC_old)
MOatAgeNew <- get_moatage(FC_new)


## Ratios for a range of years:
years <- as.character(assess_year + c(-8:1)) #2015:2024)
cbind(NatAgeOld[ , 1, drop = FALSE],
      round(NatAgeNew[, years] / NatAgeOld[, years], 2)) %>%
    write_csv(file = file.path(outputDir, "Changes_N_at_age.csv"))

years <- as.character(assess_year + c(-4:1))
cbind(NatAgeOld[ , 1, drop = FALSE],
      round(NatAgeNew[, years] / NatAgeOld[, years], 2))

## Ratio w at age:
years <- as.character(assess_year + c(-8:1))
cbind(data.frame(Age = row.names(WatAgeOld)),
      round(WatAgeNew[, years] / WatAgeOld[, years], 2)) %>%
    write_csv(file = file.path(outputDir, "Changes_W_at_age.csv"))

years <- as.character(assess_year + c(-4:1))
cbind(data.frame(Age = row.names(WatAgeOld)),
      round(WatAgeNew[, years] / WatAgeOld[, years], 2))

## Ratio SSB at age:
years <- as.character(assess_year + c(-8:1))

SSBa <- sweep(as.matrix(NatAgeNew[, years] * WatAgeNew[, years]),
                  1, tail(fit$data$propMat, 1), "*")

cbind(data.frame(Age = row.names(WatAgeOld)),
      round(sweep(as.matrix(NatAgeNew[, years] * WatAgeNew[, years]),
                  1, tail(fit$data$propMat, 1), "*") /
            sweep(as.matrix(NatAgeOld[, years] * WatAgeOld[, years]),
                  1, tail(fitOld$data$propMat, 1), "*"), 2),
      "",
      Prop_SSB_2021_23 = round(apply(SSBa[ , as.character(assess_year + c(-2:0))], 1, sum) /
                           sum(SSBa[ , as.character(assess_year + c(-2:0))]), 2)) %>%
    write_csv(file = file.path(outputDir, "Changes_SSB_at_age.csv"))


years <- as.character(assess_year + c(-4:1))
cbind(data.frame(Age = row.names(WatAgeOld)),
      sweep(as.matrix(NatAgeNew[, years] * WatAgeNew[, years]),
            1, tail(fit$data$propMat, 1), "*") /
      sweep(as.matrix(NatAgeOld[, years] * WatAgeOld[, years]),
            1, tail(fitOld$data$propMat, 1), "*"))

## Plots based on Harriet's code:
yearRange <- assess_year + c(-7:1)
repYears <- assess_year + c(-4:1)

dfNatAge <- NatAgeNew[ , c("Age", yearRange)] %>%
    mutate(WG = paste0("WGNSSK ", assess_year)) %>%
    pivot_longer(any_of(as.character(yearRange)), names_to = "Year", values_to = "N") %>%
    left_join(attr(NatAgeNew, "estType")) %>%
    bind_rows(NatAgeOld[ , c("Age", yearRange)] %>%
              mutate(WG = paste0("WGNSSK ", assess_year - 1)) %>%
              pivot_longer(any_of(as.character(yearRange)), names_to = "Year", values_to = "N") %>%
              left_join(attr(NatAgeOld, "estType"))) %>%
    mutate(Age_num = as.numeric(sub("+", "", Age, fixed = TRUE)),
           EstType = factor(EstType,
                            levels = c("Estimate", "STF, intermediate year",
                                       "STF, advice year", "STF")))

ggplot(data = dfNatAge %>% filter(Year %in% repYears)) +
    geom_point(aes(x = Age_num, y = N, colour = WG,
                   shape = EstType, size = EstType)) +
    geom_line(aes(x = Age_num, y = N, colour = WG)) +
    facet_wrap(~Year) +
    scale_size_manual(name = "Estimate type", values = c(1, 3, 3, 3)) +
    scale_shape_manual(name = "Estimate type", values = c(19, 2, 4, 5)) +
    ylab("N at age (thousands)") +
    scale_x_continuous(breaks = unique(dfNatAge$Age_num),
                       labels = unique(dfNatAge$Age),
                       name = "Age") +
    theme_bw()

ggsave(file = file.path(reportDir, paste0("N_at_age_WG_", assess_year, "-", assess_year - 1, ".png")),
       width = 11, height = 5, scale = 0.9)

dfWatAge <- WatAgeNew[ , c("Age", yearRange)] %>%
    mutate(WG = paste0("WGNSSK ", assess_year)) %>%
    pivot_longer(any_of(as.character(yearRange)), names_to = "Year", values_to = "mean_Wg") %>%
    left_join(attr(WatAgeNew, "estType")) %>%
    bind_rows(WatAgeOld[ , c("Age", yearRange)] %>%
              mutate(WG = paste0("WGNSSK ", assess_year - 1)) %>%
              pivot_longer(any_of(as.character(yearRange)), names_to = "Year", values_to = "mean_Wg") %>%
              left_join(attr(WatAgeOld, "estType"))) %>%
    mutate(Age_num = as.numeric(sub("+", "", Age, fixed = TRUE)),
           EstType = factor(EstType,
                            levels = c("Estimate", "STF, intermediate year",
                                       "STF, advice year", "STF")))

ggplot(data = dfWatAge %>% filter(Year %in% (assess_year + c(-1:1)))) +
    geom_point(aes(x = Age_num, y = mean_Wg, colour = WG,
                   shape = EstType, size = EstType)) +
    geom_line(aes(x = Age_num, y = mean_Wg, colour = WG)) +
    facet_wrap(~Year) +
    scale_size_manual(name = "Estimate type", values = c(1, 3, 3, 3)) +
    scale_shape_manual(name = "Estimate type", values = c(19, 2, 4, 5)) +
    ylab("mean weight at age (kg)") +
    scale_x_continuous(breaks = unique(dfWatAge$Age_num),
                       labels = unique(dfWatAge$Age),
                       name = "Age") +
    theme_bw()

ggsave(file = file.path(reportDir, paste0("W_at_age_WG_", assess_year, "-", assess_year - 1, ".png")),
       width = 11, height = 4, scale = 0.9)

## SSB:
SSBnew <- cbind(data.frame(Age = row.names(WatAgeNew)),
                sweep(as.matrix(NatAgeNew[, as.character(yearRange)] *
                                WatAgeNew[, as.character(yearRange)]),
                      1, tail(fit$data$propMat, 1), "*"))

SSBold <- cbind(data.frame(Age = row.names(WatAgeOld)),
                sweep(as.matrix(NatAgeOld[, as.character(yearRange)] *
                                WatAgeOld[, as.character(yearRange)]),
                      1, tail(fitOld$data$propMat, 1), "*"))

dfSSBatAge <- SSBnew[ , c("Age", yearRange)] %>%
    mutate(WG = paste0("WGNSSK ", assess_year)) %>%
    pivot_longer(any_of(as.character(yearRange)), names_to = "Year", values_to = "SSB") %>%
    left_join(attr(WatAgeNew, "estType")) %>%
    bind_rows(SSBold[ , c("Age", yearRange)] %>%
              mutate(WG = paste0("WGNSSK ", assess_year - 1)) %>%
              pivot_longer(any_of(as.character(yearRange)), names_to = "Year", values_to = "SSB") %>%
              left_join(attr(WatAgeOld, "estType"))) %>%
    mutate(Age_num = as.numeric(sub("+", "", Age, fixed = TRUE)),
           EstType = factor(EstType,
                            levels = c("Estimate", "STF, intermediate year",
                                       "STF, advice year", "STF")))

ggplot(data = dfSSBatAge %>% filter(Year %in% repYears)) +
    geom_point(aes(x = Age_num, y = SSB, colour = WG,
                   shape = EstType, size = EstType)) +
    geom_line(aes(x = Age_num, y = SSB, colour = WG)) +
    facet_wrap(~Year) +
    scale_size_manual(name = "Estimate type", values = c(1, 3, 3, 3)) +
    scale_shape_manual(name = "Estimate type", values = c(19, 2, 4, 5)) +
    ylab("SSB at age (tonnes)") +
    scale_x_continuous(breaks = unique(dfWatAge$Age_num),
                       labels = unique(dfWatAge$Age),
                       name = "Age") +
    theme_bw()

ggsave(file = file.path(reportDir, paste0("SSB_at_age_WG_", assess_year, "-", assess_year - 1, ".png")),
       width = 11, height = 5, scale = 0.9)

## F at age

dfFatAge <- FatAgeNew[ , c("Age", yearRange)] %>%
    mutate(WG = paste0("WGNSSK ", assess_year)) %>%
    pivot_longer(any_of(as.character(yearRange)), names_to = "Year", values_to = "F") %>%
    left_join(attr(FatAgeNew, "estType")) %>%
    bind_rows(FatAgeOld[ , c("Age", yearRange)] %>%
              mutate(WG = paste0("WGNSSK ", assess_year - 1)) %>%
              pivot_longer(any_of(as.character(yearRange)), names_to = "Year", values_to = "F") %>%
              left_join(attr(FatAgeOld, "estType"))) %>%
    mutate(Age_num = as.numeric(sub("+", "", Age, fixed = TRUE)),
           EstType = factor(EstType,
                            levels = c("Estimate", "STF, intermediate year",
                                       "STF, advice year", "STF"))) %>%
    group_by(WG, Year, EstType, Age_num) %>%
    group_modify(~ {
        if (.x$Age == "9")
        {
            .x[c(1, 1), ] %>%
                mutate(Age = c("9", "10+"))
        }else{.x}
    }) %>%
    ungroup() %>%
    mutate(Age_num = as.numeric(sub("+", "", Age, fixed = TRUE)))

ggplot(data = dfFatAge %>% filter(Year %in% repYears)) +
    geom_point(aes(x = Age_num, y = F, colour = WG,
                   shape = EstType, size = EstType)) +
    geom_line(aes(x = Age_num, y = F, colour = WG)) +
    facet_wrap(~Year) +
    scale_size_manual(name = "Estimate type", values = c(1, 3, 3, 3)) +
    scale_shape_manual(name = "Estimate type", values = c(19, 2, 4, 5)) +
    ylab("F at age") +
    scale_x_continuous(breaks = unique(dfFatAge$Age_num),
                       labels = unique(dfFatAge$Age),
                       name = "Age") +
    theme_bw()

ggsave(file = file.path(reportDir, paste0("F_at_age_WG_", assess_year, "-", assess_year - 1, ".png")),
       width = 11, height = 5, scale = 0.9)

ggplot(data = dfFatAge %>%
           filter(Year >= assess_year - 2, Year < assess_year + 1) %>%
           group_by(Year, WG) %>%
           mutate(Fscaled = F / max(F)) %>%
           ungroup() %>% filter(Year %in% repYears)) +
    geom_point(aes(x = Age_num, y = Fscaled, colour = WG,
                   shape = EstType, size = EstType)) +
    geom_line(aes(x = Age_num, y = Fscaled, colour = WG),
              size = 0.8) +
    facet_wrap(~Year) +
    scale_size_manual(name = "Estimate type", values = c(1, 3, 3, 3)) +
    scale_shape_manual(name = "Estimate type", values = c(19, 2, 4, 5)) +
    ylab("scaled F at age") +
    scale_x_continuous(breaks = unique(dfFatAge$Age_num),
                       labels = unique(dfFatAge$Age),
                       name = "Age") +
    theme_bw()

ggsave(file = file.path(reportDir, paste0("F_at_age_scaled_WG_", assess_year, "-", assess_year - 1, ".png")),
       width = 11, height = 4, scale = 0.9)

ggplot(data = dfFatAge %>%
           filter(Year >= assess_year - 2, Year < assess_year + 1) %>%
           group_by(Year, WG) %>%
           mutate(Fscaled = F / max(F)) %>%
           ungroup()) +
    geom_line(aes(x = Age_num, y = Fscaled, colour = Year,
                  linetype = WG), size = 0.8) +
    geom_point(aes(x = Age_num, y = Fscaled, colour = Year,
                   shape = EstType, size = EstType)) +
    facet_wrap(~Year) +
    scale_size_manual(name = "Estimate type", values = c(1, 3, 3, 3)) +
    scale_shape_manual(name = "Estimate type", values = c(19, 2, 4, 5)) +
    ylab("scaled F at age") +
    scale_x_continuous(breaks = unique(dfFatAge$Age_num),
                       labels = unique(dfFatAge$Age),
                       name = "Age") +
    scale_linetype_manual(values = c("dashed", "solid")) +
    theme_bw()

ggsave(file = file.path(reportDir, paste0("F_at_age_scaled_WG_", assess_year, "-", assess_year - 1, "_last.png")),
       width = 11, height = 5, scale = 0.9)

ggplot(data = dfFatAge %>%
           filter(Year >= assess_year - 2, Year < assess_year + 1) %>%
           group_by(Year, WG) %>%
           mutate(Fscaled = F / max(F)) %>%
           ungroup()) +
    geom_line(aes(x = Age_num, y = Fscaled, colour = Year,
                  linetype = WG), size = 0.8) +
    geom_point(aes(x = Age_num, y = Fscaled, colour = Year,
                   shape = EstType, size = EstType)) +
    ## facet_wrap(~Year) +
    scale_size_manual(name = "Estimate type", values = c(1, 3, 3, 3)) +
    scale_shape_manual(name = "Estimate type", values = c(19, 2, 4, 5)) +
    ylab("scaled F at age") +
    scale_x_continuous(breaks = unique(dfFatAge$Age_num),
                       labels = unique(dfFatAge$Age),
                       name = "Age") +
    scale_linetype_manual(values = c("dashed", "solid")) +
    theme_bw()

## color matrices:
dfNatAgeW <- dfNatAge %>%
    select(-EstType) %>%
    pivot_wider(names_from = WG, values_from = "N",
                names_repair = ~gsub("[[:blank:]]", "_", .x),
                names_prefix = "N_") %>%
    ## mutate(ratio = N_WGNSSK_2023 / N_WGNSSK_2022)
    mutate(ratio = !!sym(paste0("N_WGNSSK_", assess_year)) / !!sym(paste0("N_WGNSSK_", assess_year - 1)))

ggplot(data = dfNatAgeW) +
    geom_tile(aes(x = Year, y = Age_num, fill = ratio)) +
    scale_fill_gradient2(midpoint = 1, high = "darkgreen",
                         limits = c(0.5, 2), trans = "sqrt",
                         name = paste0(" Ratio N\n WG ",
                                       assess_year,
                                       "/", assess_year - 1),
                         oob = scales::squish) +
    geom_text(aes(x = Year, y = Age_num, label = round(ratio, 2)),
              colour = "grey50", size = 2.5) +
    scale_y_continuous(breaks = unique(dfNatAgeW$Age_num),
                       labels = unique(dfNatAgeW$Age),
                       name = "Age") +
    theme_bw()

ggsave(file = file.path(reportDir, paste0("N_at_age_WG_", assess_year, "-", assess_year - 1, "_matrix.png")),
       width = 11, height = 5, scale = 0.65)


dfSSBatAgeW <- dfSSBatAge %>%
    select(-EstType) %>%
    pivot_wider(names_from = WG, values_from = "SSB",
                names_repair = ~gsub("[[:blank:]]", "_", .x),
                names_prefix = "SSB_") %>%
    ## mutate(ratio = SSB_WGNSSK_2023 / SSB_WGNSSK_2022)
    mutate(ratio = !!sym(paste0("SSB_WGNSSK_", assess_year)) / !!sym(paste0("SSB_WGNSSK_", assess_year - 1)))

ggplot(data = dfSSBatAgeW) +
    geom_tile(aes(x = Year, y = Age_num, fill = ratio)) +
    scale_fill_gradient2(midpoint = 1, high = "darkgreen",
                         limits = c(0.5, 2), trans = "sqrt",
                         name = paste0("Ratio SSB\n WG ",
                                       assess_year,
                                       "/", assess_year - 1),
                         na.value = "grey80",
                         oob = scales::squish) +
    geom_text(aes(x = Year, y = Age_num, label = round(ratio, 2)),
              colour = "grey50", size = 2.5) +
    scale_y_continuous(breaks = unique(dfSSBatAgeW$Age_num),
                       labels = unique(dfSSBatAgeW$Age),
                       name = "Age") +
    theme_bw()


ggsave(file = file.path(reportDir, paste0("SSB_at_age_WG_", assess_year, "-", assess_year - 1, "_matrix.png")),
       width = 11, height = 5, scale = 0.65)



dfFatAgeW <- dfFatAge %>%
    select(-EstType) %>%
    pivot_wider(names_from = WG, values_from = "F",
                names_repair = ~gsub("[[:blank:]]", "_", .x),
                names_prefix = "F_") %>%
    ## mutate(ratio = F_WGNSSK_2023 / F_WGNSSK_2022)
    mutate(ratio = !!sym(paste0("F_WGNSSK_", assess_year)) / !!sym(paste0("F_WGNSSK_", assess_year - 1)))

ggplot(data = dfFatAgeW) +
    geom_tile(aes(x = Year, y = Age_num, fill = ratio)) +
    scale_fill_gradient2(midpoint = 1, low = "darkgreen",
                         high = "darkred",
                         limits = c(0.5, 2), trans = "sqrt",
                         name = paste0(" Ratio F\n WG ",
                                       assess_year,
                                       "/", assess_year - 1),
                         oob = scales::squish) +
    geom_text(aes(x = Year, y = Age_num, label = round(ratio, 2)),
              colour = "grey50", size = 2.5) +
    scale_y_continuous(breaks = unique(dfFatAgeW$Age_num),
                       labels = unique(dfFatAgeW$Age),
                       name = "Age") +
    theme_bw()

ggsave(file = file.path(reportDir, paste0("F_at_age_WG_", assess_year, "-", assess_year - 1, "_matrix.png")),
       width = 11, height = 5, scale = 0.65)

## Maturity ogive comparison:
dfMOatAge <- MOatAgeNew[ , c("Age", yearRange)] %>%
    mutate(WG = paste0("WGNSSK ", assess_year)) %>%
    pivot_longer(any_of(as.character(yearRange)), names_to = "Year", values_to = "MO") %>%
    left_join(attr(MOatAgeNew, "estType")) %>%
    bind_rows(MOatAgeOld[ , c("Age", yearRange)] %>%
              mutate(WG = paste0("WGNSSK ", assess_year - 1)) %>%
              pivot_longer(any_of(as.character(yearRange)), names_to = "Year", values_to = "MO") %>%
              left_join(attr(MOatAgeOld, "estType"))) %>%
    mutate(Age_num = as.numeric(sub("+", "", Age, fixed = TRUE)),
           EstType = factor(EstType,
                            levels = c("Estimate", "STF, intermediate year",
                                       "STF, advice year", "STF")))

ggplot(data = dfMOatAge %>% filter(Year %in% repYears)) +
    geom_point(aes(x = Age_num, y = MO, colour = WG,
                   shape = EstType, size = EstType)) +
    geom_line(aes(x = Age_num, y = MO, colour = WG)) +
    facet_wrap(~Year) +
    scale_size_manual(name = "Estimate type", values = c(1, 3, 3, 3)) +
    scale_shape_manual(name = "Estimate type", values = c(19, 2, 4, 5)) +
    ylab("Proportion mature at age") +
    scale_x_continuous(breaks = unique(dfMOatAge$Age_num),
                       labels = unique(dfMOatAge$Age),
                       name = "Age") +
    theme_bw()

ggsave(file = file.path(reportDir, paste0("MO_at_age_WG_", assess_year, "-", assess_year - 1, ".png")),
       width = 11, height = 5, scale = 0.9)


## get_natage(FC[[ii]])

## x <- FC[[ii]]

## str(x)
## str(fittmp)

## exp(fittmp$pl$logF)

## str(FC[[ii]][[2]])

## fittmp$conf$keyLogFsta
## ## fittmp$conf$keyLogFpar
## ## fittmp$conf$keyVarF
## fittmp$conf$fbarRange


## str(FC[[ii]][[1]])

## fittmp$data$stockMeanWeight


## ## TSB
## apply(get_natage(FC[[ii]])[ , -1] * get_watage(FC[[ii]])[ , -1], 2, sum)

## str(fittmp)

## ## SSB:
## tail(fittmp$data$propMat, 1) %*% as.matrix(get_natage(FC[[ii]])[ , -1] * get_watage(FC[[ii]])[ , -1])

## SSBa <- sweep(as.matrix(get_natage(FC[[ii]])[ , -1] * get_watage(FC[[ii]])[ , -1]),
##               1, STAT = tail(fittmp$data$propMat, 1), FUN = "*")


## apply(SSBa[ , as.character(2021:2023)], 1, sum) /
##     sum(SSBa[ , as.character(2021:2023)])

## disMeanWeight, landMeanWeight, catchMeanWeight

## str(FC[[i]])

## str(FC[[i]][[2]])

## head(FC[[i]][[2]]$sim)

## apply(FC[[i]][[2]]$sim, 2, function(x) summary(exp(x)))

### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
