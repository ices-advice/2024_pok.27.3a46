#-*- coding: utf-8 -*-

### File: init.R
### Time-stamp: <2024-04-25 14:29:16 a23579>
###
### Created: 04/05/2021	15:07:47
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Global parameters for the model
####################################################################################################

outDirOverall <<- "./"
dir.create(outDirOverall, recursive = TRUE)

dataDir <- file.path(outDirOverall, "data")
modelDir <- file.path(outDirOverall, "model")
outputDir <- file.path(outDirOverall, "ouput")
reportDir <- file.path(outDirOverall, "report")


assess_year <- 2024 # the intermediate year when assessment is being conducted
advice_year <- 2025 # the year for TAC advice
reopening <- FALSE  # logical, is the assessment being done in fall,
                    #   with additional IBTS Q3 index added

forecastPrev <- "Fsq, then Fmsy" ## "TAC (2023), then MSY"
forecastNew <- "Fsq, then MSY"

## Forecast parameters:
NsimForecast <- 1e4
NRy <- 10 # Number of recruitment years for the forecast.

## total catch quota for assessment (intermediate) year:
TAC_assess_year <- 73815
## source:
## https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/970133/fisheries-agreed-record-trilateral-EU-NO-UK-210316.pdf

## Reference points:
Fmsy <- 0.316
Flim <- 0.464 ## should be 0.669 (as of 2021 estimates)/ 0.668 (2018 corrected))
Fpa <- 0.392  ## F05 (HCR).
Fp.05 <- 0.392
Bpa <- 180770
Blim <- 130090
Btrigger <- 180770
MAP.FMSY.lower <- 0.192
MAP.FMSY.upper <- 0.392 ## min (Fmsy upper = 0.412, Fp.05=0.392) (as of 2024 estimates)


source("bootstrap/software/functions/misc_functions.R")




### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
