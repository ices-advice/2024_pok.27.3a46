#-*- coding: utf-8 -*-

### File: WGMIXFISH_exports.R
### Time-stamp: <2024-04-25 13:58:02 a23579>
###
### Created: 25/04/2024	13:05:44
### Author: Yves Reecht, based on Marc Taylor's scripts
###
####################################################################################################
### Description:
###
### Export stock objects for WGMIXFISH
####################################################################################################

taf.library(FLfse)
library(ggplotFL)

stock_input <- FLfse::SAM2FLStock(fit)

## FLStock objects for MIXFISH:
## ... data (+ some results):
save(stock_input,
     file = paste0(file.path(modelDir, "Pok.27.3a46_WGNSSK_"),
                   assess_year, "_FLStock_input.RData"))

## ... with estimated catches (and no uncertainty):
stock_estimated <- FLfse::SAM2FLStock(fit, catch_estimate=TRUE## ,
                                      ## ## mat_est=TRUE, stock.wt_est=TRUE,
                                      ## ## catch.wt_est=TRUE, m_est=TRUE,
                                      ## uncertainty = TRUE
                                      )
## range(stkEst)["plusgroup"] <- range(stkEst)["max"]

save(stock_estimated,
     file = paste0(file.path(modelDir, "Pok.27.3a46_WGNSSK_"),
                   assess_year, "_FLStock_estimated.RData"))

## plot comparison using ggplotFL
L <- FLStocks(obs = stock_input, est = stock_estimated)
plot(L)

## ##################################################
## Quality checks:
QCfile <- file.path(modelDir,
                    paste0("Pok.27.3a46_WGNSSK_",
                           assess_year,
                           "_FLStock_export_QC.txt"))

cat(paste0("## QC Pok.27.3a46 ",
           assess_year,
           " FLStock export\n\n"),
    file = QCfile)

# check that sum of product (SOP) calculations equal (or close to)
# aggregate slots (in the best case, all.equal is TRUE)
if (! all.equal(c(discards(stock_estimated)), c(computeDiscards(stock_estimated))))
{
    warning("FLStock export: discards estimates do not match SOPs")
    cat("Warning: x discards estimates do not match SOPs!\n\n",
        file = QCfile, append = TRUE)
}else{
    cat("         v Discards estimates match SOPs!\n\n",
        file = QCfile, append = TRUE)
}

if (! all.equal(c(landings(stock_estimated)), c(computeLandings(stock_estimated))))
{
    warning("FLStock export: landings estimates do not match SOPs")
    cat("Warning: x landings estimates do not match SOPs!\n\n",
        file = QCfile, append = TRUE)
}else{
    cat("         v Landings estimates match SOPs!\n\n",
        file = QCfile, append = TRUE)
}

if (! all.equal(c(catch(stock_estimated)), c(computeCatch(stock_estimated))))
{
    warning("FLStock export: catch estimates do not match SOPs")
    cat("Warning: x catch estimates do not match SOPs!\n\n",
        file = QCfile, append = TRUE)
}else{
    cat("         v Catch estimates match SOPs!\n\n",
        file = QCfile, append = TRUE)
}

if (! all.equal(c(stock(stock_estimated)), c(computeStock(stock_estimated))))
{
    warning("FLStock export: stock estimates do not match SOPs")
    cat("Warning: x stock estimates do not match SOPs!\n\n",
        file = QCfile, append = TRUE)
}else{
    cat("         v Stock estimates match SOPs!\n\n",
        file = QCfile, append = TRUE)
}

## check that the weighted mean of landings.wt and discards.wt equals
## (or is close to) catch.wt
df <- as.data.frame(stock_estimated)
df2 <- reshape2::dcast(data = df, formula = year ~ slot, value.var = "data")
df2$catch.wt.calc <- apply(df2, 1, FUN = function(x)
                     {
                         weighted.mean(x = c(x["landings.wt"], x["discards.wt"]),
                                       w = c(x["landings.n"], x["discards.n"]))
                     })
if (! all.equal(c(df2$catch.wt), c(df2$catch.wt.calc)))
{
    warning("FLStock export: catch weights inconsistent with landing and discard weights")
    cat("Warning: x  catch weights inconsistent with landing and discard weights!\n\n",
        file = QCfile, append = TRUE)
}else{
    cat("         v catch weights consistent with landing and discard weights!\n\n",
        file = QCfile, append = TRUE)
}


### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
