#-*- coding: utf-8 -*-

### File: Run.R
### Time-stamp: <2024-04-03 13:53:08 a23579>
###
### Created: 22/01/2024	16:38:49
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################

library(icesTAF)
icesTAF::clean()  # Remove working directories (force re-install everything).
taf.bootstrap(clean = TRUE)
sourceAll()







### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
