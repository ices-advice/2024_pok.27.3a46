# 2024_pok.27.3a46_assessment

2024 - Saithe (Pollachius virens) in Subareas 4, 6 and Division 3.a (North Sea, Rockall and West of Scotland, Skagerrak and Kattegat) - WGNSSK.

Based on WKBGAD 2024 benchmark.

Saithe assessment with SAM ('State-Space Assessment Model')

## How to run

Install the icesTAF package, version >=2.2 from CRAN.

Then open R in the `2024_pok.27.3a46` directory and run:

```
library(icesTAF)
## icesTAF::clean()  # Remove working directories (force re-install everything).
taf.bootstrap(clean = TRUE)
sourceAll()
```
