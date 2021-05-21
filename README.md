[![DOI](https://zenodo.org/badge/227451114.svg)](https://zenodo.org/badge/latestdoi/227451114)

## Introducing the Orchard Plot for Meta-analysis

# Citing orchaRd

To cite `orchaRd` in publications one can use the following reference:

Nakagawa, S., Lagisz, M., O'Dea, R. E., Rutkowska, J., Yang, Y., Noble, D. W., & Senior, A. M. (2020). The Orchard Plot: Cultivating a Forest Plot for Use in Ecology, Evolution and Beyond. *Research Synthesis Methods* https://doi.org/10.1002/jrsm.1424 (*EcoEvoRxiv* https://doi.org/10.32942/osf.io/epqa7)

# Installation

To install `orchaRd` use the following code in R:

```
install.packages("devtools")
install.packages("tidyverse")
install.packages("metafor")
install.packages("patchwork")
install.packages("R.rsp")

devtools::install_github("itchyshin/orchard_plot", subdir = "orchaRd", force = TRUE, build_vignettes = TRUE)

library(orchaRd)
library(patchwork)
library(tidyverse)
library(metafor)
```
# Vignette

To see the vignette for `orchaRd`, run the code:

```
vignette("orchaRd")
```
