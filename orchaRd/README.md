## Introducing the Orchard Plot for Meta-analysis

# Citing orchaRd

To cite `orchaRd` in publications one can use the following reference:

Nakagawa, S. et al. 2020. The Orchard Plot: Cutlivatinng the Forest Plot 
for Use in Ecology,  Evolution and Beyond. Research Synthesis Methods, in review

# Installation

To install `orchaRd` use the following code in R:

```
install.packages("devtools")
install.packages("tidyverse")
install.packages("metafor")
install.packages("patchwork")
devtools::install_github("itchyshin/orchard_plot", subdir = "orchaRd", force = TRUE, build_vignettes = TRUE)

library(orchaRd)
library(patchwork)
library(tidyverse)
library(metafor)
```
