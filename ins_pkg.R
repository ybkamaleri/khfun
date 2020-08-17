## Required packages
pkgs <- c("data.table", "epitools")

## Sjekke hvis finnes og installere hvis ikke finnes
nypkg <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nypkg)) install.packages(nypkg)

sapply(pkgs, require, character.only = TRUE)
