## -------------------------------
## install packages with renv 
## --------------------------------
pkgs <- c("RODBC",
          "sas7bdat",
          "XML",
          "zoo",
          "plyr",
          "sqldf",
          "stringr",
          "intervals",
          "readxl",
          "assertthat",
          "backports",
          "BH",
          "bit",
          "bit64",
          "blob",
          "cellranger",
          "chron",
          "cli",
          "pillar",
          "usethis",
          "data.table",
          "fs",
          "here",
          "glue",
          "logger")

sapply(pkgs, renv::install)

## renv::install("remotes")

renv::remove("crayon")

## ------------------------
## Regular install packages
## ------------------------

## Required packages
pkgs <- c("data.table", "epitools")

## Sjekke hvis finnes og installere hvis ikke finnes
nypkg <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nypkg)) install.packages(nypkg)

sapply(pkgs, require, character.only = TRUE)
