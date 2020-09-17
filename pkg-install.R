## install packages to the kh_renv
pkgs <- c("RODBC",
          "sas7bdat",
          "XML",
          "zoo",
          "plyr",
          "sqldf",
          "stringr",
          "intervals",
          "readxl")

sapply(pkgs, renv::install)
