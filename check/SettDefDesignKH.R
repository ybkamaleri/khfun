packages <- c("RODBC", "DBI", "here", "data.table")
sapply(packages, require, character.only = T)

db <- "c:/enc/DBtest/STYRING/KHELSA_dev.accdb"

