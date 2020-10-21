## Check different files produced by LagFilgruppe or LagKUBE
## .........................................................

KubePath <- "C:/enc/DBtest/PRODUKTER/KUBER/KOMMUNEHELSA/NYESTE/R"

dt <- readRDS(file.path(KubePath, "ARBLEDIGE.rds"))
head(dt)
