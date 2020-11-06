
source("hpconnect-r6.R")

## Read DB
## -------

DB <- "c:/enc/DBtest/STYRING/KHELSA_dev.accdb"

db <- hp$new(DB)
db$dbname
db$db_connect()

DBI::dbGetQuery(db$dbconn, "SELECT TOP 10 * FROM KODEBOK")

tblName <- "KODEBOK"
tblCols <- c("FILGRUPPE", "DELID", "TYPE", "ORGKODE", "NYKODE")
query <- glue::glue_sql("SELECT TOP 10 {`tblCols`*} from {`tblName`}", .con = DBI::ANSI())
DBI::dbGetQuery(db$dbconn, query)

## Ops! when using WHERE using {`varName`} will give an error
fgpName <- "ARBLEDIGE"
query <- glue::glue_sql("SELECT {`tblCols`*} from {`tblName`} where FILGRUPPE={fgpName}", .con = DBI::ANSI())
DBI::dbGetQuery(db$dbconn, query)
## Alternative with fetching
dbq <- DBI::dbSendQuery(db$dbconn, query)
data1 <- DBI::dbFetch(dbq); data1
DBI::dbClearResult(dbq)


## With dbplyr
## -----------
library(dbplyr)
library(dplyr)
fgpName
tblDB <- dplyr::tbl(db$dbconn, tblName);tblDB
tblDB %>% dplyr::select(tblCols)
tblDB %>% dplyr::filter(FILGRUPPE == "ARBLEDIGE")
dfDB <- tblDB %>% dplyr::filter(FILGRUPPE == fgpName)
dfDB %>% dplyr::show_query()
dim(dfDB)

dataDB <- dfDB %>% dplyr::collect()
dim(dataDB)

db$db_close()
## Will close connection when remove coz of finalize
rm(db)
gc()


