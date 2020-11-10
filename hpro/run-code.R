
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
query

## Join
tblName01 <- "FILGRUPPER"
tblName02 <- "KODEBOK"
varJoin <- "ALDER_ALLE"

DBI::dbGetQuery(db$dbconn, "select FILGRUPPE, DELID, TYPE from KODEBOK", n = 10)
DBI::dbGetQuery(db$dbconn, "select FILGRUPPE, ALDER_ALLE from FILGRUPPER", n = 10)

DBI::dbGetQuery(db$dbconn, "
SELECT KH_KODER.DEL,KODE, FORMAT, LABEL
FROM KH_KODER
INNER JOIN KH_DELER
ON KH_KODER.DEL=KH_DELER.DEL
WHERE TOTAL=1"
)

sql02 <- "
SELECT KH_KODER.DEL,KODE, FORMAT
FROM KH_KODER
INNER JOIN KH_DELER
ON KH_KODER.DEL=KH_DELER.DEL
WHERE TOTAL=1
"
DBI::dbGetQuery(db$dbconn, sql02)


tblCol <- c("DEL", "KODE", "FORMAT")
varJoin <- "DEL"
tbl1 <- "KH_KODER"
tbl2 <- "KH_DELER"

## ## Doesn't work with JOIN and glue
## ## for MS Access ie. DBI::ANSI()
## ## JOIN with DBI and glue
## cols <- list(
##   DBI::Id(table = tbl1, column = tblCol[1]),
##   DBI::Id(table = tbl2, column = tblCol[2]),
##   DBI::Id(table = tbl2, column = tblCol[3]),
##   DBI::Id(table = tbl1, column = "LABEL")
## )


## fg_db1 <- DBI::Id(table = tbl1, column = varJoin)
## fg_db2 <- DBI::Id(table = tbl2, column = varJoin)

## query2 <- glue::glue_sql("
##   SELECT {`cols`*}
##   FROM {`tbl1`}
##   JOIN {`tbl2`}
##   ON {`fg_db2`} = {`fg_db1`}",
##   .con = DBI::ANSI()
##   )

## query2

## DBI::dbGetQuery(db$dbconn, query2)



## With dbplyr
## -----------
library(dbplyr)
library(dplyr, warn.conflicts = FALSE)
fgpName
tblDB <- dplyr::tbl(db$dbconn, tblName);tblDB
tblDB %>% dplyr::select(tblCols)
tblDB %>% dplyr::filter(FILGRUPPE == "ARBLEDIGE")
dfDB <- tblDB %>% dplyr::filter(FILGRUPPE == fgpName);dfDB
dfDB %>% dplyr::show_query()
dim(dfDB)

dataDB <- dfDB %>% dplyr::collect()
dim(dataDB)

db$db_close()
## Will close connection when remove coz of finalize
rm(db)
gc()


## JOIN
DF <- tbl(db$dbconn, tbl2) %>%
  select("DEL", "TYPE", "FORMAT") %>%
  inner_join(tbl(db$dbconn, tbl1), by = c("DEL" = "DEL")) %>%
  show_query() %>%
  collect()

DF

DFF <- tbl(db$dbconn, tbl2) %>%
  select(DEL, TYPE, FORMAT) %>%
  inner_join(tbl(db$dbconn, tbl1), by = c("DEL")) %>%
  filter(TOTAL == 1)

DFF
dim(DFF)
dim(collect(DFF))



sql02 <- "
SELECT KH_KODER.DEL,KODE, FORMAT
FROM KH_KODER
INNER JOIN KH_DELER
ON KH_KODER.DEL=KH_DELER.DEL
WHERE TOTAL=1
"

db$dbconn %>%
  tbl(sql(sql02))


db$db_close()
db$db_connect()
