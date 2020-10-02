dbtest <- function(test = FALSE){

  if (!require(RODBC)) install.packages("RODBC")
  if (!require(data.table)) install.packages("data.table")

 if( test){
   dbfile <- "C:\\enc\\DBtest\\STYRING\\KHELSA_dev2.accdb"
 } else {
   dbfile <- "F:/Prosjekter/Kommunehelsa/PRODUKSJON/STYRING/KHELSA.mdb"
 }

  orgTb <- "ORIGINALFILER"
  khcon <- RODBC::odbcConnectAccess2007(dbfile)

  orgDT <- setDT(RODBC::sqlFetch(khcon, orgTb))

  head(orgDT)
}

dbtest()
dbtest(TRUE)



## Check ODBC
odbc::odbcListDrivers()
sort(unique(odbcListDrivers()[[1]]))


## TEST
library(RODBC)
## Set up driver info and database path
DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ="

## accdb file 
MDBPATH <- "C:\\enc\\DBtest\\STYRING\\KHELSA_dev2.accdb"
PATH <- paste0(DRIVERINFO, MDBPATH)

## mdb file
MDBPATH <- "C:\\enc\\DBtest\\STYRING\\KHELSA_dev2.mdb"
PATH <- paste0(DRIVERINFO, MDBPATH)

## Establish connection
channel <- RODBC::odbcDriverConnect(PATH)

## Load data into R dataframe
RODBC::sqlQuery(channel, "SELECT TOP 5 * FROM KH_DELER")
RODBC::sqlFetch(channel, "KH_DELER", max = 5)

## Close and remove channel
close(channel)
rm(channel)







library("DBI")
## With odbc and DBI
pkg <- c("odbc", "DBI")
sapply(pkg, require, character.only = TRUE)

db_con <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ="

## accdb file 
MDBPATH <- "C:\\enc\\DBtest\\STYRING\\KHELSA_dev2.accdb"

## mdb file
MDBPATH <- "C:\\enc\\DBtest\\STYRING\\KHELSA_dev.mdb"

MDBPATH
cs <- paste0(db_con, MDBPATH)
con <- dbConnect(odbc::odbc(), .connection_string = cs)

DBI::dbGetQuery(con, "SELECT TOP 5 * FROM KH_DELER")


## Connection Strings
MDBPATH
con <- DBI::dbConnect(odbc::odbc(),
                      driver = "Microsoft Access Driver (*.mdb, *.accdb)",
                      database = MDBPATH)
