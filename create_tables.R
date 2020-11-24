## Create Access Tables
## --------------------
library(here)
library(glue)
library(DBI)

accPath <- "C:\\enc\\DBtest\\STYRING/KHELSA_dev2.mdb"
db_con <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ="
cs <- paste0(db_con, accPath)
con <- dbConnect(odbc::odbc(), .connection_string = cs)

tabName <- "Ext_Args"

extSQL <- glue::glue('
CREATE TABLE {tabName} (
        "KOBLID"        INTEGER,
        "FILGRUPPE"     TEXT,
        "DELID"         TEXT,
        "FELTTYPE"      TEXT,
        "TYPE"	        TEXT,
        "ORG_KOL"	TEXT,
        "NY_KOL"	TEXT,
PRIMARY KEY (KOBLID)
);')

## Create Table
DBI::dbGetQuery(con, extSQL)

## Steps
## 1. Add columns EXT in ORIGINALFILER with TRUE / FALSE
## 2. When TRUE then read Ext_Args table
## 3. Read KODEBOK table for coding

#########################
## Get info from Ext_Args

## Felttype = EXT (extended)
