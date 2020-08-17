## Check output from TEST function
## ------------------------------

## Choose the most relevant for path
## Path for Windows
rdsPath <- "c:/enc/DBtest/PRODUKTER/MELLOMPROD/R/STABLAORG/NYESTE"

gsub("\\\\", "/", readClipboard())
rdsPath <- "F:/Prosjekter/Kommunehelsa/PRODUKSJON/PRODUKTER/MELLOMPROD/R/STABLAORG/NYESTE"

## Path for linux
rdsPath <- c(
  "F:/Prosjekter/Kommunehelsa/PRODUKSJON/PRODUKTER/MELLOMPROD/R/STABLAORG/NYESTE",
  "F:/Prosjekter/Kommunehelsa/PRODUKSJON/PRODUKTER/MELLOMPROD/R/STABLAORG/DATERT",
  "c:/enc/DBtest/PRODUKTER/MELLOMPROD/R/STABLAORG/NYESTE",
  "/f/Prosjekter/Kommunehelsa/TESTOMRAADE/TEST_KHFUN/DBtest/PRODUKTER/MELLOMPROD/R/STABLAORG/NYESTE"
)

(valgPath <- rdsPath[3])

## Files
## ------------------
## From Excel file width format
testFil <- "UFORE.rds"

testFil <- "ELEVUNDER.rds"


## CSV files
filnavn <- c("ELEVUNDER", "ARBLEDIGE", "LaVINNT_1G", "ELEVUNDER_2020-01-14-13-59", "LESEFERD")
testFil <- paste(filnavn, "rds", sep = ".")
(valgFil <- testFil[1])

df <- readRDS(file.path(valgPath, valgFil))

library(data.table)
setDT(df)
df





## ---------------------------
## EDIT Raw Files for testing
## ---------------------------

normalizePath(readClipboard())

rawPath <- "C:\\enc\\DBtest\\ORGDATA\\DataTest\\2019"

rawFile <- c("FHI EUhasten2017.csv")

library(data.table)
DF <- fread(paste(rawPath, rawFile[1], sep = "\\"))

DF[, .N, by = Enhetsnavn]

## get only first 5 row in each groups ie. Enhetsnavn
dt <- DF[DF[,.I[1:5],by=Enhetsnavn]$V1]
dt2 <- DF[DF[,.I[1:5],by=Enhetsnavn][, V1]] #same as above

## Give colname 'ind' for row index instead of V1
DF[, .(ind = .I[1:5]), by=Kjonn]


## Select only few groups
print(DF[Prikket == 1, .N, by = Enhetsnavn], topn = 100)
valgEnh <- unique(DF$Enhetsnavn)[1:3]
dtSub <- c(valgEnh, "Aremark","Hasvik","Grong","Lesja") #med Prikket
## Only 6 rows for Alle (A) Gutter (G) Jenter (J)
dt <- DF[DF[,.I[1:6],by=.(Enhetsnavn)]$V1][Enhetsnavn  %in% dtSub, ]

fwrite(dt, paste(rawPath, "testPrikk.csv", sep = "\\"), sep = ";")



######################
## Raw data
######################
library(RODBC)

rawPath <- "F:\\Prosjekter\\Kommunehelsa\\PRODUKSJON"

dbfile <- "c:/enc/DBtest/KHELSA_dev.mdb"
conn <- RODBC::odbcConnectAccess2007(dbfile)

## LESEFERD
sqlreq <- "select * from ORIGINALFILER where FILID = 2274"
sqlreq <- "select FILNAVN from ORIGINALFILER where FILID = 2274"

utDF <- as.data.table(sqlQuery(conn, sqlreq))
utDF$FILNAVN

rawFile <- file.path(rawPath, utDF$FILNAVN, fsep = "\\")

library(data.table)

DT <- fread(rawFile)
DT



## with glue but doens't work with RODBC. Needs DBI
## ------------------------------------------------
library(glue)
filid  <- 2274
selCol <- "FILNAVN"
tbl <- "ORIGINALFILER"

sqlreq <- glue::glue_sql("
SELECT {`selCol`}
FROM {`tbl`}
WHERE {`tbl`}.FILID = {filid}
", .con = conn)
