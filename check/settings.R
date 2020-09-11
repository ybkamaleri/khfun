library(data.table)
if (!require(here)) install.packages("here")
source(here('khfun_dev.R'))


## * LagFilgruppe
## Default for =LagFilgruppe=
function(gruppe,
         batchdate=SettKHBatchDate(),
         globs=FinnGlobs(),
         diagnose=0,
         printR=TRUE,
         printCSV=FALSE,
         printSTATA=FALSE,
         versjonert=FALSE,
         dumps=list(),
         testfil = FALSE)

# Test
## * Raw files
rawTest <- c("F:/Prosjekter/Kommunehelsa/PRODUKSJON/ORGDATA/MFR/ORG/2021/nkh_lav_hoy_fodselsvekt_enkeltfodt_dsf_grunnkrets.sql.csv")

DF <- fread(rawTest[1])


## * FinnFilgruppeParametre
## Default
function(gruppe,batchdate=SettKHBatchDate(),
                                 globs=FinnGlobs())
  
varTest <- c("ELEVUNDER", "FODEVEKT")

ds <- FinnFilgruppeParametre(varTest[2])

## Henter data fra tabell FILGRUPPER. I tillegg legger ekstra info til:
## $vals

rdsTest <- c("c:/enc/DBtest/PRODUKTER/MELLOMPROD/R/STABLAORG/NYESTE/ELEVUNDER.rds",
             "c:/enc/DBtest/PRODUKTER/MELLOMPROD/R/STABLAORG/NYESTE/FODEVEKT.rds")

dt <- readRDS(rdsTest[2])
setDT(dt)


## * FinnFilBeskGruppe
#Finn parameterbeskrivelse av delfilene
## delfiler<-FinnFilBeskGruppe(gruppe,batchdate=batchdate,globs=globs,testfil = testfil)
dut <- FinnFilBeskGruppe("ELEVUNDER", globs = FinnGlobs(), testfil = TRUE)
dut

