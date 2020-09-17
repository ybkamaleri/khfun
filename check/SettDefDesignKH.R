
## Use DBI as front-end and "odbc" as back-end driver for DBI to interface with Access
packages <- c("DBI", "odbc", "here", "data.table", "dbplyr")
sapply(packages, require, character.only = T)

db <- "c:/enc/DBtest/STYRING/KHELSA_dev.accdb"
db_drv <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq="
db_con <- paste0(db_drv, db)
conn <- DBI::dbConnect(odbc::odbc(), .connection_string = db_con, encoding = "latin1")

dbGetInfo()
DBI::dbListTables(conn)

## dbGetQuery do the dbSendQuery, fetch and dbClearResult()
Deler <- DBI::dbGetQuery(conn, "SELECT * FROM KH_DELER")
DBI::dbDisconnect(conn)

df <- SettDefDesignKH(Deler)

## Original code
## =============
library(stringr) #use str_split function

##Setter standard designegenskaper, slik som delenes kolonnenavn og status i omkoding
##Se tabell KH_DELER
SettDefDesignKH<-function(
                          globs=FinnGlobs()
                          ){

  ## Needs only connection to db from globs
  ## --------------------------------------
  Deler<-sqlQuery(globs$dbh,"SELECT * FROM KH_DELER",as.is=TRUE,stringsAsFactors=FALSE)


  ##DelKols<-lapply(as.list(setNames(Deler$DelKols, Deler$DEL)),function(x){unlist(str_split(x,pattern=","))})
  ##Tilrettelegging for enkle oppslag:
  DelKolN<-setNames(Deler$DelKol,Deler$DEL) #give attributes DEL to DelKol
  DelKolE<-setNames(Deler$DelKolE,Deler$DEL) #give attributes DEL to DelKolE ie. "FYLKE,GEO"
  DelType<-setNames(Deler$TYPE,Deler$DEL) # column TYPE with attribute from DEL
  DelFormat<-setNames(Deler$FORMAT,Deler$DEL) #column FORMAT with attribute DEL
  AggPri<-Deler$DEL[order(Deler$AGGREGERPRI)] #get ordered DEL according to AGGREGERPRI
  AggVedStand<-Deler$DEL[Deler$AGGREGERvedPRED==1] #select only A and K. Why??
  IntervallHull<-setNames(Deler$INTERVALLHULL,Deler$DEL)
  IntervallHull<-IntervallHull[!(is.na(IntervallHull) | IntervallHull=="")]

  DelKols<-as.list(DelKolN)
  DelKolsF<-DelKols # Why do we need this?
  KolsDel<-list()
  for (del in names(DelKols)){
    ## loop column DEL and check TYPE
    if (DelType[del]=="INT"){
      ## Add l (lower) and h (high) if FORMAT is INT i.e AAR and ALDER
      DelKols[[del]]<-paste(DelKols[[del]],c("l","h"),sep="")
      DelKolsF[[del]]<-DelKols[[del]] #Add lower and higher columns to DelKolsF
    }
    if (!(is.na(DelKolE[[del]]) | DelKolE[[del]]=="")){
      ## Add Fylke and GEO to the vector from DelKolE
      DelKolsF[[del]]<-c(DelKolsF[[del]],unlist(str_split(DelKolE[[del]],",")))
    }
    for (kol in DelKols[[del]]){
      KolsDel[[kol]]<-del
    }
  }

  ## Get DEL for the specified OMKODbet is. U,B,F
  UBeting<-Deler$DEL[Deler$OMKODbet=="U"]
  BetingOmk<-Deler$DEL[Deler$OMKODbet=="B"]
  BetingF<-Deler$DEL[Deler$OMKODbet=="F"]
  OmkDel<-c(UBeting,BetingOmk) #Not used in the code but returned!

  ##IntervallHull<-list(A="DekkInt/TotInt>0.999 | (NTOT>=10 & NHAR/NTOT>0.8) | (TotInt<=20 & DekkInt>=10) | TotInt<=10")

  DesignKols<-c(unlist(DelKols[c(UBeting,BetingOmk)])) #Columns without TABS
  DesignKolsF<-c(DesignKols,unlist(DelKols[BetingF])) #Columns including TABS
  DesignKolsFA<-c(DesignKolsF,setdiff(unlist(DelKolsF[c(UBeting,BetingOmk)]),
                                      unlist(DelKols[c(UBeting,BetingOmk)]))) #All columns including FYLKE and GEO



  return(
    list(DelKols=DelKols,
         DelKolsF=DelKolsF,
         KolsDel=KolsDel,
         DelKolN=DelKolN,
         DelType=DelType,
         DelFormat=DelFormat,
         UBeting=UBeting,
         BetingOmk=BetingOmk,
         BetingF=BetingF,
         OmkDel=OmkDel,
         DesignKols=DesignKols,
         DesignKolsF=DesignKolsF,
         DesignKolsFA=DesignKolsFA,
         AggPri=AggPri,
         AggVedStand=AggVedStand,
         IntervallHull=IntervallHull,
         AMissAllow=TRUE
         )
  )
}

