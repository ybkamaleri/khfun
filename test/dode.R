library(data.table)
dt <- fread("F:/Prosjekter/Kommunehelsa/PRODUKSJON/ORGDATA/SSB/DODE_SSB/ORG/2021/G42019_v3.csv")

names(dt)
dt
dt[, .N, by = landb]


## Ferdig LagFilGruppe
DT <- readRDS("c:/enc/DBtest/PRODUKTER/MELLOMPROD/R/STABLAORG/NYESTE/DODE_GK.rds")
setDT(DT)
DT



## CHECK Landbakgunn

LANDBAKvask<-function (landbak,filbesk=data.frame(),batchdate=SettKHBatchDate(),globs=FinnGlobs(),regexp=FALSE){
  if (nrow(filbesk)==0){
    landbak<-setNames(as.data.frame(landbak,stringsAsFactors=FALSE),c("LANDBAK"))
    landbak$KBOMK<-landbak[,1]
  } else {
    
    landbak$KBOMK<-KBomkod(landbak$ORG,type="LANDBAK",filbesk=filbesk,batchdate=batchdate,globs=globs)
  }  

  
  landbak$OK<-1
  landbak$OMK<-landbak$KBOMK
  if(regexp==TRUE){
    landbak$OMK<-sub("^ *(Europa.*) *$","1",landbak$OMK,ignore.case = TRUE)
    landbak$OMK<-sub("^ *(Afrika) *$","2",landbak$OMK,ignore.case = TRUE)
    landbak$OMK<-sub("^ *(Asia.*) *$","3",landbak$OMK,ignore.case = TRUE)
    landbak$OMK<-sub("^ *(Nord[ -]{1,3}Amerika) *$","4",landbak$OMK,ignore.case = TRUE)
    landbak$OMK<-sub("^ *(Sør.*Amerika) *$","5",landbak$OMK,ignore.case = TRUE)
    landbak$OMK<-sub("^ *(Oseania) *$","6",landbak$OMK,ignore.case = TRUE)
    landbak$OMK<-sub("^ *(Statsløse) *$","7",landbak$OMK,ignore.case = TRUE)
    landbak$OMK<-sub("^ *(Uoppgitt|Ukjent) *$","8",landbak$OMK,ignore.case = TRUE)
    landbak$OMK<-sub("^ *(Andre) *$","9",landbak$OMK,ignore.case = TRUE)
  }  
  landbak$OMK<-sub("^ *(Alle) *$","0",landbak$OMK,ignore.case = TRUE)
  
  #Ugyldig verdi/ukjent kode
  landbak$OMK[!(landbak$OMK %in% c(-1,0,1,2,3,4,5,6,7,8,9,"-"))]<-globs$landbak_illeg
  landbak$OK[!(landbak$OMK %in% c(-1,0,1,2,3,4,5,6,7,8,9,"-"))]<-0
  
  return(landbak)
}

## org = original codes in vector eg. 0,1B,2B etc
## type = LANDBAK
## SQL here for Landbk is:
## "SELECT TYPE, ORGKODE, NYKODE FROM KODEBOK WHERE \n
##             FELTTYPE='LANDBAK' AND FILGRUPPE='DODE_GK' AND (DELID='G_v5' OR DELID='FELLES')
##             AND VERSJONFRA<=#2020-11-24# AND VERSJONTIL>#2020-11-24#"

KBomkod<-function(org,type,filbesk,valsubs=FALSE,batchdate=NULL,globs=FinnGlobs()) {
  
  datef<-format(Sys.time(), "#%Y-%m-%d#")
  if (!is.null(batchdate)){
    datef<-format(strptime(batchdate, "%Y-%m-%d-%H-%M"),"#%Y-%m-%d#")
  }
  omk<-org
  kbf<-paste(type,"kb",sep="")
  sql<-paste("SELECT TYPE, ORGKODE, NYKODE FROM KODEBOK WHERE 
             FELTTYPE='",type,
             "' AND FILGRUPPE='",filbesk$FILGRUPPE,
             "' AND (DELID='",filbesk$DELID,"' OR DELID='FELLES')",
             " AND VERSJONFRA<=",datef, 
             " AND VERSJONTIL>",datef,sep="")
  kbok<-sqlQuery(globs$dbh,sql,as.is=TRUE)
  kbok[is.na(kbok)]<-""

  # Create empty data.frame
  subsant<-data.frame(ORG=character(0),KBOMK=character(0),OMK=character(0),FREQ=integer(0),OK=integer(0))
  if (nrow(kbok)>0){
    KBsubs<-subset(kbok,TYPE=="SUB")   #Regulæruttrykk
    KB<-subset(kbok,TYPE=="KB")       #Oppslagsliste
    i<-1
    while (i<=nrow(KBsubs)){
      KBsub<-KBsubs[i,]
      if (valsubs==TRUE){
        subsant<-rbind(subsant,data.frame(ORG=KBsub$ORGKODE,KBOMK=paste("<",KBsub$NYKODE,">",sep=""),OMK=paste("<",KBsub$NYKODE,">",sep=""),FREQ=length(grepl(KBsub$ORGKODE,omk,perl=TRUE)),OK=1))
      }
      #omk<-sub(eval(parse(text=KBsub$ORGKODE)),eval(parse(text=KBsub$NYKODE)),omk)
      omk<-sub(KBsub$ORGKODE,KBsub$NYKODE,omk,perl=TRUE)
      i<-i+1
    }
    if (valsubs==TRUE){
      #Ta bare numeriske fra
      KB<-KB[!is.na(suppressWarnings(as.numeric(KB$ORGKODE))),]
      if (nrow(KB)>0){
        freq<-table(omk)
        freq<-freq[KB$ORGKODE]
        if (!is.na(freq)){
          subsant<-rbind(subsant,data.frame(ORG=KB$ORGKODE,KBOMK=KB$NYKODE,OMK=KB$NYKODE,FREQ=freq,OK=1))
          #tmp2<-as.data.frame(table(DF$GEO,useNA="ifany"),stringsAsFactors=FALSE)
          omk<-mapvalues(omk,KB$ORGKODE,KB$NYKODE,warn_missing = FALSE)
        }        
      }
    } else {
      omk<-mapvalues(omk,KB$ORGKODE,KB$NYKODE,warn_missing = FALSE)
    }    
  }
  
  if (valsubs==FALSE){
    return(omk)
  } else {
    return(list(omk=omk,subsant=subsant))
  }
}
