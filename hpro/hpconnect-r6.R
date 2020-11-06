
library(R6)
library(odbc)
library(DBI)

hp <- R6::R6Class(
  classname = "HPConnect",
  class = FALSE,
  cloneable = FALSE,
  public = list(
    dbname = NULL,
    dbconn = NULL,
    initialize = function(dbname = NULL){
      if (is.null(dbname)) return(message(">>> DB name is missing!"))
      else {
        self$dbname <- dbname
      }
    },
    db_connect = function(){
      stopifnot(!is.null(self$dbname))
      cs <- paste0(private$..drv, self$dbname)
      self$dbconn <- DBI::dbConnect(odbc::odbc(),
                                    .connection_string = cs,
                                    encoding = "latin1")
    },
    db_close = function(){
      DBI::dbDisconnect(self$dbconn)
    }
  ),
  private = list(
    ..drv = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", 
    finalize = function(){
      DBI::dbDisconnect(self$dbconn)
    }
  )
)


