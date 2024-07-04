#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  key <- NULL
  subF <- NULL
  assay <- NULL
  set <- NULL
  dataDF <- NULL
  meta1 <- NULL
  meta2 <- NULL
  metaData1 <- NULL
  metaData2 <- NULL
  library('Rlabkey')
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['key']])) {
      #browser()
      #updateSliderInput(session, "bins", value = query[['bins']])
      key <<- query[['key']]
      subF <<- query[['sub']]
      assay <<- query[['file']]
      set <<- paste0("apikey|",key)
      type <- query[['type']]
      meta1 <<- query[['meta1']]
      meta2 <<- query[['meta2']]
      assayType <<- query[['assayType']]
      cat("meta1: ",meta1)
      cat("meta2: ",meta2)

      Rlabkey::labkey.setDefaults(apiKey=key)#"apikey|73ea3ff0973f38d52f5b1bbd8980f62c")
      Rlabkey::labkey.setDefaults(baseUrl = "https://labk.bph.u-bordeaux.fr/")#(baseUrl="https://labkey.bph.u-bordeaux.fr:8443/")
      if(type=="assay"){
        labkey.data <- labkey.selectRows(
          baseUrl="https://labk.bph.u-bordeaux.fr",
          #folderPath="/EBOVAC/assays/EBL2001/ICS",
          folderPath=subF,  #"/VASI/VICI/SISTM",
          schemaName=paste0("assay.",assayType,".",assay),#"assay.General.Vici_Sistm",
          queryName="Data",
          viewName="",
          colSort="",
          #colFilter=makeFilter(c("Run/RowId", "EQUAL", "140"),c("Antigen", "NOT_EQUAL_OR_MISSING", "Negative control")),
          containerFilter=NULL
        )
        if(length(meta1) > 0){
          metaData1 <<- labkey.selectRows(
            baseUrl="https://labk.bph.u-bordeaux.fr",
            #folderPath="/EBOVAC/assays/EBL2001/ICS",
            folderPath=subF,  #"/VASI/VICI/SISTM",
            schemaName=paste0("assay.General.",meta1),#"assay.General.Vici_Sistm",
            queryName="Data",
            viewName="",
            colSort="",
            #colFilter=makeFilter(c("Run/RowId", "EQUAL", "140"),c("Antigen", "NOT_EQUAL_OR_MISSING", "Negative control")),
            containerFilter=NULL
          )
        }
        if(length(meta2) > 0){
          metaData2 <<- labkey.selectRows(
            baseUrl="https://labk.bph.u-bordeaux.fr",
            #folderPath="/EBOVAC/assays/EBL2001/ICS",
            folderPath=subF,  #"/VASI/VICI/SISTM",
            schemaName=paste0("assay.General.",meta2),#"assay.General.Vici_Sistm",
            queryName="Data",
            viewName="",
            colSort="",
            #colFilter=makeFilter(c("Run/RowId", "EQUAL", "140"),c("Antigen", "NOT_EQUAL_OR_MISSING", "Negative control")),
            containerFilter=NULL
          )
        }
      }
      if(type=="dataset"){
        labkey.data <- labkey.selectRows(
          baseUrl="https://labk.bph.u-bordeaux.fr",
          #folderPath="/EBOVAC/assays/EBL2001/ICS",
          folderPath=subF,  #"/VASI/VICI/SISTM",
          schemaName="study",#paste0("assay.General.",assay),#"assay.General.Vici_Sistm",
          queryName=assay,
          viewName="",
          colSort="",
          #colFilter=makeFilter(c("Run/RowId", "EQUAL", "140"),c("Antigen", "NOT_EQUAL_OR_MISSING", "Negative control")),
          containerFilter=NULL
        )
        if(length(meta1) > 0){
          metaData1 <<- labkey.selectRows(
            baseUrl="https://labk.bph.u-bordeaux.fr",
            #folderPath="/EBOVAC/assays/EBL2001/ICS",
            folderPath=subF,  #"/VASI/VICI/SISTM",
            schemaName="study",#paste0("assay.General.",assay),#"assay.General.Vici_Sistm",
            queryName=meta1,
            viewName="",
            colSort="",
            #colFilter=makeFilter(c("Run/RowId", "EQUAL", "140"),c("Antigen", "NOT_EQUAL_OR_MISSING", "Negative control")),
            containerFilter=NULL
          )
        }
        if(length(meta2) > 0){
          metaData2 <<- labkey.selectRows(
            baseUrl="https://labk.bph.u-bordeaux.fr",
            #folderPath="/EBOVAC/assays/EBL2001/ICS",
            folderPath=subF,  #"/VASI/VICI/SISTM",
            schemaName="study",#paste0("assay.General.",assay),#"assay.General.Vici_Sistm",
            queryName=meta2,
            viewName="",
            colSort="",
            #colFilter=makeFilter(c("Run/RowId", "EQUAL", "140"),c("Antigen", "NOT_EQUAL_OR_MISSING", "Negative control")),
            containerFilter=NULL
          )
        }
      }
      #cat("Result request => ")
      #cat(as.character(labkey.data),"\n")
      #browser()
      dataDF <<- labkey.data
      dataDF <<- dataDF[,colSums(is.na(dataDF))<nrow(dataDF)]
    }
  })

  parents <- mod_mod_config_server("mod_config_1",dataDF,metaData1,metaData2)
  #browser()
  mod_mod_results_server("mod_results_1",dataDF,parents,metaData1,metaData2)
}
