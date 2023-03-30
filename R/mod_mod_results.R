#' mod_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import ggplot2
mod_mod_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(type = "tabs", id=ns("tabPanel"),
                tabPanel("Data",shinycssloaders::withSpinner(DT::dataTableOutput(ns("data")))),
                tabPanel("Graph",
                         div(id = ns("plotBox"),shinycssloaders::withSpinner(
                          plotOutput(ns("result")))
                          ),
                          DT::dataTableOutput(ns("tableResult")),
                          downloadButton(ns("DownloadPlot"),"Download Plots"),
                         downloadButton(ns("DownloadTable"),"Download Results")
                )
    )
  )
}

#' mod_results Server Functions
#'
#' @noRd
mod_mod_results_server <- function(id,dataDF,parent){
  moduleServer( id, function(input, output, session){

    ns <- session$ns
    plotRes <- NULL
    df <- NULL
    shinyjs::hide(id = "downloadPlot")
    output$data <- DT::renderDataTable(dataDF)

    observeEvent(parent$plot,{
      if(parent$plot == "Normal_Distribution"){
        shinyjs::show(id = "tableResult")
        shinyjs::show(id = "DownloadTable")
        shinyjs::hideElement(id = "plotBox")
        shinyjs::hide(id = "DownloadPlot")
      }
      if(parent$plot != "Normal_Distribution"){
        shinyjs::hide(id = "tableResult")
        shinyjs::showElement(id = "plotBox")
        shinyjs::hide(id = "DownloadTable")
        shinyjs::show(id = "DownloadPlot")

      }
    })

    observeEvent(parent$plotButton,{
      updateTabsetPanel(session = session, "tabPanel",
                        selected = "Graph")
      if(parent$plot == "BlandAltman"){
        colo <- c(parent$col1,parent$col2,parent$col3)
        plotRes <<- sistmr::BlandAltmanPlot(dataDF[,parent$var1],dataDF[,parent$var2],with_gradient = parent$gradient,
                                            line_color = colo,extremum_pctg = parent$extremum)
        output$result <- renderPlot(plotRes)
        shinyjs::show(id = "downloadPlot")
      }
      if(parent$plot == "MultipleBoxPlots"){
        cat("color: ")
        cat(parent$color,"\n")
        cat("fill: ")
        cat(parent$fill,"\n")
        cat("shape: ")
        cat(parent$shape,"\n")
        dataDF[,parent$var1Box] <<- factor(dataDF[,parent$var1Box],unique(dataDF[,parent$var1Box]))
        plotRes <<- multipleBoxplots(dataDF,dataDF[,parent$var1Box],unlist(dataDF[parent$var2Box]),parent$points,parent$color,fill = parent$fill, shape_chosen = parent$shape)
        output$result <- renderPlot(plotRes +
                                      ggplot2::labs(x = parent$var1Box,y = parent$var2Box)+
                                      ggplot2::guides(colour = ggplot2::guide_legend(parent$var1Box)))
        shinyjs::show(id = "downloadPlot")
      }
      if(parent$plot == "Normal_Distribution"){
        df <<- as.data.frame(sistmr::normal_distribution(dataDF[,parent$vecNorm]))
        colnames(df) <<- c("value")
        output$tableResult <- DT::renderDataTable(df)
      }
      if(parent$plot == "VolcanoPlot"){
        plotRes <<- sistmr::volcanoPlot(dataDF[,parent$log2FC],dataDF[,parent$pval],dataDF)
        output$result <- renderPlot(plotRes)
        shinyjs::show(id = "downloadPlot")
      }
      if(parent$plot == "barplot"){
        plotRes <<- barplot(dataDF,parent$varxBar,parent$varyBar,parent$varGroupBar,parent$colorBar,
                                       parent$xscale,parent$yscale)
        output$result <- renderPlot(plotRes)
        shinyjs::show(id = "downloadPlot")
      }
      if(parent$plot == "pie"){
        plotRes <<- piePlot(dataDF,parent$vecPie,parent$groupPie,parent$pieTreatment,parent$colorPie)
        output$result <- renderPlot(plotRes)
        shinyjs::show((id = "downloadPlot"))
      }
    })

    output$DownloadPlot <- downloadHandler(
        filename = function(){paste0(parent$plot,".png")},
        content = function(file){
          if(!is.null(plotRes)){
          ggsave(file,plot(plotRes))
          }
        }
    )
    output$DownloadTable <- downloadHandler(
      filename = function(){"Results.csv"},
      content = function(fname){
        cat("DownloadTable")
        write.csv(df,fname)
      }
    )
  })
}

## To be copied in the UI
# mod_mod_results_ui("mod_results_1")

## To be copied in the server
# mod_mod_results_server("mod_results_1")
