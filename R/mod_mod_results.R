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
#' @import ComplexHeatmap
#' @import InteractiveComplexHeatmap
mod_mod_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(type = "tabs", id=ns("tabPanel"),
                tabPanel("Data",shinycssloaders::withSpinner(DT::dataTableOutput(ns("data")))),
                tabPanel("Metadata1",shinycssloaders::withSpinner(DT::dataTableOutput(ns("meta1tab")))),
                tabPanel("Metadata2",shinycssloaders::withSpinner(DT::dataTableOutput(ns("meta2tab")))),
                tabPanel("Graph",
                         div(id = ns("plotBox"),shinycssloaders::withSpinner(
                          plotOutput(ns("result")))
                         ),
                        DT::dataTableOutput(ns("tableResult")),
                          #htmlOutput(ns("hmOut")),
                        # div(id = ns("hmOut"),
                        #   InteractiveComplexHeatmapOutput(heatmap_id = ns("hmPlot")) # InteractiveComplexHeatmap don't working with shiny module yet
                        # ),
                        downloadButton(ns("DownloadPlot"),"Download Plots"),
                        downloadButton(ns("DownloadTable"),"Download Results")
                )
    )
  )
}

#' mod_results Server Functions
#'
#' @noRd
mod_mod_results_server <- function(id,dataDF,parent,metaData1,metaData2){
  moduleServer( id, function(input, output, session){
    #browser()
    ns <- session$ns
    plotRes <- NULL
    df <- NULL
    shinyjs::hide(id = "downloadPlot")
    output$data <- DT::renderDataTable(dataDF)
    output$meta1 <- DT::renderDataTable(metaData1)
    output$meta2 <- DT::renderDataTable(metaData2)

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
      #browser()
      updateTabsetPanel(session = session, "tabPanel",
                        selected = "Graph")
      if(parent$plot == "BlandAltman"){
        if(parent$col1 == "" | parent$col2 == "" | parent$col3 == "" | parent$var1 == "" |
           parent$var2 == "" | parent$gradient == "" | parent$extremum == ""){
          showModal(modalDialog(
            title = "Information",
            "Please select columns for parameters and colors"
          ))
        }else{
        colo <- c(parent$col1,parent$col2,parent$col3)
        plotRes <<- sistmr::BlandAltmanPlot(dataDF[,parent$var1],dataDF[,parent$var2],with_gradient = parent$gradient,
                                            line_color = colo,extremum_pctg = parent$extremum)
        output$result <- renderPlot(plotRes)
        shinyjs::show(id = "downloadPlot")
        }
      }
      if(parent$plot == "MultipleBoxPlots"){
        if(parent$color == "" | parent$fill == "" | parent$shape == "" | parent$var1Box == "" |
           parent$var2Box == "" | parent$points == ""){
          showModal(modalDialog(
            title = "Information",
            "Please select columns for parameters and colors"
          ))
        }else{
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
      }

      if(parent$plot == "Normal_Distribution"){
        df <<- as.data.frame(sistmr::normal_distribution(dataDF[,parent$vecNorm]))
        colnames(df) <<- c("value")
        output$tableResult <- DT::renderDataTable(df)
      }

      if(parent$plot == "VolcanoPlot"){
        #browser()
        if(parent$log2FC == "" | parent$pval == ""){
          cat("Must show alert")
          # shinyalert("Please Select Column",type = "warning",showConfirmButton = TRUE,
          #            closeOnClickOutside = TRUE)
          showModal(modalDialog(
            title = "Information",
            "Please select columns for log2FC and PValue"
          ))
        }else{
          log2FC <- parent$log2FC
          pval <- parent$pval
          geneName <- parent$GenesName
          cat("geneName: ",geneName)
          cat("Shouldn't show alert")
          if(geneName == "NONE" | geneName==""){
            plotRes <<- volcanoPlot(log2fc = log2FC,pValue = pval,data = dataDF)
          }
          else{
            plotRes <<- volcanoPlot(log2fc = log2FC,pValue = pval,data = dataDF,geneNames = geneName)
          }
          #browser()
          output$result <- renderPlot(plotRes)
          shinyjs::show(id = "downloadPlot")
        }
      }
      if(parent$plot == "barplot"){
        if(parent$varxBar == ""|parent$varyBar == ""|parent$varGroupBar == ""|parent$colorBar == ""|
           parent$xscale == ""|parent$yscale == ""){
          showModal(modalDialog(
            title = "Information",
            "Please select columns for parameters and colors"
          ))
        }else{
        plotRes <<- barplot(dataDF,parent$varxBar,parent$varyBar,parent$varGroupBar,parent$colorBar,
                                       parent$xscale,parent$yscale)
        output$result <- renderPlot(plotRes)
        shinyjs::show(id = "downloadPlot")
        }
      }
      if(parent$plot == "pie"){
        if(parent$vecPie == ""|parent$groupPie == ""|parent$pieTreatment == ""|parent$colorPie == ""){
          showModal(modalDialog(
            title = "Information",
            "Please select columns for parameters and colors"
          ))
        }else{
          plotRes <<- piePlot(dataDF,parent$vecPie,parent$groupPie,parent$pieTreatment,parent$colorPie)
          output$result <- renderPlot(plotRes)
          shinyjs::show((id = "downloadPlot"))
        }
      }
      if(parent$plot == "line"){
        if(parent$varxline == ""|parent$varyline == ""|parent$varGroupline == ""){
          showModal(modalDialog(
            title = "Information",
            "Please select columns for parameters and colors"
          ))
        }else{
        plotRes <<- lineplot(dataDF,parent$varxline,parent$varyline,parent$varGroupline,parent$varGroupline)
        output$result <- renderPlot(plotRes)
        shinyjs::show(id = "downloadPlot")
        }
      }
      if(parent$plot == "heatmap"){
        #browser()
        #shinyjs::showElement(id = "hmOut")
        #shinyjs::hideElement(id = "plotBox")
        dataDF <- as.matrix(dataDF)
        if(parent$colCluster == FALSE & parent$rowCluster == FALSE){
          cat("No clustering at all")
          plotRes <<- ComplexHeatmap::Heatmap(matrix = dataDF,cluster_rows = FALSE,cluster_columns = FALSE)
        }
        else if(parent$colCluster == FALSE){
          cat("No column clustering")
          plotRes <<- ComplexHeatmap::Heatmap(matrix = dataDF,cluster_columns = FALSE)
        }
        else if(parent$rowCluster == FALSE){
          cat("No Row clustering")
          plotRes <<- ComplexHeatmap::Heatmap(matrix = dataDF,cluster_rows = FALSE)
        }else{
          cat("Full Clustering")
          plotRes <<- ComplexHeatmap::Heatmap(dataDF)
        }
        output$result <- renderPlot(plotRes)
        # data(rand_mat) # simply a random matrix
        # ht1 = Heatmap(rand_mat, name = "mat",
        #              show_row_names = FALSE, show_column_names = FALSE)
        # ht1 = draw(ht1)
        # plotRes <- draw(plotRes)
        #browser()
        #makeInteractiveComplexHeatmap(input = input, output = output,session = ns,ht_list = ht1,heatmap_id = "hmPlot")
        #shinyjs::show(id = "downloadPlot")
      }
      # if(parent$plot != "heatmap"){
      #   shinyjs::hideElement("hmOut")
      #   shinyjs::showElement(id = "plotBox")
      # }
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
