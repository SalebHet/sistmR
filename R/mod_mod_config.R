#' mod_config UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import colourpicker
#' @import colourInput
#' @import shinyjs
mod_mod_config_ui <- function(id){
  library(colourpicker)
  ns <- NS(id)
  shinyjs::useShinyjs()
  tagList(
    radioButtons(ns("plot"), "Plot Type",
                 choices = c(BlandAltman = "BlandAltman",
                             MultipleBoxPlots = "MultipleBoxPlots",
                             Normal_Distribution = "Normal_Distribution",
                            VolcanoPlot = "VolcanoPlot"),
                 selected = "BlandAltman"),

    conditionalPanel(condition = "input$plot == BlandAltman",
                     selectizeInput(ns("var1"),label = "var1",
                       choices = c(Choose = "", NULL),
                       options = list(placeholder = 'Please select a column name below'))
                     ),

    conditionalPanel(condition = "input$plot == BlandAltman",
                     selectizeInput(ns("var2"),label = "var2",
                       choices = c(Choose = "", NULL),
                       options = list(placeholder = 'Please select a column name below'))
                     ),

    conditionalPanel(condition = "input$plot == BlandAltman",
                     checkboxInput(ns("gradient"),label = "With Gradient", value = FALSE)),

    conditionalPanel(condition = "input$plot == BlandAltman",
                     checkboxInput(ns("extremum"),label = "With Extremum", value = TRUE)),

    colourInput(ns("col1"),"select color 1", "blue"),
    colourInput(ns("col2"),"select color 2", "red"),
    colourInput(ns("col3"),"select color 3", "black"),

    conditionalPanel(condition = "input$plot == MultipleBoxPlots",
                     selectizeInput(ns("var1Box"),label = "x Var",
                                    choices = c(Choose = "", NULL),
                                    options = list(placeholder = 'Please select a column name below'))
    ),
    conditionalPanel(condition = "input$plot == MultipleBoxPlots",
                     selectizeInput(ns("var2Box"),label = "y Var",
                                    choices = c(Choose = "", NULL),
                                    options = list(placeholder = 'Please select a column name below'))
    ),
    conditionalPanel(condition = "input$plot == MultipleBoxPlots",
                     checkboxInput(ns("points"),label = "Add Points", value = TRUE)
    ),
    conditionalPanel(condition = "input$plot == Normal_Distribution",
                     selectizeInput(ns("vecNorm"),label = "Vector",
                                    choices = c(Choose = "", NULL),
                                    options = list(placeholder = 'Please select a column name below'))
    ),
    conditionalPanel(condition = "input$plot == VolcanoPlot",
                     selectizeInput(ns("log2FC"),label = "log2FC",
                                    choices = c(Choose = "", NULL),
                                    options = list(placeholder = 'Please select a column name below'))
    ),
    conditionalPanel(condition = "input$plot == VolcanoPlot",
                     selectizeInput(ns("pval"),label = "pval",
                                    choices = c(Choose = "", NULL),
                                    options = list(placeholder = 'Please select a column name below'))
    ),

    actionButton(ns("plotButton"),"Draw Plot")
  )
}

#' mod_config Server Functions
#'
#' @noRd
mod_mod_config_server <- function(id,dataDF){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$plot,{
      if(input$plot != "MultipleBoxPlots"){
        shinyjs::hide(id = "var1Box")
        shinyjs::hide(id = "var2Box")
        shinyjs::hide(id = "points")
      }
      if(input$plot != "BlandAltman"){
        shinyjs::hide(id = "var1")
        shinyjs::hide(id = "var2")
        shinyjs::hide(id = "gradient")
        shinyjs::hide(id = "extremum")
        shinyjs::hide(id = "col1")
        shinyjs::hide(id = "col2")
        shinyjs::hide(id = "col3")
      }
      if(input$plot != "Normal_Distribution"){
        shinyjs::hide(id = "vecNorm")
      }
      if(input$plot != "VolcanoPlot"){
        shinyjs::hide(id = "log2FC")
        shinyjs::hide(id = "pval")
      }
      if(input$plot == "BlandAltman"){
        shinyjs::show(id = "var1")
        shinyjs::show(id = "var2")
        shinyjs::show(id = "gradient")
        shinyjs::show(id = "extremum")
        shinyjs::show(id = "col1")
        shinyjs::show(id = "col2")
        shinyjs::show(id = "col3")
        updateSelectizeInput(session, inputId = "var1",
                             selected = '',
                             choices = c('',colnames(dataDF)),
                             options = list(placeholder = 'Please select a variable below'))
        updateSelectizeInput(session, inputId = "var2",
                             selected = '',
                             choices = c('',colnames(dataDF)),
                             options = list(placeholder = 'Please select a variable below'))
      }
      if(input$plot == "MultipleBoxPlots"){
        shinyjs::show(id = "var1Box")
        shinyjs::show(id = "var2Box")
        shinyjs::show(id = "points")
        updateSelectizeInput(session, inputId = "var1Box",
                             selected = '',
                             choices = c('',colnames(dataDF)),
                             options = list(placeholder = 'Please select a variable below'))
        updateSelectizeInput(session, inputId = "var2Box",
                             selected = '',
                             choices = c('',colnames(dataDF)),
                             options = list(placeholder = 'Please select a variable below'))
      }
      if(input$plot == "Normal_Distribution"){
        shinyjs::show(id = "vecNorm")
        updateSelectizeInput(session, inputId = "vecNorm",
                             selected = '',
                             choices = c('',colnames(dataDF)),
                             options = list(placeholder = 'Please select a variable below'))
      }
      if(input$plot == "VolcanoPlot"){
        shinyjs::show(id = "log2FC")
        shinyjs::show(id = "pval")
        updateSelectizeInput(session, inputId = "log2FC",
                             selected = '',
                             choices = c('',colnames(dataDF)),
                             options = list(placeholder = 'Please select a variable below'))
        updateSelectizeInput(session, inputId = "pval",
                             selected = '',
                             choices = c('',colnames(dataDF)),
                             options = list(placeholder = 'Please select a variable below'))
      }
    })
    return(input)
  })
}

## To be copied in the UI
# mod_mod_config_ui("mod_config_1")

## To be copied in the server
# mod_mod_config_server("mod_config_1")
