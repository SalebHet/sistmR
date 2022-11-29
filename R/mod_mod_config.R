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
#' @import shinyjs
#' @import shinywidget
mod_mod_config_ui <- function(id){
  library(colourpicker)
  ns <- NS(id)
  shinyjs::useShinyjs()
  #browser()
  tagList(
    #browser(),
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

    colourpicker::colourInput(ns("col1"),"select color 1", "blue"),
    colourpicker::colourInput(ns("col2"),"select color 2", "red"),
    colourpicker::colourInput(ns("col3"),"select color 3", "black"),

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
    conditionalPanel(condition = "input$plot == MultipleBoxPlots",
                     checkboxInput(ns("fill"),label = "Fill Boxplots", value = TRUE)
    ),
    conditionalPanel(condition = "input$plot == MultipleBoxPlots && input$points == TRUE",
      pickerInput(inputId = ns("shape"),
                  label = "Shape of points",
                  choices = listShape <- list("circle","square","diamond","triangle","plus",
                                            "circle open","square open","diamond open","triangle open","triangle down open","cross",
                                            "circle filled","square filled","diamond filled","triangle filled","triangle down filled","asterisk",
                                            "cricle cross","square cross","circle plus","square plus","diamond plus","circle small","square triangle","bullet"),
                  selected = "circle open",
                  choicesOpt = list(content = c(sprintf("<img src='./shapes/circle.png' width=30px><div class='jhr'>%s</div></img>", "circle"),
                                                sprintf("<img src='./shapes/square.png' width=30px><div class='jhr'>%s</div></img>", "square"),
                                                sprintf("<img src='./shapes/diamond.png' width=30px><div class='jhr'>%s</div></img>", "diamond"),
                                                sprintf("<img src='./shapes/triangle.png' width=30px><div class='jhr'>%s</div></img>", "triangle"),
                                                sprintf("<img src='./shapes/plus.png' width=30px><div class='jhr'>%s</div></img>", "plus"),
                                                sprintf("<img src='./shapes/circle open.png' width=30px><div class='jhr'>%s</div></img>", "circle open"),
                                                sprintf("<img src='./shapes/square open.png' width=30px><div class='jhr'>%s</div></img>", "square open"),
                                                sprintf("<img src='./shapes/diamond open.png' width=30px><div class='jhr'>%s</div></img>", "diamond open"),
                                                sprintf("<img src='./shapes/triangle open.png' width=30px><div class='jhr'>%s</div></img>", "triangle open"),
                                                sprintf("<img src='./shapes/triangle down.png' width=30px><div class='jhr'>%s</div></img>", "triangle down open"),
                                                sprintf("<img src='./shapes/cross.png' width=30px><div class='jhr'>%s</div></img>", "cross"),
                                                sprintf("<img src='./shapes/circle filled.png' width=30px><div class='jhr'>%s</div></img>", "circle filled"),
                                                sprintf("<img src='./shapes/squar filled.png' width=30px><div class='jhr'>%s</div></img>", "square filled"),
                                                sprintf("<img src='./shapes/diamond filled.png' width=30px><div class='jhr'>%s</div></img>", "diamond filled"),
                                                sprintf("<img src='./shapes/triangle filled.png' width=30px><div class='jhr'>%s</div></img>", "triangle filled"),
                                                sprintf("<img src='./shapes/triangle down filled.png' width=30px><div class='jhr'>%s</div></img>", "triangle down filled"),
                                                sprintf("<img src='./shapes/asterisk.png' width=30px><div class='jhr'>%s</div></img>", "asterisk"),
                                                sprintf("<img src='./shapes/circle cross.png' width=30px><div class='jhr'>%s</div></img>", "circle cross"),
                                                sprintf("<img src='./shapes/square cross' width=30px><div class='jhr'>%s</div></img>", "square cross"),
                                                sprintf("<img src='./shapes/circle plus.png' width=30px><div class='jhr'>%s</div></img>", "circle plus"),
                                                sprintf("<img src='./shapes/square plus.png' width=30px><div class='jhr'>%s</div></img>", "square plus"),
                                                sprintf("<img src='./shapes/diamond plus.png' width=30px><div class='jhr'>%s</div></img>", "diamond plus"),
                                                sprintf("<img src='./shapes/circle small.png' width=30px><div class='jhr'>%s</div></img>", "circle small"),
                                                sprintf("<img src='./shapes/square triangle.png' width=30px><div class='jhr'>%s</div></img>", "square triangle"),
                                                sprintf("<img src='./shapes/bullet.png' width=30px><div class='jhr'>%s</div></img>", "bullet")
                                                ))
                  )
    ),
    conditionalPanel(condition = "input$plot == MultipleBoxPlots",
                    pickerInput(inputId = ns("color"),
                    label = "pickerInput Palettes",
                    choices = listPal <- list("Blues","BuGn","BuPu","GnBu","Greens","Greys","Oranges","OrRd","PuBu",
                                              "PuBuGn","PuRd","Purples","RdPu","Reds","YlGn","YlGnBu","YlOrBr","YlOrRd",
                                              "BrBG","PiYG","PRGn","PuOr","RdBu","RdGy","RdYlBu","RdYlGn","Spectral",
                                              "Set3","Set2","Set1","Pastel2","Pastel1","Paired","Dark2","Accent"),#c("pal1","pal2", "pal3", "pal4"),#df$val,
                    selected = "Set1",
                    choicesOpt = list(content = c(sprintf("<img src='./palette/blues.png' width=30px><div class='jhr'>%s</div></img>", "Blues"),
                                                  sprintf("<img src='./palette/BuGn.png' width=30px><div class='jhr'>%s</div></img>", "BuGn"),
                                                  sprintf("<img src='./palette/BuPu.png' width=30px><div class='jhr'>%s</div></img>", "BuPu"),
                                                  sprintf("<img src='./palette/GnBu.png' width=30px><div class='jhr'>%s</div></img>", "GnBu"),
                                                  sprintf("<img src='./palette/Greens.png' width=30px><div class='jhr'>%s</div></img>", "Greens"),
                                                  sprintf("<img src='./palette/Greys.png' width=30px><div class='jhr'>%s</div></img>", "Greys"),
                                                  sprintf("<img src='./palette/Oranges.png' width=30px><div class='jhr'>%s</div></img>", "Oranges"),
                                                  sprintf("<img src='./palette/OrRd.png' width=30px><div class='jhr'>%s</div></img>", "OrRd"),
                                                  sprintf("<img src='./palette/PuBu.png' width=30px><div class='jhr'>%s</div></img>", "PuBu"),
                                                  sprintf("<img src='./palette/PuBuGn.png' width=30px><div class='jhr'>%s</div></img>", "PuBuGn"),
                                                  sprintf("<img src='./palette/PuRd.png' width=30px><div class='jhr'>%s</div></img>", "PuRd"),
                                                  sprintf("<img src='./palette/Purples.png' width=30px><div class='jhr'>%s</div></img>", "Purples"),
                                                  sprintf("<img src='./palette/RdPu.png' width=30px><div class='jhr'>%s</div></img>", "RdPu"),
                                                  sprintf("<img src='./palette/Reds.png' width=30px><div class='jhr'>%s</div></img>", "Reds"),
                                                  sprintf("<img src='./palette/YlGn.png' width=30px><div class='jhr'>%s</div></img>", "YlGn"),
                                                  sprintf("<img src='./palette/YlGnBu.png' width=30px><div class='jhr'>%s</div></img>", "YlGnBu"),
                                                  sprintf("<img src='./palette/YlOrBr.png' width=30px><div class='jhr'>%s</div></img>", "YlOrBr"),
                                                  sprintf("<img src='./palette/YlOrRd.png' width=30px><div class='jhr'>%s</div></img>", "YlOrRd"),
                                                  sprintf("<img src='./palette/BrBG.png' width=30px><div class='jhr'>%s</div></img>", "BrBG"),
                                                  sprintf("<img src='./palette/PiYG.png' width=30px><div class='jhr'>%s</div></img>", "PiYG"),
                                                  sprintf("<img src='./palette/PRGn.png' width=30px><div class='jhr'>%s</div></img>", "PRGn"),
                                                  sprintf("<img src='./palette/PuOr.png' width=30px><div class='jhr'>%s</div></img>", "PuOr"),
                                                  sprintf("<img src='./palette/RdBu.png' width=30px><div class='jhr'>%s</div></img>", "RdBu"),
                                                  sprintf("<img src='./palette/RdGy.png' width=30px><div class='jhr'>%s</div></img>", "RdGy"),
                                                  sprintf("<img src='./palette/RdYlBu.png' width=30px><div class='jhr'>%s</div></img>", "RdYlBu"),
                                                  sprintf("<img src='./palette/RdYlGn.png' width=30px><div class='jhr'>%s</div></img>", "RdYlGn"),
                                                  sprintf("<img src='./palette/Spectral.png' width=30px><div class='jhr'>%s</div></img>", "Spectral"),
                                                  sprintf("<img src='./palette/Set3.png' width=30px><div class='jhr'>%s</div></img>", "Set3"),
                                                  sprintf("<img src='./palette/Set2.png' width=30px><div class='jhr'>%s</div></img>", "Set2"),
                                                  sprintf("<img src='./palette/Set1.png' width=30px><div class='jhr'>%s</div></img>", "Set1"),
                                                  sprintf("<img src='./palette/Pastel2.png' width=30px><div class='jhr'>%s</div></img>", "Pastel2"),
                                                  sprintf("<img src='./palette/Pastel1.png' width=30px><div class='jhr'>%s</div></img>", "Pastel1"),
                                                  sprintf("<img src='./palette/Paired.png' width=30px><div class='jhr'>%s</div></img>", "Paired"),
                                                  sprintf("<img src='./palette/Dark2.png' width=30px><div class='jhr'>%s</div></img>", "Dark2"),
                                                  sprintf("<img src='./palette/Accent.png' width=30px><div class='jhr'>%s</div></img>", "Accent")
                                                  #sprintf("<img src='https://d9np3dj86nsu2.cloudfront.net/image/eaf97ff8dcbc7514d1c1cf055f2582ad' width=30px><div class='jhr'>%s</div></img>", "pal1"),
                                                  # sprintf("<img src='https://www.color-hex.com/palettes/33187.png' width=30px><div class='jhr'>%s</div></img>", "pal2"),
                                                  # sprintf("<img src='https://www.color-hex.com/palettes/16042.png' width=30px><div class='jhr'>%s</div></img>", "pal3"),
                                                  # sprintf("<img src='https://www.stlawrencegallery.com/wp-content/uploads/2018/09/unique-navy-blue-color-palette-five-stunning-palettes-for-weddings-dark.jpg' width=30px><div class='jhr'>%s</div></img>", "pal4"))#df$img))

      )))
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
        shinyjs::hide(id = "color")
        shinyjs::hide(id = "shape")
        shinyjs::hide(id = "fill")
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
        shinyjs::show(id = "fill")
        updateSelectizeInput(session, inputId = "var1Box",
                             selected = '',
                             choices = c('',colnames(dataDF)),
                             options = list(placeholder = 'Please select a variable below'))
        updateSelectizeInput(session, inputId = "var2Box",
                             selected = '',
                             choices = c('',colnames(dataDF)),
                             options = list(placeholder = 'Please select a variable below'))
        shinyjs::show(id = "color")
      }
      if(input$plot == "MultipleBoxPlots" && input$points == TRUE){
        shinyjs::show(id = "shape")
      }
      if(input$points == FALSE){
        shinyjs::hide(id = "shape")
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
