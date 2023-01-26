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
#' @import shinyWidgets
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
                            VolcanoPlot = "VolcanoPlot",
                            barplot = "barplot",
                            pie = "pie"),
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
                    choicesOpt = listHTML()

      )),
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
    conditionalPanel(condition = "input$plot == barplot",
                     selectizeInput(ns("varxBar"),label = "x Var",
                                    choices = c(Choose = "", NULL),
                                    options = list(placeholder = 'Please select a column name below'))
    ),
    conditionalPanel(condition = "input$plot == barplot",
                     selectizeInput(ns("varyBar"),label = "y Var",
                                    choices = c(Choose = "", NULL),
                                    options = list(placeholder = 'Please select a column name below'))
    ),
    conditionalPanel(condition = "input$plot == barplot",
                     selectizeInput(ns("varGroupBar"),label = "Group",
                                    choices = c(Choose = "", NULL),
                                    options = list(placeholder = 'Please select a column name below'))
    ),
    conditionalPanel(condition = "input$plot == barplot",
                     pickerInput(inputId = ns("colorBar"),
                                 label = "pickerInput Palettes",
                                 choices = listPal <- list("Blues","BuGn","BuPu","GnBu","Greens","Greys","Oranges","OrRd","PuBu",
                                                           "PuBuGn","PuRd","Purples","RdPu","Reds","YlGn","YlGnBu","YlOrBr","YlOrRd",
                                                           "BrBG","PiYG","PRGn","PuOr","RdBu","RdGy","RdYlBu","RdYlGn","Spectral",
                                                           "Set3","Set2","Set1","Pastel2","Pastel1","Paired","Dark2","Accent"),#c("pal1","pal2", "pal3", "pal4"),#df$val,
                                 selected = "Set1",
                                 choicesOpt = listHTML()

                     )),
    conditionalPanel(condition = "input$plot == barplot",
                     selectizeInput(ns("xscale"),label = "X Scale",
                                    choices = c("none","log2","log10","sqrt"),
                                    selected = "none"
                                    )),
    conditionalPanel(condition = "input$plot == barplot",
                     selectizeInput(ns("yscale"),label = "Y Scale",
                                    choices = c("none","log2","log10","sqrt"),
                                    selected = "none")),
    conditionalPanel(condition = "input$plot == pie",
                     selectizeInput(ns("vecPie"),label = "Vector",
                                    choices = c(Choose = "", NULL),
                                    options = list(placeholder = 'Please select a column name below'))),
    conditionalPanel(condition = "input$plot == pie",
                     selectizeInput(ns("groupPie"),label = "group",
                                    choices = c(Choose = "", NULL),
                                    options = list(placeholder = 'Please select a column name below'))),
    radioButtons(ns("pieTreatment"), "Plot Type",
                 choices = c(sum="sum",
                             median="median",
                             mean="mean"
                             ),
                 selected = "sum"),
    conditionalPanel(condition = "input$plot == pie",
                     pickerInput(inputId = ns("colorPie"),
                                 label = "pickerInput Palettes",
                                 choices = listPal <- list("Blues","BuGn","BuPu","GnBu","Greens","Greys","Oranges","OrRd","PuBu",
                                                           "PuBuGn","PuRd","Purples","RdPu","Reds","YlGn","YlGnBu","YlOrBr","YlOrRd",
                                                           "BrBG","PiYG","PRGn","PuOr","RdBu","RdGy","RdYlBu","RdYlGn","Spectral",
                                                           "Set3","Set2","Set1","Pastel2","Pastel1","Paired","Dark2","Accent"),#c("pal1","pal2", "pal3", "pal4"),#df$val,
                                 selected = "Set1",
                                 choicesOpt = listHTML()

                     )),
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

      if(input$plot != "pie"){
        shinyjs::hide(id = "vecPie")
        shinyjs::hide(id = "groupPie")
        shinyjs::hide(id = "pieTreatment")
        shinyjs::hide(id = "colorPie")
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
      if(input$plot != "barplot"){
        shinyjs::hide(id = "varxPlot")
        shinyjs::hide(id = "varyPlot")
        shinyjs::hide(id = "varGroupPlot")
        shinyjs::hide(id = "colorBar")
        shinyjs::hide(id = "xscale")
        shinyjs::hide(id = "yscale")
        shinyjs::hide(id = "varxBar")
        shinyjs::hide(id = "varyBar")
        shinyjs::hide(id = "varGroupBar")
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

      if(input$plot == "pie"){
        shinyjs::show(id = "vecPie")
        shinyjs::show(id = "groupPie")
        shinyjs::show(id = "colorPie")
        updateSelectizeInput(session, inputId = "vecPie",
                             selected = '',
                             choices = c('',colnames(dataDF)),
                             options = list(placeholder = 'Please select a variable below'))
        updateSelectizeInput(session, inputId = "groupPie",
                             selected = '',
                             choices = c('',colnames(dataDF)),
                             options = list(placeholder = 'Please select a variable below'))
        shinyjs::show(id = "pieTreatment")
      }

      if(input$plot == "barplot"){
        shinyjs::show(id = "varxBar");
        shinyjs::show(id = "varyBar");
        shinyjs::show(id = "varGroupBar");
        shinyjs::show(id = "colorBar");
        shinyjs::show(id = "xscale");
        shinyjs::show(id = "yscale");
        updateSelectizeInput(session, inputId = "varxBar",
                             selected = '',
                             choices = c('',colnames(dataDF)),
                             options = list(placeholder = 'Please select a variable below'));
        updateSelectizeInput(session, inputId = "varyBar",
                             selected = '',
                             choices = c('',colnames(dataDF)),
                             options = list(placeholder = 'Please select a variable below'));
        updateSelectizeInput(session, inputId = "varGroupBar",
                             selected = 'NONE',
                             choices = c('NONE',colnames(dataDF)),
                             options = list(placeholder = 'Please select a variable below'));
      }
    })
    return(input)
  })
}

## To be copied in the UI
# mod_mod_config_ui("mod_config_1")

## To be copied in the server
# mod_mod_config_server("mod_config_1")
