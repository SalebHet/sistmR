#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyjs
#' @import colourInput
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    #golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      shinyjs::useShinyjs(),
      h1("sistmR"),
      tags$hr(),
      sidebarLayout(
        sidebarPanel(
          mod_mod_config_ui("mod_config_1")
        ),
        mainPanel(
        mod_mod_results_ui("mod_results_1")
        )
      )

    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
# golem_add_external_resources <- function() {
#   # add_resource_path(
#   #   "www",
#   #   #app_sys("app/www")
#   # )
#
#   tags$head(
#     favicon(),
#     bundle_resources(
#       #path = app_sys("app/www"),
#       app_title = "sistmR"
#     )
#     # Add here other external resources
#     # for example, you can add shinyalert::useShinyalert()
#   )
# }
