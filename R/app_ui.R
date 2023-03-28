#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      navbarPage(collapsible = T,
                 id = 'navbarid',
                 title = "scholar navi",
                 mod_Researcher_ui('mod_researcher_1', 'Researcher', navid = 1),
                 mod_Collection_ui('mod_collection_1', 'Collection', navid = 2),
                 mod_Comparison_ui('mod_comparison_1', 'Comparison', navid = 3),
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
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "scholarApp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
