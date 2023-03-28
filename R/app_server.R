#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_Researcher_server("mod_researcher_1")
  mod_Collection_server("mod_collection_1")
  mod_Comparison_server("mod_comparison_1")
}
