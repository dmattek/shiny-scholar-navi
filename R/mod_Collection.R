#' Collection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Collection_ui <- function(id, label, navid){
  ns <- NS(id)

  tabPanel(label,
           value = navid,
           tagList(
             h2('Collection'),

             fluidPage(
               fluidRow(
                 selectInput(ns('siSrc'),
                             'Publications from',
                             c('PertzLab',
                               'other')),
                 plotly::plotlyOutput(ns('plotPubCollCitHist'))
               )
             )
           )
  )

}

#' Collection Server Functions
#'
#' @noRd
mod_Collection_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      message("mod_collection_server ")
    })

    # Prepare data for the plot
    makeDat4Plot <- reactive({
      if (input$siSrc == 'PertzLab') {
        # get data from the Google Docs Spreadsheet with the publication list of the group
        locPubColl <- fGetDataFromGD(pubCollPertzLab)

        # cat("\n\nlocPubColl\n")
        # print(locPubColl)

        # get citation history of the article collection
        locRes <- fGetArtCollCitHist(locPubColl)

        # cat("\n\nlocRes\n")
        # print(locRes)

        # calculate cumulated citations and a year from publication
        locRes[,
               `:=`(cites_cumul = cumsum(cites),
                    year = year - first(year)),
               by = pubID]

      } else {
        locRes <- NULL
      }

      return(locRes)
    })

    output$plotPubCollCitHist <- plotly::renderPlotly({
      cat("mod_Collection_server:plotPubCollCitHist\n")

      locDat <- makeDat4Plot()
      shiny::validate(
        shiny::need(!is.null(locDat),
                    'Cannot plot citation history. Provide a URL to publication collection.')
      )

      p1 <- plotly::plot_ly(data = locDat,
                            x = ~year,
                            y = ~cites_cumul,
                            type = 'scatter',
                            mode = 'lines+markers',
                            text = ~Description,
                            color = ~title,
                            hovertemplate = paste('<b>', '%{text}', '</b><br>',
                                                  'Years from pub.: %{x}', '<br>',
                                                  'Cumul. cits: %{y}',
                                                  '<extra></extra>'),
                            line = list(colorscale = 'Accent'),
                            marker = list(colorscale = 'Accent')) %>%
        plotly::layout(title = sprintf("Citations of %s", input$siSrc),
                       xaxis = list(title = "Years from publication"),
                       yaxis = list (title = "Cumulated citations"))

      p1
    })
  })
}

## To be copied in the UI
# mod_Collection_ui("Collection_1")

## To be copied in the server
# mod_Collection_server("Collection_1")
