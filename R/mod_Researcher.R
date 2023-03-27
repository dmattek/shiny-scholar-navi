#' Researcher UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import data.table
#' @importFrom data.table ":="
mod_Researcher_ui <- function(id, label, navid){
  ns <- NS(id)

  tabPanel(label,
           value = navid,
           tagList(
             h2('Researcher'),

             fluidPage(
               fluidRow(
                 textInput(inputId = ns('tiGSurl'),
                           label = 'Type in your Google Scholar ID or copy the URL of your Google Scholar profile',
                           value = '_dYonf8AAAAJ',
                           width = "50%"),
                 actionButton(inputId = ns('butGSurlSubmit'),
                              label = 'Submit'),
                 br()
               ),
               fluidRow(
                 tabsetPanel(
                   tabPanel('Overall citation trend',
                            br(),
                            fluidRow(
                              radioButtons(ns('rbRawCum'),
                                           label = 'Citations:',
                                           choices = c('Per year' = 'per_year',
                                                       'Cumulative' = 'cumulative')),
                              plotly::plotlyOutput(ns('plotCitationTrend'))
                            )
                   ),
                   tabPanel('Paper citation history',
                            br(),
                            fluidRow(
                              selectInput(ns('siNcit'),
                                          label = 'Number of top citations:',
                                          choices = list('5' = 5,
                                                         '10' = 10,
                                                         '15' = 15,
                                                         '20' = 20),
                                          selected = 5),
                              plotly::plotlyOutput(ns('plotTopNcitHist'))
                            )
                   )
                 )
               )
             )
           )
  )
}

#' Researcher Server Functions
#'
#' @noRd
mod_Researcher_server <- function(id){
  library(magrittr)

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    resID <- reactiveVal()

    observe({
      message("mod_researcher_server ")
    })

    # Process the URL from th einput text box
    rfProcessURL <- function() {
      cat('mod_Researcher_server:rfProcessURL\n')

      dummyDep <- input$butGSurlSubmit

      locS <- gsub('.*user=([A-Z,a-z,0-9]+)&.*',
                   '\\1',
                   isolate(input$tiGSurl))

      return(locS)
    }

    # Plot citations for researcher ID
    output$plotCitationTrend <- plotly::renderPlotly({
      cat("mod_Researcher_server:plotCitationTrend\n")

      # Obtain researcher ID from the URL
      locResID <- rfProcessURL()

      shiny::validate(
        shiny::need(!locResID == '',
                    'Cannot plot citation trend. Provide researcher ID.')
      )

      # Get citation history for the researcher ID
      locCH <- fGetResCitHist(locResID)

      # Get researcher's name
      locName <- fGetResName(locResID)

      locPlotVar <- switch(input$rbRawCum,
                           per_year = 'cites',
                           cumulative = 'cites_cumul')

      p1 = plotly::plot_ly(data = locCH,
                           x = ~year, y = ~get(locPlotVar),
                           name = "Citations",
                           type = 'scatter',
                           mode = 'lines+markers') %>%
        plotly::layout(title = sprintf("Citations of %s", locName),
               xaxis = list(title = "Year"),
               yaxis = list (title = "Citations"))
      p1
    })

    # Plot citation history of top N papers for the researcher ID
    output$plotTopNcitHist <- plotly::renderPlotly({
      cat("mod_Researcher_server:plotTopNcitHist\n")

      # Obtain researcher ID from the URL
      locResID <- rfProcessURL()
      shiny::validate(
        shiny::need(!locResID == '',
                    'Cannot plot paper citation history. Provide researcher ID.')
      )

      # Get a publication list for the researcher ID
      locPubList <- fGetResPubList(locResID)

      locD <- fPrepareTopNcitHist(locResID, locPubList, input$siNcit)

      # Get researcher's name
      locName <- fGetResName(locResID)

      p1 <- plotly::plot_ly(data = locD,
                            x = ~year,
                            y = ~cites_cumul,
                            type = 'scatter',
                            mode = 'lines+markers',
                            color = ~title,
                            line = list(colorscale = 'Accent'),
                            marker = list(colorscale = 'Accent')) %>%
        plotly::layout(title = sprintf("Citations of %s", locName),
                       xaxis = list(title = "Years from publication"),
                       yaxis = list (title = "Cumulated citations"))

      p1
    })
  })
}

## To be copied in the UI
# mod_Researcher_ui("Researcher_1")

## To be copied in the server
# mod_Researcher_server("Researcher_1")
