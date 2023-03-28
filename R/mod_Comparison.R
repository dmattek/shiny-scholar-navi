#' Comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Comparison_ui <- function(id, label, navid){
  ns <- NS(id)

  tabPanel(label,
           value = navid,
           tagList(
             h2('Comparison'),

             fluidPage(
               fluidRow(
                 textInput(inputId = ns('tiGSurl1'),
                           label = 'Researcher 1: type in Google Scholar ID or copy the URL of Google Scholar profile',
                           value = '',
                           width = "50%"),
                 textInput(inputId = ns('tiGSurl2'),
                           label = 'Researcher 2: type in Google Scholar ID or copy the URL of Google Scholar profile',
                           value = '',
                           width = "50%"),
                 actionButton(inputId = ns('butGSurlSubmit'),
                              label = 'Submit'),
                 br(), br(),
                 radioButtons(ns('rbRawCum'),
                              label = 'Citations:',
                              choices = c('Per year' = 'per_year',
                                          'Cumulative' = 'cumulative')),
                 plotly::plotlyOutput(ns('plotCitationTrend'))
               )
             )
           )
  )
}

#' Comparison Server Functions
#'
#' @noRd
mod_Comparison_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Process the URL from the input text box
    rfProcessURL1 <- function() {
      cat('mod_Researcher_server:rfProcessURL1\n')

      dummyDep <- input$butGSurlSubmit

      locS <- gsub('.*user=([A-Z,a-z,0-9]+)&.*',
                   '\\1',
                   isolate(input$tiGSurl1))

      return(locS)
    }

    # Process the URL from the input text box
    rfProcessURL2 <- function() {
      cat('mod_Researcher_server:rfProcessURL2\n')

      dummyDep <- input$butGSurlSubmit

      locS <- gsub('.*user=([A-Z,a-z,0-9]+)&.*',
                   '\\1',
                   isolate(input$tiGSurl2))

      return(locS)
    }

    # Plot citation comparison
    output$plotCitationTrend <- plotly::renderPlotly({
      cat("mod_Comparison_server:plotCitationTrend\n")

      # Obtain researcher ID from the URL
      locResID1 <- rfProcessURL1()
      locResID2 <- rfProcessURL2()

      shiny::validate(
        shiny::need(!locResID1 == '',
                    'Cannot plot citation trend. Provide ID of the 1st researcher.'),
        shiny::need(!locResID2 == '',
                    'Cannot plot citation trend. Provide ID of the 2nd researcher.')

      )

      # Get citation history for the researcher ID
      locCH <- as.data.table(scholar::compare_scholar_careers(c(locResID1, locResID2)))

      locCH[,
            cites_cumul := cumsum(cites),
            by = name]

      locPlotVar <- switch(input$rbRawCum,
                           per_year = 'cites',
                           cumulative = 'cites_cumul')

      p1 = plotly::plot_ly(data = locCH,
                           x = ~career_year, y = ~get(locPlotVar),
                           type = 'scatter',
                           mode = 'lines+markers',
                           color = ~name,
                           line = list(colorscale = 'Accent'),
                           marker = list(colorscale = 'Accent')) %>%
        plotly::layout(title = "Citation history comparison",
                       xaxis = list(title = "Career year"),
                       yaxis = list (title = "Citations"))
      p1
    })

  })
}

## To be copied in the UI
# mod_Comparison_ui("Comparison_1")

## To be copied in the server
# mod_Comparison_server("Comparison_1")
