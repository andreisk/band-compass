library(shiny)
library(shinydashboard)
library(rhandsontable)
library(ggplot2)

body = dashboardBody(
  tabItems(
    tabItem(tabName = 'rawtable',
            fluidPage(
              sidebarLayout(
                sidebarPanel(
                  helpText("Right-click on the table to delete/insert rows/columns.", 
                           "Double-click on a cell to edit."),
                  br(),
                  actionButton("save", "Save table")
                ),
                mainPanel(
                  rHandsontableOutput("hot")
                )
              )
            )
            ),
    tabItem(tabName = 'compass'#,
            #for every pair of vars, return a political compass
            ),
    tabItem(tabName = 'pca')
  )
)


ui = dashboardPage(
  dashboardHeader(title = 'Bands Charts'),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Raw Table", tabName = "rawtable", icon = icon("table")),
      menuItem("Insulting Number of Compasses", tabName = "compasses", icon = icon("th-large")),
      menuItem("PCA", tabName = "pca", icon = icon("fire"))
    )),
    body
)

server = function(input, output){
  df = readRDS('bands.RDS')
  values <- reactiveValues()
  
  ## Handsontable
  observe({
    if (!is.null(input$hot)) {
      df = hot_to_r(input$hot)
    } else {
       if (is.null(values[["df"]]))
         df <- df
       else
        df <- values[["df"]]
    }
    values[["df"]] <- df
  })
  
  output$hot <- renderRHandsontable({
    df <- values[["df"]]
    if (!is.null(df))
      rhandsontable(df, useTypes = FALSE, stretchH = "all")
  })
  
  ## Save 
  observeEvent(input$save, {
    finaldf <- isolate(values[["df"]])
    saveRDS(finaldf, 'bands.RDS')
  })
}

shinyApp(ui, server)