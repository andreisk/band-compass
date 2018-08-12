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
                           "Double-click on a cell to edit.",
                           "Table must have more columns than rows to run principal components analysis."),
                  br(),
                  actionButton("save", "Save table/update plots")
                ),
                mainPanel(
                  rHandsontableOutput("hot")
                )
              )
            )
            ),
    tabItem(tabName = 'compasses',
            #for every pair of vars, return a political compass
            fluidPage(
              uiOutput('comps')
              )
            )
    ,
    tabItem(tabName = 'pca',
            fluidRow(
              box(title = 'Biplot',
                  plotOutput('biplot')),
              box(title = 'Component Loadings',
                  verbatimTextOutput('loadings'))
            ))
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
       if (is.null(values[['df']]))
         df <- df
       else
        df <- values[['df']]
    }
    values[['df']] <- df
  })
  
  output$hot <- renderRHandsontable({
    df <- values[['df']]
    if (!is.null(df))
      rhandsontable(df, useTypes = FALSE, stretchH = "all")
  })
  
  # save/update/generate plots
  observeEvent(input$save, {
    #save
    finaldf <- isolate(values[['df']])
    saveRDS(finaldf, 'bands.RDS')
    rownames(finaldf) = finaldf[,1]
    #compass plots
    output$comps = renderUI({
      lapply(2:dim(values[['df']])[2]-1,function(i){
        output[[paste0('comp',i)]] = renderPlot({
          ggplot(finaldf,
                 aes(as.numeric(finaldf[,i]),
                     as.numeric(finaldf[,i+1]),
                     label=finaldf[,1])) + 
            xlim(0,10) + 
            ylim(0,10) + 
            labs(x=colnames(finaldf)[i+1],
                 y=colnames(finaldf)[i]) + 
            annotate("rect", xmin = 5, xmax = 10, ymin = 5, ymax = 10, fill= "red")  + 
            annotate("rect", xmin = 0, xmax = 5, ymin = 0, ymax = 5 , fill= "blue") + 
            annotate("rect", xmin = 5, xmax = 10, ymin = 0, ymax = 5, fill= "yellow") + 
            annotate("rect", xmin = 0, xmax = 5, ymin = 5, ymax = 10, fill= "green") +
            geom_label() +
            theme(axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.ticks.y=element_blank(),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank()
            ) +
            scale_x_continuous(position ='top') +
            scale_y_continuous(position = 'right')
        })
        plotOutput(paste0('comp',i))
      })
    })
    #pca plots
    if(dim(finaldf)[1] > dim(finaldf)[2]-1){
      pc = princomp(finaldf[,-1]) #cor=F as items are already on the same scale
      output$biplot = renderPlot({
        biplot(pc)
      })
      output$loadings = renderPrint({
        pc$loadings[,1:2]
      })
    }
  }
    )
}

shinyApp(ui, server)