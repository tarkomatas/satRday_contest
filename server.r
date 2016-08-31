library(shiny)
library(ggplot2)
library(plotly)
library(openxlsx)
library(googleVis)
library(DT)

#-------------------------------------
# LOAD DATABASE
#-------------------------------------

database = read.xlsx("BUD flights 2007-2012 v2.xlsx", startRow = 1, colNames = TRUE)

#-------------------------------------
# MODIFY DATE
#-------------------------------------

database$DATE = convertToDate(database$DATE)

#-------------------------------------
# MODIFY DATABASE
#-------------------------------------

g_database = database
g_database$DATE = format(g_database$DATE, "%Y-%m")

#-------------------------------------
# SHINY
#-------------------------------------

shinyServer(
  function(input, output, session) {
    
    ### DATABASES
    
    ## Capacity usage by destination DATABASE
    
    databaseFilter <- reactive({
        database[database$DESTINATION == input$DEST & database$FLIGHT_DIRECTION == input$FL_DIR & database$FLIGHT_TYPE == input$FL_TYPE ,]
    })
    
    ## Country specific aggregation DATABASE
    
     g_databaseFilter <- reactive({
         if (input$MONTHLY_FILTER == TRUE) {
           y_m = paste(input$YEAR, input$MONTH, sep = "")
           g_database = g_database[g_database$FLIGHT_DIRECTION == input$FL_DIR & g_database$FLIGHT_TYPE == input$FL_TYPE ,]
           g_database_aggr = aggregate(x = eval(parse(text = paste("g_database$",input$select_var))), by=list(g_database$COUNTRY, g_database$DATE), FUN = sum)
         }         
         else {
           y_m = input$YEAR
           g_database = g_database[g_database$FLIGHT_DIRECTION == input$FL_DIR & g_database$FLIGHT_TYPE == input$FL_TYPE ,]
           g_database_aggr = aggregate(x = eval(parse(text = paste("g_database$",input$select_var))), by=list(g_database$COUNTRY, g_database$DATE_YEAR), FUN = sum)
         }
         g_database_aggr = g_database_aggr[g_database_aggr$Group.2 == y_m,]  
         g_database_aggr = cbind(g_database_aggr, Population$Flag[match(g_database_aggr$Group.1, Population$Country)])
     })

    ### PLOTS    
    
    ## Time series of seat capacity and number of passangers PLOT
     
    # GGPLOT 

    plotInput <- reactive({
       ggplot(databaseFilter(),
          aes(x = DATE, y = NBR_OF_PASSENGERS)) +
          geom_area() +
          geom_area(aes(x = DATE, y = SEAT_CAPACITY), fill = "blue", alpha = 0.5) +
          xlab("date") +
          scale_y_continuous("unit")
    })
     
    # PLOTLY
     
     output$phonePlot <- renderPlotly({
        ggplotly(plotInput())
     })
     
    ## Capacity usage per year PLOT
    
    output$gauge <- renderGvis({
      g_database = g_database[g_database$FLIGHT_DIRECTION == input$FL_DIR & g_database$FLIGHT_TYPE == input$FL_TYPE ,]
      g_database_PASS = aggregate(x = g_database$NBR_OF_PASSENGERS, by=list(g_database$DESTINATION, g_database$DATE_YEAR), FUN = sum)
      g_database_CAP = aggregate(x = g_database$SEAT_CAPACITY, by=list(g_database$DESTINATION, g_database$DATE_YEAR), FUN = sum)
      g_database_CAP$x = g_database_PASS$x / g_database_CAP$x
      g_database_CAP = g_database_CAP[g_database_CAP$Group.1 == input$DEST,]
      g_database_CAP[,2] = as.character(g_database_CAP[,2])
        gvisGauge(g_database_CAP[,c(2,3)],
            options=list(min = 0, max = 1, greenFrom=0.66,
                  greenTo=1, yellowFrom=0.34, yellowTo=0.65,
                      redFrom=0, redTo=0.33
            )
        )
    })
    
    ### Country map by target value PLOTS & DATABASE
    
    output$gvis <- renderGvis({
      G = gvisGeoChart(g_databaseFilter(),
            locationvar = "Group.1", colorvar = "x",
              options = list(width = 750, height = 400,
              colorAxis = "{colors:['#FFFFFF', '#0000FF']}",
              region = input$REGION
              )                    
          )
      g_databaseFilter = g_databaseFilter()
      g_databaseFilter = g_databaseFilter[,c(1,4,3)]
      names(g_databaseFilter) = c("Country name", "Flag", "sum")
      g_databaseFilter = g_databaseFilter[with(g_databaseFilter, order(sum, decreasing = TRUE)),]
      
      T = gvisTable(g_databaseFilter,
             options=list(page='enable')
          )
      
      gvisMerge(G,T, horizontal=TRUE) 
    })
    
    ### FOR DYNAMIC TABPANEL    
    
    observe({
      updateSelectInput(session, "CITY", choices= unique(database$CITY[database$COUNTRY == input$COUNTRY & is.na(database$CITY) == FALSE]))
    })  
    
    observe({
    updateSelectInput(session, "DEST", choices= unique(database$DEST[database$CITY == input$CITY & is.na(database$DEST) == FALSE]))
    })
  }
)
