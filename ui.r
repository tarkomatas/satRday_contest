library(shiny)
library(ggplot2)
library(plotly)
library(openxlsx)
library(shinythemes)
library(DT)

#-------------------------------------
# LOAD DATABASE
#-------------------------------------

database = read.xlsx("BUD flights 2007-2012 v2.xlsx", startRow = 1, colNames = TRUE)

#-------------------------------------
# MODIFY DATABASE
#-------------------------------------

g_database = database
g_database$DATE = convertToDate(g_database$DATE)
g_database$DATE = format(g_database$DATE, "%Y-%m")

#-------------------------------------
# SHINY
#-------------------------------------

shinyUI(pageWithSidebar(
  
  ### HEADER
  
  tags$div(class ="header",
    tags$style(HTML("
      .header {
        background-color: black;
        border-style: solid;
        border-radius: 5px;
        border: 2px solid #f5f5f5;
        height: 165px;
      }
    ")),
    a(tags$img(id ="image", 
      tags$style(HTML("
        #image {
          width: 150px;
          display: block;
          margin-top: 10px;
          margin-right: 10px;
          display: block;
          float: right;
        }
      ")),
      src="logo.png"),
      href = "http://budapest.satrdays.org/"
    ),
    tags$h2(id ="title1", "ANALYZE OF FERENC LISZT INTERNATIONAL AIRPORT DATABASE",
       tags$style(HTML("
            #title1 {
              margin-top: 50px;
              margin-left: 10px;
              color: white;
              text-align: center;
              font-size: 250%;
            }
       "))
    ),
    tags$h4(id ="title2", "DYNAMIC RSTUDIO-SHINY APPLICATION",
      tags$style(HTML("
        #title2 {
           margin-top: 10px;
           margin-left: 10px;
           color: #5F9AE2;
           text-align: center;
           font-size: 175%;
        }
      "))
    )
  ),
  
  ### NAVBAR
  
  sidebarPanel(
    wellPanel(
      icon("wrench"),
      HTML("<b>NAVIGATION BAR</b>"),
      br(),
      br(),
      helpText("Use the radio-buttons to choose the right option for You."),
      radioButtons("FL_DIR", "Select flight direction:", choices= unique(database$FLIGHT_DIRECTION)),
      radioButtons("FL_TYPE", "Select fligt type:", choices= unique(database$FLIGHT_TYPE), selected = "Scheduled")
    ),
    wellPanel(
      HTML("<b>ABOUT ME</b>"),
      br(),
      HTML("Tamás Markó"),
      br(a(icon("linkedin-square"),
           href="https://www.linkedin.com/in/tam%C3%A1s-mark%C3%B3-b88505b9?trk=hp-identity-name"
         ),
         a(icon("envelope "),
           href="mailto:marko.tamas1991@gmail.com"
         )
      )
    ),
    wellPanel(
      HTML("<b>SOURCE CODE</b>"),
      br(a(icon("github"),
           href="https://github.com/tarkomatas/satRday_contest"
      ))
    ),
    wellPanel(
      HTML("<b>LAST UPDATE</b>"),
      br(HTML("31/08/2016"))
    ),
    br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
    br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),  
    width = 3
  ),
  
  ### MAIN PANEL
  
  mainPanel(
    tabsetPanel(type = "pills",
      
      ## Country specific aggregation TABPANEL          
                
      tabPanel("Country specific aggregation", 
          icon = icon("map-marker"),
          tags$h3(id ="title3", "Country specific aggregation",
            tags$style(HTML("
                #title3 {
                   font-weight: bold;
                   text-align: center;
                   color: #5F9AE2;
                 }
            "))
          ),
          br(),
          fluidRow(
            column(4, wellPanel(
                selectInput("select_var", "Select target value:", choices= c("Number of passangers" = "NBR_OF_PASSENGERS" , "Number of flights" = "NBR_OF_FLIGHTS", "Sum of cargo Weight" = "CARGO_WEIGHT", "Number of seat capacity" = "SEAT_CAPACITY"))
             )),
            column(4, wellPanel(
              sliderInput("YEAR", "Select year:", min = 2007, max = 2012, value = 2007, sep = "", animate = animationOptions(interval = 3000, loop = F)),
              checkboxInput("MONTHLY_FILTER", "Monthly filter"),
              conditionalPanel(
                  condition = "input.MONTHLY_FILTER == true",
                  selectInput("MONTH", "Select month:", choices = c("January" = "-01", "Februray" = "-02", "March" = "-03", "April" = "-04", "May" = "-05", "June" = "-06", "July" = "-07", "August" = "-08", "September" = "-09", "October" = "-10", "November" = "-11", "December" = "-12"))
              )
            )),   
            column(3, wellPanel(
              radioButtons("REGION", "Select region:", choices= c("Whole world" = "world", "Europe" = 150))
            ))
          ),
          tags$h4(id ="title4", "Country map by target value"),
          br(),
          htmlOutput("gvis"),
          htmlOutput("table")
      ),
      
      ## Capacity usage by destination TABPANEL
      
      tabPanel("Capacity usage by destination", 
          icon = icon("line-chart"),
          tags$h3(id ="title3", "Capacity usage by destination"),
          br(),
          column(4, wellPanel(
              selectInput("COUNTRY", "Select country:", choices= sort(unique(database$COUNTRY)), selected = c("Austria"))
          )),
          column(4, wellPanel(
              selectInput("CITY", "Select city:", choices= NULL)
          )),
          column(4, wellPanel(
              selectInput("DEST", "Select destination:", choices= NULL)
          )),
          fluidRow(
             column(12,
                 tags$h4(id ="title4", "Time series of seat capacity and number of passangers ",
                     tags$style(HTML("
                         #title4 {
                            font-weight: bold;
                             text-align: center;
                         }
                     "))        
                 ),
                 plotlyOutput("phonePlot"),
                 tags$h4(id ="title4", "Capacity usage per year"),
                 br(),
                 htmlOutput("gauge")
             )
          )
      ),
      
      ## Help TABPANEL
      
      tabPanel("Help", 
          icon = icon("medkit"),
          br(),
          tags$h3(id ="title3", "Analyze of Ferenc Liszt International Airport database with RStudio Shiny application"),
          tags$div(id ="help_format",
              tags$style(HTML("
                  #help_format {
                      font-size: 130%;
                  }
              ")),
              br("This application was built for", a("Budapest satRday Conference", href = "http://budapest.satrdays.org/"), "data visualization challenge, which will take place in 03/09/2016."),
              br("The dataset was come from the", a("Hungarian Central Statistical Office", href = "https://www.ksh.hu/?lang=en"), "which includes the flights to and from the Budapest Ferenc Liszt International Airport between 2007 and 2012. You can download the Excel database", a("here", href = "http://budapest.satrdays.org/data/BUD%20flights%202007-2012%20v2.xlsx"), "."),
              br("This is an interactive", a("RStudio Shiny", href = "http://shiny.rstudio.com/"), "web-based application so it was written in R language but this also includes some little HTML and CSS tricks."),
              tags$img(id ="gif_size", src = "animation1.gif",
                       tags$style(HTML("
                         #gif_size {
                            width: 60%;
                            height: 60%;
                            display: block;
                            margin: 0 auto;
                         }"
                       ))
              ),
              br("The application has two main parts:"),
              tags$ul(
                  tags$li("You can compare cities according to the key variables on", icon("map-marker") ,tags$b("the country specific report"), ", with a country map, and also with fancy table."),
                  tags$li("There is a capacity usage report which consists", icon("line-chart") ,tags$b("a year level aggregation"), "(which is a nice", a("googleVis", href = "https://github.com/mages/googleVis#googlevis"), "chart by the way) and also a seat capacity and number of passengers comparison.")
              ),
              br("This application is", tags$b("fully customizable:"),"so for example in the capacity usage report You can select any of the possible destinations with a dynamic list. In the country specific aggregation not only the year is selectable (try to animate it!",icon("play") ,"), You can also filter the data by month (if You want) and It is also possible to narrow the aspect of the map. There are another useful customizable solutions if You check the",icon("wrench") ,"sidebar."),
              tags$img(id ="gif_size", src = "animation2.gif"),
              br("I think this is a nice tool for quick analysis to discover the key meaning of the data. There is a semi-closed structure: the possible key analysis aspects are seen, but the user can also discover some interesting facts, can check the details.")
          )
      )  
    ) 
  )  
))  
