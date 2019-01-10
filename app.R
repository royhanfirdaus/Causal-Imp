#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(CausalImpact)
library(forecast)
library(lubridate)
library(ggplot2)
library(plotly)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title="Causal Impact",
    tags$li(a(href = 'https://www.linkedin.com/in/royhan-firdaus-032094141/',
              icon("linkedin"),
              title = "About the Author"),
            class = "dropdown"),
    
    tags$li(a(href = 'https://algorit.ma',
              img(src = 'logo.png',
                  title = "Company Home", height = "20px"),
              style = "padding-top:14px; padding-bottom:10px;"),
            class = "dropdown")
    
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id="sidebar",
      menuItem("Overview", tabName = "overview", icon=icon("star")),
      menuItem("Impact Analyst", tabName = "impact", icon=icon("brain")),
      menuItem("Github Links", 
               icon=icon("github"), href="https://github.com/royhanfirdaus")
    )
    
    
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    
    tabItems(
      # Overview page
      tabItem("overview",
              h2("Trends Plot"),
              fluidRow(
                box(
                  title="Trends / Interest Over Time", solidHeader=T,
                  collapsible = T,
                  width=12,
                  plotOutput("trend1", height = 300)
                ),
                sidebarPanel(
                  sliderInput("dates",
                              "Pick The Dates:",
                              min = as.Date("2016-01-01","%Y-%m-%d"),
                              max = as.Date("2019-01-11","%Y-%m-%d"),
                              value=c(as.Date("2018-01-01"),as.Date("2018-06-30"))
                                      ,timeFormat="%Y-%m-%d")
                ),
                box(
                  title="Internal Memo",
                  solidHeader = T,
                  width=8,
                  p("Trends / Interest over time is Numbers represent search interest relative to the highest point on the chart for the given region and time. A value of 100 is the peak popularity for the term. A value of 50 means that the term is half as popular. A score of 0 means there was not enough data for this term.")
                )
                
              )
      ),
      
      tabItem("impact",
              h2("Impact Plot"),
              fluidRow(
                box(
                  title="How Strong The Impact?", solidHeader=T,
                  collapsible = T,
                  width=8,
                  plotOutput("offs1", height = 500)
                ),
                box(
                  tabsetPanel(
                  tabPanel("Analysis Summary", 
                  verbatimTextOutput("summ") 
                ),
                  tabPanel("Analysis Report",
                  textOutput("rep")
                  )
                ), width = 4
              )
              
              
      )
      
    )
    )
    )
  )
  

# Define server logic required to draw a histogram
server <- function(input, output) {

  ## Ouput Overview
  
  data1 <- read.csv("bp.csv", stringsAsFactors = F)
  
  
  files <- dir('data_input/')
  df <- read.csv(paste0('data_input/',files[1]),skip=2)
  for(i in 2:length(files)){
    temp <- read.csv(paste0('data_input/',files[i]), skip=2)
    df <- rbind(df, temp)
  }
  df$Day <- ymd(df$Day)
  
  output$trend1 <- renderPlot({
    cat(input$dates)
    # ggplotly(plot1,originalData = F)
    
    plot1 <- ggplot(df, aes(Day, BLACKPINK...Worldwide.)) + geom_line()+
      xlab("Date") + ylab("BLACKPINK Trend Worldwide") + theme_bw() +
      scale_x_date(limits = as.Date(input$dates, origin="1970-01-01"))
    plot1
  })
  
  ## Ouput Analyst
  tsdat <- data1$BLACKPINK...Worldwide.
  ts1 <- ts(tsdat, frequency = 7)
  post.period <- c(165, 181)
  post.period.response <- ts1[post.period[1] : post.period[2]]
  ts1[post.period[1] : post.period[2]] <- NA
  # 
  # ss <- AddLocalLevel(list(), ts1)
  # bsts.model <- bsts(ts1 ~ 1, ss, niter = 5000)
  
  bsts.model <- readRDS("bsts.RDS")
  
  impact1 <- CausalImpact(bsts.model = bsts.model,
                          post.period.response = post.period.response)
  
  output$offs1 <- renderPlot(
    plot(impact1)
    )
  
  output$summ <- renderPrint(
    summary(impact1)
  )
  
  output$rep <- renderPrint(
    gsub("[\r\n]", "", impact1$report)
  )
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

