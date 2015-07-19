#UI for interval

library(shiny)
library(ggplot2)


fluidPage(
  tabsetPanel(type = "tabs"
              ,
              tabPanel( "1) Data"
                        ,
                        sidebarLayout(
                          sidebarPanel(
                            h2("Select data file")
                            ,
                            fileInput("infile", "Choose tab-delim file")
                            ,
                            h2("Choose variables")
                            ,
                            selectInput("varnames", "Interval", "Choose..." )    
                            ,
                            selectInput("targetname", "Target (with 2 classes only)", "Choose...")    
                            ,
                            textOutput(outputId = "text.1")
                            
                          )#sidebarpanel
                          ,
                          mainPanel(
                            #         plotOutput(outputId = "plot.1.test", height = 300, click = "plot.1.click")
                            #         ,
                            #         verbatimTextOutput(outputId = "plot.1.text")
                            #         ,
                            tableOutput(outputId = "plot.1.table")
                            ,
                            h3("Variable density")
                            ,
                            plotOutput("plot.1.histogram", height = 300)
                            ,
                            h3("Distribution of variable per target class")
                            ,
                            plotOutput("plot.1.distributions", height = 300)
                            
                            
                          )#mainpanel
                        )#sidebarlayout
              )#tabpanel
              ,
              tabPanel("2) Pre-bucket"
                       ,
                       sidebarLayout(
                         sidebarPanel(
                           h2("Bucket numbers")
                           ,
                           p("Cuts variable in sized pieces... fewer buckets = shorter calc time")
                           ,
                           uiOutput(outputId = "ui_slider")
#                            sliderInput("setbuckets", label = "Number of buckets"
#                                        , min = 0, max = 1, value = 0.5, step = 1)
                           ,
                           textOutput("nr_of_buckets_to_calculate")
                           ,
                           h3("Chosen variables")
                           ,
                           selectInput("preinterval1", label = "Variable:", choices = "Choose...")
                           ,
                           selectInput("preinterval2", label = "Target:", choices = "Choose...")
                           
                         )#sidebarpanel
                         ,
                         mainPanel(
                           #         div("main panel")
                           #         ,
                           h3("Target rate per bucket")
                           ,
                           plotOutput("plot.2.top", height = 300)
                           ,
                           h3("Distribution per target class")
                           ,
                           plotOutput("plot.2.bottom", height = 300)
                           ,
                           textOutput("text.2")
                         )#mainpanel
                       )#sidebarlayout
                       
              )#tabpanel
              ,
              tabPanel("3) Calculate"
                       ,
                       actionButton(inputId = "calculate", label = "Run calculation")
                       ,
                       textOutput("text3")
                       ,
                       textOutput("text31")
              )#tabpanel
              ,
              tabPanel("4) Plot"
                       ,
                       sidebarLayout(
                         sidebarPanel(
                           h2("Filter settings")
                           ,
                           uiOutput(outputId = "ui_n_slider")
                           ,
                           uiOutput(outputId = "ui_m_slider")
                           ,
                           
#                            ,
#                            sliderInput(inputId = "n_slider", label = "Number of observations", min = 0, max = 10, value = c(0, 5), step = 1, ticks = F)
#                            ,
#                            sliderInput(inputId = "m_slider", label = "Targetrate (%)", min = 0, max = 10, value = c(0, 5), step = 0.001)
#                            ,
                           sliderInput(inputId = "pointsize_slider", label = "Point size", min = 1, max = 10, value = c(1), step = 1)
                           ,
                           h2("Zoom")
                           ,
                           uiOutput(outputId = "ui_xlimit")
                           ,
                           uiOutput(outputId = "ui_ylimit")

#                            ,
#                            sliderInput(inputId = "xlimit", label = "X-axis limits", min = 1, max = 10, value = c(1, 10), step = 1)
#                            ,
#                            sliderInput(inputId = "ylimit", label = "Y-axis limits", min = 1, max = 10, value = c(1, 10), step = 1)
                         )#siderbarpanel 
                         ,
                         mainPanel(
                           h3("Intervals with targetrate color")
                           ,
                           p("Each point is 1 interval, x=lower limit, y=upper limit. Zooom by dragging + dbl-click. One click = show data for point.")
                           #,
                           #textOutput("text4")
                           ,
                           plotOutput("plot.4.top", height = 300, click = "plot.4.click", brush = brushOpts(id = "plot.4.brush", resetOnNew = T), dblclick = "plot.4.dblclick")
                           ,
                           tableOutput(outputId = "plot.4.table")
                           ,
                           verbatimTextOutput(outputId = "plot.4.text")
                         )#mainpanel
                       )#sidebarlayout
                       
              )#tabpanel
              
              
              
              
              
              
  )#tabsetpanel
)#fluidpage