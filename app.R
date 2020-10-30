library(shiny)
library(datasets)
library(plotly)
library(ggplot2)
library(shinythemes)
library(GGally)
library(dslabs)
library(ggthemes)

#example data
data(iris)
data(mtcars)
data("PlantGrowth")
data("murders")
data("USArrests")


ui <- shinyUI(fluidPage(theme = shinytheme("darkly"),
                        titlePanel("Data visualization using Pair plots and Scatter plot with GG plot"),
                        br(),
                        br(),
                        tabsetPanel(
                            
                            tabPanel("Introduction",
                                     br(),
                                     h3("Scatter Plot", align = "left"),
                                     br(),
                                     h5("A scatter plot is a type of plot or mathematical diagram using Cartesian coordinates to display values for typically two variables for a set of data. If the points are coded , one additional variable can be displayed. The data are displayed as a collection of points, each having the value of one variable determining the position on the horizontal axis and the value of the other variable determining the position on the vertical axis."),
                                     h5("A scatter plot shows the direction of a relationship between the variables. A clear direction happens when there is either: High values of one variable occurring with high values of the other variable or low values of one variable occurring with low values of the other variable."),
                                     br(),
                                     h3("Pair plot",align="left"),
                                     h5("A pairplot plot a pairwise relationships in a dataset. The pairplot function creates a grid of Axes such that each variable in data will by shared in the y-axis across a single row and in the x-axis across a single column."),
                                     h5("To plot multiple pairwise bivariate distributions in a dataset, you can use the pairplot() function. This shows the relationship for (n,2) combination of variable in a DataFrame as a matrix of plots and the diagonal plots are the univariate plots."),
                                     plotOutput("Introduction")
                            ),
                            #--------------------------------------------------------------------------------------------------------------
                            tabPanel("Upload File",
                                     br(),
                                     titlePanel("Uploading Files"),
                                     sidebarLayout(
                                         sidebarPanel(
                                             fileInput('file1', 'Choose CSV File',
                                                       accept=c('text/csv', 
                                                                'text/comma-separated-values,text/plain', 
                                                                '.csv')),
                                             tags$br(),
                                             checkboxInput('header', 'Header', TRUE),  
                                         ),
                                         mainPanel(
                                             tableOutput('contents')
                                         )
                                     )
                            ),
                            
                            #--------------------------------------------------------------------------------------------------------------
                            
                            tabPanel("Scatterplot",
                                     
                                     pageWithSidebar(
                                         headerPanel('Scatter plot'),
                                         sidebarPanel(
                                             
                                             
                                             selectInput('xcol', 'X Variable', ""),
                                             selectInput('ycol', 'Y Variable', "", selected = ""),
                                             selectInput("plot.type","Plot Type:",
                                                         list(scatterplots = "scatterplots")
                                             ),
                                         ),
                                         mainPanel(
                                             plotlyOutput('MyPlot')
                                         )
                                     )),
                            
                            
                            #-------------------------------------------------------------------------------------------------
                            tabPanel("Pair Plot",
                                     br(),
                                     pageWithSidebar(
                                         
                                         headerPanel('Pair plot'),
                                         sidebarPanel(" ",
                                                      
                                                      
                                                      selectInput("data1","Choose a dataset",choices = list(
                                                          "Iris",
                                                          "Cars",
                                                          "Murders",
                                                          "USArrests",
                                                          "MTcars"), selected=NULL),
                                         ),
                                         mainPanel(
                                             plotlyOutput('MyPlot2')
                                         )
                                     )
                                     
                            ),
                            #------------------------------------------------------------------------------------------------------
                            tabPanel("About Us",
                                     br(),
                                     h2("Our Team", align = "left"),
                                     br(),
                                     h4("Name:  Yash Nareshkumar Patel"),
                                     h5("E Mail: yashnpatel18@gnu.ac.in"),
                                     br(),
                                     br(),
                                     h4("Name:  Henal S Patel"),
                                     h5("E Mail: henalpatel18@gnu.ac.in"),
                                     plotOutput("aboutUs"))
                            
                            
                        )
                        
)
)

server <- shinyServer(function(input, output, session) {
    
    data <- reactive({ 
        req(input$file1)
        
        inFile <- input$file1 
        df <- read.csv(inFile$datapath, header = input$header, sep =",",quote = input$quote)
        updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                          choices = names(df), selected = names(df))
        updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                          choices = names(df), selected = names(df)[2])
        
        return(df)
    })
    
    output$contents <- renderTable({
        data()
    })
    
    output$MyPlot <- renderPlotly({
        
        p <-ggplot(data(),aes(x=data()[,input$xcol],y=data()[,input$ycol],col=data()[,input$xcol]))+geom_point()+xlab(input$xcol)+ylab(input$ycol)
    })
    
    
    
    
    #------------------------------------
    output$MyPlot2 <- renderPlotly({
        if(input$data1=="Iris"){
            ggpairs(iris)
        }
        else if(input$data1 =="Murders"){
            ggpairs(murders[,c(3,4,5)])
        }
        else if(input$data1 =="USArrests"){
            ggpairs(USArrests)
        }
        else if(input$data1 =="MTcars"){
            ggpairs(mtcars)
        }
        else{
            ggpairs(cars)
        }
    })
})

shinyApp(ui, server)