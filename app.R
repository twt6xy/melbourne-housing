library(shiny)
library(DBI)
library(qcc)
library(nortest)
library(shiny)
library(e1071)
library(shinydashboard)
library(ggplot2)
library(DT)
library(shinythemes)
library(dplyr)
library(DBI)
library(readxl)
library(formattable)
library(lubridate)
library(reshape2)
library(data.table)
library(aplpack)
library(stringr)
library(splitstackshape)
library(zoo)
library(tidyr)
library(tseries)
library(forecast)
library(openxlsx)
library(shinyjs)
library(DiagrammeR)
library(bizdays)
library(fmsb)
library(crosstalk)
library(plotly)
library(DescTools)
library(rpart)
library(rpart.plot)
library("gains")
library(leaflet)
library(viridis)
library(xts)
library(dygraphs)
library(sankeywheel)
library(tigris)
library(fastDummies)
library(caret)
library(dplyr)
library(stats)
library(car)
library(qpcR)
library(pwr)
library(multcomp)
library(leaps)
library(shinyMatrix)
library(corrplot)
library(conquer)
library(lawstat)

# bring in your data 
# setwd(dir = "C:/Users/MSachs.MSACHS-DELL/Documents/UVA MSDS/STAT 6021/melbourne-housing")
melb.df <- read.csv("melbourne_cleaned.csv", stringsAsFactors = FALSE, na.strings = c(""," ","NA"))
melb.df <- data.frame(melb.df[,-1], row.names=melb.df[,1])
melb.df$Date <- as.Date(melb.df$Date,"%Y-%m-%d")
melb.df$Date2 <- format(melb.df$Date, "%Y-%m")
colnames(melb.df)[which(names(melb.df) == "Walk.Score")] <- "WalkScore"
melb.df$Population <- as.numeric(gsub("[,]","",melb.df$Population))
colnames(melb.df)[which(names(melb.df) == "Longtitude")] <- "Longitude"
colnames(melb.df)[which(names(melb.df) == "Lattitude")] <- "Latitude"
# names(melb.df)[names(melb.df) %in% c("Longtitude","Lattitude")] <- c("Longitude","Latitude")

'%notin%' <- Negate('%in%')

# mapbox token
#MAPBOX_TOKEN <- "pk.eyJ1IjoibWRzOWIiLCJhIjoiY2tvNm40a3I4MGNsZDJvb2E1N2ZkanNwbiJ9.xP-9d8brDK8KOi5qkl0bGw"

# create customer load page format
appCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"


# add elements as arguments to fluidPage()
ui <- shinyUI(dashboardPage(
  
  
  dashboardHeader(title = "Melbourne Real Estate Data",dropdownMenuOutput("messageMenu")),
  
  
  dashboardSidebar(
    sidebarMenu(id="tabs",
                sidebarMenuOutput("menu")))
  ,
  
  
  dashboardBody(
    useShinyjs(),
    inlineCSS(appCSS
    ),
    
    # Loading message
    div(id = "loading-content",
        h2("An A+ STATS 6021 Project Awaits....")
    ),
    # Application title
    
    hidden(
      div(id = "app-content",
          
          
          tabItems(
            tabItem("exp",
                    fluidRow(box(title = "Histogram Of Chosen Quantitative Variable By Chosen Categorical", plotlyOutput("hist")),
                             box(title = "Box Plot Of Chosen Quantitative Variable By Chosen Categorical", plotlyOutput("box"))),
                    fluidRow(box(title = "Heat Map Of Quantitative Variable", plotlyOutput("map")),
                             box(title = "Time Series Of Median Value Of Quantitative Variable", dygraphOutput("ts"))),
                    fluidRow(tabPanel("Table",DT::dataTableOutput("table")))),
            tabItem("stat",
                    fluidRow(box(title = "Scatter Plots Of Quantitative Variables",plotOutput("scatt")),
                    box(title = "Correlation Plot Of Quantitative Variables",plotOutput("corr"))),
                    fluidRow(tabBox(title = "Initial Model",tabPanel("Model Explanation",br(),shiny::p("This is the start of the modeling process. Select a predictor and response variable. Next, select your seed to make the results reproducible and determine your training-testing data split. In the output, is the coefficient statistically significant? Are the assumptions of linearity being met?",style="color:black;text-align:justify"),
                                                                 br(),width = 12,style="background-color:papayawhip;border-left:8px solid coral;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),tabPanel("Training vs Testing Split",numericInput("obs4s", "What percentage of data should be allocated to training:", 0.7, min = 0.5, max = 1)),
                           tabPanel("Set Seed", numericInput("obs5s", "Please set your seed so results are reproducible:", 10, min = 1, max = 100)),tabPanel("Model Output",verbatimTextOutput("smodel"))),
                    tabBox(title = "Assumption Checks",tabPanel("What To Look For",br(),shiny::p("There are four things we want to check for: 1) is there a linear relationship, 2) do the residuals have constant variance, 3) are the residuals independent, and 4) are the residuals normally distributed?",style="color:black;text-align:justify"),
                                                            br(),width = 12,style="background-color:papayawhip;border-left:8px solid coral;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),tabPanel("Linearity",plotOutput("smplot")),
                    tabPanel("Constant Variance",plotOutput("smplot1")), tabPanel("Independence",plotOutput("smplot2")),tabPanel("Normality",plotOutput("smplot3")))),
                    fluidRow(tabBox(title = "Box Cox Transformation",tabPanel("Why We Would Do It",br(),shiny::p("If our assumption of constant variance has been violated, or the assumption of linearity is not met, a Box Cox Transformation may be called for.",style="color:black;text-align:justify"),br(),width = 12,style="background-color:lightblue;border-left:8px solid blue;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
                           tabPanel("Box Cox Plot",plotOutput("bc")), tabPanel("Suggested Transformation",valueBoxOutput("bcvalue")),tabPanel("Chosen Transformation",numericInput("obs2", "Which Transformation Would You Like:", 0, min = -3, max = 3))),
                    tabBox(title = "Re-Check Assumptions",tabPanel("What To Look For",br(),shiny::p("Did our transformation of the response resolve our outstanding assumption issues? If not, we should consider transforming our predictor as well.",style="color:black;text-align:justify"),br(),width = 12,style="background-color:lightblue;border-left:8px solid blue;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
                           tabPanel("Linearity",plotOutput("tsmplot")),tabPanel("Constant Variance",plotOutput("tsmplot1")), tabPanel("Independence",plotOutput("tsmplot2")),tabPanel("Normality",plotOutput("tsmplot3")))),
                    fluidRow(tabBox(title = "Predictor Transformation",tabPanel("Why We Would Do It",br(),shiny::p("If our assumption of  linearity is not met after a Box Cox Transformation, a predictor transformation may be in order.",style="color:black;text-align:justify"),br(),width = 12,style="background-color:pink;border-left:8px solid red;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
                           tabPanel("Chosen Transformation",numericInput("obs3", "Which Transformation Would You Like:", 0, min = -3, max = 3))),
                    tabBox(title = "Re-Check Assumptions",tabPanel("What To Look For",br(),shiny::p("Did our transformation of the response resolve our outstanding assumption issues? If not, we should consider transforming our predictor as well.",style="color:black;text-align:justify"),br(),width = 12,style="background-color:pink;border-left:8px solid red;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
                           tabPanel("Linearity",plotOutput("ttsmplot")),tabPanel("Constant Variance",plotOutput("ttsmplot1")), tabPanel("Independence",plotOutput("ttsmplot2")),tabPanel("Independence 2",plotOutput("ttsmplot2a")),tabPanel("Normality",plotOutput("ttsmplot3")))),
                    fluidRow(tabBox(title = "Final Model With Categorical Variables",tabPanel("Explanation",br(),shiny::p("You have made your transformations of the response and predictor (if necessary), so now choose which categorical predictors you wish to add for your final model. We have also adjusted the response variable by month and year to deal with time series implications.",style="color:black;text-align:justify"),br(),width = 12,style="background-color:lightgreen;border-left:8px solid green;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
                           tabPanel("Model Output",verbatimTextOutput("fmodel")),tabPanel("Model Accuracy",valueBoxOutput("fpreds")),tabPanel("Prediction Interval Plot",plotOutput("fpredsplot"))),
                    tabBox(title = "Validate Assumptions",tabPanel("What To Look For",br(),shiny::p("Our final model should meet our three assumptions: 1) the residuals have constant variance, 2) the residuals are independent, and 4) the residuals are normally distributed.",style="color:black;text-align:justify"),br(),width = 12,style="background-color:lightgreen;border-left:8px solid green;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
                           tabPanel("Constant Variance",plotOutput("tsmplot1.sa")), tabPanel("Independence",plotOutput("tsmplot2.sa")),tabPanel("Normality",plotOutput("tsmplot3.sa")), tabPanel("Levene's Test",verbatimTextOutput("levenea"))))
                    ),
            tabItem("mod",fluidRow(tabBox(title = "Model Prep With Quantitative Variables",tabPanel("Training vs Testing Split",numericInput("obs4", "What percentage of data should be allocated to training:", 0.7, min = 0.5, max = 1)),
                                 tabPanel("Set Seed", numericInput("obs5", "Please set your seed so results are reproducible:", 10, min = 1, max = 100)),
                                 tabPanel("Model Criteria", DT::dataTableOutput("regsub"))),
                                 tabBox(title = "Inspect For Multicollinearity & Outliers",tabPanel("Residual Plots", plotOutput("totresplotm")),tabPanel("High Leverage Points", DT::dataTableOutput("lev")),tabPanel("DFFIT's", DT::dataTableOutput("dffits")),tabPanel("Cook's Distance", DT::dataTableOutput("cook")),tabPanel("Partial Regression Plots", plotOutput("partreg", height = "100%")),tabPanel("VIF Output",verbatimTextOutput("vif")))),
                    fluidRow(tabBox(title = "Model With Quantitative Predictors",tabPanel("Box Cox Plot",plotOutput("bc2")), tabPanel("Suggested Transformation",valueBoxOutput("bcvalue2")),tabPanel("Chosen Transformations",matrixInput("matrix",rows = list(names = TRUE,editableNames = TRUE),value = matrix(data = rep.int(1,12), 12,  1,dimnames = list(c("Price", c("Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")),c("User Transformation"))
                    ))), tabPanel("Model Output", verbatimTextOutput("fmodel2"))
                    ),tabBox(title = "Check Assumptions", tabPanel("Constant Variance",plotOutput("resplotm")), tabPanel("Independence",plotOutput("acfplotm")),
                             tabPanel("Normality",plotOutput("qplotm")))),
                    fluidRow(tabBox(title = "Model With Quantitative & Categorical Variables", tabPanel("Model Output", verbatimTextOutput("fmodel3")),tabPanel("Model Accuracy",valueBoxOutput("fpred")), tabPanel("Prediction Interval Plot",plotOutput("fpredplot")),
                                    tabPanel("Property Fields",matrixInput("matrix2",rows = list(names = TRUE,editableNames = TRUE),value = matrix(data = rep_len("Insert Value",21), 21,  1,dimnames = list(c("Price", sort(c("Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude","Type","Postcode","Suburb","CouncilArea","Regionname","SellerG","Method","Year","Month"), decreasing = FALSE)),c("User Inputs"))))),
                                    tabPanel("Estimated Price",valueBoxOutput("fpredval", width = 7))),
                             tabBox(title = "Re-Check Assumptions", tabPanel("Constant Variance",plotOutput("resplotm2")), tabPanel("Independence",plotOutput("acfplotm2")),
                                     tabPanel("Normality",plotOutput("qplotm2")),tabPanel("Residual Plots", plotOutput("totresplotm2")),tabPanel("High Leverage Points", DT::dataTableOutput("lev2")),tabPanel("DFFIT's", DT::dataTableOutput("dffits2")),tabPanel("Cook's Distance", DT::dataTableOutput("cook2")),tabPanel("Partial Regression Plots", plotOutput("partreg2", height = "100%")),tabPanel("VIF Output",verbatimTextOutput("vif2")),
                                    tabPanel("Levene's Test",verbatimTextOutput("levene")))))
            
          )
      )
    )
  )
  
))

# use the server function to assemble inputs into outputs
server <- shinyServer(function(input, output,session)
{
  hide(id = "loading-content", anim = TRUE, animType = "fade")    
  shinyjs::show("app-content")
  
  output$categ <- renderUI({
    categ <- sort(c("Type","Postcode","Suburb","CouncilArea","Regionname","SellerG","Method","Year","Month"), decreasing = FALSE)
    selectInput("categchoose", "Select Categorical Variable:", categ, selected = categ[1], multiple = FALSE)
  })
  
  output$categl <- renderUI({
    categl <- sort(unique(melb.df[,names(melb.df) %in% input$categchoose]), decreasing = FALSE)
    categl <- append(as.character(categl), "All", after =  0)
    selectInput("categlchoose", "Select Level To Categorical:", categl, selected = "All", multiple = TRUE)
  })
  
  output$quant <- renderUI({
    quant <- sort(c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude"),decreasing = FALSE)
    selectInput("quantchoose", "Select Quantitative Variable:", quant, selected = "Price", multiple = FALSE)
  })
  
  output$categ2 <- renderUI({
    categ2 <- sort(c("Type","SellerG","Postcode","Suburb","CouncilArea","Regionname","Method","Year","Month"), decreasing = FALSE)
    selectInput("categchoose2", "Select Categorical Predictor Variable(s) To Add:", categ2, selected = categ2[1], multiple = TRUE)
  })
  
  output$categ2a <- renderUI({
    categ2a <- sort(c("Type","SellerG","Postcode","Suburb","CouncilArea","Regionname","Method","Year","Month"), decreasing = FALSE)
    selectInput("categchoose2a", "Select Categorical Variable For Levene's:", categ2a, selected = categ2a[1], multiple = FALSE)
  })
  
  
  output$quant3 <- renderUI({
    quant3 <- sort(c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude"), decreasing = FALSE)
    selectInput("quantchoose3", "Select Predictor Variable:", quant3, selected = "Rooms", multiple = FALSE)
  })
  
  output$quant2 <- renderUI({
    quant2 <- sort(c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude"),decreasing = FALSE)
    selectInput("quantchoose2", "Select Response Variable:", quant2, selected = "Price", multiple = FALSE)
  })
  
  output$categ3 <- renderUI({
    categ3 <- sort(c("Type","SellerG","Postcode","Suburb","CouncilArea","Regionname","Method","Year","Month"), decreasing = FALSE)
    selectInput("categchoose3", "Select Categorical Predictor Variable(s) To Add:", categ3, selected = categ3[1], multiple = TRUE)
  })
  
  output$categ4 <- renderUI({
    categ4<- sort(c("Type","SellerG","Postcode","Suburb","CouncilArea","Regionname","Method","Year","Month"), decreasing = FALSE)
    selectInput("categchoose4", "Select Categorical Variable For Levene's:", categ4, selected = categ4[1], multiple = FALSE)
  })
  
  output$quant5 <- renderUI({
    quant5 <- sort(c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude"), decreasing = FALSE)
    selectInput("quantchoose5", "Select Predictor Variable(s):", quant5, selected = list("Rooms","YearBuilt"), multiple = TRUE)
  })
  
  output$quant4 <- renderUI({
    quant4 <- sort(c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude"),decreasing = FALSE)
    selectInput("quantchoose4", "Select Response Variable:", quant4, selected = "Price", multiple = FALSE)
  })
  
  output$quant6 <- renderUI({
    quant6 <- sort(c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude"), decreasing = FALSE)
    selectInput("quantchoose6", "Select Predictor Variable To Plot Predicted Values Against:", quant6, selected = "Rooms", multiple = FALSE)
  })
  
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Exploratory", tabName = "exp", icon = icon("globe"),selected = TRUE, startExpanded = TRUE,
               menuSubItem(dateRangeInput('dateRange',
                                          label = 'Date range input: yyyy-mm-dd',
                                          start = min(melb.df$Date), end = max(melb.df$Date)
               ), tabName = "exp"),
               menuSubItem(uiOutput("quant")),
               menuSubItem(uiOutput("categ")),
               menuSubItem(uiOutput("categl")),
               downloadButton("downloader", "Download Raw Data")
      ),
      menuItem("Simple Linear Regression", tabName = "stat" , icon = icon("signal"),
               menuSubItem(uiOutput("quant2"), tabName = "stat"),
               menuSubItem(uiOutput("quant3")),
               menuSubItem(uiOutput("categ2")),
               menuSubItem(uiOutput("categ2a"))),
      menuItem("Multivariate Linear Regression", tabName = "mod", icon = icon("laptop-code"),
               menuSubItem(uiOutput("quant4"), tabName = "mod"),
               menuSubItem(uiOutput("quant5")),
               menuSubItem(uiOutput("categ3")),
               menuSubItem(uiOutput("quant6")),
               menuSubItem(uiOutput("categ4")),
               downloadButton("downloadData", "Download Predicted Data"))
      
    )
  })
  
  
  data <- reactive({
    req(input$categlchoose)
    req(input$categchoose)
    req(input$quantchoose)
    req(input$dateRange)

    if("All" %in% input$categlchoose) {
      melb.df %>%
        filter_(quote(get(input$categchoose) != "@?><")) %>%
        filter_(quote(Date >= input$dateRange[1])) %>%
        filter_(quote(Date <= input$dateRange[2]))
    } else {
      melb.df %>%
        filter_(quote(get(input$categchoose) %in% input$categlchoose)) %>%
        filter_(quote(Date >= input$dateRange[1])) %>%
        filter_(quote(Date <= input$dateRange[2]))
    }

  })
  
  data2 <- reactive({
    req(input$quantchoose2)
    req(input$quantchoose3)
    req(input$categchoose2)
    req(input$obs5s)
    req(input$obs4s)
    # melb.df$Year <- year(melb.df$Date)
    # melb.df$Month <- month(melb.df$Date)
    set.seed(input$obs5s)
    dt = sort(sample(nrow(melb.df), nrow(melb.df)*input$obs4s))
    train<-melb.df[dt,]
    test<-melb.df[-dt,]
    train
  })
  
  data2t <- reactive({
    req(input$quantchoose2)
    req(input$quantchoose3)
    req(input$categchoose2)
    req(input$obs5s)
    req(input$obs4s)
    # melb.df$Year <- year(melb.df$Date)
    # melb.df$Month <- month(melb.df$Date)
    set.seed(input$obs5s)
    dt = sort(sample(nrow(melb.df), nrow(melb.df)*input$obs4s))
    train<-melb.df[dt,]
    test<-melb.df[-dt,]
    test
  })
  
  data3 <- reactive({
    req(input$quantchoose4)
    req(input$quantchoose5)
    req(input$categchoose3)
    req(input$obs5)
    req(input$obs4)
    # melb.df$Year <- year(melb.df$Date)
    # melb.df$Month <- month(melb.df$Date)
    melb.df <- melb.df[,names(melb.df) %in% c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Type","Method","Year","Month","SellerG","Postcode","Suburb","CouncilArea","Regionname","Year","Month","Population","WalkScore","Longitude","Latitude")]
    set.seed(input$obs5)
    dt = sort(sample(nrow(melb.df), nrow(melb.df)*input$obs4))
    train<-melb.df[dt,]
    test<-melb.df[-dt,]
    train
  })
  
  data4 <- reactive({
    req(input$quantchoose4)
    req(input$quantchoose5)
    req(input$categchoose3)
    req(input$obs5)
    req(input$obs4)
    # melb.df$Year <- year(melb.df$Date)
    # melb.df$Month <- month(melb.df$Date)
    melb.df <- melb.df[,names(melb.df) %in% c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Type","Method","Year","Month","SellerG","Postcode","Suburb","CouncilArea","Regionname","Year","Month","Population","WalkScore","Longitude","Latitude")]
    set.seed(input$obs5)
    dt = sort(sample(nrow(melb.df), nrow(melb.df)*input$obs4))
    train<-melb.df[dt,]
    test<-melb.df[-dt,]
    test
  })
  
  output$hist <- renderPlotly({
    fig <- plot_ly(
      type='histogram',
      data = data(),
      x=~get(input$quantchoose),
      split = ~get(input$categchoose),
      bingroup=1)
    
    fig <- fig %>% layout(
      barmode="stack", xaxis = list(title = input$quantchoose) ,
      bargap=0.1)
    
    fig
  })
  
  output$box <- renderPlotly({
    pl <- plot_ly(data = data(), y=~get(input$quantchoose), split = ~get(input$categchoose),
                  type="box")
    pl <- pl %>% layout(
      yaxis = list(title = input$quantchoose))
    pl
  })
  
  output$map <- renderPlotly({
    fig <- data()
    fig <- fig %>%
      plot_ly(
        lat = ~Latitude,
        lon = ~Longitude,
        marker = list(color = ~get(input$quantchoose), colorscale = "Viridis",showscale = TRUE),
        type = 'scattermapbox',
        hovertext = ~paste(Regionname, CouncilArea, Suburb,Postcode, paste(input$quantchoose,":", get(input$quantchoose)), sep = "<br />")) 
    fig <- fig %>% colorbar(title = paste("Intensity By",input$quantchoose, sep = "<br />")) %>%
      layout(
        mapbox = list(
          style = 'stamen-terrain',
          zoom =9,
          center = list(lat=-37.8, lon=145))) 
    
    fig
    
  })
  
  output$ts <- renderDygraph({
    ts <- data()
    ts <- aggregate(data = ts, get(input$quantchoose) ~Date2, FUN = median)
    names(ts) <- c("Date",input$quantchoose)
    ts$Date <- as.Date(paste(ts$Date,"-01",sep=""))
    ts <- xts( ts, order.by = ts$Date)
    ts <- dygraph(ts)
    ts
  })
  
  output$table <-  DT::renderDataTable({
    datatable(data(), extensions = 'Responsive')
  })
  
  
  output$corr <- renderPlot({
    data2 <- data2()
    names.list <- c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")
    data2 <- data2[names(data2) %in% names.list]
    data2 <- apply(data2, 2, as.numeric)
    data2 <- as.data.frame(data2)
    # data2 <- data2[complete.cases(data2),]
    res1 <- cor.mtest(data2, conf.level = .95)
    M <-cor(data2,use = "pairwise.complete.obs")
    corrplot.mixed(M, order="original", tl.cex = 1,tl.pos = "lt",tl.col = "black", tl.srt = 50, number.cex = 1,p.mat = res1$p, sig.level = .05
    )
  },width = "auto", height = "auto", execOnResize = TRUE,
  outputArgs = list())
  
  output$scatt <- renderPlot({
    pairs(data2()[,names(data2()) %in%   c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")], lower.panel = NULL)
  })
  
  output$smodel <- renderPrint({
    res<- lm(get(input$quantchoose2)~get(input$quantchoose3), data = data2())
    summary(res)
  })
  
  output$smplot <- renderPlot({
    plot(get(input$quantchoose2)~get(input$quantchoose3), data = data2(), main="Plot of Response Against Predictor")
  })
  
  output$smplot1 <- renderPlot({
    model <- lm(get(input$quantchoose2)~get(input$quantchoose3), data = data2())
    plot(model$fitted.values,model$residuals, main="Plot of Residuals against Fitted Values")
    abline(h=0,col="red")
  })
  
  output$smplot2 <- renderPlot({
    model <- lm(get(input$quantchoose2)~get(input$quantchoose3), data = data2())
    acf(model$residuals, main="ACF of Residuals (Squared)")
  })
  
  output$smplot3 <- renderPlot({
    model <- lm(get(input$quantchoose2)~get(input$quantchoose3), data = data2())
    qqnorm(model$residuals)
    qqline(model$residuals, col="red")
  })
  
  
  output$bc <- renderPlot({
    bc <- boxcox(get(input$quantchoose2) ~ get(input$quantchoose3), data = data2(),lambda = seq(-2, 2, 1/10))
  })
  
  output$bcvalue <- renderValueBox({
    bc <- boxcox(get(input$quantchoose2) ~ get(input$quantchoose3), data = data2(),lambda = seq(-2, 2, 1/10))
    bc <- bc$x[which.max(bc$y)]
    valueBox(
      round(bc, digits = 3), "Suggested Transformation", icon = icon("list-alt", lib = "glyphicon"), color = "blue")
    
  })
  
  output$tsmplot <- renderPlot({
    data <- data2()
    if(input$obs2 == 0){
      data$Transformed.Response <- log(data[,names(data) %in% input$quantchoose2])
    } else {
      data$Transformed.Response <- (data[,names(data) %in% input$quantchoose2]) ^ input$obs2
    }
    plot(Transformed.Response~get(input$quantchoose3), data = data, main="Plot of Response Against Predictor")
  })
  
  output$tsmplot1 <- renderPlot({
    data <- data2()
    if(input$obs2 == 0){
      data$Transformed.Response <- log(data[,names(data) %in% input$quantchoose2])
    } else {
      data$Transformed.Response <- (data[,names(data) %in% input$quantchoose2]) ^ input$obs2
    }
    model <- lm(Transformed.Response~get(input$quantchoose3), data = data)
    plot(model$fitted.values,model$residuals, main="Plot of Residuals against Fitted Values")
    abline(h=0,col="red")
  })
  
  output$tsmplot2 <- renderPlot({
    data <- data2()
    if(input$obs2 == 0){
      data$Transformed.Response <- log(data[,names(data) %in% input$quantchoose2])
    } else {
      data$Transformed.Response <- (data[,names(data) %in% input$quantchoose2]) ^ input$obs2
    }
    model <- lm(Transformed.Response~get(input$quantchoose3), data = data)
    acf(model$residuals, main="ACF of Residuals (Squared)")
  })
  
  output$tsmplot3 <- renderPlot({
    data <- data2()
    if(input$obs2 == 0){
      data$Transformed.Response <- log(data[,names(data) %in% input$quantchoose2])
    } else {
      data$Transformed.Response <- (data[,names(data) %in% input$quantchoose2]) ^ input$obs2
    }
    model <- lm(Transformed.Response~get(input$quantchoose3), data = data)
    qqnorm(model$residuals)
    qqline(model$residuals, col="red")
  })
  
  output$ttsmplot <- renderPlot({
    data <- data2()
    if(input$obs2 == 0){
      data$Transformed.Response <- log(data[,names(data) %in% input$quantchoose2])
    } else {
      data$Transformed.Response <- (data[,names(data) %in% input$quantchoose2]) ^ input$obs2
    }
    if(input$obs3 == 0){
      data$Transformed.Predictor <- log(data[,names(data) %in% input$quantchoose3])
    } else {
      data$Transformed.Predictor <- (data[,names(data) %in% input$quantchoose3]) ^ input$obs3
    }
    plot(Transformed.Response~Transformed.Predictor, data = data, main="Plot of Response Against Predictor")
  })
  
  output$ttsmplot1 <- renderPlot({
    data <- data2()
    if(input$obs2 == 0){
      data$Transformed.Response <- log(data[,names(data) %in% input$quantchoose2])
    } else {
      data$Transformed.Response <- (data[,names(data) %in% input$quantchoose2]) ^ input$obs2
    }
    if(input$obs3 == 0){
      data$Transformed.Predictor <- log(data[,names(data) %in% input$quantchoose3])
    } else {
      data$Transformed.Predictor <- (data[,names(data) %in% input$quantchoose3]) ^ input$obs3
    }
    model <- lm(Transformed.Response~Transformed.Predictor, data = data)
    plot(model$fitted.values,model$residuals, main="Plot of Residuals against Fitted Values")
    abline(h=0,col="red")
  })
  
  output$ttsmplot2 <- renderPlot({
    data <- data2()
    if(input$obs2 == 0){
      data$Transformed.Response <- log(data[,names(data) %in% input$quantchoose2])
    } else {
      data$Transformed.Response <- (data[,names(data) %in% input$quantchoose2]) ^ input$obs2
    }
    if(input$obs3 == 0){
      data$Transformed.Predictor <- log(data[,names(data) %in% input$quantchoose3])
    } else {
      data$Transformed.Predictor <- (data[,names(data) %in% input$quantchoose3]) ^ input$obs3
    }
    model <- lm(Transformed.Response~Transformed.Predictor, data = data)
    acf(model$residuals, main="ACF of Residuals (Squared)")
  })
  
  output$ttsmplot2a <- renderPlot({
    data <- data2()
    if(input$obs2 == 0){
      data$Transformed.Response <- log(data[,names(data) %in% input$quantchoose2])
    } else {
      data$Transformed.Response <- (data[,names(data) %in% input$quantchoose2]) ^ input$obs2
    }
    if(input$obs3 == 0){
      data$Transformed.Predictor <- log(data[,names(data) %in% input$quantchoose3])
    } else {
      data$Transformed.Predictor <- (data[,names(data) %in% input$quantchoose3]) ^ input$obs3
    }
    model <- lm(Transformed.Response~Transformed.Predictor, data = data)
    pacf(model$residuals, main="PACF of Residuals (Squared)")
  })
  
  output$ttsmplot3 <- renderPlot({
    data <- data2()
    if(input$obs2 == 0){
      data$Transformed.Response <- log(data[,names(data) %in% input$quantchoose2])
    } else {
      data$Transformed.Response <- (data[,names(data) %in% input$quantchoose2]) ^ input$obs2
    }
    if(input$obs3 == 0){
      data$Transformed.Predictor <- log(data[,names(data) %in% input$quantchoose3])
    } else {
      data$Transformed.Predictor <- (data[,names(data) %in% input$quantchoose3]) ^ input$obs3
    }
    model <- lm(Transformed.Response~Transformed.Predictor, data = data)
    qqnorm(model$residuals)
    qqline(model$residuals, col="red")
  })
  
  output$fmodel <- renderPrint({
    data = data2()
    data2 = data2t()
    # data.y <- aggregate(data[,names(data) %in% input$quantchoose2], by = list(data$Year), FUN = median)
    # names(data.y) <- c("Year","Median.y")
    # data.y$Year_Index <- data.y$Median.y/median(data[,names(data) %in% input$quantchoose2])
    # data.m <- aggregate(data[,names(data) %in% input$quantchoose2], by = list(data$Month), FUN = median)
    # names(data.m) <- c("Month","Median.m")
    # data.m$Month_Index <- data.m$Median.m/median(data[,names(data) %in% input$quantchoose2])
    # data <- merge(data,data.y, by = "Year", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose2] <- data[,names(data) %in% input$quantchoose2] * data$Year_Index
    # data <- merge(data,data.m, by = "Month", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose2] <- data[,names(data) %in% input$quantchoose2] * data$Month_Index
    for (f in input$categchoose2){
      data[,names(data) %in% f] <- as.factor(data[,names(data) %in% f])
    }
    data <- data[sample(NROW(data)),]
    if(input$obs2 == 0){
      data$Transformed.Response <- log(data[,names(data) %in% input$quantchoose2])
    } else {
      data$Transformed.Response <- (data[,names(data) %in% input$quantchoose2]) ^ input$obs2
    }
    if(input$obs3 == 0){
      data$Transformed.Predictor <- log(data[,names(data) %in% input$quantchoose3])
    } else {
      data$Transformed.Predictor <- (data[,names(data) %in% input$quantchoose3]) ^ input$obs3
    }
    f <- as.formula(paste("Transformed.Response", paste("Transformed.Predictor", ' + ',paste(input$categchoose2, collapse = " + ")), sep = " ~ "))
    reduced <- lm(f, data = data)
    summary(reduced)

  })
  
  output$fpreds <- renderValueBox({
    data = data2()
    data2 = data2t()
    # data.y <- aggregate(data[,names(data) %in% input$quantchoose2], by = list(data$Year), FUN = median)
    # names(data.y) <- c("Year","Median.y")
    # data.y$Year_Index <- data.y$Median.y/median(data[,names(data) %in% input$quantchoose2])
    # data.m <- aggregate(data[,names(data) %in% input$quantchoose2], by = list(data$Month), FUN = median)
    # names(data.m) <- c("Month","Median.m")
    # data.m$Month_Index <- data.m$Median.m/median(data[,names(data) %in% input$quantchoose2])
    # data <- merge(data,data.y, by = "Year", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose2] <- data[,names(data) %in% input$quantchoose2] * data$Year_Index
    # data <- merge(data,data.m, by = "Month", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose2] <- data[,names(data) %in% input$quantchoose2] * data$Month_Index
    for (f in input$categchoose2){
      data[,names(data) %in% f] <- as.factor(data[,names(data) %in% f])
    }
    data <- data[sample(NROW(data)),]
    if(input$obs2 == 0){
      data[,names(data) %in% input$quantchoose2] <- log(data[,names(data) %in% input$quantchoose2])
    } else {
      data[,names(data) %in% input$quantchoose2] <- (data[,names(data) %in% input$quantchoose2]) ^ input$obs2
    }
    if(input$obs3 == 0){
      data[,names(data) %in% input$quantchoose3] <- log(data[,names(data) %in% input$quantchoose3])
    } else {
      data[,names(data) %in% input$quantchoose3] <- (data[,names(data) %in% input$quantchoose3]) ^ input$obs3
    }
    f <- as.formula(paste(input$quantchoose2, paste(input$quantchoose3, ' + ',paste(input$categchoose2, collapse = " + ")), sep = " ~ "))
    reduced <- lm(f, data = data)
    quant <- c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")
    for (c in quant[quant %notin% input$quantchoose2]){
      if(input$obs3 == 0){
        data2[,names(data2) == c] <- log(data2[,names(data2) == c])
      } else {
        data2[,names(data2) == c] <- data2[,names(data2) == c] ^ input$obs3
      }
    }
    pred <- predict(reduced, newdata = data2)
    data2 <- cbind.data.frame(data2,pred)
    for (c in c(input$quantchoose2)){
      if(input$obs2 == 0){
        # data2[,names(data2) == c] <- log(data2[,names(data2) == c])
        data2[,names(data2) == "pred"] <- exp(data2[,names(data2) == "pred"])
      } else {
        # data2[,names(data2) == c] <- data2[,names(data2) == c] ^ (1/as.numeric(input$matrix[rownames(input$matrix) == c,]))
        data2[,names(data2) == "pred"] <- data2[,names(data2) == "pred"] ^ (1/input$obs2)
        
      }
    }
    valueBox(
      paste0(round(mean(abs((data2[,names(data2) %in% "pred"] - data2[,names(data2) %in% input$quantchoose2])/data2[,names(data2) %in% input$quantchoose2]))*100, digits = 2),"%"), "MAPE", icon = icon("list-alt", lib = "glyphicon"), color = "blue")
    
  })
  
  output$fpredsplot <- renderPlot({
    data = data2()
    data2 = data2t()
    # data.y <- aggregate(data[,names(data) %in% input$quantchoose2], by = list(data$Year), FUN = median)
    # names(data.y) <- c("Year","Median.y")
    # data.y$Year_Index <- data.y$Median.y/median(data[,names(data) %in% input$quantchoose2])
    # data.m <- aggregate(data[,names(data) %in% input$quantchoose2], by = list(data$Month), FUN = median)
    # names(data.m) <- c("Month","Median.m")
    # data.m$Month_Index <- data.m$Median.m/median(data[,names(data) %in% input$quantchoose2])
    # data <- merge(data,data.y, by = "Year", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose2] <- data[,names(data) %in% input$quantchoose2] * data$Year_Index
    # data <- merge(data,data.m, by = "Month", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose2] <- data[,names(data) %in% input$quantchoose2] * data$Month_Index
    for (f in input$categchoose2){
      data[,names(data) %in% f] <- as.factor(data[,names(data) %in% f])
    }
    data <- data[sample(NROW(data)),]
    if(input$obs2 == 0){
      data[,names(data) %in% input$quantchoose2] <- log(data[,names(data) %in% input$quantchoose2])
    } else {
      data[,names(data) %in% input$quantchoose2] <- (data[,names(data) %in% input$quantchoose2]) ^ input$obs2
    }
    if(input$obs3 == 0){
      data[,names(data) %in% input$quantchoose3] <- log(data[,names(data) %in% input$quantchoose3])
    } else {
      data[,names(data) %in% input$quantchoose3] <- (data[,names(data) %in% input$quantchoose3]) ^ input$obs3
    }
    f <- as.formula(paste(input$quantchoose2, paste(input$quantchoose3, ' + ',paste(input$categchoose2, collapse = " + ")), sep = " ~ "))
    reduced <- lm(f, data = data)
    quant <- c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")
    for (c in quant[quant %notin% input$quantchoose2]){
      if(input$obs3 == 0){
        data2[,names(data2) == c] <- log(data2[,names(data2) == c])
      } else {
        data2[,names(data2) == c] <- data2[,names(data2) == c] ^ input$obs3
      }
    }
    pred <- predict(reduced, newdata = data2)
    pred_range <- predict(reduced, newdata = data2, level=0.95, interval="prediction")
    data2 <- cbind.data.frame(data2,pred,pred_range)
    for (c in c(input$quantchoose2)){
      if(input$obs2 == 0){
        # data2[,names(data2) == c] <- log(data2[,names(data2) == c])
        data2[,names(data2) == "pred"] <- exp(data2[,names(data2) == "pred"])
        data2[,names(data2) == "lwr"] <- exp(data2[,names(data2) == "lwr"])
        data2[,names(data2) == "upr"] <- exp(data2[,names(data2) == "upr"])
      } else {
        # data2[,names(data2) == c] <- data2[,names(data2) == c] ^ (1/as.numeric(input$matrix[rownames(input$matrix) == c,]))
        data2[,names(data2) == "pred"] <- data2[,names(data2) == "pred"] ^ (1/input$obs2)
        data2[,names(data2) == "lwr"] <- data2[,names(data2) == "lwr"] ^ (1/input$obs2)
        data2[,names(data2) == "upr"] <- data2[,names(data2) == "upr"] ^ (1/input$obs2)
        
      }
    }
    ggplot(data=data2, aes(x = get(input$quantchoose3))) + geom_point(aes(y = get(input$quantchoose2))) + 
      geom_ribbon(data=data2, aes(ymin = lwr, ymax = upr), fill = "blue", alpha = 0.2) + xlab(input$quantchoose3) + ylab(input$quantchoose2) 
    
  })
  
  
  output$levenea <- renderPrint({
    data = data2()
    # data.y <- aggregate(data[,names(data) %in% input$quantchoose2], by = list(data$Year), FUN = median)
    # names(data.y) <- c("Year","Median.y")
    # data.y$Year_Index <- data.y$Median.y/median(data[,names(data) %in% input$quantchoose2])
    # data.m <- aggregate(data[,names(data) %in% input$quantchoose2], by = list(data$Month), FUN = median)
    # names(data.m) <- c("Month","Median.m")
    # data.m$Month_Index <- data.m$Median.m/median(data[,names(data) %in% input$quantchoose2])
    # data <- merge(data,data.y, by = "Year", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose2] <- data[,names(data) %in% input$quantchoose2] * data$Year_Index
    # data <- merge(data,data.m, by = "Month", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose2] <- data[,names(data) %in% input$quantchoose2] * data$Month_Index
    for (f in input$categchoose2){
      data[,names(data) %in% f] <- as.factor(data[,names(data) %in% f])
    }
    data <- data[sample(NROW(data)),]
    if(input$obs2 == 0){
      data$Transformed.Response <- log(data[,names(data) %in% input$quantchoose2])
    } else {
      data$Transformed.Response <- (data[,names(data) %in% input$quantchoose2]) ^ input$obs2
    }
    if(input$obs3 == 0){
      data$Transformed.Predictor <- log(data[,names(data) %in% input$quantchoose3])
    } else {
      data$Transformed.Predictor <- (data[,names(data) %in% input$quantchoose3]) ^ input$obs3
    }
    levene.test(data$Transformed.Response,data[,names(data) %in% input$categchoose2a])
  })
  
  output$tsmplot1.sa <- renderPlot({
    data = data2()
    # data.y <- aggregate(data[,names(data) %in% input$quantchoose2], by = list(data$Year), FUN = median)
    # names(data.y) <- c("Year","Median.y")
    # data.y$Year_Index <- data.y$Median.y/median(data[,names(data) %in% input$quantchoose2])
    # data.m <- aggregate(data[,names(data) %in% input$quantchoose2], by = list(data$Month), FUN = median)
    # names(data.m) <- c("Month","Median.m")
    # data.m$Month_Index <- data.m$Median.m/median(data[,names(data) %in% input$quantchoose2])
    # data <- merge(data,data.y, by = "Year", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose2] <- data[,names(data) %in% input$quantchoose2] * data$Year_Index
    # data <- merge(data,data.m, by = "Month", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose2] <- data[,names(data) %in% input$quantchoose2] * data$Month_Index
    for (f in input$categchoose2){
      data[,names(data) %in% f] <- as.factor(data[,names(data) %in% f])
    }
    data <- data[sample(NROW(data)),]
    if(input$obs2 == 0){
      data$Transformed.Response <- log(data[,names(data) %in% input$quantchoose2])
    } else {
      data$Transformed.Response <- (data[,names(data) %in% input$quantchoose2]) ^ input$obs2
    }
    if(input$obs3 == 0){
      data$Transformed.Predictor <- log(data[,names(data) %in% input$quantchoose3])
    } else {
      data$Transformed.Predictor <- (data[,names(data) %in% input$quantchoose3]) ^ input$obs3
    }
    f <- as.formula(paste("Transformed.Response", paste("Transformed.Predictor", ' + ',paste(input$categchoose2, collapse = " + ")), sep = " ~ "))
    reduced <- lm(f, data = data)
    
    plot(res$fitted.values,res$residuals, main="Plot of Residuals against Fitted Values")
    abline(h=0,col="red")
  })
  
  output$tsmplot2.sa <- renderPlot({
    data = data2()
    # data.y <- aggregate(data[,names(data) %in% input$quantchoose2], by = list(data$Year), FUN = median)
    # names(data.y) <- c("Year","Median.y")
    # data.y$Year_Index <- data.y$Median.y/median(data[,names(data) %in% input$quantchoose2])
    # data.m <- aggregate(data[,names(data) %in% input$quantchoose2], by = list(data$Month), FUN = median)
    # names(data.m) <- c("Month","Median.m")
    # data.m$Month_Index <- data.m$Median.m/median(data[,names(data) %in% input$quantchoose2])
    # data <- merge(data,data.y, by = "Year", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose2] <- data[,names(data) %in% input$quantchoose2] * data$Year_Index
    # data <- merge(data,data.m, by = "Month", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose2] <- data[,names(data) %in% input$quantchoose2] * data$Month_Index
    for (f in input$categchoose2){
      data[,names(data) %in% f] <- as.factor(data[,names(data) %in% f])
    }
    data <- data[sample(NROW(data)),]
    if(input$obs2 == 0){
      data$Transformed.Response <- log(data[,names(data) %in% input$quantchoose2])
    } else {
      data$Transformed.Response <- (data[,names(data) %in% input$quantchoose2]) ^ input$obs2
    }
    if(input$obs3 == 0){
      data$Transformed.Predictor <- log(data[,names(data) %in% input$quantchoose3])
    } else {
      data$Transformed.Predictor <- (data[,names(data) %in% input$quantchoose3]) ^ input$obs3
    }
    f <- as.formula(paste("Transformed.Response", paste("Transformed.Predictor", ' + ',paste(input$categchoose2, collapse = " + ")), sep = " ~ "))
    reduced <- lm(f, data = data)
    acf(res$residuals, main="ACF of Residuals (Squared)")
  })
  
  output$tsmplot3.sa <- renderPlot({
    data = data2()
    # data.y <- aggregate(data[,names(data) %in% input$quantchoose2], by = list(data$Year), FUN = median)
    # names(data.y) <- c("Year","Median.y")
    # data.y$Year_Index <- data.y$Median.y/median(data[,names(data) %in% input$quantchoose2])
    # data.m <- aggregate(data[,names(data) %in% input$quantchoose2], by = list(data$Month), FUN = median)
    # names(data.m) <- c("Month","Median.m")
    # data.m$Month_Index <- data.m$Median.m/median(data[,names(data) %in% input$quantchoose2])
    # data <- merge(data,data.y, by = "Year", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose2] <- data[,names(data) %in% input$quantchoose2] * data$Year_Index
    # data <- merge(data,data.m, by = "Month", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose2] <- data[,names(data) %in% input$quantchoose2] * data$Month_Index
    for (f in input$categchoose2){
      data[,names(data) %in% f] <- as.factor(data[,names(data) %in% f])
    }
    data <- data[sample(NROW(data)),]
    if(input$obs2 == 0){
      data$Transformed.Response <- log(data[,names(data) %in% input$quantchoose2])
    } else {
      data$Transformed.Response <- (data[,names(data) %in% input$quantchoose2]) ^ input$obs2
    }
    if(input$obs3 == 0){
      data$Transformed.Predictor <- log(data[,names(data) %in% input$quantchoose3])
    } else {
      data$Transformed.Predictor <- (data[,names(data) %in% input$quantchoose3]) ^ input$obs3
    }
    f <- as.formula(paste("Transformed.Response", paste("Transformed.Predictor", ' + ',paste(input$categchoose2, collapse = " + ")), sep = " ~ "))
    reduced <- lm(f, data = data)
    
    qqnorm(res$residuals)
    qqline(res$residuals, col="red")
  })
  
  output$regsub <- DT::renderDataTable({
    data <- data3()
    quant <- c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")
    data <- data[,names(data) %in% c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")]
    allreg <- regsubsets(y = data[,names(data) %in% input$quantchoose4], x= data[,names(data) %in% quant[which(quant != input$quantchoose)]],, nbest=9)
    
    ##create a "data frame" that stores the predictors in the various models considered as well as their various criteria
    best <- as.data.frame(summary(allreg)$outmat)
    best$p <- as.numeric(substr(rownames(best),1,1))+1
    best$r2 <- summary(allreg)$rsq
    best$adjr2 <- summary(allreg)$adjr2
    best$mse <- (summary(allreg)$rss)/(dim(data)[1]-best$p)
    best$cp <- summary(allreg)$cp
    best$bic <- summary(allreg)$bic
    best <- best[,c(12:17,1:11)]
    datatable(best, extensions = 'Responsive')
  })
  
  output$partreg <- renderPlot({
    data <- data3()
    data <- data[,names(data) %in% c(input$quantchoose4, input$quantchoose5)]
    if(length(input$quantchoose5)<7){
      pred.space <- ceiling(length(input$quantchoose5)/2)
      par(mfrow=c(pred.space,2))
    } else {
      pred.space <- ceiling(length(input$quantchoose5)/3)
      par(mfrow=c(pred.space,3))
    }
    for (l in 1:length(input$quantchoose5)){
      f <- as.formula(paste(input$quantchoose4, paste(paste(input$quantchoose5[input$quantchoose5 %notin% input$quantchoose5[l]], collapse = " + ")), sep = " ~ "))
      f2 <- as.formula(paste(input$quantchoose5[l], paste(paste(input$quantchoose5[input$quantchoose5 %notin% input$quantchoose5[l]], collapse = " + ")), sep = " ~ "))
      result.y.x2<-lm(f, data = data) ##fit y against other predictors
      result.x2<-lm(f2, data = data) ##fit x2 against other predictors
      
      res.y.x2<-result.y.x2$residuals ##store residuals. info in y not explained by x7 and x8
      res.x2<-result.x2$residuals ##store residuals. info in x2 not explained by x7 and x8
      
      ##partial residual plot for x2
      plot(res.x2,res.y.x2, main=paste0("Partial Residual Plot For ",input$quantchoose5[l]), xlab = paste("Residuals Of",input$quantchoose5[l],"Against Other Predictors"), ylab = paste("Residuals Of",input$quantchoose4,"Against Other Predictors"))
      ##overlay regression line
      abline(lm(res.y.x2~res.x2), col="red") 
    }

  }, height = 1000)
  
  output$vif <- renderPrint({
    data <- data3()
    if(length(input$quantchoose5) == 2){
      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]), data=data) 
    } else {
      if(length(input$quantchoose5) == 3){
        reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]), data=data) 
      } else {
        if(length(input$quantchoose5) == 4){
          reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) +  get(input$quantchoose5[4]), data=data) 
        } else {
          if(length(input$quantchoose5) == 5){
            reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]), data=data) 
          } else {
            if(length(input$quantchoose5) == 6){
              reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]), data=data) 
            } else {
              if(length(input$quantchoose5) == 7){
                reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]), data=data) 
              } else {
                if(length(input$quantchoose5) == 8){
                  reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]), data=data) 
                } else {
                  if(length(input$quantchoose5) == 9){
                    reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]), data=data) 
                  } else {
                    if(length(input$quantchoose5) == 10){
                      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[10]), data=data) 
                      
                    } else {
                      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) +  get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]) + get(input$quantchoose5[10]) + get(input$quantchoose5[11]), data=data) 
                    }
                  }
                }
              }                                                                    
            }                                                                  
          }        
        }
      }
    }  
    vif(reduced) 
  })
  
  output$totresplotm <- renderPlot({
    data <- data3()
    if(length(input$quantchoose5) == 2){
      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]), data=data) 
    } else {
      if(length(input$quantchoose5) == 3){
        reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]), data=data) 
      } else {
        if(length(input$quantchoose5) == 4){
          reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) +  get(input$quantchoose5[4]), data=data) 
        } else {
          if(length(input$quantchoose5) == 5){
            reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]), data=data) 
          } else {
            if(length(input$quantchoose5) == 6){
              reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]), data=data) 
            } else {
              if(length(input$quantchoose5) == 7){
                reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]), data=data) 
              } else {
                if(length(input$quantchoose5) == 8){
                  reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]), data=data) 
                } else {
                  if(length(input$quantchoose5) == 9){
                    reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]), data=data) 
                  } else {
                    if(length(input$quantchoose5) == 10){
                      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[10]), data=data) 
                      
                    } else {
                      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) +  get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]) + get(input$quantchoose5[10]) + get(input$quantchoose5[11]), data=data) 
                    }
                  }
                }
              }                                                                    
            }                                                                  
          }        
        }
      }
    }  
    res<-reduced$residuals ##residuals
    
    student.res<-rstandard(reduced) ##studentized residuals
    
    ext.student.res<-rstudent(reduced) ##externally studentized residuals
    
    
    ##all the residual plots. They look the same (generally) but the scale for the ordinary residuals is different
    par(mfrow=c(1,3))
    plot(reduced$fitted.values,res,main="Residuals")
    plot(reduced$fitted.values,student.res,main="Studentized Residuals")
    plot(reduced$fitted.values,ext.student.res,main="Externally  Studentized Residuals")
  })
  
  output$lev <- DT::renderDataTable({
    data <- data3()
    if(length(input$quantchoose5) == 2){
      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]), data=data) 
    } else {
      if(length(input$quantchoose5) == 3){
        reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]), data=data) 
      } else {
        if(length(input$quantchoose5) == 4){
          reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) +  get(input$quantchoose5[4]), data=data) 
        } else {
          if(length(input$quantchoose5) == 5){
            reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]), data=data) 
          } else {
            if(length(input$quantchoose5) == 6){
              reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]), data=data) 
            } else {
              if(length(input$quantchoose5) == 7){
                reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]), data=data) 
              } else {
                if(length(input$quantchoose5) == 8){
                  reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]), data=data) 
                } else {
                  if(length(input$quantchoose5) == 9){
                    reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]), data=data) 
                  } else {
                    if(length(input$quantchoose5) == 10){
                      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[10]), data=data) 
                      
                    } else {
                      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) +  get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]) + get(input$quantchoose5[10]) + get(input$quantchoose5[11]), data=data) 
                    }
                  }
                }
              }                                                                    
            }                                                                  
          }        
        }
      }
    }  
    n<-length(data)
    p<- length(input$quantchoose5) + 1
    ext.student.res<-rstudent(reduced)
    ##this check which externally studentized residuals are greater than the critical value
    ext.student.res[abs(ext.student.res)>qt(1-0.05/(2*n), n-p-1)]
    ##returns a null, so no observations are outlying in the response.
    
    lev<-lm.influence(reduced)$hat ##leverages
    lev <- as.data.frame(lev[lev>2*p/n])
    datatable(lev, extensions = 'Responsive')
  })
  
  output$dffits <- DT::renderDataTable({
    data <- data3()
    if(length(input$quantchoose5) == 2){
      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]), data=data) 
    } else {
      if(length(input$quantchoose5) == 3){
        reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]), data=data) 
      } else {
        if(length(input$quantchoose5) == 4){
          reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) +  get(input$quantchoose5[4]), data=data) 
        } else {
          if(length(input$quantchoose5) == 5){
            reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]), data=data) 
          } else {
            if(length(input$quantchoose5) == 6){
              reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]), data=data) 
            } else {
              if(length(input$quantchoose5) == 7){
                reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]), data=data) 
              } else {
                if(length(input$quantchoose5) == 8){
                  reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]), data=data) 
                } else {
                  if(length(input$quantchoose5) == 9){
                    reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]), data=data) 
                  } else {
                    if(length(input$quantchoose5) == 10){
                      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[10]), data=data) 
                      
                    } else {
                      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) +  get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]) + get(input$quantchoose5[10]) + get(input$quantchoose5[11]), data=data) 
                    }
                  }
                }
              }                                                                    
            }                                                                  
          }        
        }
      }
    }  
    n<-length(data)
    p<- length(input$quantchoose5) + 1
    DFFITS<-dffits(reduced)
    
    DFFITS <- as.data.frame(DFFITS[abs(DFFITS)>2*sqrt(p/n)])
    datatable(DFFITS, extensions = 'Responsive')
  })
  
  
  output$cook <- DT::renderDataTable({
    data <- data3()
    if(length(input$quantchoose5) == 2){
      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]), data=data) 
    } else {
      if(length(input$quantchoose5) == 3){
        reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]), data=data) 
      } else {
        if(length(input$quantchoose5) == 4){
          reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) +  get(input$quantchoose5[4]), data=data) 
        } else {
          if(length(input$quantchoose5) == 5){
            reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]), data=data) 
          } else {
            if(length(input$quantchoose5) == 6){
              reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]), data=data) 
            } else {
              if(length(input$quantchoose5) == 7){
                reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]), data=data) 
              } else {
                if(length(input$quantchoose5) == 8){
                  reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]), data=data) 
                } else {
                  if(length(input$quantchoose5) == 9){
                    reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]), data=data) 
                  } else {
                    if(length(input$quantchoose5) == 10){
                      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[10]), data=data) 
                      
                    } else {
                      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) +  get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]) + get(input$quantchoose5[10]) + get(input$quantchoose5[11]), data=data) 
                    }
                  }
                }
              }                                                                    
            }                                                                  
          }        
        }
      }
    }  
    n<-length(data)
    p<- length(input$quantchoose5) + 1
    COOKS<-cooks.distance(reduced)
    COOKS <- as.data.frame(COOKS)
    COOKS <- as.data.frame(COOKS[COOKS>qf(0.5,p,n-p)])
    datatable(COOKS, extensions = 'Responsive')
  })
  
  
  
  output$fmodel2 <- renderPrint({
    data <- data3()
    quant <- c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")
    data <- data[,names(data) %in% quant]
    for (c in names(data)){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        data[,names(data) == c] <- log(data[,names(data) == c])
      } else {
        data[,names(data) == c] <- data[,names(data) == c] ^ as.numeric(input$matrix[rownames(input$matrix) == c,])
      }
    }
    if(length(input$quantchoose5) == 2){
      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]), data=data) 
    } else {
      if(length(input$quantchoose5) == 3){
        reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]), data=data) 
      } else {
        if(length(input$quantchoose5) == 4){
          reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) +  get(input$quantchoose5[4]), data=data) 
        } else {
          if(length(input$quantchoose5) == 5){
            reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]), data=data) 
          } else {
            if(length(input$quantchoose5) == 6){
              reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]), data=data) 
            } else {
              if(length(input$quantchoose5) == 7){
                reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]), data=data) 
              } else {
                if(length(input$quantchoose5) == 8){
                  reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]), data=data) 
                } else {
                  if(length(input$quantchoose5) == 9){
                    reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]), data=data) 
                  } else {
                    if(length(input$quantchoose5) == 10){
                      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[10]), data=data) 
                      
                  } else {
                    reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) +  get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]) + get(input$quantchoose5[10]) + get(input$quantchoose5[11]), data=data) 
                  }
                }
              }
            }                                                                    
          }                                                                  
        }        
      }
     }
    }  
    summary(reduced) 
  })
  
  output$bc2 <- renderPlot({
    data <- data3()
    quant <- c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")
    data <- data[,names(data) %in% quant]
    if(length(input$quantchoose5) == 2){
      bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]), data=data,lambda = seq(-2, 2, 1/10))
    } else {
      if(length(input$quantchoose5) == 3){
        bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]), data=data,lambda = seq(-2, 2, 1/10))
      } else {
        if(length(input$quantchoose5) == 4){
          bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) +  get(input$quantchoose5[4]), data=data,lambda = seq(-2, 2, 1/10)) 
        } else {
          if(length(input$quantchoose5) == 5){
            bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]), data=data,lambda = seq(-2, 2, 1/10))
          } else {
            if(length(input$quantchoose5) == 6){
              bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]), data=data,lambda = seq(-2, 2, 1/10)) 
            } else {
              if(length(input$quantchoose5) == 7){
                bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]), data=data,lambda = seq(-2, 2, 1/10))
              } else {
                if(length(input$quantchoose5) == 8){
                  bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]), data=data,lambda = seq(-2, 2, 1/10)) 
                } else {
                  if(length(input$quantchoose5) == 9){
                    bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]), data=data,lambda = seq(-2, 2, 1/10)) 
                  } else {
                    if(length(input$quantchoose5) == 10){
                      bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[10]), data=data,lambda = seq(-2, 2, 1/10)) 
                      
                    } else {
                      bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) +  get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]) + get(input$quantchoose5[10]) + get(input$quantchoose5[11]), data=data,lambda = seq(-2, 2, 1/10)) 
                    }
                  }
                }
              }                                                                    
            }                                                                  
          }        
        }
      }
    }  

  })
  
  output$bcvalue2 <- renderValueBox({
    data <- data3()
    quant <- c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")
    data <- data[,names(data) %in% quant]
    if(length(input$quantchoose5) == 2){
      bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]), data=data,lambda = seq(-2, 2, 1/10))
    } else {
      if(length(input$quantchoose5) == 3){
        bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]), data=data,lambda = seq(-2, 2, 1/10))
      } else {
        if(length(input$quantchoose5) == 4){
          bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) +  get(input$quantchoose5[4]), data=data,lambda = seq(-2, 2, 1/10)) 
        } else {
          if(length(input$quantchoose5) == 5){
            bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]), data=data,lambda = seq(-2, 2, 1/10))
          } else {
            if(length(input$quantchoose5) == 6){
              bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]), data=data,lambda = seq(-2, 2, 1/10)) 
            } else {
              if(length(input$quantchoose5) == 7){
                bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]), data=data,lambda = seq(-2, 2, 1/10))
              } else {
                if(length(input$quantchoose5) == 8){
                  bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]), data=data,lambda = seq(-2, 2, 1/10)) 
                } else {
                  if(length(input$quantchoose5) == 9){
                    bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]), data=data,lambda = seq(-2, 2, 1/10)) 
                  } else {
                    if(length(input$quantchoose5) == 10){
                      bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[10]), data=data,lambda = seq(-2, 2, 1/10)) 
                      
                    } else {
                      bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) +  get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]) + get(input$quantchoose5[10]) + get(input$quantchoose5[11]), data=data,lambda = seq(-2, 2, 1/10)) 
                    }
                  }
                }
              }                                                                    
            }                                                                  
          }        
        }
      }
    }  
    bc <- bc$x[which.max(bc$y)]
    valueBox(
      round(bc, digits = 3), "Suggested Transformation", icon = icon("list-alt", lib = "glyphicon"), color = "blue")
    
  })
  
  output$partreg2 <- renderPlot({
    data <- data3()
    data <- data[,names(data) %in% c(input$quantchoose4, input$quantchoose5)]
    for (c in c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        data[,names(data) == c] <- log(data[,names(data) == c])
      } else {
        data[,names(data) == c] <- data[,names(data) == c] ^ as.numeric(input$matrix[rownames(input$matrix) == c,])
      }
    }
    if(length(input$quantchoose5)<7){
      pred.space <- ceiling(length(input$quantchoose5)/2)
      par(mfrow=c(pred.space,2))
    } else {
      pred.space <- ceiling(length(input$quantchoose5)/3)
      par(mfrow=c(pred.space,3))
    }
    for (l in 1:length(input$quantchoose5)){
      f <- as.formula(paste(input$quantchoose4, paste(paste(input$quantchoose5[input$quantchoose5 %notin% input$quantchoose5[l]], collapse = " + ")), sep = " ~ "))
      f2 <- as.formula(paste(input$quantchoose5[l], paste(paste(input$quantchoose5[input$quantchoose5 %notin% input$quantchoose5[l]], collapse = " + ")), sep = " ~ "))
      result.y.x2<-lm(f, data = data) ##fit y against other predictors
      result.x2<-lm(f2, data = data) ##fit x2 against other predictors
      
      res.y.x2<-result.y.x2$residuals ##store residuals. info in y not explained by x7 and x8
      res.x2<-result.x2$residuals ##store residuals. info in x2 not explained by x7 and x8
      
      ##partial residual plot for x2
      plot(res.x2,res.y.x2, main=paste0("Partial Residual Plot For ",input$quantchoose5[l]), xlab = paste("Residuals Of",input$quantchoose5[l],"Against Other Predictors"), ylab = paste("Residuals Of",input$quantchoose4,"Against Other Predictors"))
      ##overlay regression line
      abline(lm(res.y.x2~res.x2), col="red") 
    }
    
  }, height = 1000)
  
  output$vif2 <- renderPrint({
    data <- data3()
    for (c in c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        data[,names(data) == c] <- log(data[,names(data) == c])
      } else {
        data[,names(data) == c] <- data[,names(data) == c] ^ as.numeric(input$matrix[rownames(input$matrix) == c,])
      }
    }
    if(length(input$quantchoose5) == 2){
      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]), data=data) 
    } else {
      if(length(input$quantchoose5) == 3){
        reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]), data=data) 
      } else {
        if(length(input$quantchoose5) == 4){
          reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) +  get(input$quantchoose5[4]), data=data) 
        } else {
          if(length(input$quantchoose5) == 5){
            reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]), data=data) 
          } else {
            if(length(input$quantchoose5) == 6){
              reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]), data=data) 
            } else {
              if(length(input$quantchoose5) == 7){
                bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]), data=data,lambda = seq(-2, 2, 1/10))
              } else {
                if(length(input$quantchoose5) == 8){
                  bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]), data=data,lambda = seq(-2, 2, 1/10)) 
                } else {
                  if(length(input$quantchoose5) == 9){
                    reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]), data=data) 
                  } else {
                    if(length(input$quantchoose5) == 10){
                      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[10]), data=data) 
                      
                    } else {
                      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) +  get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]) + get(input$quantchoose5[10]) + get(input$quantchoose5[11]), data=data) 
                    }
                  }
                }
              }                                                                    
              
            }                                                                    
          }                                                                  
        }        
      }
    }
    vif(reduced) 
  })
  
  output$totresplotm2 <- renderPlot({
    data <- data3()
    for (c in c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        data[,names(data) == c] <- log(data[,names(data) == c])
      } else {
        data[,names(data) == c] <- data[,names(data) == c] ^ as.numeric(input$matrix[rownames(input$matrix) == c,])
      }
    }
    if(length(input$quantchoose5) == 2){
      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]), data=data) 
    } else {
      if(length(input$quantchoose5) == 3){
        reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]), data=data) 
      } else {
        if(length(input$quantchoose5) == 4){
          reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) +  get(input$quantchoose5[4]), data=data) 
        } else {
          if(length(input$quantchoose5) == 5){
            reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]), data=data) 
          } else {
            if(length(input$quantchoose5) == 6){
              reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]), data=data) 
            } else {
              if(length(input$quantchoose5) == 7){
                bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]), data=data,lambda = seq(-2, 2, 1/10))
              } else {
                if(length(input$quantchoose5) == 8){
                  bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]), data=data,lambda = seq(-2, 2, 1/10)) 
                } else {
                  if(length(input$quantchoose5) == 9){
                    reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]), data=data) 
                  } else {
                    if(length(input$quantchoose5) == 10){
                      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[10]), data=data) 
                      
                    } else {
                      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) +  get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]) + get(input$quantchoose5[10]) + get(input$quantchoose5[11]), data=data) 
                    }
                  }
                }
              }                                                                    
              
            }                                                                    
          }                                                                  
        }        
      }
    }
    res<-reduced$residuals ##residuals
    
    student.res<-rstandard(reduced) ##studentized residuals
    
    ext.student.res<-rstudent(reduced) ##externally studentized residuals
    
    
    ##all the residual plots. They look the same (generally) but the scale for the ordinary residuals is different
    par(mfrow=c(1,3))
    plot(reduced$fitted.values,res,main="Residuals")
    plot(reduced$fitted.values,student.res,main="Studentized Residuals")
    plot(reduced$fitted.values,ext.student.res,main="Externally  Studentized Residuals")
  })
  
  output$lev2 <- DT::renderDataTable({
    data <- data3()
    for (c in c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        data[,names(data) == c] <- log(data[,names(data) == c])
      } else {
        data[,names(data) == c] <- data[,names(data) == c] ^ as.numeric(input$matrix[rownames(input$matrix) == c,])
      }
    }
    if(length(input$quantchoose5) == 2){
      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]), data=data) 
    } else {
      if(length(input$quantchoose5) == 3){
        reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]), data=data) 
      } else {
        if(length(input$quantchoose5) == 4){
          reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) +  get(input$quantchoose5[4]), data=data) 
        } else {
          if(length(input$quantchoose5) == 5){
            reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]), data=data) 
          } else {
            if(length(input$quantchoose5) == 6){
              reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]), data=data) 
            } else {
              if(length(input$quantchoose5) == 7){
                bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]), data=data,lambda = seq(-2, 2, 1/10))
              } else {
                if(length(input$quantchoose5) == 8){
                  bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]), data=data,lambda = seq(-2, 2, 1/10)) 
                } else {
                  if(length(input$quantchoose5) == 9){
                    reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]), data=data) 
                  } else {
                    if(length(input$quantchoose5) == 10){
                      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[10]), data=data) 
                      
                    } else {
                      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) +  get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]) + get(input$quantchoose5[10]) + get(input$quantchoose5[11]), data=data) 
                    }
                  }
                }
              }                                                                    
              
            }                                                                    
          }                                                                  
        }        
      }
    }
    n<-length(data)
    p<- length(input$quantchoose5) + 1
    ext.student.res<-rstudent(reduced)
    ##this check which externally studentized residuals are greater than the critical value
    ext.student.res[abs(ext.student.res)>qt(1-0.05/(2*n), n-p-1)]
    ##returns a null, so no observations are outlying in the response.
    
    lev<-lm.influence(reduced)$hat ##leverages
    lev <- as.data.frame(lev[lev>2*p/n])
    datatable(lev, extensions = 'Responsive')
  })
  
  output$dffits2 <- DT::renderDataTable({
    data <- data3()
    for (c in c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        data[,names(data) == c] <- log(data[,names(data) == c])
      } else {
        data[,names(data) == c] <- data[,names(data) == c] ^ as.numeric(input$matrix[rownames(input$matrix) == c,])
      }
    }
    if(length(input$quantchoose5) == 2){
      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]), data=data) 
    } else {
      if(length(input$quantchoose5) == 3){
        reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]), data=data) 
      } else {
        if(length(input$quantchoose5) == 4){
          reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) +  get(input$quantchoose5[4]), data=data) 
        } else {
          if(length(input$quantchoose5) == 5){
            reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]), data=data) 
          } else {
            if(length(input$quantchoose5) == 6){
              reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]), data=data) 
            } else {
              if(length(input$quantchoose5) == 7){
                bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]), data=data,lambda = seq(-2, 2, 1/10))
              } else {
                if(length(input$quantchoose5) == 8){
                  bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]), data=data,lambda = seq(-2, 2, 1/10)) 
                } else {
                  if(length(input$quantchoose5) == 9){
                    reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]), data=data) 
                  } else {
                    if(length(input$quantchoose5) == 10){
                      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[10]), data=data) 
                      
                    } else {
                      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) +  get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]) + get(input$quantchoose5[10]) + get(input$quantchoose5[11]), data=data) 
                    }
                  }
                }
              }                                                                    
              
            }                                                                    
          }                                                                  
        }        
      }
    }
    n<-length(data)
    p<- length(input$quantchoose5) + 1
    DFFITS<-dffits(reduced)
    
    DFFITS <- as.data.frame(DFFITS[abs(DFFITS)>2*sqrt(p/n)])
    datatable(DFFITS, extensions = 'Responsive')
  })
  
  
  output$cook2 <- DT::renderDataTable({
    data <- data3()
    for (c in c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        data[,names(data) == c] <- log(data[,names(data) == c])
      } else {
        data[,names(data) == c] <- data[,names(data) == c] ^ as.numeric(input$matrix[rownames(input$matrix) == c,])
      }
    }
    if(length(input$quantchoose5) == 2){
      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]), data=data) 
    } else {
      if(length(input$quantchoose5) == 3){
        reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]), data=data) 
      } else {
        if(length(input$quantchoose5) == 4){
          reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) +  get(input$quantchoose5[4]), data=data) 
        } else {
          if(length(input$quantchoose5) == 5){
            reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]), data=data) 
          } else {
            if(length(input$quantchoose5) == 6){
              reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]), data=data) 
            } else {
              if(length(input$quantchoose5) == 7){
                bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]), data=data,lambda = seq(-2, 2, 1/10))
              } else {
                if(length(input$quantchoose5) == 8){
                  bc <- boxcox(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]), data=data,lambda = seq(-2, 2, 1/10)) 
                } else {
                  if(length(input$quantchoose5) == 9){
                    reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]), data=data) 
                  } else {
                    if(length(input$quantchoose5) == 10){
                      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) + get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[10]), data=data) 
                      
                    } else {
                      reduced<-lm(get(input$quantchoose4)~ get(input$quantchoose5[1]) + get(input$quantchoose5[2]) + get(input$quantchoose5[3]) + get(input$quantchoose5[4]) + get(input$quantchoose5[5]) + get(input$quantchoose5[6]) +  get(input$quantchoose5[7]) + get(input$quantchoose5[8]) + get(input$quantchoose5[9]) + get(input$quantchoose5[10]) + get(input$quantchoose5[11]), data=data) 
                    }
                  }
                }
              }                                                                    
              
            }                                                                    
          }                                                                  
        }        
      }
    }
    n<-length(data)
    p<- length(input$quantchoose5) + 1
    COOKS<-cooks.distance(reduced)
    COOKS <- as.data.frame(COOKS)
    COOKS <- as.data.frame(COOKS[COOKS>qf(0.5,p,n-p)])
    datatable(COOKS, extensions = 'Responsive')
  })
  
  
  output$fmodel3 <- renderPrint({
    data <- data3()
    # data.y <- aggregate(data[,names(data) %in% input$quantchoose4], by = list(data$Year), FUN = median)
    # names(data.y) <- c("Year","Median.y")
    # data.y$Year_Index <- data.y$Median.y/median(data[,names(data) %in% input$quantchoose4])
    # data.m <- aggregate(data[,names(data) %in% input$quantchoose4], by = list(data$Month), FUN = median)
    # names(data.m) <- c("Month","Median.m")
    # data.m$Month_Index <- data.m$Median.m/median(data[,names(data) %in% input$quantchoose4])
    # data <- merge(data,data.y, by = "Year", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose4] <- data[,names(data) %in% input$quantchoose4] * data$Year_Index
    # data <- merge(data,data.m, by = "Month", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose4] <- data[,names(data) %in% input$quantchoose4] * data$Month_Index
    for (f in input$categchoose3){
      data[,names(data) %in% f] <- as.factor(data[,names(data) %in% f])
    }
    quant <- c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")
    # data <- data[,names(data) %in% quant]
    for (c in quant){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        data[,names(data) == c] <- log(data[,names(data) == c])
      } else {
        data[,names(data) == c] <- data[,names(data) == c] ^ as.numeric(input$matrix[rownames(input$matrix) == c,])
      }
    }
    f <- as.formula(paste(input$quantchoose4, paste(paste(input$quantchoose5, collapse = " + "), ' + ',paste(input$categchoose3, collapse = " + ")), sep = " ~ "))
    reduced <- lm(f, data = data)
    summary(reduced) 
  })
  
  output$fpred <- renderValueBox({
    data <- data3()
    data2 <- data4()
    # data.y <- aggregate(data[,names(data) %in% input$quantchoose4], by = list(data$Year), FUN = median)
    # names(data.y) <- c("Year","Median.y")
    # data.y$Year_Index <- data.y$Median.y/median(data[,names(data) %in% input$quantchoose4])
    # data.m <- aggregate(data[,names(data) %in% input$quantchoose4], by = list(data$Month), FUN = median)
    # names(data.m) <- c("Month","Median.m")
    # data.m$Month_Index <- data.m$Median.m/median(data[,names(data) %in% input$quantchoose4])
    # data <- merge(data,data.y, by = "Year", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose4] <- data[,names(data) %in% input$quantchoose4] * data$Year_Index
    # data <- merge(data,data.m, by = "Month", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose4] <- data[,names(data) %in% input$quantchoose4] * data$Month_Index
    for (f in input$categchoose3){
      data[,names(data) %in% f] <- as.factor(data[,names(data) %in% f])
    }
    quant <- c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")
    # data <- data[,names(data) %in% quant]
    for (c in quant){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        data[,names(data) == c] <- log(data[,names(data) == c])
      } else {
        data[,names(data) == c] <- data[,names(data) == c] ^ as.numeric(input$matrix[rownames(input$matrix) == c,])
      }
    }
    f <- as.formula(paste(input$quantchoose4, paste(paste(input$quantchoose5, collapse = " + "), ' + ',paste(input$categchoose3, collapse = " + ")), sep = " ~ "))
    reduced <- lm(f, data = data)
    for (c in quant[quant %notin% input$quantchoose4]){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        data2[,names(data2) == c] <- log(data2[,names(data2) == c])
      } else {
        data2[,names(data2) == c] <- data2[,names(data2) == c] ^ as.numeric(input$matrix[rownames(input$matrix) == c,])
      }
    }
    for (f in input$categchoose3){
      data2[,names(data2) %in% f] <- as.factor(data2[,names(data2) %in% f])
    }
    pred <- predict(reduced, newdata = data2)
    data2 <- cbind.data.frame(data2,pred)
    for (c in c(input$quantchoose4)){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        # data2[,names(data2) == c] <- log(data2[,names(data2) == c])
        data2[,names(data2) == "pred"] <- exp(data2[,names(data2) == "pred"])
      } else {
        # data2[,names(data2) == c] <- data2[,names(data2) == c] ^ (1/as.numeric(input$matrix[rownames(input$matrix) == c,]))
        data2[,names(data2) == "pred"] <- data2[,names(data2) == "pred"] ^ (1/as.numeric(input$matrix[rownames(input$matrix) == c,]))
        
      }
    }
    valueBox(
      paste0(round(mean(abs((data2[,names(data2) %in% "pred"] - data2[,names(data2) %in% input$quantchoose4])/data2[,names(data2) %in% input$quantchoose4]))*100, digits = 2),"%"), "MAPE", icon = icon("list-alt", lib = "glyphicon"), color = "blue")
  
  })
  
  output$fpredplot <- renderPlot({
    data <- data3()
    data2 <- data4()
    # data.y <- aggregate(data[,names(data) %in% input$quantchoose4], by = list(data$Year), FUN = median)
    # names(data.y) <- c("Year","Median.y")
    # data.y$Year_Index <- data.y$Median.y/median(data[,names(data) %in% input$quantchoose4])
    # data.m <- aggregate(data[,names(data) %in% input$quantchoose4], by = list(data$Month), FUN = median)
    # names(data.m) <- c("Month","Median.m")
    # data.m$Month_Index <- data.m$Median.m/median(data[,names(data) %in% input$quantchoose4])
    # data <- merge(data,data.y, by = "Year", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose4] <- data[,names(data) %in% input$quantchoose4] * data$Year_Index
    # data <- merge(data,data.m, by = "Month", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose4] <- data[,names(data) %in% input$quantchoose4] * data$Month_Index
    for (f in input$categchoose3){
      data[,names(data) %in% f] <- as.factor(data[,names(data) %in% f])
    }
    quant <- c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")
    # data <- data[,names(data) %in% quant]
    for (c in quant){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        data[,names(data) == c] <- log(data[,names(data) == c])
      } else {
        data[,names(data) == c] <- data[,names(data) == c] ^ as.numeric(input$matrix[rownames(input$matrix) == c,])
      }
    }
    f <- as.formula(paste(input$quantchoose4, paste(paste(input$quantchoose5, collapse = " + "), ' + ',paste(input$categchoose3, collapse = " + ")), sep = " ~ "))
    reduced <- lm(f, data = data)
    for (c in quant[quant %notin% input$quantchoose4]){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        data2[,names(data2) == c] <- log(data2[,names(data2) == c])
      } else {
        data2[,names(data2) == c] <- data2[,names(data2) == c] ^ as.numeric(input$matrix[rownames(input$matrix) == c,])
      }
    }
    for (f in input$categchoose3){
      data2[,names(data2) %in% f] <- as.factor(data2[,names(data2) %in% f])
    }
    pred <- predict(reduced, newdata = data2)
    pred_range <- predict(reduced, newdata = data2, level=0.95, interval="prediction")
    data2 <- cbind.data.frame(data2,pred,pred_range)
    for (c in c(input$quantchoose4)){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        # data2[,names(data2) == c] <- log(data2[,names(data2) == c])
        data2[,names(data2) == "pred"] <- exp(data2[,names(data2) == "pred"])
        data2[,names(data2) == "lwr"] <- exp(data2[,names(data2) == "lwr"])
        data2[,names(data2) == "upr"] <- exp(data2[,names(data2) == "upr"])
      } else {
        # data2[,names(data2) == c] <- data2[,names(data2) == c] ^ (1/as.numeric(input$matrix[rownames(input$matrix) == c,]))
        data2[,names(data2) == "pred"] <- data2[,names(data2) == "pred"] ^ (1/as.numeric(input$matrix[rownames(input$matrix) == c,]))
        data2[,names(data2) == "lwr"] <- data2[,names(data2) == "lwr"] ^ (1/as.numeric(input$matrix[rownames(input$matrix) == c,]))
        data2[,names(data2) == "upr"] <- data2[,names(data2) == "upr"] ^ (1/as.numeric(input$matrix[rownames(input$matrix) == c,]))
        
      }
    }
    ggplot(data=data2, aes(x = get(input$quantchoose6))) + geom_point(aes(y = get(input$quantchoose4))) + 
      geom_ribbon(data=data2, aes(ymin = lwr, ymax = upr), fill = "blue", alpha = 0.2) + xlab(input$quantchoose6) + ylab(input$quantchoose4) 
    
  })
  
  output$fpredval <- renderValueBox({
    data <- data3()
    # data.y <- aggregate(data[,names(data) %in% input$quantchoose4], by = list(data$Year), FUN = median)
    # names(data.y) <- c("Year","Median.y")
    # data.y$Year_Index <- data.y$Median.y/median(data[,names(data) %in% input$quantchoose4])
    # data.m <- aggregate(data[,names(data) %in% input$quantchoose4], by = list(data$Month), FUN = median)
    # names(data.m) <- c("Month","Median.m")
    # data.m$Month_Index <- data.m$Median.m/median(data[,names(data) %in% input$quantchoose4])
    # data <- merge(data,data.y, by = "Year", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose4] <- data[,names(data) %in% input$quantchoose4] * data$Year_Index
    # data <- merge(data,data.m, by = "Month", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose4] <- data[,names(data) %in% input$quantchoose4] * data$Month_Index
    for (f in input$categchoose3){
      data[,names(data) %in% f] <- as.factor(data[,names(data) %in% f])
    }
    quant <- c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")
    # data <- data[,names(data) %in% quant]
    for (c in quant){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        data[,names(data) == c] <- log(data[,names(data) == c])
      } else {
        data[,names(data) == c] <- data[,names(data) == c] ^ as.numeric(input$matrix[rownames(input$matrix) == c,])
      }
    }
    f <- as.formula(paste(input$quantchoose4, paste(paste(input$quantchoose5, collapse = " + "), ' + ',paste(input$categchoose3, collapse = " + ")), sep = " ~ "))
    reduced <- lm(f, data = data)
    data2 <- as.data.frame(t(input$matrix2))
    for (f in input$categchoose3){
      data2[,names(data2) %in% f] <- as.factor(data2[,names(data2) %in% f])
    }
    for (f in input$quantchoose4){
      data2[,names(data2) %in% f] <- as.numeric(as.character(data2[,names(data2) %in% f]))
    }
    # for (f in input$quantchoose5){
    #   data2[,names(data2) %in% f] <- as.numeric(as.character(data2[,names(data2) %in% f]))
    # }
    for (c in quant[quant %notin% input$quantchoose4]){
      if(as.numeric(as.character(input$matrix[rownames(input$matrix) == c,])) == 0){
        data2[,names(data2) == c] <- log(as.numeric(as.character(data2[,names(data2) == c])))
      } else {
        data2[,names(data2) == c] <- as.numeric(as.character(data2[,names(data2) == c])) ^ as.numeric(as.character(input$matrix[rownames(input$matrix) == c,]))
      }
    }

    pred <- predict(reduced, newdata = data2)
    pred_range <- predict(reduced, newdata = data2, level=0.95, interval="prediction")
    data2 <- cbind.data.frame(data2,pred, pred_range)
    for (c in c(input$quantchoose4)){
      if(as.numeric(as.character(input$matrix[rownames(input$matrix) == c,])) == 0){
        # data2[,names(data2) == c] <- log(data2[,names(data2) == c])
        data2[,names(data2) == "pred"] <- exp(data2[,names(data2) == "pred"])
        data2[,names(data2) == "lwr"] <- exp(data2[,names(data2) == "lwr"])
        data2[,names(data2) == "upr"] <- exp(data2[,names(data2) == "upr"])
      } else {
        # data2[,names(data2) == c] <- data2[,names(data2) == c] ^ (1/as.numeric(input$matrix[rownames(input$matrix) == c,]))
        data2[,names(data2) == "pred"] <- data2[,names(data2) == "pred"] ^ (1/as.numeric(input$matrix[rownames(input$matrix) == c,]))
        data2[,names(data2) == "lwr"] <- data2[,names(data2) == "lwr"] ^ (1/as.numeric(input$matrix[rownames(input$matrix) == c,]))
        data2[,names(data2) == "upr"] <- data2[,names(data2) == "upr"] ^ (1/as.numeric(input$matrix[rownames(input$matrix) == c,]))

      }
    }
    valueBox(
      paste0("$",comma(round(data2[,names(data2) %in% "pred"] , digits = 2))), paste0("With an interval of ","$",comma(round(data2[,names(data2) %in% "lwr"] , digits = 2))," to $",comma(round(data2[,names(data2) %in% "upr"] , digits = 2))), icon = icon("home", lib = "glyphicon"), color = "green")
  
  })
  
  
  output$resplotm <- renderPlot({
    data <- data3()
    quant <- c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")
    data <- data[,names(data) %in% quant]
    for (c in names(data)){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        data[,names(data) == c] <- log(data[,names(data) == c])
      } else {
        data[,names(data) == c] <- data[,names(data) == c] ^ as.numeric(input$matrix[rownames(input$matrix) == c,])
      }
    }
    f <- as.formula(paste(input$quantchoose4, paste(paste(input$quantchoose5, collapse = " + ")), sep = " ~ "))
    reduced <- lm(f, data = data)
    plot(reduced$fitted.values,reduced$residuals,main="Residual Plot of Full Model")
    abline(h=0,col="red")
  })
  
  output$acfplotm <- renderPlot({
    data <- data3()
    quant <- c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")
    data <- data[,names(data) %in% quant]
    for (c in names(data)){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        data[,names(data) == c] <- log(data[,names(data) == c])
      } else {
        data[,names(data) == c] <- data[,names(data) == c] ^ as.numeric(input$matrix[rownames(input$matrix) == c,])
      }
    }
    f <- as.formula(paste(input$quantchoose4, paste(paste(input$quantchoose5, collapse = " + ")), sep = " ~ "))
    reduced <- lm(f, data = data)
    acf(reduced$residuals, main="ACF of Residuals from Full Model")
  })
  
  output$qplotm <- renderPlot({
    data <- data3()
    quant <- c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")
    data <- data[,names(data) %in% quant]
    for (c in names(data)){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        data[,names(data) == c] <- log(data[,names(data) == c])
      } else {
        data[,names(data) == c] <- data[,names(data) == c] ^ as.numeric(input$matrix[rownames(input$matrix) == c,])
      }
    }
    f <- as.formula(paste(input$quantchoose4, paste(paste(input$quantchoose5, collapse = " + ")), sep = " ~ "))    
    reduced <- lm(f, data = data)
    qqnorm(reduced$residuals)
    qqline(reduced$residuals, col="red")
  })
  
  output$resplotm2 <- renderPlot({
    data <- data3()
    # data.y <- aggregate(data[,names(data) %in% input$quantchoose4], by = list(data$Year), FUN = median)
    # names(data.y) <- c("Year","Median.y")
    # data.y$Year_Index <- data.y$Median.y/median(data[,names(data) %in% input$quantchoose4])
    # data.m <- aggregate(data[,names(data) %in% input$quantchoose4], by = list(data$Month), FUN = median)
    # names(data.m) <- c("Month","Median.m")
    # data.m$Month_Index <- data.m$Median.m/median(data[,names(data) %in% input$quantchoose4])
    # data <- merge(data,data.y, by = "Year", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose4] <- data[,names(data) %in% input$quantchoose4] * data$Year_Index
    # data <- merge(data,data.m, by = "Month", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose4] <- data[,names(data) %in% input$quantchoose4] * data$Month_Index
    for (f in input$categchoose3){
      data[,names(data) %in% f] <- as.factor(data[,names(data) %in% f])
    }
    quant <- c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")
    # data <- data[,names(data) %in% quant]
    for (c in quant){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        data[,names(data) == c] <- log(data[,names(data) == c])
      } else {
        data[,names(data) == c] <- data[,names(data) == c] ^ as.numeric(input$matrix[rownames(input$matrix) == c,])
      }
    }
    f <- as.formula(paste(input$quantchoose4, paste(paste(input$quantchoose5, collapse = " + "), ' + ',paste(input$categchoose3, collapse = " + ")), sep = " ~ "))
    reduced <- lm(f, data = data)
    plot(reduced$fitted.values,reduced$residuals,main="Residual Plot of Full Model")
    abline(h=0,col="red")
  })
  
  output$acfplotm2 <- renderPlot({
    data <- data3()
    # data.y <- aggregate(data[,names(data) %in% input$quantchoose4], by = list(data$Year), FUN = median)
    # names(data.y) <- c("Year","Median.y")
    # data.y$Year_Index <- data.y$Median.y/median(data[,names(data) %in% input$quantchoose4])
    # data.m <- aggregate(data[,names(data) %in% input$quantchoose4], by = list(data$Month), FUN = median)
    # names(data.m) <- c("Month","Median.m")
    # data.m$Month_Index <- data.m$Median.m/median(data[,names(data) %in% input$quantchoose4])
    # data <- merge(data,data.y, by = "Year", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose4] <- data[,names(data) %in% input$quantchoose4] * data$Year_Index
    # data <- merge(data,data.m, by = "Month", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose4] <- data[,names(data) %in% input$quantchoose4] * data$Month_Index
    for (f in input$categchoose3){
      data[,names(data) %in% f] <- as.factor(data[,names(data) %in% f])
    }
    quant <- c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")
    # data <- data[,names(data) %in% quant]
    for (c in quant){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        data[,names(data) == c] <- log(data[,names(data) == c])
      } else {
        data[,names(data) == c] <- data[,names(data) == c] ^ as.numeric(input$matrix[rownames(input$matrix) == c,])
      }
    }
    f <- as.formula(paste(input$quantchoose4, paste(paste(input$quantchoose5, collapse = " + "), ' + ',paste(input$categchoose3, collapse = " + ")), sep = " ~ "))
    reduced <- lm(f, data = data)
    acf(reduced$residuals, main="ACF of Residuals from Full Model")
  })
  
  output$qplotm2 <- renderPlot({
    data <- data3()
    # data.y <- aggregate(data[,names(data) %in% input$quantchoose4], by = list(data$Year), FUN = median)
    # names(data.y) <- c("Year","Median.y")
    # data.y$Year_Index <- data.y$Median.y/median(data[,names(data) %in% input$quantchoose4])
    # data.m <- aggregate(data[,names(data) %in% input$quantchoose4], by = list(data$Month), FUN = median)
    # names(data.m) <- c("Month","Median.m")
    # data.m$Month_Index <- data.m$Median.m/median(data[,names(data) %in% input$quantchoose4])
    # data <- merge(data,data.y, by = "Year", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose4] <- data[,names(data) %in% input$quantchoose4] * data$Year_Index
    # data <- merge(data,data.m, by = "Month", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose4] <- data[,names(data) %in% input$quantchoose4] * data$Month_Index
    for (f in input$categchoose3){
      data[,names(data) %in% f] <- as.factor(data[,names(data) %in% f])
    }
    quant <- c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")
    # data <- data[,names(data) %in% quant]
    for (c in quant){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        data[,names(data) == c] <- log(data[,names(data) == c])
      } else {
        data[,names(data) == c] <- data[,names(data) == c] ^ as.numeric(input$matrix[rownames(input$matrix) == c,])
      }
    }
    f <- as.formula(paste(input$quantchoose4, paste(paste(input$quantchoose5, collapse = " + "), ' + ',paste(input$categchoose3, collapse = " + ")), sep = " ~ "))
    reduced <- lm(f, data = data)
    qqnorm(reduced$residuals)
    qqline(reduced$residuals, col="red")
  })
  
  output$downloader <- downloadHandler(filename = function() {
    
    paste0( "Raw Data.csv")
    
  },
  content = function(file) {
    data <- data()

    write.csv(data, file, row.names = FALSE)
    
  })
  
  
  output$downloadData <- downloadHandler(filename = function() {
    
    paste0( "Predicted Values.csv")
    
  },
  content = function(file) {
    data <- data3()
    data2 <- data4()
    # data.y <- aggregate(data[,names(data) %in% input$quantchoose4], by = list(data$Year), FUN = median)
    # names(data.y) <- c("Year","Median.y")
    # data.y$Year_Index <- data.y$Median.y/median(data[,names(data) %in% input$quantchoose4])
    # data.m <- aggregate(data[,names(data) %in% input$quantchoose4], by = list(data$Month), FUN = median)
    # names(data.m) <- c("Month","Median.m")
    # data.m$Month_Index <- data.m$Median.m/median(data[,names(data) %in% input$quantchoose4])
    # data <- merge(data,data.y, by = "Year", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose4] <- data[,names(data) %in% input$quantchoose4] * data$Year_Index
    # data <- merge(data,data.m, by = "Month", all.x = TRUE)
    # data[,names(data) %in% input$quantchoose4] <- data[,names(data) %in% input$quantchoose4] * data$Month_Index
    for (f in input$categchoose3){
      data[,names(data) %in% f] <- as.factor(data[,names(data) %in% f])
    }
    quant <- c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")
    # data <- data[,names(data) %in% quant]
    for (c in quant){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        data[,names(data) == c] <- log(data[,names(data) == c])
      } else {
        data[,names(data) == c] <- data[,names(data) == c] ^ as.numeric(input$matrix[rownames(input$matrix) == c,])
      }
    }
    f <- as.formula(paste(input$quantchoose4, paste(paste(input$quantchoose5, collapse = " + "), ' + ',paste(input$categchoose3, collapse = " + ")), sep = " ~ "))
    reduced <- lm(f, data = data)
    for (c in quant[quant %notin% input$quantchoose4]){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        data2[,names(data2) == c] <- log(data2[,names(data2) == c])
      } else {
        data2[,names(data2) == c] <- data2[,names(data2) == c] ^ as.numeric(input$matrix[rownames(input$matrix) == c,])
      }
    }
    for (f in input$categchoose3){
      data2[,names(data2) %in% f] <- as.factor(data2[,names(data2) %in% f])
    }
    for (f in input$quantchoose4){
      data2[,names(data2) %in% f] <- as.numeric(as.character(data2[,names(data2) %in% f]))
    }
    pred <- predict(reduced, newdata = data2)
    pred_range <- predict(reduced, newdata = data2, level=0.95, interval="prediction")
    data2 <- cbind.data.frame(data2,pred,pred_range)
    for (c in c(input$quantchoose4)){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        # data2[,names(data2) == c] <- log(data2[,names(data2) == c])
        data2[,names(data2) == "pred"] <- exp(data2[,names(data2) == "pred"])
        data2[,names(data2) == "lwr"] <- exp(data2[,names(data2) == "lwr"])
        data2[,names(data2) == "upr"] <- exp(data2[,names(data2) == "upr"])
      } else {
        # data2[,names(data2) == c] <- data2[,names(data2) == c] ^ (1/as.numeric(input$matrix[rownames(input$matrix) == c,]))
        data2[,names(data2) == "pred"] <- data2[,names(data2) == "pred"] ^ (1/as.numeric(input$matrix[rownames(input$matrix) == c,]))
        data2[,names(data2) == "lwr"] <- data2[,names(data2) == "lwr"] ^ (1/as.numeric(input$matrix[rownames(input$matrix) == c,]))
        data2[,names(data2) == "upr"] <- data2[,names(data2) == "upr"] ^ (1/as.numeric(input$matrix[rownames(input$matrix) == c,]))
        
      }
    }
    write.csv(data2, file, row.names = FALSE)
    
  })
  
  output$levene <- renderPrint({
    data <- data3()
    for (f in input$categchoose4){
      data[,names(data) %in% f] <- as.factor(data[,names(data) %in% f])
    }
    quant <- c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt","Population","WalkScore","Longitude","Latitude")
    # data <- data[,names(data) %in% quant]
    for (c in quant){
      if(as.numeric(input$matrix[rownames(input$matrix) == c,]) == 0){
        data[,names(data) == c] <- log(data[,names(data) == c])
      } else {
        data[,names(data) == c] <- data[,names(data) == c] ^ as.numeric(input$matrix[rownames(input$matrix) == c,])
      }
    }
    levene.test(data[,names(data) %in% input$quantchoose4],data[,names(data) %in% input$categchoose4])
    
  })
  
})


shinyApp(ui = ui, server = server)


