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
library(corrplot)

# bring in your data 
#setwd(dir = "C:/Users/MSachs.MSACHS-DELL/Documents/UVA MSDS/STAT 6021/melbourne-housing")
melb.df <- read.csv("melbourne_cleaned.csv", stringsAsFactors = FALSE, na.strings = c(""," ","NA"))
melb.df <- data.frame(melb.df[,-1], row.names=melb.df[,1])
melb.df$Date <- as.Date(melb.df$Date,"%Y-%m-%d")
melb.df$Date2 <- format(melb.df$Date, "%Y-%m")

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
                    tabBox(title = "Initial Model",tabPanel("Model Explanation",br(),shiny::p("This is the start of the modeling process. Select a predictor and response variable. In the output, is the coefficient statistically significant? Are the assumptions of linearity being met?",style="color:black;text-align:justify"),
                                                                 br(),width = 12,style="background-color:papayawhip;border-left:8px solid coral;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),tabPanel("Model Output",verbatimTextOutput("smodel"))),
                    tabBox(title = "Assumption Checks",tabPanel("What To Look For",br(),shiny::p("There are four things we want to check for: 1) is there a linear relationship, 2) do the residuals have constant variance, 3) are the residuals independent, and 4) are the residuals normally distributed?",style="color:black;text-align:justify"),
                                                            br(),width = 12,style="background-color:papayawhip;border-left:8px solid coral;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),tabPanel("Linearity",plotOutput("smplot")),
                    tabPanel("Constant Variance",plotOutput("smplot1")), tabPanel("Independence",plotOutput("smplot2")),tabPanel("Normality",plotOutput("smplot3"))),
                    tabBox(title = "Box Cox Transformation",tabPanel("Why We Would Do It",br(),shiny::p("If our assumption of constant variance has been violated, or the assumption of linearity is not met, a Box Cox Transformation may be called for.",style="color:black;text-align:justify"),br(),width = 12,style="background-color:lightblue;border-left:8px solid blue;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
                           tabPanel("Box Cox Plot",plotOutput("bc")), tabPanel("Suggested Transformation",valueBoxOutput("bcvalue")),tabPanel("Chosen Transformation",numericInput("obs2", "Which Transformation Would You Like:", 0, min = -3, max = 3))),
                    tabBox(title = "Re-Check Assumptions",tabPanel("What To Look For",br(),shiny::p("Did our transformation of the response resolve our outstanding assumption issues? If not, we should consider transforming our predictor as well.",style="color:black;text-align:justify"),br(),width = 12,style="background-color:lightblue;border-left:8px solid blue;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
                           tabPanel("Linearity",plotOutput("tsmplot")),tabPanel("Constant Variance",plotOutput("tsmplot1")), tabPanel("Independence",plotOutput("tsmplot2")),tabPanel("Normality",plotOutput("tsmplot3"))),
                    tabBox(title = "Predictor Transformation",tabPanel("Why We Would Do It",br(),shiny::p("If our assumption of  linearity is not met after a Box Cox Transformation, a predictor transformation may be in order.",style="color:black;text-align:justify"),br(),width = 12,style="background-color:pink;border-left:8px solid red;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
                           tabPanel("Chosen Transformation",numericInput("obs3", "Which Transformation Would You Like:", 0, min = -3, max = 3))),
                    tabBox(title = "Re-Check Assumptions",tabPanel("What To Look For",br(),shiny::p("Did our transformation of the response resolve our outstanding assumption issues? If not, we should consider transforming our predictor as well.",style="color:black;text-align:justify"),br(),width = 12,style="background-color:pink;border-left:8px solid red;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
                           tabPanel("Linearity",plotOutput("ttsmplot")),tabPanel("Constant Variance",plotOutput("ttsmplot1")), tabPanel("Independence",plotOutput("ttsmplot2")),tabPanel("Independence 2",plotOutput("ttsmplot2a")),tabPanel("Normality",plotOutput("ttsmplot3"))),
                    tabBox(title = "Final Model With Categorical Variables",tabPanel("Explanation",br(),shiny::p("You have made your transformations of the response and predictor (if necessary), so now choose which categorical predictors you wish to add for your final model. We have also adjusted the response variable by month and year to deal with time series implications.",style="color:black;text-align:justify"),br(),width = 12,style="background-color:lightgreen;border-left:8px solid green;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
                           tabPanel("Model Output",verbatimTextOutput("fmodel"))),
                    tabBox(title = "Validate Assumptions",tabPanel("What To Look For",br(),shiny::p("Our final model should meet our three assumptions: 1) the residuals have constant variance, 2) the residuals are independent, and 4) the residuals are normally distributed.",style="color:black;text-align:justify"),br(),width = 12,style="background-color:lightgreen;border-left:8px solid green;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"),
                           tabPanel("Constant Variance",plotOutput("tsmplot1.sa")), tabPanel("Independence",plotOutput("tsmplot2.sa")),tabPanel("Normality",plotOutput("tsmplot3.sa")))
                    ),
            tabItem("mod",)
            
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
    categ <- sort(c("Type","Postcode","Suburb","CouncilArea","Regionname","SellerG"), decreasing = FALSE)
    selectInput("categchoose", "Select Categorical Variable:", categ, selected = categ[1], multiple = FALSE)
  })
  
  output$categl <- renderUI({
    categl <- sort(unique(melb.df[,names(melb.df) %in% input$categchoose]), decreasing = FALSE)
    categl <- append(as.character(categl), "All", after =  0)
    selectInput("categlchoose", "Select Level To Categorical:", categl, selected = "All", multiple = TRUE)
  })
  
  output$quant <- renderUI({
    quant <- sort(c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt"),decreasing = FALSE)
    selectInput("quantchoose", "Select Quantitative Variable:", quant, selected = "Price", multiple = FALSE)
  })
  
  output$categ2 <- renderUI({
    categ2 <- sort(c("Type","SellerG","Postcode","Suburb","CouncilArea","Regionname"), decreasing = FALSE)
    selectInput("categchoose2", "Select Categorical Predictor Variable(s) To Add:", categ2, selected = categ2[1], multiple = TRUE)
  })
  
  output$quant3 <- renderUI({
    quant3 <- sort(c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt"), decreasing = FALSE)
    selectInput("quantchoose3", "Select Predictor Variable:", quant3, selected = "Rooms", multiple = FALSE)
  })
  
  output$quant2 <- renderUI({
    quant2 <- sort(c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt"),decreasing = FALSE)
    selectInput("quantchoose2", "Select Response Variable:", quant2, selected = "Price", multiple = FALSE)
  })
  
  output$categ3 <- renderUI({
    categ3 <- sort(c("Type","SellerG","Postcode","Suburb","CouncilArea","Regionname"), decreasing = FALSE)
    selectInput("categchoose3", "Select Categorical Predictor Variable(s) To Add:", categ3, selected = categ3[1], multiple = TRUE)
  })
  
  output$quant5 <- renderUI({
    quant5 <- sort(c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt"), decreasing = FALSE)
    selectInput("quantchoose5", "Select Predictor Variable(s):", quant5, selected = list("Rooms","YearBuilt"), multiple = TRUE)
  })
  
  output$quant4 <- renderUI({
    quant4 <- sort(c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt"),decreasing = FALSE)
    selectInput("quantchoose4", "Select Response Variable:", quant4, selected = "Price", multiple = FALSE)
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
               menuSubItem(uiOutput("categl"))
      ),
      menuItem("Simple Linear Regression", tabName = "stat" , icon = icon("stats"),
               menuSubItem(uiOutput("quant2"), tabName = "stat"),
               menuSubItem(uiOutput("quant3")),
               menuSubItem(uiOutput("categ2"))),
      menuItem("Multivariate Linear Regression", tabName = "mod", icon = icon("laptop-code"),
               menuSubItem(uiOutput("quant4"), tabName = "stat"),
               menuSubItem(uiOutput("quant5")),
               menuSubItem(uiOutput("categ3")))
      
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
    # melb.df$Year <- year(melb.df$Date)
    # melb.df$Month <- month(melb.df$Date)
    melb.df
  })
  
  data3 <- reactive({
    req(input$quantchoose4)
    req(input$quantchoose5)
    req(input$categchoose3)
    # melb.df$Year <- year(melb.df$Date)
    # melb.df$Month <- month(melb.df$Date)
    melb.df
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
        lat = ~Lattitude,
        lon = ~Longtitude,
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
    names.list <- c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt")
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
    pairs(data2()[,names(data2()) %in%   c("Price","Rooms","Bathroom","Distance","Car","Landsize","BuildingArea","YearBuilt")], lower.panel = NULL)
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
    data.y <- aggregate(data[,names(data) %in% input$quantchoose2], by = list(data$Year), FUN = median)
    names(data.y) <- c("Year","Median.y")
    data.y$Year_Index <- data.y$Median.y/median(data[,names(data) %in% input$quantchoose2])
    data.m <- aggregate(data[,names(data) %in% input$quantchoose2], by = list(data$Month), FUN = median)
    names(data.m) <- c("Month","Median.m")
    data.m$Month_Index <- data.m$Median.m/median(data[,names(data) %in% input$quantchoose2])
    data <- merge(data,data.y, by = "Year", all.x = TRUE)
    data[,names(data) %in% input$quantchoose2] <- data[,names(data) %in% input$quantchoose2] * data$Year_Index
    data <- merge(data,data.m, by = "Month", all.x = TRUE)
    data[,names(data) %in% input$quantchoose2] <- data[,names(data) %in% input$quantchoose2] * data$Month_Index
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
    if(length(input$categchoose2) == 1){
      res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]), data = data)
    } else {
      if(length(input$categchoose2) == 2){
        res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]) + get(input$categchoose2[2]), data = data)
      } else {
        if(length(input$categchoose2) == 3){
          res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]) + get(input$categchoose2[2]) + get(input$categchoose2[3]), data = data)
        } else {
          if(length(input$categchoose2) == 4){
            res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]) + get(input$categchoose2[2]) + get(input$categchoose2[3]) + get(input$categchoose2[4]), data = data)
          } else {
            if(length(input$categchoose2) == 5){
              res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]) + get(input$categchoose2[2]) + get(input$categchoose2[3]) + get(input$categchoose2[4]) + get(input$categchoose2[5]), data = data)
            } else {
              res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]) + get(input$categchoose2[2]) + get(input$categchoose2[3]) + get(input$categchoose2[4]) + get(input$categchoose2[5]) + + get(input$categchoose2[6]), data = data)

            }

          }
        }
      }
    }
    summary(res)

  })
  
  output$tsmplot1.sa <- renderPlot({
    data = data2()
    data.y <- aggregate(data[,names(data) %in% input$quantchoose2], by = list(data$Year), FUN = median)
    names(data.y) <- c("Year","Median.y")
    data.y$Year_Index <- data.y$Median.y/median(data[,names(data) %in% input$quantchoose2])
    data.m <- aggregate(data[,names(data) %in% input$quantchoose2], by = list(data$Month), FUN = median)
    names(data.m) <- c("Month","Median.m")
    data.m$Month_Index <- data.m$Median.m/median(data[,names(data) %in% input$quantchoose2])
    data <- merge(data,data.y, by = "Year", all.x = TRUE)
    data[,names(data) %in% input$quantchoose2] <- data[,names(data) %in% input$quantchoose2] * data$Year_Index
    data <- merge(data,data.m, by = "Month", all.x = TRUE)
    data[,names(data) %in% input$quantchoose2] <- data[,names(data) %in% input$quantchoose2] * data$Month_Index
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
    if(length(input$categchoose2) == 1){
      res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]), data = data)
    } else {
      if(length(input$categchoose2) == 2){
        res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]) + get(input$categchoose2[2]), data = data)
      } else {
        if(length(input$categchoose2) == 3){
          res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]) + get(input$categchoose2[2]) + get(input$categchoose2[3]), data = data)
        } else {
          if(length(input$categchoose2) == 4){
            res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]) + get(input$categchoose2[2]) + get(input$categchoose2[3]) + get(input$categchoose2[4]), data = data)
          } else {
            if(length(input$categchoose2) == 5){
              res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]) + get(input$categchoose2[2]) + get(input$categchoose2[3]) + get(input$categchoose2[4]) + get(input$categchoose2[5]), data = data)
            } else {
              res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]) + get(input$categchoose2[2]) + get(input$categchoose2[3]) + get(input$categchoose2[4]) + get(input$categchoose2[5]) + + get(input$categchoose2[6]), data = data)
              
            }
            
          }
        }
      }
    }
    
    plot(res$fitted.values,res$residuals, main="Plot of Residuals against Fitted Values")
    abline(h=0,col="red")
  })
  
  output$tsmplot2.sa <- renderPlot({
    data = data2()
    data.y <- aggregate(data[,names(data) %in% input$quantchoose2], by = list(data$Year), FUN = median)
    names(data.y) <- c("Year","Median.y")
    data.y$Year_Index <- data.y$Median.y/median(data[,names(data) %in% input$quantchoose2])
    data.m <- aggregate(data[,names(data) %in% input$quantchoose2], by = list(data$Month), FUN = median)
    names(data.m) <- c("Month","Median.m")
    data.m$Month_Index <- data.m$Median.m/median(data[,names(data) %in% input$quantchoose2])
    data <- merge(data,data.y, by = "Year", all.x = TRUE)
    data[,names(data) %in% input$quantchoose2] <- data[,names(data) %in% input$quantchoose2] * data$Year_Index
    data <- merge(data,data.m, by = "Month", all.x = TRUE)
    data[,names(data) %in% input$quantchoose2] <- data[,names(data) %in% input$quantchoose2] * data$Month_Index
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
    if(length(input$categchoose2) == 1){
      res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]), data = data)
    } else {
      if(length(input$categchoose2) == 2){
        res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]) + get(input$categchoose2[2]), data = data)
      } else {
        if(length(input$categchoose2) == 3){
          res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]) + get(input$categchoose2[2]) + get(input$categchoose2[3]), data = data)
        } else {
          if(length(input$categchoose2) == 4){
            res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]) + get(input$categchoose2[2]) + get(input$categchoose2[3]) + get(input$categchoose2[4]), data = data)
          } else {
            if(length(input$categchoose2) == 5){
              res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]) + get(input$categchoose2[2]) + get(input$categchoose2[3]) + get(input$categchoose2[4]) + get(input$categchoose2[5]), data = data)
            } else {
              res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]) + get(input$categchoose2[2]) + get(input$categchoose2[3]) + get(input$categchoose2[4]) + get(input$categchoose2[5]) + + get(input$categchoose2[6]), data = data)
              
            }
            
          }
        }
      }
    }
    acf(res$residuals, main="ACF of Residuals (Squared)")
  })
  
  output$tsmplot3.sa <- renderPlot({
    data = data2()
    data.y <- aggregate(data[,names(data) %in% input$quantchoose2], by = list(data$Year), FUN = median)
    names(data.y) <- c("Year","Median.y")
    data.y$Year_Index <- data.y$Median.y/median(data[,names(data) %in% input$quantchoose2])
    data.m <- aggregate(data[,names(data) %in% input$quantchoose2], by = list(data$Month), FUN = median)
    names(data.m) <- c("Month","Median.m")
    data.m$Month_Index <- data.m$Median.m/median(data[,names(data) %in% input$quantchoose2])
    data <- merge(data,data.y, by = "Year", all.x = TRUE)
    data[,names(data) %in% input$quantchoose2] <- data[,names(data) %in% input$quantchoose2] * data$Year_Index
    data <- merge(data,data.m, by = "Month", all.x = TRUE)
    data[,names(data) %in% input$quantchoose2] <- data[,names(data) %in% input$quantchoose2] * data$Month_Index
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
    if(length(input$categchoose2) == 1){
      res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]), data = data)
    } else {
      if(length(input$categchoose2) == 2){
        res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]) + get(input$categchoose2[2]), data = data)
      } else {
        if(length(input$categchoose2) == 3){
          res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]) + get(input$categchoose2[2]) + get(input$categchoose2[3]), data = data)
        } else {
          if(length(input$categchoose2) == 4){
            res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]) + get(input$categchoose2[2]) + get(input$categchoose2[3]) + get(input$categchoose2[4]), data = data)
          } else {
            if(length(input$categchoose2) == 5){
              res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]) + get(input$categchoose2[2]) + get(input$categchoose2[3]) + get(input$categchoose2[4]) + get(input$categchoose2[5]), data = data)
            } else {
              res<- lm(Transformed.Response~Transformed.Predictor + get(input$categchoose2[1]) + get(input$categchoose2[2]) + get(input$categchoose2[3]) + get(input$categchoose2[4]) + get(input$categchoose2[5]) + + get(input$categchoose2[6]), data = data)
              
            }
            
          }
        }
      }
    }
    
    qqnorm(res$residuals)
    qqline(res$residuals, col="red")
  })
  
})


shinyApp(ui = ui, server = server)


# obs2 <- 0
# obs3 <- 0
# quantchoose2 <- "Price"
# quantchoose3 <- "Rooms"
# categchoose2 <- list("Type")
# data = melb.df
# data.y <- aggregate(data[,names(data) %in% quantchoose2], by = list(data$Year), FUN = median)
# names(data.y) <- c("Year","Median.y")
# data.y$Year_Index <- data.y$Median.y/median(data[,names(data) %in% quantchoose2])
# data.m <- aggregate(data[,names(data) %in% quantchoose2], by = list(data$Month), FUN = median)
# names(data.m) <- c("Month","Median.m")
# data.m$Month_Index <- data.m$Median.m/median(data[,names(data) %in% quantchoose2])
# data <- merge(data,data.y, by = "Year", all.x = TRUE)
# data[,names(data) %in% quantchoose2] <- data[,names(data) %in% quantchoose2] * data$Year_Index
# data <- merge(data,data.m, by = "Month", all.x = TRUE)
# data[,names(data) %in% quantchoose2] <- data[,names(data) %in% quantchoose2] * data$Month_Index
# for (f in categchoose2){
#   data[,names(data) %in% categchoose2] <- as.factor(data[,names(data) %in% categchoose2])
# }
# data <- data[sample(NROW(data)),]
# if(obs2 == 0){
#   data$Transformed.Response <- log(data[,names(data) %in% quantchoose2])
# } else {
#   data$Transformed.Response <- (data[,names(data) %in% quantchoose2]) ^ obs2
# }
# if(obs3 == 0){
#   data$Transformed.Predictor <- log(data[,names(data) %in% quantchoose3])
# } else {
#   data$Transformed.Predictor <- (data[,names(data) %in% quantchoose3]) ^ obs3
# }
# if(length(categchoose2) == 1){
#   res<- lm(Transformed.Response~Transformed.Predictor + get(categchoose2[1]), data = data)
# } else {
#   if(length(categchoose2) == 2){
#     res<- lm(Transformed.Response~Transformed.Predictor + get(categchoose2[1]) + get(categchoose2[2]), data = data)
#   } else {
#     if(length(categchoose2) == 3){
#       res<- lm(Transformed.Response~Transformed.Predictor + get(categchoose2[1]) + get(categchoose2[2]) + get(categchoose2[3]), data = data)
#     } else {
#       if(length(categchoose2) == 4){
#         res<- lm(Transformed.Response~Transformed.Predictor + get(categchoose2[1]) + get(categchoose2[2]) + get(categchoose2[3]) + get(categchoose2[4]), data = data)
#       } else {
#         if(length(categchoose2) == 5){
#           res<- lm(Transformed.Response~Transformed.Predictor + get(categchoose2[1]) + get(categchoose2[2]) + get(categchoose2[3]) + get(categchoose2[4]) + get(categchoose2[5]), data = data)
#         } else {
#           res<- lm(Transformed.Response~Transformed.Predictor + get(categchoose2[1]) + get(categchoose2[2]) + get(categchoose2[3]) + get(categchoose2[4]) + get(categchoose2[5]) + + get(categchoose2[6]), data = data)
#           
#         }
#         
#       }
#     }
#   }
# }
# summary(res)