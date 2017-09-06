## app.R ##
library(shinydashboard)
library(shiny)
library(quantmod)
library(Quandl)
library(zoo)
library(plyr)
require(gdata)
library(dplyr)
library(ggplot2)
library(ggfortify)
library("twitteR")
library("ROAuth")
library(tidyr)
library(PerformanceAnalytics)

readDGTL <- function(date){
  df <- read.xls("./www/iShares-Digitalisation-UCITS-ETF_fund.xlsx",sheet=3,header=TRUE)
  df <- df[,c(1,3)]
  df$As.Of <- as.Date(df$As.Of, "%d/%b/%Y")
  DGTL <- xts(df[,-1], order.by=df$As.Of)
  colnames(DGTL) <- c("DGTL")
  return (DGTL <- DGTL[paste0(date,'::')])
}

loadPrices <- function(stocksData, startDate){
  res <- dlply(stocksData, 1, function(x){generarDatos(x,startDate,progress)})
  if ((length(res))>1) {
    prices <- do.call(merge, lapply(res, function(x) x))
  }else{
    prices <- res[[1]]
  }
  prices <- na.locf(prices)
  colnames(prices) <- gsub(".Adjusted","",colnames(prices))
  return (prices)
}

createPortfolio <- function(dataFrame, startDate, benchmarkTicker)
{
  prices <- loadPrices(dataFrame,startDate)
  bench <- leerYahoo(benchmarkTicker,startDate)
  bench <- na.locf(bench)
  allData <- merge(prices,bench)
  allData <- allData[complete.cases(allData),]
  return (allData)
}





leerYahoo <- function(ticker,fromDate)
{
  r <- NULL
  attempt <- 1
  while( is.null(r) && attempt <= 20 ) {
    attempt <- attempt + 1
    try(
      symb <- getSymbols(ticker,auto.assign=FALSE, src="yahoo", from=fromDate)
    )
  } 
  symb <- Cl(symb)
  return(symb)
}


generarDatos <- function(stockData,fecha,progress){
  print(paste("Consultando ",stockData))
  # Increment the progress bar, and update the detail text.
  #progress$inc(1/nrow(data), detail = paste("Downloading ", stockData$ticker))
  ticker <- as.character(stockData$ticker)
  currency <- as.character(stockData$currency)
  if (ticker=="DGTL"){
    stockPrices <- readDGTL(fecha)
  }else{
    r <- NULL
    attempt <- 1
    while( is.null(r) && attempt <= 20 ) {
      attempt <- attempt + 1
      try(
        stockPrices <- getSymbols(ticker,auto.assign=FALSE, src="yahoo", from=fecha)
      )
    } 
    print(paste("Hay ",nrow(stockPrices),"datos"))
    stockPrices <- Ad(stockPrices)
    Sys.sleep(2)
  }
  if (!(stockData[2]=="EUR")){
    #aligning dates
    ## merge the 2 frames  and order vs Time
    print(head(Divisa))
    currency <- as.character(stockData[[2]])
    t1 <- stockPrices[paste0(fecha,'::')]
    print(class(t1))
    print(head(t1))
    t2 <- as.xts(Divisa[,currency])
    print(class(t2))
    print(head(t2))
    t2 <- t2[paste0(fecha,'::')]
    print(class(t2))
    print(head(t2))
    dat.all = merge(t1,t2)
    print(head(dat.all))
    dat.all = dat.all[complete.cases(dat.all),]
    colnames(dat.all) <- c("Price","Currency")
    return (stockPrices <- dat.all$Price/dat.all$Currency)
  }else{
    return (stockPrices)
  }
}


consumer_key <- "NVzwx8wpFYhEJsH0T0xEixGvf"
consumer_secret <- "7I8MbCq2LCb1hmsEHJbSolQvbYVKyR0nyDO93Ftot2tmnjieGK"
access_token <- "62368186-lMm2zdYidKKkl42NgzERPkMjZNfysiFQpL96crdik"
access_secret <- "8kkWv2wDyBFQjmAGR0teRFrdSWEPFkzN989JHhCBOcXZ3"


#setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
stockTickers <- read.xls("./www/tick3.xlsx",sheet=1,header=TRUE)
benchTickers <- read.xls("./www/bench.xlsx",sheet=1,header=TRUE)
colnames(stockTickers) <- c("label","value")
colnames(benchTickers) <- c("label","value")

update <- FALSE
data <- read.csv("data.csv",header=TRUE,sep = ";",dec = ",",
                 colClasses=(c("character","character","numeric")))
colnames(data) <- c("ticker","currency","weight")
initValue <- 100000
if (update==TRUE){
  EURUSD <- Quandl("ECB/EURUSD",start_date="2016-01-01",type="xts")
  EURGBP <- Quandl("ECB/EURGBP",start_date="2016-01-01",type="xts")
  Divisa <<- cbind(EURUSD,EURGBP)
  colnames(Divisa) <<- c("USD","GBP")
  benchmarkTicker <- "SPY"
  startDate <- "2017-05-30"
  prices <- loadPrices(data,startDate)
  bench <- leerYahoo("SPY",startDate)
  bench <- na.locf(bench)
  allData <- merge(prices,bench)
  allData <- allData[complete.cases(allData),]
  write.zoo(allData,"prueba.csv")
  write.zoo(Divisa,"currencyData.csv")
} else {
  allData <- read.zoo("prueba.csv",index.column = 1, sep = " ", header = TRUE)
  Divisa <- read.zoo("currencyData.csv",index.column = 1, sep = " ", header = TRUE)
}

ui <- dashboardPage(
  dashboardHeader(title = "Porfolio Management"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Portfolio", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Manage Portfolio", tabName = "widgets", icon = icon("th")),
      menuItem("Create Portfolio", tabName = "createPortfolio", icon = icon("th"))
    )
  ),
  dashboardBody(
    includeCSS("www/custom.css"),
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidPage(
                title = "Dashboard",
                fluidRow(
                  column(width = 12,
                         valueBoxOutput("usViBox", width = 3),
                         valueBoxOutput("highestViBox", width = 3),
                         valueBoxOutput("usAnnualBox", width = 3),
                         valueBoxOutput("highestAnnualBox", width = 3)
                  )#end of column
                ),# end of row
                fluidRow(
                  column(width = 4,includeHTML("twitter.html")
                           ),# end of column
                  column(width = 4,
                         box(
                           title = "Top 10 Stocks",
                           status = "primary",
                           width = 12,
                           height = 255,
                           solidHeader = FALSE,
                           collapsible = TRUE,
                           tableOutput("top10")
                         )#End of Box
                         ),# End of column
                  column(width = 4,
                         box(
                           title = "Worst 10 Stocks",
                           status = "primary",
                           width = 12,
                           height = 255,
                           solidHeader = FALSE,
                           collapsible = TRUE,
                           tableOutput("bottom10")
                         ) #End of Box
                  )# End of column
              ), # End of Fluid Row
                fluidRow(
                  column(width = 12,
                         box(
                           title = "Evolution",
                           status = "primary",
                           width = 12,
                           solidHeader = FALSE,
                           collapsible = TRUE,
                           plotOutput("evolDashboard")
                           #showOutput("top10StatesTS", "nvd3")
                         ) #End of Box
                  )# end of column
                  # column(width = 6,
                  #        box(
                  #          title = "Performance Attribution",
                  #          status = "primary",
                  #          width = 12,
                  #          solidHeader = FALSE,
                  #          collapsible = TRUE
                  #          #showOutput("top10CitiesTS", "nvd3")
                  #        ) #End of Box
                  # )# end of column
                ),#end of fluidrow
                fluidRow(
                  column(width = 12,
                         valueBoxOutput("numStatesBox", width = 3),
                         valueBoxOutput("numCountiesBox", width = 3),
                         valueBoxOutput("numCitiesBox", width = 3),
                         valueBoxOutput("numZipsBox", width = 3)
                  )# end of column
                )# end of fluidrow
                ) # End of fluidPage
      ), # End of tabItem
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content"),
              plotOutput("evolucion")
      ),
      tabItem(tabName = "createPortfolio",
      fluidRow(
          box(
            title = "Input Data", status = "primary", solidHeader = TRUE, width = 4,
            dateRangeInput(inputId="dates", label = "Rango de Fechas", format="yyyy-mm-dd"),
            # use regions as option groups
            selectizeInput(inputId='accion', 'Acciones', choices = NULL , multiple = FALSE),
            selectizeInput(inputId='divisa', 'Divisa', choices = list(
              Divisas = c(`EUR` = 'EUR', `USD` = 'USD', `GBP` = 'GBP')
            ), multiple = FALSE),
            numericInput(inputId="peso", label="Peso",100),
            selectizeInput(inputId='benchmark', 'Benchmark', choices = NULL , multiple = FALSE),
            actionButton(inputId="anadir", label="Agregar"),
            actionButton(inputId="evolucionButton", label="Analizar"),
            actionButton(inputId="guardar", label="Guardar"),
            actionButton(inputId="nuevo", label="Nuevo Portfolio")
            ),
          box(
            title = "Evolution", status = "primary", solidHeader = TRUE, width = 8,
            htmlOutput(outputId="texto",FALSE),
            tableOutput('table'),
            plotOutput("evolCreatedPorfolio"),
            tableOutput('summaryTable')
          )
      ),
      infoBoxOutput("out1")
      )
    )
  )
)


server <- function(input, output, session) {
  
  
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Loading Data", value = 0)
  
  updateSelectizeInput(session,'accion',choices=stockTickers, server=TRUE)
  updateSelectizeInput(session,'benchmark',choices=benchTickers, server=TRUE)
  
  allData <- allData[complete.cases(allData),]
  prices <- na.locf(allData[,(-ncol(allData))])
  bench <- na.locf(allData[,ncol(allData)])
  #Create Portfolio
  buyPrices <- prices[1]
  weights <- data[,"weight"]
  quantity <- initValue*weights/100
  quantity <- quantity/buyPrices
  totalInverted <- sum(quantity*buyPrices)
  lag <- stats::lag
  s <- lag(as.xts(prices))
  
  s <- lag(allData)
  rets = allData/lag(allData) - 1
  #rets <- Return.calculate(allData,method="discrete")#da error
  returns <- as.xts(rets[,(-ncol(rets))])
  retsBench <- as.xts(rets[,ncol(rets)])
  print(class(retsBench))
  retsBench[1,] <- 0
  returns[1,] <- 0
  print(head(returns))
  print(head(retsBench))
 
  portfolio_analytics <- Return.portfolio(R = returns, weights=weights, verbose=TRUE)
  # Plot the time-series
  portfolios.2 <<-cbind(cumprod(portfolio_analytics$returns+1),cumprod(retsBench+1))
  colnames(portfolios.2) <<-c("Port","Bench")
  print("PORT")
  print(head(portfolios.2))
   output$evolDashboard <- renderPlot({
     temp <- as.data.frame(portfolios.2)
     #print(class(temp))
     #print(head(temp))
     ggplot(temp, aes(x=index(portfolios.2),y=Port)) + geom_line(aes(color="Portfolio")) + geom_line(data=temp,aes(y=Bench,color="Benchmark")) + 
       labs(color="") + xlab("") + ylab("NAV")
     
   })
  totalRentPort <- Return.cumulative(rets)*100
  totalRentBench <- Return.cumulative(retsBench)*100
  NAV <- (totalRentPort/100+1)*initValue
  
  #===============================================================================
  #                        CREATE PORTFOLIO TAB SERVER                           #
  #===============================================================================
  # 
  weight <- 100
  df <- data.frame(ticker=character(),
                   currency=character(),
                   weight=double(),
                   stringsAsFactors=FALSE)
  stocks <- list()
  startTime <- Sys.time()
  data <- observeEvent (input$anadir, {
    newrow <- data.frame(ticker=input$accion,currency=input$divisa,weight=as.double(input$peso))
    weight <<- weight - input$peso
    df <<- rbind(df,newrow)
    output$table <- renderTable(df)
    # output$texto <- renderPrint({paste(startTime," ",stocks)})
    observe({
      # This will change the value of input$inText, based on x
      updateTextInput(session, "peso", value = weight)
    })
    
  })
  
  #===============================================================================
  #                        DASHBOARD SERVER FUNCTIONS                            #
  #===============================================================================
  # Render National Home Value Index Box
  output$usViBox <- renderValueBox({
    #current <- currentState[ which(currentState$State == "United States"), ]
    valueBox(
      paste0("???",round(NAV,2)), "Valor de la cartera", 
      icon = icon("dollar"), color = "green"
    )
  })
  
  # Highest Home Value Index by City Box
  output$highestViBox <- renderValueBox({
    #current <- currentCity[ which.max(currentCity$Value), ]
    valueBox(
      paste0(round(totalRentPort,2),"%"), paste("Rent. Acumulada"), 
      icon = icon("money"), color = "blue"
    )
  })
  
  # Render Annual Price Growth  Box
  output$usAnnualBox <- renderValueBox({
    # current <- currentState[ which(currentState$State == "United States"), ]
    valueBox(
      paste0(round(totalRentBench,2), "%"), paste("Rent. Benchmark"), 
      icon = icon("bar-chart"), color = "red"
    )
  })
  
  # Render Highest Annual Price Growth  Box
  output$highestAnnualBox <- renderValueBox({
    #current <- currentCity[ which.max(currentCity$Annual), ]
    valueBox(
      paste0(round(3000 * 100,4), "%"), paste("Highest Annual Change in Home Values in ", "Bilbao"), 
      icon = icon("line-chart"), color = "purple"
    )
  })
  
  output$top10 <- renderTable({
    tableStocks <<- gather(round(as.data.frame(Return.cumulative(returns)*100),2),"stock","return")
    tableStocks <<- arrange(tableStocks, desc(return))
    head(tableStocks,5)
    })
  output$bottom10 <- renderTable({
    #table <- gather(round(as.data.frame(Return.cumulative(returns)*100),2),"stock","return")
    temp <- arrange(tableStocks, return)
    head(arrange(temp),5)
  })
  
  output$evolucion <- renderPlot({
      print(class(portfolios.2))
      temp <- as.data.frame(portfolios.2)
      print(class(temp))
      print(head(temp))
      ggplot(temp, aes(x=index(temp$Port),y=Port)) + geom_line(aes(color="Portfolio")) + geom_line(data=temp,aes(y=Bench,color="Benchmark")) + 
        labs(color="") + xlab("") + ylab("NAV")
  })
  
  observeEvent(input$evolucionButton,{
    print(head(df))
    intDf <- df
    head(df)
    allDataInt <- createPortfolio(df,startDate = input$dates[1],benchmarkTicker = input$benchmark)
    #Create Portfolio
    buyPrices <- allDataInt[1,(-ncol(allDataInt))]
    weights <- intDf[,"weight"]/100
    quantity <- initValue*weights/100
    quantity <- quantity/buyPrices
    totalInverted <- sum(quantity*buyPrices)
    lag <- stats::lag
    rets = allDataInt/lag(allDataInt) - 1
    #rets <- Return.calculate(allData,method="discrete")#da error
    returns <- as.xts(rets[,(-ncol(rets))])
    retsBench <- as.xts(rets[,ncol(rets)])
    retsBench[1,] <- 0
    returns[1,] <- 0
    print(head(returns))
    print(head(retsBench))
    portfolio_analytics <- Return.portfolio(R = returns, weights=weights, verbose=TRUE)
    # Plot the time-series
    portfolios.2 <<-cbind(cumprod(portfolio_analytics$returns+1),cumprod(retsBench+1))
    colnames(portfolios.2) <<-c("Port","Bench")
    print("PORT")
    print(head(portfolios.2))
    output$evolCreatedPorfolio <- renderPlot({
      temp <- as.data.frame(portfolios.2)
      ggplot(temp, aes(x=index(portfolios.2),y=Port)) + geom_line(aes(color="Portfolio")) + geom_line(data=temp,aes(y=Bench,color="Benchmark")) + 
        labs(color="") + xlab("") + ylab("NAV")

    })
    #Statistics summary
    totalRentPort <- Return.cumulative(returns)*100
    totalRentBench <- Return.cumulative(retsBench)*100
    summary <- table.SFM(returns,retsBench,Rf=0)
    summary <- rbind(c(totalRentPort),c(totalRentPort-totalRentBench),summary)
    rownames(summary)[1:2] <- c("Total Return","Excess Returns")
    output$summaryTable <- renderTable(summary,rownames=TRUE)
  })
  
  observeEvent(input$nuevo,{
    weight <<- 100
    df <<- data.frame(ticker=character(),
                      currency=character(),
                      weight=double(),
                      stringsAsFactors=FALSE)
    output$table <- renderTable(df)
  })
  
}

shinyApp(ui, server)