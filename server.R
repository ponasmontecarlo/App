library(mnormt)
library(ggplot2)
library(reshape2)
source("functions.R")

shinyServer(function(input, output) {
  
  ###############
  ### DATA TABLES
  ###############
  
  # univariate
  dataUni <- reactive({
    input$run
    
    isolate ({
      n <- input$n
    df <- input$df
    X <- rt(n,df)
    t <- seq(-3.2,3.2,0.01)
    p <- pt(t,df)
    
    est <- rep(NA,641)
    var <- rep(NA,641)
    
    for (i in 1:641) {
      est[i] <- tUnivariate(X,t[i])
    }
    
    for (i in 1:641) {
      var[i] <- tUnivariateVar(n,X,t[i],est[i])
    }
    
    dataUni <- data.frame(t=t,estimate=est,trueValue=p,
                          low=-2*sqrt(var)+est,high=2*sqrt(var)+est)
    dataUni <- melt(dataUni,id=c("t","low","high"))
    return(dataUni)
    })
  })
  
  # bivariate
  dataBi <- reactive({
    input$runBi
    isolate ({
      n <- input$nBi
      df <- input$dfBi

      sigma <- diag(2) 
      mu <- rep(0,2) 
      X <- rmt(n,mu,sigma,df) 
      t <- data.frame(t1=seq(-3.2,3.2,0.01),t2=seq(-3.2,3.2,0.01))
      p <- apply(t,1,function(x) pmt(x,mu,sigma,df))
      
      estBi <- rep(NA,641)
      
      for (i in 1:641) {
        estBi[i] <- tMultivariate(X,t[i,],n)
      }
      
      dataBi <- data.frame(t=t[,1],estimate=estBi,trueValue=p)
      dataBi <- melt(dataBi,id="t")
      return(dataBi)
    })
  })
  
  ##################
  ### APPROXIMATIONS
  ##################
  output$approxUni <- renderPrint({
    input$calculateUni
    
    isolate ({
      X <- rt(input$nApproxUni,input$dfApproxUni)
      approx <- tUnivariate(X,input$tUni)
      true <- pt(input$tUni,input$dfApproxUni)
      paste("True value is",true,", while approximated value is",approx,
            ",therefore difference: ",abs(true-approx))
      })
  })
  
  output$approxBi <- renderPrint({
    input$calculateBi
    
    isolate ({
      sigma <- diag(2) 
      mu <- rep(0,2) 
      X <- rmt(input$nApproxBi,mu,sigma,input$dfApproxBi)
      approx <- tMultivariate(X,c(input$tBi1,input$tBi2),input$nApproxBi)
      true <- pmt(c(input$tBi1,input$tBi2),mu,sigma,input$dfApproxBi)
      paste("True value is",true,", while approximated value is",approx,
            ",therefore difference: ",abs(true-approx))
    })
  })
  
  #############
  ###### GRAPHS
  #############
  
  output$uniPlot <- renderPlot({
    dataUni <- dataUni()
    p <- ggplot(dataUni,aes(t,value,colour=variable))+geom_line()+
      geom_ribbon(aes(ymin=low,ymax=high),fill="yellow",colour=NA,alpha=0.5)+
      ggtitle("Univariate t-distribution approximation")
    print(p)
  })
  
  output$biPlot <- renderPlot({
    dataBi <- dataBi()
    p <- ggplot(dataBi,aes(t,value,colour=variable))+geom_line()+
      ggtitle("Bivariate t-distribution")
    print(p)
  })
  
  ###########
  ###### TABS
  ###########
  
  # Univariate tab 
  output$univariateTab <- renderUI({
    uiOutput("univariate")
  })
  
  # Univariate tab 
  output$univariate <- renderUI({
    fluidPage(plotOutput("uniPlot"),
              fluidRow(
                column(3,h4("Plot CDF approximation"),
                       actionButton("run","Plot!"),
                       numericInput("n","Number of generated random variables:",500),
                       numericInput("df","Degrees of freedom:",1)
                       ),
                column(4,h4("Probability approximation"),
                       actionButton("calculateUni","Calculate!"),
                       numericInput("tUni","Value to approximate:",0,step=0.1),
                       numericInput("nApproxUni","Number of generated random variables:",1000),
                       numericInput("dfApproxUni","Degrees of freedom",1)),
                column(4,textOutput("approxUni"))
                )
              
    )
  })
  
  # Bivariate tab
  output$bivariateTab <- renderUI({
    uiOutput("bivariate")
  })
  
  # Bivariate tab
  output$bivariate <- renderUI({
    fluidPage(
      fluidRow(plotOutput("biPlot"),
        column(3,h4("Plot CDF approximation"),
               actionButton("runBi","Plot!"),
               numericInput("nBi","Number of generated random variables",500),
               numericInput("dfBi","Degrees of freedom",1)),
        column(3,h4("Probability approximation"),
               actionButton("calculateBi","Calculate!"),
               numericInput("nApproxBi","Number of generated random variables:",500),
               numericInput("dfApproxBi","Degrees of freedom",1)),
        column(3,hr(),numericInput("tBi1","First value to approximate:",0,step=0.1),
               numericInput("tBi2","Second value to approximate:",0,step=0.1)),
        column(3,textOutput("approxBi"))
        ))
  })
  
})