library(shinyIncubator)
library(ggplot2)
library(reshape2)

shinyServer(function(input, output) {
  
  ### data
  
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
    
    dataUni <- data.frame(t=t,estimate=est,prob=p,
                          low=-2*sqrt(var)+est,high=2*sqrt(var)+est)
    dataUni <- melt(dataUni,id=c("t","low","high"))
    return(dataUni)
    })
  })
  
  ### PLOT
  
  output$uniPlot <- renderPlot({
    dataUni <- dataUni()
    p <- ggplot(dataUni,aes(t,value,colour=variable))+geom_line()+
      geom_ribbon(aes(ymin=low,ymax=high),fill="yellow",colour=NA,alpha=0.5)+
      ggtitle("Vienmaèio pasiskirstymo f-ja su 2 SE")
    print(p)
  })
  
  
  output$test <- renderTable({
    dataUni <- dataUni()
    return(dataUni[1:10,])
  })
  
  ##### UI TAB #####
  
  output$tab1 <- renderUI({
    uiOutput("tdistribution")
  })
  
  output$tdistribution <- renderUI({
    fluidPage(title = "t distribution",
              fluidRow(
                column(3,
                       actionButton("run","Run!"),
                       numericInput("n","Number of simulations:",1000),
                       numericInput("df","Degrees of freedom:",1)
                       ),
                column(9,plotOutput("uniPlot")))
              
    )
  })
  
})