tags$head(tags$style(type="text/css",
                     "label.radio { display: inline-block; }",
                     ".radio input[type=\"radio\"] { float: none; }"
))

shinyUI(navbarPage("Monte Carlo Simulations",
                   tabPanel("t distribution",uiOutput("tab1"))
))