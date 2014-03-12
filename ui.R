tags$head(tags$style(type="text/css",
                     "label.radio { display: inline-block; }",
                     ".radio input[type=\"radio\"] { float: none; }"
))

shinyUI(navbarPage("Monte Carlo. t-distribution",inverse=TRUE,
                   tabPanel("Univariate",uiOutput("univariateTab")),
                   tabPanel("Bivariate",uiOutput("bivariateTab"))
))