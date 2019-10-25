
#https://shiny.rstudio.com/reference/shiny/0.14/fillPage.html
#https://stackoverflow.com/questions/17838709/scale-and-size-of-plot-in-rstudio-shiny

shinyUI(fluidPage(
       useShinyjs(),
        hr(),
        tabsetPanel(
            type = "tab",
            tabPanel("Cause of Death",
                     #htmlOutput("mainTitle"),
                     hr(),
                     fluidRow(column(6,selectInput("myCounty",NULL,choices = list("Select a county" = countyList),
                                                   selected = "CALIFORNIA",selectize = F)),
                              column(3),
                              column(3, downloadButton("report", "Download Report"))
                              ),
                     fluidRow(#column(4, plotOutput("DEATHS1"),offset = 2 ),
                         column(6, id = "deathTest", plotOutput("DEATHS1")),
                         column(6, plotOutput("YLL1"))),
                     fluidRow(column(6, plotOutput("CHANGE1")),
                              column(6, plotOutput("DISPARITY1")) ),
                     hr()
                     ),
            tabPanel("Non-fatal Measures",
                     # htmlOutput("mainTitle"),
                     hr(),
                     # fluidRow(
                     #     column(6,selectInput("myCounty", NULL, choices=list("Select a county"=countyList), selected = "CALIFORNIA", selectize=F )),
                     #     column(3),
                     #     column(3,downloadButton("report", "Download Report"))
                     #     ),
                     fluidRow(column(6, plotOutput("CASES1")) ),#, # ,offset = 1
                     #          column(6, plotOutput("Placeholder for Hospital Discharge")) ),
                     hr()
                     ),
            tabPanel("State Measures",
                     # htmlOutput("mainTitle"),
                     hr(),
                     fluidRow(column(6, plotOutput("RISK1")),
                              column(6, plotOutput("YLD1")) ),
                     hr()
                     )
        )

    # fluidRow(
    #     column(12,div(style="height:50px;"))
    #     ),
    # fluidRow(
    #     column(1),
    #     column(10,uiOutput("datasource")),
    #     column(1)
    # ),
    # fluidRow(
    #     column(12,div(style="height:50px;"))
    # ),
    # fluidRow(
    #     column(12,div(style="height:30px;background-color: #006f91;"))
    # ),
    # fluidRow(
    #     column(12,div(style="height:50px;"))
    # ),
    # fluidRow(
    #     column(1),
    #     column(10,uiOutput("summary")),
    #     column(1)
    # ),
    # fluidRow(
    #     column(12,div(style="height:50px;"))
    # ),
    # fluidRow(
    #     column(12,div(style="height:30px;background-color: #006f91;"))
    # )
    
))
