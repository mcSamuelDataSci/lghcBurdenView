
#https://shiny.rstudio.com/reference/shiny/0.14/fillPage.html
#https://stackoverflow.com/questions/17838709/scale-and-size-of-plot-in-rstudio-shiny

shinyUI(fluidPage(

    hr(),
    
    fluidRow(
        column(6,
               # selectInput("myCounty", NULL, choices=countyList, selected = "CALIFORNIA", selectize=F )
                selectInput("myCounty", NULL, choices=list("Select a county"=countyList), selected = "CALIFORNIA", selectize=F )
               #selectInput("myCounty", NULL, choices=c("Select a County"="",countyList), selectize=F)
               # selectizeInput("myCounty", NULL, choices=countyList, selected = "CALIFORNIA",options=list(placeholder='select a county'))
               ),
        column(3),
        column(3,
               downloadButton("report", "Download Report")
        )
        ),
    htmlOutput("mainTitle"),
    fluidRow(
        #column(4, plotOutput("DEATHS1"),offset = 2 ),
        column(3, id="deathTest",plotOutput("DEATHS1")),
        column(3, plotOutput("YLL1")),
        column(3, plotOutput("CHANGE1")),
        column(3, plotOutput("DISPARITY1"))
    ) ,
    
    # fluidRow(
    #     #column(4, plotOutput("DEATHS1"),offset = 2 ),
    #     column(6, plotOutput("DEATHS1")),
    #     column(6, plotOutput("YLL1"))
    # ),
    # fluidRow(
    #     column(6, plotOutput("CHANGE1")),
    #     column(6, plotOutput("DISPARITY1"))
    # ) ,
    
    fluidRow(
           column(3, plotOutput("CASES1"),offset = 1),
           column(3, plotOutput("YLD1")),
           column(3, plotOutput("RISK1"))  
    ),
    
    fluidRow(
        column(12,div(style="height:50px;"))
    ),
    fluidRow(
        column(12,htmlOutput("datasource"))
    ),
    fluidRow(
        column(12,div(style="height:50px;"))
    ),
    fluidRow(
        column(12,div(style="height:30px;background-color: #006f91;"))
    ),
    fluidRow(
        column(12,div(style="height:100px;"))
    ),
    # fluidRow(
    #     column(1),
    #     column(10,textOutput("summary")),
    #     column(1)
    # ),
    fluidRow(
        column(12,div(style="height:50px;"))
    ),
    fluidRow(
        column(12,div(style="height:30px;background-color: #006f91;"))
    ),
    
    shinyjs::useShinyjs()
    )
)    

