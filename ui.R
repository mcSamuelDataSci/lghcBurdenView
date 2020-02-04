
#https://shiny.rstudio.com/reference/shiny/0.14/fillPage.html
#https://stackoverflow.com/questions/17838709/scale-and-size-of-plot-in-rstudio-shiny


shinyUI(fluidPage(
    useShinyjs(),
    # tags$style(HTML("
    # .tabbable > .nav > li > a                  {background-color: white;  color:black}
    # .tabbable > .nav > li > a[data-value='t1'] {background-color: red;   color:white}
    # .tabbable > .nav > li > a[data-value='t2'] {background-color: blue;  color:white}
    # .tabbable > .nav > li > a[data-value='t3'] {background-color: green; color:white}
    # .tabbable > .nav > li[class=active]    > a {background-color: black; color:white}
    #                 ")),
    hr(),
    fluidRow(column(3,selectInput("myCounty",NULL,choices = list("Select a county" = countyList),
                                  selected = "CALIFORNIA", selectize = F)),
             column(3,selectInput("myObserv",NULL,choices = list("How many shown?" = c(5,10,15,20)), 
                                  selected=10)),
             column(3),
             column(3, downloadButton("downloadData", "Download Report"))
             ),
    tabsetPanel(
        type = "tab",
        tabPanel("Cause of Death",
                 fluidRow(
                     tags$head(tags$style("#DEATHS1{height:40vh !important;}")),
                     tags$head(tags$style("#YLL1{height:40vh !important;}")),
                     column(6, id = "deathTest", plotOutput("DEATHS1")),
                     column(6, plotOutput("YLL1"))),
                 fluidRow(
                     tags$head(tags$style("#CHANGE1{height:40vh !important;}")),
                     tags$head(tags$style("#DISPARITY1{height:40vh !important;}")),
                     column(6, plotOutput("CHANGE1")),
                     column(6, plotOutput("DISPARITY1")) )
                 ),
        tabPanel("Non-fatal Measures",
                 tags$head(tags$style("#CASES1{height:80vh !important;}")),
                 tags$head(tags$style("#HOSP1{height:80vh !important;}")),
                 fluidRow(column(6, plotOutput("CASES1")),
                          column(6, plotOutput("HOSP1")) )
                 ),
        tabPanel("State Measures",
                 tags$head(tags$style("#YLD1{height:80vh !important;}")),
                 tags$head(tags$style("#RISK1{height:80vh !important;}")),
                 fluidRow(column(6, plotOutput("YLD1")),
                          column(6, plotOutput("RISK1")) )
                 )
        )

############## Bottom Text ################

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

) )
