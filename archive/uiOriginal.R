
shinyUI(fluidPage(

    hr(),
    
    fluidRow(
        column(6,
               # selectInput("myCounty", NULL, choices=countyList, selected = "CALIFORNIA", selectize=F )
                selectInput("myCounty", NULL, choices=list("Select a county"=countyList), selected = "CALIFORNIA", selectize=F )
               #selectInput("myCounty", NULL, choices=c("Select a County"="",countyList), selectize=F)
               # selectizeInput("myCounty", NULL, choices=countyList, selected = "CALIFORNIA",options=list(placeholder='select a county'))
               ),
        column(3,
               downloadButton("CAreport", "Download State Report", class = "butt")
               ),
        column(3,
               downloadButton("Coreport", "Download County Report", class = "butt")
        )
        ),
    fluidRow(
        column(6, plotOutput("DEATHS1")
        ),
        column(6, plotOutput("CASES1")     
        )
    )
    )
    )

