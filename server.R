
shinyServer(function(input, output,session) {
  

  output$DEATHS1     <- renderPlot({plotMeasures(IDnum=1,input$myCounty)})
  output$YLL1        <- renderPlot({plotMeasures(IDnum=2,input$myCounty)})
  output$CHANGE1     <- renderPlot({plotMeasures(IDnum=3,input$myCounty)})
  output$DISPARITY1  <- renderPlot({plotMeasures(IDnum=4,input$myCounty)})
  
  output$CASES1      <- renderPlot({plotMeasures(IDnum=5,input$myCounty)})

  output$YLD1        <- renderPlot({plotMeasures(IDnum=6,input$myCounty)})
  output$RISK1       <- renderPlot({plotMeasures(IDnum=7,input$myCounty)})
  
  
  output$mainTitle <- renderUI({h3(strong(
    HTML(paste0("Measures for ",input$myCounty," in 2017"))))})
  
  
  output$datasource  <- renderUI({ tags$span(
    AppText(DataSourceText,1),
    AppText(DataSourceText,2),
    AppText(DataSourceText,3),
    AppText(DataSourceText,4))
  })
  
  output$summary  <- renderUI({
    list(
      AppText(SummaryText,1),
      AppText(SummaryText,2),
      AppText(SummaryText,3),
      AppText(SummaryText,4),
      AppText(SummaryText,5),
      AppText(SummaryText,6),
      AppText(SummaryText,7),
      AppText(SummaryText,8),
      AppText(SummaryText,9),
      AppText(SummaryText,10),
      AppText(SummaryText,11),
      AppText(SummaryText,12),
      AppText(SummaryText,13),
      AppText(SummaryText,14),
      AppText(SummaryText,15),
      AppText(SummaryText,16)
    )
  })
  
  shinyjs::onclick("deathTest",    showModal(modalDialog(HTML("test"),
                                                         easyClose = TRUE,
                                                         footer = modalButton("Close"))))  
  
  
  
  output$report <- downloadHandler(
    
    filename = paste0("County_Snapshot_Report_",Sys.Date(),".docx"),
    content = function(file) {
      tempReport <- file.path(getwd(), "cntyReport.Rmd")
      #file.copy("cntyReport.Rmd", tempReport, overwrite = TRUE)
      
      tempStyle <- file.path(getwd(), "cntyReport_styles.docx")
      #file.copy("cntyReport_styles.docx", tempStyle, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      #params <- list(c = input$county2)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,
                        word_document(reference_docx = "cntyReport_styles.docx"),
                        output_file = filename,
                        #params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
})


#rmarkdown::render(tempReport,
#                  envir = new.env(parent = globalenv()) )
