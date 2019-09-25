
shinyServer(function(input, output) {
  
  output$DEATHS1 <- renderPlot({plotMeasures(IDnum=1,input$myCounty)})
  output$CASES1  <- renderPlot({plotMeasures(IDnum=2,input$myCounty)})
  output$YLD1    <- renderPlot({plotMeasures(IDnum=3,input$myCounty)})
  output$RISK1   <- renderPlot({plotMeasures(IDnum=4,input$myCounty)})
  
  
output$Coreport <- downloadHandler(
    
    # For PDF output, change this to "report.pdf"
    filename = paste0("County_Snapshot_Report_",Sys.Date(),".docx"),
    content = function(file) {
      directory <- dirname(rstudioapi::getSourceEditorContext()$path)
      
      tempReport <- file.path(tempdir(), "cntyReport.Rmd")
      file.copy(file.path(directory,"cntyReport.Rmd"), tempReport, overwrite = TRUE)

      tempStyle <- file.path(tempdir(), "cntyReport_styles.docx")
      file.copy(file.path(directory,"cntyReport_styles.docx"), tempStyle, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(c = isolate(input$myCounty))

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,
                        word_document(reference_docx = tempStyle),
                        output_file = filename,
                        output_dir = "Z:\\lghcBurdenView\\New folder",    #"H:\\Ben's Work file",
                        params = params,
                        envir = new.env(parent = globalenv())
                        )
    }
  )

  output$CAreport <- downloadHandler(
    
    filename = paste0("State_Snapshot_Report_",Sys.Date(),".docx"),
    content = function(file) {
      tempReport <- file.path(tempdir(), "caReport.Rmd")
      file.copy("caReport.Rmd", tempReport, overwrite = TRUE)
      
      tempStyle <- file.path(tempdir(), "cntyReport_styles.docx")
      file.copy("cntyReport_styles.docx", tempStyle, overwrite = TRUE)
      
      rmarkdown::render(tempReport,
                        word_document(reference_docx = "cntyReport_styles.docx"),
                        output_file = file,
                        #params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
})
