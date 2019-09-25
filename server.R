
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
  
  
  output$datasource  <- renderUI({
    Tit1    <- paste("Data Sources")
    Cit1    <- paste("1. Number of Deaths, Years of Life Lost, Percent Increase, and Disparity Ratio:
                 Fusion Center analysis prepared using CDPH Vital Statistics Death Data Files;
                 2007-2017.")
    Cit1url <- tagList("www.cdph.ca.gov/Programs/CHSI/Pages/Data-and-Statistics-.aspx",
                       a("CHSI Data and Statistics",
                         href="www.cdph.ca.gov/Programs/CHSI/Pages/Data-and-Statistics-.aspx"))
    Cit2    <- paste("2. Years Lived with Disability: Institute for Health metrics and Evaluation (IHME).
                GBD Compare. Seattle, WA: IHME, University of Washington, 2015. vizhub.healthdata.
                org/gbdcompare")
    Cit2hov <- tagList("This text shows",
                       a("This is the hover tag"))
    Cit3    <- paste("3. Infectious Disease: Center for Infectious Diseases, California Department of
                 Public Health. www.cdph.ca.gov/Programs/CID/Pages/CID")
    HTML(paste(Tit1,Cit1,Cit1url,Cit2,Cit3,sep= '<br/>'))
  })
  
  # output$summary  <- renderUI({
  #   Tit1 <- paste("Summary")
  #   Cit1 <- paste("1. Number of Deaths, Years of Life Lost, Percent Increase, and Disparity Ratio:
  #                Fusion Center analysis prepared using CDPH Vital Statistics Death Data Files;
  #                2007-2017. www.cdph.ca.gov/Programs/CHSI/Pages/Data-and-Statistics-.aspx")
  #   Cit2 <- paste("2. Years Lived with Disability: Institute for Health metrics and Evaluation (IHME).
  #               GBD Compare. Seattle, WA: IHME, University of Washington, 2015. vizhub.healthdata.
  #               org/gbdcompare")
  #   Cit3 <- paste("3. Infectious Disease: Center for Infectious Diseases, California Department of
  #                Public Health. www.cdph.ca.gov/Programs/CID/Pages/CID")
  #   HTML(paste(Tit1,Cit1,Cit2,Cit3,sep= '<br/>'))
  # })
  
  shinyjs::onclick("deathTest",    showModal(modalDialog(HTML("test"),
                                                         easyClose = TRUE,
                                                         footer = modalButton("Close"))))  
  
  
  
  output$report <- downloadHandler(

    filename = paste0("County_Snapshot_Report_",Sys.Date(),".docx"),
    content = function(file) {
      tempReport <- file.path(tempdir(), "cntyReport.Rmd")
      file.copy("cntyReport.Rmd", tempReport, overwrite = TRUE)
      
      tempStyle <- file.path(tempdir(), "cntyReport_styles.docx")
      file.copy("cntyReport_styles.docx", tempStyle, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      #params <- list(c = input$county2)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,
                        #word_document(reference_docx = "cntyReport_styles.docx"),
                        output_file = file,
                        #params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

})
