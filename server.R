
shinyServer(function(input, output,session) {
  
  output$DEATHS1     <- renderPlot({plotMeasures(IDnum=1,input$myCounty,input$myObserv)})
  output$YLL1        <- renderPlot({plotMeasures(IDnum=2,input$myCounty,input$myObserv)})
  output$CHANGE1     <- renderPlot({plotMeasures(IDnum=3,input$myCounty,input$myObserv)})
  output$DISPARITY1  <- renderPlot({plotMeasures(IDnum=4,input$myCounty,input$myObserv)})
  
  output$CASES1      <- renderPlot({plotMeasures(IDnum=6,input$myCounty,input$myObserv)})
  output$HOSP1       <- renderPlot({plotMeasures(IDnum=5,input$myCounty,input$myObserv)})
  
  output$YLD1        <- renderPlot({plotMeasures(IDnum=7,input$myCounty,input$myObserv)})
  output$RISK1       <- renderPlot({plotMeasures(IDnum=8,input$myCounty,input$myObserv)})
  
  
  output$mainTitle <- renderUI({h3(strong(
    HTML(paste0("Measures for ",input$myCounty," in 2017"))))})
  

  # output$datasource  <- renderUI({ tags$span(
  #   AppText(DataSourceText,1),
  #   AppText(DataSourceText,2),
  #   AppText(DataSourceText,3),
  #   AppText(DataSourceText,4))
  # })
  
  # output$summary  <- renderUI({
  #   list(
  #     AppText(SummaryText,1),
  #     AppText(SummaryText,2),
  #     AppText(SummaryText,3),
  #     AppText(SummaryText,4),
  #     AppText(SummaryText,5),
  #     AppText(SummaryText,6),
  #     AppText(SummaryText,7),
  #     AppText(SummaryText,8),
  #     AppText(SummaryText,9),
  #     AppText(SummaryText,10),
  #     AppText(SummaryText,11),
  #     AppText(SummaryText,12),
  #     AppText(SummaryText,13),
  #     AppText(SummaryText,14),
  #     AppText(SummaryText,15),
  #     AppText(SummaryText,16)
  #   )
  # })
  
  output$downloadData <- downloadHandler(
    filename = function() {paste0(input$myCounty,"_Measures_Snapshot_Report_",Sys.Date(),".docx")},
    content = function(file){
      
      flpath<-paste0(tempdir(),"/") #LIVE=tempdir TESTING=getwd
      tempStyle <- file.path(tempdir(), "County_Snapshot_Report.docx")
      file.copy("County_Snapshot_Report.docx", tempStyle, overwrite = TRUE)
      
      #Height and Width of plot (text and spacing) scaled to download document dimensions. Landscape#Portait
      h<-3.5
      #w<-6.7  #landscape
      w<-5    #portrait
      ggsave(paste0(flpath,"1.png"), plot = plotMeasures(IDnum=1,myCounty = input$myCounty,input$myObserv),
             device = "png", width = w, height = h, units = 'in')
      ggsave(paste0(flpath,"2.png"), plot = plotMeasures(IDnum=2,myCounty = input$myCounty,input$myObserv),
             device = "png", width = w, height = h, units = 'in')
      ggsave(paste0(flpath,"3.png"), plot = plotMeasures(IDnum=3,myCounty = input$myCounty,input$myObserv),
             device = "png", width = w, height = h, units = 'in')
      ggsave(paste0(flpath,"4.png"), plot = plotMeasures(IDnum=4,myCounty = input$myCounty,input$myObserv),
             device = "png", width = w, height = h, units = 'in')
      ggsave(paste0(flpath,"5.png"), plot = plotMeasures(IDnum=5,myCounty = input$myCounty,input$myObserv),
             device = "png", width = w, height = h, units = 'in')
      ggsave(paste0(flpath,"6.png"), plot = plotMeasures(IDnum=6,myCounty = input$myCounty,input$myObserv),
             device = "png", width = w, height = h, units = 'in')
      ggsave(paste0(flpath,"7.png"), plot = plotMeasures(IDnum=7,myCounty = input$myCounty,input$myObserv),
             device = "png", width = w, height = h, units = 'in')
      ggsave(paste0(flpath,"8.png"), plot = plotMeasures(IDnum=8,myCounty = input$myCounty,input$myObserv),
             device = "png", width = w, height = h, units = 'in')
      Texttest<-list(
        AppText(SummaryText,1),
        AppText(SummaryText,2))
      my_doc<-Summary_doc(input$myCounty,
                          paste0(flpath,"1.png"),
                          paste0(flpath,"2.png"),
                          paste0(flpath,"3.png"),
                          paste0(flpath,"4.png"),
                          paste0(flpath,"5.png"),
                          paste0(flpath,"6.png"),
                          paste0(flpath,"7.png"),
                          paste0(flpath,"8.png"))
      print(my_doc,target=file)
      }
    )
  }
  )