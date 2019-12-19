library(dplyr)
#library(fs)
library(readr)
library(ggplot2)
library(shiny)
library(shinyBS)
library(magrittr)
library(tidyr)
library(plotly)
library(shinyjs)
library(officer) # writing document

myPlace <- getwd()


# --Global constants and settings-----------------------------------


#myMeasure <-  "Ndeaths"
myYear    <-  2017
mySex     <-  "Total"
#myLev     <-  "lev2"


# --CCB DEATH DATA ------------------------------------------------


ccbData     <- readRDS(paste0(myPlace,"/Data/CCB/datCounty.RDS")) %>%
  filter(                                       Level %in% c("lev2") )
#   filter(!(CAUSE %in% c("A02","D04","E03") ) & Level %in% c("lev2","lev3") )


causeNames  <- read_csv("Info/causeNames.csv")  %>%
  mutate(causeName = ifelse(CAUSE=="D05","Alzheimers",causeName),
         causeName = ifelse(CAUSE=="Z01","Ill-Defined",causeName))


ccb         <- filter(ccbData,year==myYear,sex==mySex) %>%
  left_join(causeNames,by="CAUSE") 



ccbDeaths   <- ccb %>%
  mutate(measure=Ndeaths,
         mValues = causeName)

ccbYLL      <- ccb %>%
  mutate(measure= YLLper,
         mValues = causeName)


ccbChange      <- filter(ccbData,year %in% c(2007,2017), sex==mySex) %>% 
  select(county,year,CAUSE,aRate) %>%
  spread(key=year,value=aRate) 
names(ccbChange)      <- c(names(ccbChange)[1:2],     paste0("rate",names(ccbChange)[3:4]))
ccbChange      <- ccbChange %>%
  mutate(change = round(100*(rate2017-rate2007)/rate2007,1)) %>%
  filter(CAUSE != "Z01" ) %>%  #   Symptoms, signs and ill-defined conditions, not elsewhere classified
  left_join(causeNames,by="CAUSE") %>%
  mutate(measure=change,
         mValues = causeName) %>%
  mutate(mValues = ifelse(measure < 0,NA,mValues))


# --CCB RACE DATA ---------------------------------------------------


ccbRace     <- read_csv(paste0(myPlace,"/Data/CCB/raceDisparity.csv")) %>%
  mutate(causeName = ifelse(CAUSE=="Z01","Ill-Defined",causeName)) %>%
  mutate(measure=round(rateRatio,1),
         mValues = causeName)


# -- CID DATA ------------------------------------------------


cidData     <- read_csv(paste0(myPlace,"/Data/CID/dcdcData2017.csv")) 

cidData     <- filter(cidData,Year==myYear) %>%
  mutate(county = County,
         measure=Cases,
         mValues = Disease)


# -- HOSPITALZATION DATA -----------------------------------------------


hospData <- read_csv(paste0(myPlace,"/Data/OSHPD/Hospital_Discharge_CCS_grouping_2016.csv"))  %>%
  mutate(county = countyName,
         measure=nHosp,
         mValues = ccsName)


# -- IMHE DATA -----------------------------------------------


myLevel <- c(2,3)

dataIHME     <- readRDS(paste0(myPlace,"/Data/IHME/v2IHME.RDS"))

dat.YLD.cause <- dataIHME %>%  filter(measure_id ==  3,    #YLD  
                                      year_id    == 2017,
                                      display    == "cause",
                                      level      %in% myLevel,
                                      sex_id     == 3,   # Both
                                      metric_id  == 3)  %>%    # Rate 
  mutate(measure = val,
         mValues = id_name)



dat.DALY.risk <- dataIHME %>%  filter(measure_id ==  2,    #YLD  
                                      year_id    == 2017,
                                      display    == "risk",
                                      level      %in% myLevel,
                                      sex_id     == 3,   # Both
                                      metric_id  == 3)  %>%  # Rate
  mutate(measure = val,
         mValues = id_name)


# --APP Constants ------------------------------------------------------


countyList  <- sort(as.character(unique(ccbData$county)))

BAR_WIDTH <-  0.9
PLOT_WIDTH_MULTIPLIER <- 1.0

plot_title <- c("Deaths",
                "Years of Life Lost",
                "Increase in Deaths",
                "Race Disparity in Deaths",
                "Number of Hospitalizations",
                "Reportable Disease Cases",
                "Years Lived with Disability",
                "Risk Factors")

metric <-     c("Number",
                "Rate",
                "Percent",
                "Rate Ratio",
                "Number",
                "Number",
                "Rate",
                "Rate")

dataSets   <- list(ccbDeaths, ccbYLL,ccbChange, ccbRace,hospData,cidData, dat.YLD.cause,dat.DALY.risk)
ourColors <-    c("#8F98B5", "#E9A291", "#E9A291","#8ECAE3", "#E6C8A0","#8F98B5","#E9A291","#8F98B5")



# https://stackoverflow.com/questions/50600425/r-convert-colors-to-pastel-colors
a <-c("red","red1","red2","red3","grey","darkgreen","skyblue","blue","magenta","magenta4","yellow","orange","pink","pink","black")
# transform to rgb
a1 <- col2rgb(a)
# transform to HSV space
a2 <- rgb2hsv(a1)
# calculate hue for HCl
hue <- a2["h",]*360
# create color with suitable chroma and luminance to get pastel color
a3 <- hcl(hue, 35, 85)

#barplot(seq_along(a), col=a3, main="Pastel_hcl")

ourColors <- a3[c(5,6,7,8,9,11,12,15)]

# --APP Plot Function-----------------------------------------------

lblwrap <- function (x,L) { # x=object, L=desired character length
  sapply(lapply(x, strwrap, L),paste, collapse = "\n")
}

plotMeasures <- function(IDnum, myCounty = "Los Angeles",myObserv = 10){ 
  
 #  if (dMode == "display") { 
 #    SHOW_TOP <- 5   
 #    tSize1   <- 8 
 #    tSize2   <- 5 
 #    tSize3   <- 5 
 #    } 
 # if (dMode == "study") { 
 #    SHOW_TOP <- 15 
 #    tSize1   <- 5 
 #    tSize2   <- 3 
 #    tSize3   <- 2.5 
 #    }    

    SHOW_TOP <- myObserv  
    tSize1   <- round((myObserv/-10)+6.5,1)#round((myObserv/-20)+4.75)
    tSize2   <- 4#round((myObserv/-12)+4.75)
    tSize3   <- 3#round((myObserv/-12.5)+4.75)

  if (1==2) {
    myDataSet <- cidData
    IDnum <- 5
    myCounty <- "Humboldt"
  }
  
  if(IDnum %in% 1:6)  work.dat  <- filter(dataSets[[IDnum]],county==myCounty)
  if(IDnum %in% 7:8)  work.dat  <-        dataSets[[IDnum]]                 
  
  
  test <- data.frame(xrow=1:SHOW_TOP)
  
  
  work.dat <- work.dat %>%
    mutate(rankX = rank(-measure)) %>%
    filter(rankX <= SHOW_TOP)   %>%
    arrange(rankX) %>%
    mutate(xrow = row_number()  ) %>%
    full_join(test,by="xrow")    %>%
    mutate(xValues = ifelse(is.na(mValues),xrow,paste(xrow,mValues)))  %>%
    mutate(xSize1 =ifelse(is.na(mValues),0.01,tSize1),   #5
           xSize2 =ifelse(is.na(mValues),0.01,tSize2),   #3
           xSize3 =ifelse(is.na(mValues),0.01,tSize3),   #2.5
    )  %>%
    mutate(measure=ifelse(is.na(mValues),0,measure))  %>%
    
    arrange(xrow)
  
  
  
  # if (IDnum == 4) work.dat <- mutate(work.dat,xValues=paste0(xValues,"      (",raceCode,":",lowRace,")"))
  if (IDnum == 4) work.dat <- mutate(work.dat,xRaceValue=paste0("(",raceCode,":",lowRace,")"))
  if (IDnum == 4) myYear <- "2016-2018"
  if (IDnum == 3) myYear <- "2007 to 2017"
  
  
  
  
  plot_width <- max(work.dat$measure)*PLOT_WIDTH_MULTIPLIER
  
  
  tPlot <-  
    ggplot(data=work.dat, aes(x=reorder(xValues, -xrow),y=measure)) +
    coord_flip() +
    geom_bar(position="dodge", stat="identity", width=BAR_WIDTH, fill=ourColors[IDnum]) +
    geom_text(hjust=0, y=0, label=lblwrap(paste0(work.dat$xValues),ifelse(SHOW_TOP<13,38,48) ),
              size=work.dat$xSize1, lineheight = 0.7) +  # , size=xSize
    annotate(geom="text", hjust=1, x=work.dat$xValues, y=plot_width, label=work.dat$measure,size=work.dat$xSize2) +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.background=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position="none",
          # panel.border = element_rect(colour = "gray", fill=NA, size=1),
          plot.title=element_text(size=20, face="bold", vjust=-4),                 # size units?
          plot.subtitle=element_text(size=16, face="bold", hjust=1, vjust=-2)
    ) +
    labs(title=paste(plot_title[IDnum]), subtitle=metric[IDnum])  +
    scale_y_continuous(expand = c(0,0), limits = c(0, plot_width))
  
  
  
  if (IDnum == 4) {
    tPlot <- tPlot + geom_text(hjust=0, aes(x=xValues,y=plot_width*.72, label=paste0(xRaceValue)),size=work.dat$xSize3)
  }
  
  tPlot + theme(plot.margin = margin(0,0,0,0,"cm"))
  
}

#plotMeasures(3, myCounty = "Contra Costa",8)
#p<-plotMeasures(1, myCounty = "Contra Costa","study")
#q<-plotMeasures(3, myCounty = "Contra Costa","study")
#grid.arrange(grobs = list(p,q),nrow=1,widths =c(5,0.1,5), layout_matrix = rbind(c(1,NA,2)) )


# --APP General Text ------------------------------------------------------
#tooltips and popovers  https://rdrr.io/cran/shinyBS/man/Tooltips_and_Popovers.html

capwords <- function(x) {
  s <- tolower(x) 
  s <- strsplit(s," ")[[1]]
  paste0(toupper(substring(s, 1,1)), substring(s, 2), collapse=" ")
}

AppText<-function(Tbl=Datasources,TblRw=1) {list(
  HTML(paste(Tbl[TblRw,1],Tbl[TblRw,2],Tbl[TblRw,3],tagList(a(Tbl[TblRw,4],href=Tbl[TblRw,5],target="_blank")) )),
  if(Tbl[TblRw,6]!=""){
    tags$span(tipify(bsButton(paste0("pB",deparse(substitute(Tbl)),TblRw),Tbl[TblRw,6],size="extra-small"),Tbl[TblRw,7],placement = "top"))
  }
)}
 
 DataSourceText  <- read.csv(paste0(myPlace,"/Info/DataSourceText.csv"), colClasses = "character",na.strings = "NA")
 SummaryText     <- read.csv(paste0(myPlace,"/Info/SummaryText.csv"),    colClasses = "character",na.strings = "NA")
 MeasureText     <- read.csv(paste0(myPlace,"/Info/MeasureText.csv"),    colClasses = "character",na.strings = "NA")

# --Download ---------------------------
#https://cran.r-project.org/web/packages/officer/officer.pdf
#https://davidgohel.github.io/officer/reference/index.html#section-replace-content-in-word-documents

#stylesdoc<-read_docx(paste0(getwd(),"/County_Snapshot_Report.docx"))  
#styles_info(stylesdoc) #can be modified in word document template. Must keep name.

Summary_doc <- function (Title,Figure1,Figure2,Figure3,Figure4,Figure5,Figure6,Figure7,Figure8) {
  read_docx(paste0(tempdir(),"/County_Snapshot_Report.docx"))  %>%
# page 1  
    cursor_reach("WordSummaryText1") %>%
  body_add_par(value = paste0(MeasureText$GeneralText[1]), style="Subtitle",pos="on") %>%
    cursor_reach("WordTitle1") %>%
  body_add_par(value = paste0(capwords(Title)," Cause of Death Measures"), style = "heading 1", pos="on") %>%
    cursor_reach("WordFigure1") %>%
  body_add_img(src = Figure1, width = 3.2, height = 2 , style = "Normal", pos="on") %>%
    cursor_reach("WordFigure2") %>%
  body_add_img(src = Figure2, width = 3.2, height = 2 , style = "Normal", pos="on") %>%
    cursor_reach("WordFigure3") %>%
  body_add_img(src = Figure3, width = 3.2, height = 2 , style = "Normal", pos="on") %>%
    cursor_reach("WordFigure4") %>%
  body_add_img(src = Figure4, width = 3.2, height = 2 , style = "Normal", pos="on") %>%
    cursor_reach("WordTitle2") %>%
  body_add_par(value = paste0(capwords(Title)," Quality of Life Measures"), style = "heading 1", pos="on") %>%
    cursor_reach("WordFigure5") %>%
  body_add_img(src = Figure5, width = 3.2, height = 2 , style = "Normal", pos="on") %>%
    cursor_reach("WordFigure6") %>%
  body_add_img(src = Figure6, width = 3.2, height = 2 , style = "Normal", pos="on") %>%
# page 2
    cursor_reach("WordTitle3") %>%
  body_add_par(value = "State Quality of Life Measures", style = "heading 1", pos="on") %>%
    cursor_reach("WordFigure7") %>%
  body_add_img(src = Figure7, width = 3.2, height = 2 , style = "Normal", pos="on") %>%
    cursor_reach("WordFigure8") %>%
  body_add_img(src = Figure8, width = 3.2, height = 2 , style = "Normal", pos="on") %>%
  #text (temporary) %>%
    cursor_reach("WordSources1") %>%
  body_add_par(value = paste0(DataSourceText$GeneralText[1]), style="heading 1",pos="on") %>%
    cursor_reach("WordSourcesText1") %>%
  body_add_par(value = paste(DataSourceText$GeneralText[2],DataSourceText$LinkText[2]), style="Compact",pos="on") %>%
  body_add_par(value = paste(DataSourceText$GeneralText[3],DataSourceText$LinkText[3]), style="Compact") %>%
  body_add_par(value = paste(DataSourceText$GeneralText[4],DataSourceText$LinkText[4]), style="Compact") %>%
# page 3    
    cursor_reach("WordSummary2") %>%
  body_add_par(value = paste0(MeasureText$GeneralText[2]), style="heading 1",pos="on") %>%
    cursor_reach("WordSummaryText2") %>%
  body_add_par(value = paste0(MeasureText$GeneralText[3]), style="heading 2",pos="on") %>%
    body_add_par(value = paste0(MeasureText$GeneralText[4]), style="Compact") %>%
  body_add_par(value = paste0(MeasureText$GeneralText[5]), style="heading 2") %>%
    body_add_par(value = paste0(MeasureText$GeneralText[6]), style="Compact") %>%
  body_add_par(value = paste0(MeasureText$GeneralText[7]), style="heading 2") %>%
    body_add_par(value = paste0(MeasureText$GeneralText[8]), style="Compact") %>%
  body_add_par(value = paste0(MeasureText$GeneralText[9]), style="heading 2") %>%
    body_add_par(value = paste0(MeasureText$GeneralText[10]), style="Compact") %>%
  body_add_par(value = paste0(MeasureText$GeneralText[11]), style="heading 2") %>%
    body_add_par(value = paste0(MeasureText$GeneralText[12]), style="Compact") %>%
  body_add_par(value = paste0(MeasureText$GeneralText[13]), style="heading 2") %>%
    body_add_par(value = paste0(MeasureText$GeneralText[14]), style="Compact") %>%
  body_add_par(value = paste0(MeasureText$GeneralText[15]), style="heading 2") %>%
    body_add_par(value = paste0(MeasureText$GeneralText[16]), style="Compact") %>%
  body_add_par(value = paste0(MeasureText$GeneralText[17]), style="heading 2") %>%
    body_add_par(value = paste0(MeasureText$GeneralText[18]), style="Compact")
  }

 