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

myPlace <- getwd()

# --Global constants and settings-----------------------------------


#myMeasure <-  "Ndeaths"
myYear    <-  2017
mySex     <-  "Total"
#myLev     <-  "lev2"


dMode <- "display"
#dMode <- "study"

if (dMode == "display") {
  SHOW_TOP <- 5  
  tSize1   <- 8
  tSize2   <- 5
  tSize3   <- 5
}

if (dMode == "study") {
  SHOW_TOP <- 15
  tSize1   <- 5
  tSize2   <- 3
  tSize3   <- 2.5
}   


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

# -- HOSPITALZATION DATA DATA -----------------------------------------------

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
                "Years Lived with Disability (State Only)",
                "Risk Factors (State Only)")

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


plotMeasures <- function(IDnum, myCounty = "Los Angeles"){ 
  
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
    geom_text(hjust=0, y=0, label=paste0(work.dat$xValues),size=work.dat$xSize1) +  # , size=xSize
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
    tPlot <- tPlot + geom_text(hjust=0, aes(x=xValues,y=plot_width*.7, label=paste0(xRaceValue)),size=work.dat$xSize3)
  }
  
  tPlot
  
}


# plotMeasures(3, myCounty = "Contra Costa") 

# --APP General Text ------------------------------------------------------
#tooltips and popovers  https://rdrr.io/cran/shinyBS/man/Tooltips_and_Popovers.html

AppText<-function(Tbl=Datasources,TblRw=1) {list(
  HTML(paste(Tbl[TblRw,1],Tbl[TblRw,2],Tbl[TblRw,3],tagList(a(Tbl[TblRw,4],href=Tbl[TblRw,5],target="_blank")) )),
  if(Tbl[TblRw,6]!=""){
    tags$span(tipify(bsButton(paste0("pB",deparse(substitute(Tbl)),TblRw),Tbl[TblRw,6],size="extra-small"),Tbl[TblRw,7],placement = "top"))
  }
)}

DataSourceText  <- read.csv(paste0(myPlace,"/Info/DataSourceText.csv"), colClasses = "character",na.strings = "NA")
SummaryText     <- read.csv(paste0(myPlace,"/Info/SummaryText.csv"),    colClasses = "character",na.strings = "NA")
