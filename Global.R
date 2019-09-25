library(dplyr)
#library(fs)
library(readr)
library(ggplot2)
library(shiny)
library(magrittr)
library(tidyr)
library(plotly)
library(shinyjs)

myPlace <- getwd()

# --CCB DEATH DATA ------------------------------------------------

ccbData     <- readRDS(paste0(myPlace,"/Data/CCB/datCounty.RDS")) 

#myMeasure <-  "Ndeaths"
myYear    <-  2017
mySex     <-  "Total"
myLev     <-  "lev2"
myN       <-  10




causeNames  <- read_csv("Info/causeNames.csv")  %>%
                   mutate(causeName=ifelse(CAUSE=="D05","Alzheimers",causeName))

ccb         <- filter(ccbData,year==myYear,Level=="lev2", sex==mySex) %>%
               left_join(causeNames,by="CAUSE") 

ccbDeaths   <- ccb %>%
                mutate(measure=Ndeaths,
                       mValues = causeName)

ccbYLL      <- ccb %>%
               mutate(measure= YLLper,
               mValues = causeName)


ccbChange      <- filter(ccbData,year %in% c(2007,2017), Level=="lev2", sex==mySex) %>% 
                select(county,year,CAUSE,aRate) %>%
                spread(key=year,value=aRate) 
names(ccbChange)      <- c(names(ccbChange)[1:2],     paste0("rate",names(ccbChange)[3:4]))
ccbChange      <- ccbChange %>%
                  mutate(change = round(100*(rate2017-rate2007)/rate2007,1)) %>%
                  filter(CAUSE != "Z01" ) %>%  #   Symptoms, signs and ill-defined conditions, not elsewhere classified
                  left_join(causeNames,by="CAUSE") %>%
                  mutate(measure=change,
                         mValues = causeName)


# --CCB RACE DATA ---------------------------------------------------


ccbRace     <- read_csv(paste0(myPlace,"/Data/CCB/raceDisparity.csv")) %>%
                 mutate(measure=round(rateRatio,1),
                        mValues = causeName)

# -- CID DATA ------------------------------------------------

cidData     <- read_csv(paste0(myPlace,"/Data/CID/dcdcData2017.csv")) 

cidData     <- filter(cidData,Year==myYear) %>%
                 mutate(county = County,
                        measure=Cases,
                        mValues = Disease)

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

SHOW_TOP <- 15
BAR_WIDTH <- 0.9
PLOT_WIDTH_MULTIPLIER <- 1.0

plot_title <- c("Deaths",
                "Years of Life Lost",
                "Increase in Deaths",
                "Race Disparity in Deaths",
                "Reporable Disease Cases",
                "Years Lived with Disability",
                "Risk Factors")

metric <-     c("Number",
                "Rate",
                "Percent",
                "Rate Ratio",
                "Number",
                "Rate",
                "Rate")

dataSets   <- list(ccbDeaths, ccbYLL,ccbChange, ccbRace,cidData, dat.YLD.cause,dat.DALY.risk)
ourColors <-    c("#8F98B5", "#E9A291", "#E9A291","#8ECAE3", "#E6C8A0","#8F98B5","#E9A291")



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

ourColors <- a3[c(5,6,7,8,9,11,12)]



# THIS approach does not work for now
# measures <- c("Ndeaths","junk")
# mNames   <- c("causeName")
# mutate(rankX = rank(-measures[IDnum])) %>%
  
  
# --APP Plot Function-----------------------------------------------


#plotMeasures <- function(myDataSet, IDnum, myCounty = "Los Angeles"){ 
plotMeasures <- function(IDnum, myCounty = "Los Angeles"){ 
    
  if (1==2) {
    myDataSet <- ccbRace
    IDnum <- 3
    myCounty <- "Contra Costa"
  }
     
   if(IDnum %in% 1:5)  work.dat  <- filter(dataSets[[IDnum]],county==myCounty)
   if(IDnum %in% 6:7)  work.dat  <-        dataSets[[IDnum]]                 
   
   
   work.dat <- work.dat %>%
         mutate(rankX = rank(-measure),
                xValues = paste(rankX,mValues)) %>%
         filter(rankX <= SHOW_TOP)

   
   if (IDnum == 4) work.dat <- mutate(work.dat,xValues=paste0(xValues,"      (",raceCode,":",lowRace,")"))
   if (IDnum == 4) myYear <- "2016-2018"
   if (IDnum == 3) myYear <- "2007 to 2017"
   
   
   
    
 plot_width <- max(work.dat$measure)*PLOT_WIDTH_MULTIPLIER


 
ggplot(data=work.dat, aes(x=reorder(xValues, measure),y=measure)) +
coord_flip() +
  geom_bar(position="dodge", stat="identity", width=BAR_WIDTH, fill=ourColors[IDnum]) +
  geom_text(hjust=0, aes(x=xValues,y=0, label=paste0(xValues))) + 
  annotate(geom="text", hjust=1, x=work.dat$xValues, y=plot_width, label=work.dat$measure) +
         theme(panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(),
         panel.background=element_blank(),
         axis.title.x=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),
         axis.title.y=element_blank(),
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank(),
        # panel.border = element_rect(colour = "gray", fill=NA, size=1),
         plot.title=element_text(size=16, face="bold", vjust=-4),
         plot.subtitle=element_text(size=10, face="bold", hjust=1, vjust=-2)
         ) +
         labs(title=paste(plot_title[IDnum]), subtitle=metric[IDnum])  +
        scale_y_continuous(expand = c(0,0), limits = c(0, plot_width))



}
   
   
# plotMeasures(3, myCounty = "Contra Costa") 


