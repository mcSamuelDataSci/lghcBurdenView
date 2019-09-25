library(dplyr)
#library(fs)
library(readr)
library(ggplot2)
library(shiny)
library(magrittr)

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

ccbDeaths <- filter(ccbData,year==myYear,Level=="lev2", sex==mySex) %>%
             left_join(causeNames,by="CAUSE")  %>%
             mutate(measure=Ndeaths,
                    mValues = causeName)
                    
# -- CID DATA ------------------------------------------------

cidData     <- read_csv(paste0(myPlace,"/Data/CID/dcdcData2017.csv")) 

cidData     <- filter(cidData,Year==myYear) %>%
                 mutate(county = County,
                        measure=Cases,
                        mValues = Disease)



# -- IMHE DATA -----------------------------------------------


## !! WHAT TO DO  HERE
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
PLOT_WIDTH_MULTIPLIER <- 1.2

plot_title <- c("Deaths","Cases","Years Lived with Disability","Risk for DALY")
percent_sign <- c("(N)","X","Rate","Rate")
dataSets   <- list(ccbDeaths, cidData, dat.YLD.cause,dat.DALY.risk)
ourColors <-    c("#8F98B5", "#E9A291", "#8ECAE3", "#E6C8A0")


# THIS approach does not work for now
# measures <- c("Ndeaths","junk")
# mNames   <- c("causeName")
# mutate(rankX = rank(-measures[IDnum])) %>%
  
  
# --APP Plot Function-----------------------------------------------


#plotMeasures <- function(myDataSet, IDnum, myCounty = "Los Angeles"){ 
plotMeasures <- function(IDnum, myCounty = "Los Angeles"){ 
    
  if (1==2) {
    myDataSet <- ccbDeaths
    IDnum <- 1
    myCounty <- "Contra Costa"
  }
     
   if(IDnum %in% c(1,2))  work.dat  <- filter(dataSets[[IDnum]],county==myCounty)
   if(IDnum %in% c(3,4))  work.dat  <-        dataSets[[IDnum]]                 
   
   
   work.dat <- work.dat %>%
         mutate(rankX = rank(-measure)) %>%
         filter(rankX <= SHOW_TOP)

    
 plot_width <- max(work.dat$measure)*PLOT_WIDTH_MULTIPLIER


ggplot(data=work.dat, aes(x=reorder(mValues, measure),y=measure)) + 
  coord_flip() +
  geom_bar(position="dodge", stat="identity", width=BAR_WIDTH, fill=ourColors[IDnum]) +
  geom_text(hjust=0, aes(x=mValues,y=0, label=paste0(" ", rankX, ". ", mValues))) + 
  annotate(geom="text", hjust=1, x=work.dat$mValues, y=plot_width, label=work.dat$measure) +
         theme(panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(),
         panel.background=element_blank(),
         axis.title.x=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),
         axis.title.y=element_blank(),
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank(),
         plot.title=element_text(size=16, face="bold", vjust=-4),
         plot.subtitle=element_text(size=10, face="bold", hjust=1, vjust=-2)) +
         labs(title=paste(plot_title[IDnum],"in",myCounty,"in",myYear), subtitle=percent_sign)  +
        scale_y_continuous(expand = c(0,0), limits = c(0, plot_width))
}
   
   
# plotMeasures(2, myCounty = "Alameda") 




# gold from Jonah--------------------------------------------------------------------


if (1==2) {
  
  
  VALID_YEARS <- c(1990:2017)
  DEATH_ID <- 1
  DALY_ID  <- 2
  YLD_ID   <- 3
  YLL_ID   <- 4
  
  
  percent_sign <- switch(filtered$metric[1],
                         "Number",
                         "Percent",
                         "Per 100,000")
  plot_title <- switch(measure,
                       "Deaths",
                       "Disability-Adjusted Life Years",
                       "Years Lived with Disability",
                       "Years of Life Lost")
  color <- switch(measure,
                  "#8F98B5",
                  "#E9A291",
                  "#8ECAE3",
                  "#E6C8A0")
  
  
  radioGroupButtons("display",
                    label = h4("Display:"),
                    choices = c("Cause" = "cause", "Risk" = "risk"),
                    selected = "cause",
                    justified = TRUE, status = "primary")
  
  
  radioGroupButtons("sex",
                    label = h4("Sex:"), 
                    choices = c("Male" = 1, "Female" = 2, "Both" = 3),
                    selected = 3,
                    justified = TRUE, status = "primary")
  
  radioGroupButtons("metric",
                    label = h4("Metric:"), 
                    choices = c("#" = 1, "%" = 2, "Rate" = 3),
                    selected = 3,
                    justified = TRUE, status = "primary")
}
