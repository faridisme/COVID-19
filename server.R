#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#setwd("C:/Users/Olivia Oh/Documents/Farid/COVID-19CumulativeCurves")
library(shiny)

library(utils)
library(httr)
#library(xlsx)
library(ggplot2)
#install.packages("tidyr")
library(tidyr)
#install.packages("gdata")
library(gdata)
#install.packages("naniar")
library(naniar)
library(scales)
library(ggrepel)
library(dplyr)
#install.packages("directlabels")
library(directlabels)

#DATADIR <- "data/";
#DATAFILE <- "COVID-19-geographic-disbtribution-worldwide.xlsx";

#### Data preparation ####

# Read ECDC data
#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))

#read the Dataset sheet into "R". The dataset will be called "data".
df <- read.csv(tf, stringsAsFactors=FALSE)

# read in HK data from file separately
df_HK <- read.csv("HK.csv", sep=",",header=TRUE,comment.char="", stringsAsFactors=FALSE)

df <- rbind(df,df_HK)

# URL <- paste0("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx");#"https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",Sys.Date()-1,".xlsx")
# download.file(URL, destfile=paste0(DATADIR,DATAFILE), mode="wb")
# 
# df <- read.xlsx(paste0(DATADIR,DATAFILE), sheetIndex = "COVID-19-geographic-disbtributi")
df$dateRep <- as.Date(df[,1],"%d/%m/%Y")

message(paste0("Data source: ECDC (as of ",format(max(df$dateRep),"%d %B %Y"),")"))

#convert long to wide
# Cases
dfcases <- spread(df[,c("dateRep","countriesAndTerritories","cases")],dateRep,cases);
dfcases[is.na(dfcases)] <- 0;

# Convert daily numbers to cumulative numbers
for (i in 3:dim(dfcases)[2]){
  dfcases[,i] <- dfcases[,i-1]+ dfcases[,i];
}
dfCases <-  gather(dfcases,"dateRep", "Cases",2:dim(dfcases)[2]);
dfCases$dateRep <- as.Date(dfCases$dateRep)
dfCases <- dfCases %>% mutate(label = if_else(dateRep == max(dateRep),
                                              as.character(countriesAndTerritories), ""))

clist1<- sort(unique(dfCases$countriesAndTerritories[dfCases$Cases>=100]))

casesthresh1 <- 100;

# Deaths
dfdeaths <- spread(df[,c("dateRep","countriesAndTerritories","deaths")],dateRep,deaths);
dfdeaths[is.na(dfdeaths)] <- 0;

# Convert daily numbers to cumulative numbers
for (i in 3:dim(dfdeaths)[2]){
  dfdeaths[,i] <- dfdeaths[,i-1]+ dfdeaths[,i];
}
dfDeaths <-  gather(dfdeaths,"dateRep", "Deaths",2:dim(dfdeaths)[2]);
dfDeaths$dateRep <- as.Date(dfDeaths$dateRep)
dfDeaths <- dfDeaths %>% mutate(label = if_else(dateRep == max(dateRep),
                                              as.character(countriesAndTerritories), ""))

deathsthresh1 <- 5;


# Define server logic required to draw figure
shinyServer(function(input, output) {
  
  #casesthresh1 <- input$casesthresh
  
  #dfcases1 <- reactive({})
  
  #d1 <- reactive ({})
  
  clist <- reactive({
    if(any(input$clist=="Whole World")){
      clist <- clist1
    } else {
      input$clist
    }
  })
  
  
  #### Plots for Cases ####
  
  output$PlotCaseLinear <- renderPlot({
    #Plot cumulative curves;
    
    
    dfCases <- dfCases[dfCases$countriesAndTerritories %in% clist() & dfCases$Cases>0,]
    ggplot(data=dfCases,
           aes(x=dateRep, y=Cases, group=countriesAndTerritories, colour=countriesAndTerritories)) +
      scale_x_date(name="Date",labels = date_format("%d %b %Y"),expand = c(0, 0))+
      scale_y_continuous(name="Cumulative number of cases", labels=comma,
                         limits = c(0,max(dfCases$Cases)))+
      geom_line(lwd=1, alpha=0.6)+
      geom_point(alpha=0.6)+
      geom_text_repel(aes(label = label),
                      alpha = 1,
                      nudge_x = 1)+
      theme_bw()+theme_classic()+theme(legend.position = "none")
  })
  
  output$PlotCaseLog <- renderPlot({
    #Plot cumulative curves;
    

    dfCases <- dfCases[dfCases$countriesAndTerritories %in% clist() & dfCases$Cases>0,]
    ggplot(data=dfCases,
           aes(x=dateRep, y=Cases, group=countriesAndTerritories, colour=countriesAndTerritories)) +
      scale_x_date(name="Date",labels = date_format("%d %b %Y"),expand = c(0, 0))+
      scale_y_continuous(name="Cumulative number of cases", labels=comma,trans='log10',
                         limits = c(1,max(dfCases$Cases)))+
      annotation_logticks(sides = "l")+
      geom_line(lwd=1, alpha=0.6)+
      geom_point(alpha=0.6)+
      geom_text_repel(aes(label = label),
                      alpha = 1,
                      nudge_x = 1)+
      theme_bw()+theme_classic()+theme(legend.position = "none")
  })

  output$PlotCaseTraj <- renderPlot({
    
    dfcases0 <- dfcases[,c(2:dim(dfcases)[2])] %>% replace_with_na_all(condition = ~.x <= 100)
    dfcases0 <- apply(dfcases0, 1, sort); #extract only non-NA values;
    dfcases0 <- t(sapply(dfcases0, '[', seq(max(sapply(dfcases0, length))))); #put values in matrix;
    #adjust starting number downward to 100 if exceed 100;
    for (i in 1:dim(dfcases0)[1]){
      if(all(is.na(dfcases0[i,]))){next}
      dfcases0[i,] <- dfcases0[i,]-min(dfcases0[i,],na.rm=TRUE)+casesthresh1;
    }
    #make dataframe join with countries label;
    dfcases0 <- data.frame(dfcases$countriesAndTerritories,dfcases0)
    names(dfcases0) <- c("CountriesAndTerritories",paste("Day",1:(dim(dfcases0)[2]-1)))
    
    dfcases1 <- gather(dfcases0,"Day", "Cases",2:dim(dfcases0)[2]);
    dfcases1$Day <- as.numeric(gsub("Day ","",dfcases1$Day))
    dfcases1 <- dfcases1[!is.na(dfcases1$Cases),]
    
    #Add label for last Day;
    temp <- dfcases1 %>% group_by(CountriesAndTerritories) %>%
      summarise(maxDay = max(Day))
    dfcases1 <- merge(dfcases1,temp,by="CountriesAndTerritories", all.x=TRUE)
    dfcases1 <- dfcases1 %>% mutate(label = if_else(Day == maxDay,
                                                    as.character(CountriesAndTerritories), ""))
    dfcases1$maxDay <- NULL
    #casesthresh1 <<- input$casesthresh;
    df_temp <- dfcases1#()
    maxDay <- max(df_temp$Day,na.rm=TRUE)
    
    #Plot cumulative cases after threshold case;
    d1 <- data.frame(Day=1:maxDay, casesthresh=casesthresh1);
    d1$double1 <- d1$casesthresh*2^(d1$Day-1);
    d1$double2 <- d1$casesthresh*2^((1/2)*(d1$Day-1));
    d1$double3 <- d1$casesthresh*2^((1/3)*(d1$Day-1));
    d1$double7 <- d1$casesthresh*2^((1/7)*(d1$Day-1));
    d1$double14 <- d1$casesthresh*2^((1/14)*(d1$Day-1));
    d1$casesthresh<- NULL;
    d1 <- gather(d1,"CountriesAndTerritories","Cases",2:dim(d1)[2])
    d1$label <- "";
    
    #Define plot boundaries based on country list selected
    df_temp <- dfcases1
    df_temp <- subset(df_temp,CountriesAndTerritories %in% clist())
    maxCases <- max(df_temp$Cases,na.rm=TRUE)
    maxDays <- max(df_temp$Day,na.rm=TRUE)
    
    p <- ggplot(data=df_temp,
                aes(x=Day, y=Cases, group=CountriesAndTerritories, colour=CountriesAndTerritories))+
      scale_x_continuous(name=paste0("Days since ",casesthresh1,"th case"), labels=comma,
                         limits = c(0,maxDays+3),expand = c(0, 0))+
      scale_y_continuous(name="Cumulative number of cases (log10)", labels=comma,trans='log10',
                         limits = c(casesthresh1,maxCases))+
      annotation_logticks(sides = "l")+
      geom_line(lwd=1, alpha=0.6)+
      geom_point(alpha=0.6)+
      geom_text_repel(aes(label = label),
                      alpha = 1,
                      nudge_x = 1)+
      theme_bw()+theme_classic()+theme(legend.position = "none")
    
    
    #limit d1 to max cases
    df_temp2<-d1;#();
    df_temp2 <- subset(df_temp2,Day<maxDays & Cases<maxCases)
    
    #Add label for last Day;
    temp <- df_temp2 %>% group_by(CountriesAndTerritories) %>%
      summarise(maxDay = max(Day))
    df_temp2 <- merge(df_temp2,temp,by="CountriesAndTerritories", all.x=TRUE)
    df_temp2 <- df_temp2 %>% mutate(label = if_else(Day == maxDay, 
                                        paste("doubles every",as.character(sub("double","",CountriesAndTerritories)),
                                              ifelse(as.numeric(sub("double","",CountriesAndTerritories))==1,"day","days")), ""))
    df_temp2$maxDay <- NULL
    
    # plot 
    p + geom_line(data=as.data.frame(df_temp2), aes(x=Day, y=Cases),
                 colour="darkgrey",lwd=1,alpha=0.8, linetype="dashed") +
      geom_text_repel(data=as.data.frame(df_temp2), aes(label = label),
                      alpha = 1,
                      colour="darkgrey",
                      size=3, 
                      nudge_x = 1)  
  })
  
  #### Plots for Death ####

  output$PlotDeathLinear <- renderPlot({

    #Plot cumulative curves;
    dfDeaths <- dfDeaths[dfDeaths$countriesAndTerritories %in% clist() & dfDeaths$Deaths>0,]
    ggplot(data=dfDeaths,
           aes(x=dateRep, y=Deaths, group=countriesAndTerritories, colour=countriesAndTerritories)) +
      scale_x_date(name="Date",labels = date_format("%d %b %Y"),expand = c(0, 0))+
      scale_y_continuous(name="Cumulative number of deaths", labels=comma,
                         limits = c(0,max(dfDeaths$Deaths)))+
      geom_line(lwd=1, alpha=0.6)+
      geom_point(alpha=0.6)+
      geom_text_repel(aes(label = label),
                      alpha = 1,
                      nudge_x = 1)+
      theme_bw()+theme_classic()+theme(legend.position = "none")
  })

  output$PlotDeathLog <- renderPlot({
    #Plot cumulative curves;


    dfDeaths <- dfDeaths[dfDeaths$countriesAndTerritories %in% clist() & dfDeaths$Deaths>0,]
    ggplot(data=dfDeaths,
           aes(x=dateRep, y=Deaths, group=countriesAndTerritories, colour=countriesAndTerritories)) +
      scale_x_date(name="Date",labels = date_format("%d %b %Y"),expand = c(0, 0))+
      scale_y_continuous(name="Cumulative number of deaths", labels=comma,trans='log10',
                         limits = c(1,max(dfDeaths$Deaths)))+
      annotation_logticks(sides = "l")+
      geom_line(lwd=1, alpha=0.6)+
      geom_point(alpha=0.6)+
      geom_text_repel(aes(label = label),
                      alpha = 1,
                      nudge_x = 1)+
      theme_bw()+theme_classic()+theme(legend.position = "none")
  })

  output$PlotDeathTraj <- renderPlot({

    dfdeaths0 <- dfdeaths[,c(2:dim(dfdeaths)[2])] %>% replace_with_na_all(condition = ~.x <= 5)
    dfdeaths0 <- apply(dfdeaths0, 1, sort); #extract only non-NA values;
    dfdeaths0 <- t(sapply(dfdeaths0, '[', seq(max(sapply(dfdeaths0, length))))); #put values in matrix;
    # #adjust starting number downward to threshold if exceed threshold;
    # for (i in 1:dim(dfdeaths0)[1]){
    #   if(all(is.na(dfdeaths0[i,]))){next}
    #   dfdeaths0[i,] <- dfdeaths0[i,]-min(dfdeaths0[i,],na.rm=TRUE)+deathsthresh1;
    # }
    #make dataframe join with countries label;
    dfdeaths0 <- data.frame(dfdeaths$countriesAndTerritories,dfdeaths0)
    names(dfdeaths0) <- c("CountriesAndTerritories",paste("Day",1:(dim(dfdeaths0)[2]-1)))

    dfdeaths1 <- gather(dfdeaths0,"Day", "Deaths",2:dim(dfdeaths0)[2]);
    dfdeaths1$Day <- as.numeric(gsub("Day ","",dfdeaths1$Day))
    dfdeaths1 <- dfdeaths1[!is.na(dfdeaths1$Deaths),]

    #Add label for last Day;
    temp <- dfdeaths1 %>% group_by(CountriesAndTerritories) %>%
      summarise(maxDay = max(Day))
    dfdeaths1 <- merge(dfdeaths1,temp,by="CountriesAndTerritories", all.x=TRUE)
    dfdeaths1 <- dfdeaths1 %>% mutate(label = if_else(Day == maxDay,
                                                    as.character(CountriesAndTerritories), ""))
    dfdeaths1$maxDay <- NULL
    #deathsthresh1 <<- input$deathsthresh;
    df_temp <- dfdeaths1#()
    maxDay <- max(df_temp$Day,na.rm=TRUE)

    #Plot cumulative deaths after threshold case;
    d1 <- data.frame(Day=1:maxDay, deathsthresh=deathsthresh1);
    d1$double1 <- d1$deathsthresh*2^(d1$Day-1);
    d1$double2 <- d1$deathsthresh*2^((1/2)*(d1$Day-1));
    d1$double3 <- d1$deathsthresh*2^((1/3)*(d1$Day-1));
    d1$double7 <- d1$deathsthresh*2^((1/7)*(d1$Day-1));
    d1$double14 <- d1$deathsthresh*2^((1/14)*(d1$Day-1));
    d1$deathsthresh<- NULL;
    d1 <- gather(d1,"CountriesAndTerritories","Deaths",2:dim(d1)[2])
    d1$label <- "";

    #Define plot boundaries based on country list selected
    df_temp <- dfdeaths1
    df_temp <- subset(df_temp,CountriesAndTerritories %in% clist())
    maxDeaths <- max(df_temp$Deaths,na.rm=TRUE)
    maxDays <- max(df_temp$Day,na.rm=TRUE)

    p <- ggplot(data=df_temp,
                aes(x=Day, y=Deaths, group=CountriesAndTerritories, colour=CountriesAndTerritories))+
      scale_x_continuous(name=paste0("Days since ",deathsthresh1,"th case"), labels=comma,
                         limits = c(0,maxDays+3),expand = c(0, 0))+
      scale_y_continuous(name="Cumulative number of deaths (log10)", labels=comma,trans='log10',
                         limits = c(deathsthresh1,maxDeaths))+
      annotation_logticks(sides = "l")+
      geom_line(lwd=1, alpha=0.6)+
      geom_point(alpha=0.6)+
      geom_text_repel(aes(label = label),
                      alpha = 1,
                      nudge_x = 1)+
      theme_bw()+theme_classic()+theme(legend.position = "none")


    #limit d1 to max deaths
    df_temp2<-d1;#();
    df_temp2 <- subset(df_temp2,Day<maxDays & Deaths<maxDeaths)

    #Add label for last Day;
    temp <- df_temp2 %>% group_by(CountriesAndTerritories) %>%
      summarise(maxDay = max(Day))
    df_temp2 <- merge(df_temp2,temp,by="CountriesAndTerritories", all.x=TRUE)
    df_temp2 <- df_temp2 %>% mutate(label = if_else(Day == maxDay,
                                                    paste("doubles every",as.character(sub("double","",CountriesAndTerritories)),
                                                          ifelse(as.numeric(sub("double","",CountriesAndTerritories))==1,"day","days")), ""))
    df_temp2$maxDay <- NULL

    # plot
    p + geom_line(data=as.data.frame(df_temp2), aes(x=Day, y=Deaths),
                  colour="darkgrey",lwd=1,alpha=0.8, linetype="dashed") +
      geom_text_repel(data=as.data.frame(df_temp2), aes(label = label),
                      alpha = 1,
                      colour="darkgrey",
                      size=3,
                      nudge_x = 1)
  })
  
})
