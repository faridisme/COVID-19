#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#setwd("C:/Users/Farid/Dropbox/academic/R projects/COVID-19")
#setwd("C:/Users/Olivia Oh/Documents/Farid/COVID-19CumulativeCurves")
library(shiny)

library(utils)
library(httr)
#library(xlsx)
library(readxl)
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
#library(lubridate)
#install.packages("directlabels")
# library(directlabels)

DATADIR <- "data/";
DATAFILE <- "COVID-19-geographic-disbtribution-worldwide.xlsx";

#### Data preparation ####

# Read ECDC data
# URL <- paste0("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx");#"https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",Sys.Date()-1,".xlsx")
# download.file(URL, destfile=DATAFILE, mode="wb")
# df <- read_excel(DATAFILE)
df <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM", stringsAsFactors=FALSE)
df$dateRep <- as.Date(df$dateRep,"%d/%m/%Y")
timestamp1 <- max(df$dateRep)
message(paste0("Data source: ECDC (as of ",format(timestamp1,"%d %B %Y"),")"))


# # read in HK data from file separately
df_HK <- read.csv("HK.csv", sep=",",header=TRUE,comment.char="", stringsAsFactors=FALSE)
df_HK$dateRep <- as.Date(df_HK$dateRep,"%d/%m/%Y")
timestamp2 <- max(df_HK$dateRep)
message(paste0("Data source: HK DOH (as of ",format(timestamp2,"%d %B %Y"),")"))
# if (timestamp1!=timestamp2){
#   df_HK$dateRep <- df_HK$dateRep+timestamp1-timestamp2
# }

df <- rbind(df[,c("dateRep","cases","deaths","countriesAndTerritories","geoId","popData2019")],df_HK[,c("dateRep","cases","deaths","countriesAndTerritories","geoId","popData2019")])

# Read and merge mobility data
#install.packages("RCurl")
# library(RCurl)
# URL <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=a843e7dddeca3af6";
# x <- getURL(URL)
# mobdf <- read.csv(textConnection(x), stringsAsFactors=FALSE)
# mobdf$dateRep <- as.Date(mobdf$date)
# mobdf$geoId <- mobdf$country_region_code
# mobdf <- mobdf[mobdf$sub_region_1=="",c("dateRep","geoId","residential_percent_change_from_baseline")]
# mobdf <- mobdf %>% group_by(dateRep,geoId) %>%
#   summarise(residential_percent_change_from_baseline = mean(residential_percent_change_from_baseline,na.rm=TRUE))
# df <- merge(df, mobdf,
#             by = c("dateRep","geoId"),all.x=TRUE)

# read in Regions data from file separately
df_regions <- read.csv("Regions.csv", sep=",",header=TRUE,comment.char="", stringsAsFactors=FALSE)

#convert long to wide
# Cases
dfcases <- spread(df[,c("dateRep","countriesAndTerritories","cases")],dateRep,cases);
dfcases[is.na(dfcases)] <- 0;
if(any(dfcases$countriesAndTerritories=="Hong Kong")){ #Remove HK numbers from China
  dfcases[dfcases$countriesAndTerritories=="China",c(2:dim(dfcases)[2])] <- 
    (dfcases[dfcases$countriesAndTerritories=="China",c(2:dim(dfcases)[2])])-
    (dfcases[dfcases$countriesAndTerritories=="Hong Kong",c(2:dim(dfcases)[2])])
}

# Convert daily numbers to cumulative numbers
for (i in 3:dim(dfcases)[2]){
  dfcases[,i] <- dfcases[,i-1]+ dfcases[,i];
}
dfCases <-  gather(dfcases,"dateRep", "Cases",2:dim(dfcases)[2]);
dfCases$dateRep <- as.Date(dfCases$dateRep)
dfCases <- dfCases %>% mutate(label = if_else(dateRep == max(dateRep),
                                              as.character(countriesAndTerritories), ""))

clist1<- sort(unique(dfCases$countriesAndTerritories[dfCases$Cases>=1]))

casesthresh1 <- 10; #100;

#Merge in population data and new cases;
dfCases <- merge(dfCases,(df[-which(duplicated(df[,c("countriesAndTerritories","popData2019")])),
                             c("countriesAndTerritories","popData2019")]),
                 by=c("countriesAndTerritories"),all.x=TRUE)
dfCases <- merge(dfCases,df[,c("countriesAndTerritories","dateRep","cases")], #,"residential_percent_change_from_baseline"
                 by=c("countriesAndTerritories","dateRep"),all.x=TRUE)
dfCases$cases[is.na(dfCases$cases)] <- 0;
#smoothen new cases over past week;
dfCases<-arrange(dfCases,countriesAndTerritories,dateRep) %>% group_by(countriesAndTerritories)%>%
  mutate(lag1=lag(cases),
         lag2=lag(cases,2),
         lag3=lag(cases,3),
         lag4=lag(cases,4),
         lag5=lag(cases,5),
         lag6=lag(cases,6),
         lag7=lag(cases,7),
         lag8=lag(cases,8),
         lag9=lag(cases,9),
         lag10=lag(cases,10),
         lag11=lag(cases,11),
         lag12=lag(cases,12),
         lag13=lag(cases,13),
         lag14=lag(cases,14),
         lag15=lag(cases,15),
         lag16=lag(cases,16),
         lag17=lag(cases,17),
         lag18=lag(cases,18),
         lag19=lag(cases,19),
         lag20=lag(cases,20),
         lag21=lag(cases,21),
         lag22=lag(cases,22),
         lag23=lag(cases,23),
         lag24=lag(cases,24),
         lag25=lag(cases,25),
         lag26=lag(cases,26),
         lag27=lag(cases,27),
         lag28=lag(cases,28),
         lag29=lag(cases,29),
         lag30=lag(cases,30),
         lag31=lag(cases,31),
         lag32=lag(cases,32),
         lag33=lag(cases,33),
         lag34=lag(cases,34),
         newCases=cases,
         ma5=(cases+lag1+lag2+lag3+lag4)/5,
         ma7=(cases+lag1+lag2+lag3+lag4+lag5+lag6)/7,
         ma28_1=(lag7+lag8+lag9+lag10+lag11+lag12+lag13+lag14+lag15+lag16+lag17+lag18+lag19+lag20+
                   lag21+lag22+lag23+lag24+lag25+lag26+lag27+lag28+lag29+lag30+lag31+lag32+lag33+lag34)/28,
         ma28=(cases+lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9+lag10+lag11+lag12+lag13+
                 lag14+lag15+lag16+lag17+lag18+lag19+lag20+lag21+lag22+lag23+lag24+lag25+lag26+lag27)/28) 
dfCases$ma7[is.na(dfCases$ma7)] <- 0;
dfCases[,c("cases","lag1","lag2","lag3","lag4","lag5","lag6",
           "lag7","lag8","lag9","lag10","lag11","lag12","lag13",
           "lag14","lag15","lag16","lag17","lag18","lag19","lag20",
           "lag21","lag22","lag23","lag24","lag25","lag26","lag27",
           "lag28","lag29","lag30","lag31","lag32","lag33","lag34")] <- list(NULL)

# Deaths
dfdeaths <- spread(df[,c("dateRep","countriesAndTerritories","deaths")],dateRep,deaths);
dfdeaths[is.na(dfdeaths)] <- 0;
if(any(dfdeaths$countriesAndTerritories=="Hong Kong")){ #Remove HK numbers from China
  dfdeaths[dfdeaths$countriesAndTerritories=="China",c(2:dim(dfdeaths)[2])] <- 
    (dfdeaths[dfdeaths$countriesAndTerritories=="China",c(2:dim(dfdeaths)[2])])-
    (dfdeaths[dfdeaths$countriesAndTerritories=="Hong Kong",c(2:dim(dfdeaths)[2])])
}

# Convert daily numbers to cumulative numbers
for (i in 3:dim(dfdeaths)[2]){
  dfdeaths[,i] <- dfdeaths[,i-1]+ dfdeaths[,i];
}
dfDeaths <-  gather(dfdeaths,"dateRep", "Deaths",2:dim(dfdeaths)[2]);
dfDeaths$dateRep <- as.Date(dfDeaths$dateRep)
dfDeaths <- dfDeaths %>% mutate(label = if_else(dateRep == max(dateRep),
                                              as.character(countriesAndTerritories), ""))
#Merge in population data and new deaths;
dfDeaths <- merge(dfDeaths,(df[-which(duplicated(df[,c("countriesAndTerritories","popData2019")])),
                             c("countriesAndTerritories","popData2019")]),
                 by=c("countriesAndTerritories"),all.x=TRUE)
dfDeaths <- merge(dfDeaths,df[,c("countriesAndTerritories","dateRep","deaths")],
                 by=c("countriesAndTerritories","dateRep"),all.x=TRUE)
dfDeaths$deaths[is.na(dfDeaths$deaths)] <- 0;

#smoothen new cases over past week;
dfDeaths<-arrange(dfDeaths,countriesAndTerritories,dateRep) %>% group_by(countriesAndTerritories)%>%
  mutate(lag1=lag(deaths),
         lag2=lag(deaths,2),
         lag3=lag(deaths,3),
         lag4=lag(deaths,4),
         lag5=lag(deaths,5),
         lag6=lag(deaths,6),
         lag7=lag(deaths,7),
         lag8=lag(deaths,8),
         lag9=lag(deaths,9),
         lag10=lag(deaths,10),
         lag11=lag(deaths,11),
         lag12=lag(deaths,12),
         lag13=lag(deaths,13),
         lag14=lag(deaths,14),
         lag15=lag(deaths,15),
         lag16=lag(deaths,16),
         lag17=lag(deaths,17),
         lag18=lag(deaths,18),
         lag19=lag(deaths,19),
         lag20=lag(deaths,20),
         lag21=lag(deaths,21),
         lag22=lag(deaths,22),
         lag23=lag(deaths,23),
         lag24=lag(deaths,24),
         lag25=lag(deaths,25),
         lag26=lag(deaths,26),
         lag27=lag(deaths,27),
         newDeaths=deaths,
         deaths_ma7=(deaths+lag1+lag2+lag3+lag4+lag5+lag6)/7,
         deaths_ma28=(deaths+lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9+lag10+lag11+lag12+lag13+
                 lag14+lag15+lag16+lag17+lag18+lag19+lag20+lag21+lag22+lag23+lag24+lag25+lag26+lag27)/28)
dfDeaths$deaths_ma7[is.na(dfDeaths$deaths_ma7)] <- 0;
dfDeaths[,c("deaths","lag1","lag2","lag3","lag4","lag5","lag6",
           "lag7","lag8","lag9","lag10","lag11","lag12","lag13",
           "lag14","lag15","lag16","lag17","lag18","lag19","lag20",
           "lag21","lag22","lag23","lag24","lag25","lag26","lag27")] <- list(NULL)

deathsthresh1 <- 5 #5;


# Define server logic required to draw figure
shinyServer(function(input, output) {
  
  #casesthresh1 <- input$casesthresh
  clist <- reactive({
    if(any(input$clist=="Whole World")){
      clist <- clist1
    } else {
      input$clist
    }
  })
  
  numCty <- reactive({length(clist())})
  dateperiod <- reactive({input$date[2]- input$date[1]});
  dateperiodsinceStart <- reactive({Sys.Date()- input$date[1]});
  
  textStartDate <- reactive({input$date[1]})
  output$textStartDate <- renderText({paste0("Trajectories by Days since ",format(textStartDate(),"%d %B %Y"))})#
  
  #### Table ####
  
  output$Countrytable <- renderTable({
    dfCases <- dfCases %>% filter(input$date[1] <= as.Date(dateRep)) 
    start <- dfCases %>% group_by(countriesAndTerritories) %>%
      summarise(CasesUntilStartDate = min(Cases,na.rm=TRUE))
    dfCases <- merge(dfCases,start, by=c("countriesAndTerritories"),all.x=TRUE)
    dfDeaths <- dfDeaths %>% filter(input$date[1] <= as.Date(dateRep)) 
    start <- dfDeaths %>% group_by(countriesAndTerritories) %>%
      summarise(DeathsUntilStartDate = min(Deaths,na.rm=TRUE))
    dfCases <- merge(dfCases,start, by=c("countriesAndTerritories"),all.x=TRUE)
    rm(start)
    
    dfCases <- dfCases[dfCases$countriesAndTerritories %in% clist() & dfCases$label!="",]
    dfDeaths <- dfDeaths[dfDeaths$countriesAndTerritories %in% clist() & dfDeaths$label!="",]
    dftable <- merge(dfCases,dfDeaths[,c("countriesAndTerritories","Deaths","deaths_ma7","deaths_ma28")], 
                     by=c("countriesAndTerritories"),all.x=TRUE) 
    dftable %>% 
      transmute(Country=gsub("_","",countriesAndTerritories),
                Population=format(as.numeric(popData2019), big.mark=","),
                C=Cases,
                D=Deaths,
                CasesStart=format(CasesUntilStartDate, big.mark=","),
                DeathsStart=format(DeathsUntilStartDate, big.mark=","),
                Cases=format(C, big.mark=","),
                Deaths=format(D, big.mark=","),
                CFR=format(round(as.numeric(D)/as.numeric(C)*100,1), big.mark=","),
                CasesPP=ifelse(as.numeric(C)/popData2019*100000,format(round(as.numeric(C)/popData2019*100000,2),nsmall=1, big.mark=","),
                               formatC(signif(as.numeric(C)/popData2019*100000,1),format="fg")),
                DeathsPP=ifelse(as.numeric(D)/popData2019*100000>0.001,format(round(as.numeric(C)/popData2019*100000,2),nsmall=1, big.mark=","),
                                formatC(signif(as.numeric(D)/popData2019*100000,1),format="fg")),
                #AveNewCases7=format(round(as.numeric(ma7),0), format="d", big.mark=","),
                AveNewCases=ifelse(ma28<1,formatC(signif(ma28,1),format="fg"),
                                   format(round(as.numeric(ma28),0),big.mark=",", format="d")),
                AveNewCases7PP=format(round(ma7/popData2019*100000,2),nsmall=1, big.mark=","),
                AveNewCasesPP=ifelse(ma28/popData2019*100000>0.001,format(round(ma28/popData2019*100000,3),nsmall=1, big.mark=","),
                                     formatC(signif(ma28/popData2019*100000,1),format="fg")),
                #AveNewDeaths7=format(round(as.numeric(deaths_ma7),2),nsmall=1, big.mark=","),
                AveNewDeaths=ifelse(deaths_ma28<1,formatC(signif(deaths_ma28,1),format="fg"),
                                    format(round(as.numeric(deaths_ma28),0),big.mark=",", format="d")),
                #AveNewDeaths7PP=format(round(deaths_ma7/popData2019*100000,2),nsmall=1, big.mark=","),
                AveNewDeathsPP=ifelse(deaths_ma28/popData2019*100000>0.001,format(round(deaths_ma28/popData2019*100000,3),nsmall=1, big.mark=","),
                                      formatC(signif(deaths_ma28/popData2019*100000,1),format="fg"))
                ) %>%
      arrange(desc(Cases)) %>% 
      select(-C,-D)
    
  },align='lrrrrrrrrrrrrr')
  
  output$CountrytableSelect <- renderTable({
    
    dfCases <- dfCases[dfCases$label!="",]
    dfDeaths <- dfDeaths[dfDeaths$label!="",]
    dftable <- merge(dfCases,dfDeaths[,c("countriesAndTerritories","Deaths","deaths_ma7","deaths_ma28")], 
                     by=c("countriesAndTerritories"),all.x=TRUE) 
    dftable <- dftable[]
    dftable <- merge(dftable, df_regions, by=c("countriesAndTerritories"),all.x=TRUE)
    dftable <- dftable %>% filter(countriesAndTerritories %in% clist())

    dftable1 <- dftable %>% 
      transmute(Country=gsub("_","",countriesAndTerritories),
                Population=format(as.numeric(popData2019), big.mark=","),
                Region=Region,
                C=Cases,
                D=Deaths,
                Cases=format(C, big.mark=","),
                AveNewCases=ifelse(ma28<1,formatC(signif(ma28,1),format="fg"),
                                   format(round(as.numeric(ma28),0),big.mark=",", format="d")),
                AveNewCasesPP=ifelse(ma28/popData2019*100000>0.001,format(round(ma28/popData2019*100000,3),nsmall=1, big.mark=","),
                                     formatC(signif(ma28/popData2019*100000,1),format="fg")),
                AveNewCases7PP=ifelse(ma7/popData2019*100000>0.001,format(round(ma7/popData2019*100000,3),nsmall=1, big.mark=","),
                                     formatC(signif(ma7/popData2019*100000,1),format="fg")),
                Trend=ifelse(ma28>ma28_1,"UP",ifelse(ma28==ma28_1,"-","DOWN"))
      ) %>%
      arrange(Region,as.numeric(AveNewCasesPP)) %>% 
      select(-Population,-C,-D,-Region)
    
  },align='lrrrrl')
  
  output$CountrytableWatch <- renderTable({
    
    dfCases <- dfCases[dfCases$label!="",]
    dfDeaths <- dfDeaths[dfDeaths$label!="",]
    dftable <- merge(dfCases,dfDeaths[,c("countriesAndTerritories","Deaths","deaths_ma7","deaths_ma28")], 
                     by=c("countriesAndTerritories"),all.x=TRUE) 
    dftable <- dftable[]
    dftable <- merge(dftable, df_regions, by=c("countriesAndTerritories"),all.x=TRUE)
    dftable <- dftable %>% filter(!is.na(Region))
    
    dftable1 <- dftable %>% 
      transmute(Country=gsub("_","",countriesAndTerritories),
                Population=format(as.numeric(popData2019), big.mark=","),
                Region=Region,
                C=Cases,
                D=Deaths,
                Cases=format(C, big.mark=","),
                AveNewCases=ifelse(ma28<1,formatC(signif(ma28,1),format="fg"),
                                   format(round(as.numeric(ma28),0),big.mark=",", format="d")),
                AveNewCasesPP=ifelse(ma28/popData2019*100000>0.001,format(round(ma28/popData2019*100000,3),nsmall=1, big.mark=","),
                                     formatC(signif(ma28/popData2019*100000,1),format="fg")),
                Trend=ifelse(ma28>ma28_1,"UP",ifelse(ma28==ma28_1,"-","DOWN"))
      ) %>%
      arrange(Region,as.numeric(AveNewCasesPP)) %>% 
      select(-Population,-C,-D,-Region)
    
  },align='lrrrl')
  
  #### Plots for Cases ####
  
  #### Mobility ####
  
  # output$PlotnewCasesMobperday <- renderPlot({
  #   #Plot new Cases per day and 28-day moving average;
  # 
  #   dfCases <- dfCases[dfCases$countriesAndTerritories %in% clist() &
  #                        dfCases$Cases>=1 & dfCases$ma7>0 & !is.na(dfCases$ma7),]
  # 
  #   #plot trend over input date range;
  #   dfCases <- dfCases %>% filter(input$date[2] >= as.Date(dateRep), input$date[1] <= as.Date(dateRep)) 
  #   
  #   yfactor <- dfCases %>% group_by(countriesAndTerritories) %>%
  #     summarise(maxCases = max(newCases,na.rm=TRUE),
  #               maxMob = max(residential_percent_change_from_baseline, na.rm=TRUE),
  #               minMob = min(residential_percent_change_from_baseline, na.rm=TRUE),
  #               yfactor = maxCases/maxMob) %>%
  #     select(-maxCases) %>% as.data.frame()
  #   dfCases <- merge(dfCases,yfactor,by="countriesAndTerritories", all.x=TRUE)
  #   dfCases$secylabel <- ifelse((dfCases$residential_percent_change_from_baseline==dfCases$maxMob),paste0(dfCases$maxMob,"%"),
  #                               ifelse((dfCases$residential_percent_change_from_baseline==dfCases$minMob),paste0(dfCases$minMob,"%"),""))
  #   rm(yfactor)
  #   ggplot(data=dfCases,
  #            aes(x=dateRep, y=newCases)) +
  #     geom_line(aes(x=dateRep, y=residential_percent_change_from_baseline*yfactor), lwd=1, colour = "dark red")+
  #     geom_text(aes(x=dateRep, y=residential_percent_change_from_baseline*yfactor,label=secylabel),
  #               hjust=-0.25,size=3, colour="sienna4")+
  #     geom_line(aes(x=dateRep, y=ma5), lwd=1, colour = "dark blue")+
  #     geom_hline(yintercept=0)+
  #     scale_x_date(name="Date",labels = date_format("%d %b %Y"),expand = c(0, 0))+
  #     scale_y_continuous(name="New cases", labels=number_format(accuracy = 0.01))+
  #     
  #     facet_grid(countriesAndTerritories~.,scales="free") +
  #     theme_bw()+theme(legend.position = "none")+ #theme_classic()+
  #     theme(
  #       axis.title.y = element_text(color = "dark blue", size = 13),
  #       axis.title.y.right = element_text(color = "dark red", size = 13))
  # }, height=function(){200*numCty()})
 
  output$PlotnewCasesperday <- renderPlot({
    #Plot new Cases per day and 28-day moving average;
    
    dfCases <- dfCases[dfCases$countriesAndTerritories %in% clist() & 
                         dfCases$Cases>=1 & dfCases$ma28>0 & !is.na(dfCases$ma28),]
    #plot trend over input date range;
    dfCases <- dfCases %>% filter(input$date[2] >= as.Date(dateRep), input$date[1] <= as.Date(dateRep))
    
    if (dateperiod()>42){
      ggplot(data=dfCases,
           aes(x=dateRep, y=newCases)) +
        geom_bar(stat="identity", fill="sienna3") +
        geom_line(aes(x=dateRep, y=ma28), lwd=1, colour = "dark blue", alpha=0.6)+
        geom_line(aes(x=dateRep, y=ma7), lwd=1, colour = "dark red", alpha=0.6)+
        scale_x_date(name="Date",labels = date_format("%d %b %Y"),expand = c(0, 0))+
        scale_y_continuous(name="New cases", labels=comma)+
        facet_grid(countriesAndTerritories~.,scales="free") +
        theme_bw()+theme_classic()+theme(legend.position = "none")
    } else {
      ggplot(data=dfCases,
             aes(x=dateRep, y=newCases)) +
        geom_bar(stat="identity", fill="sienna3") +
        geom_text(aes(label=ifelse(newCases==0,"",formatC(newCases, big.mark=",", format="d"))),
                  vjust=-0.25,size=3, colour="sienna4")+
        geom_line(aes(x=dateRep, y=ma28), lwd=1, colour = "dark blue", alpha=0.6)+
        geom_text(aes(x=dateRep, y=ma28, label=formatC(round(ma28), big.mark=",", format="d")),
                  vjust=-0.25,size=3, colour="dark blue")+
        geom_line(aes(x=dateRep, y=ma7), lwd=1, colour = "dark red", alpha=0.6)+
        geom_text(aes(x=dateRep, y=ma7, label=formatC(round(ma7), big.mark=",", format="d")),
                  vjust=-0.25,size=3, colour="dark red")+
        scale_x_date(name="Date",labels = date_format("%d %b %Y"),expand = c(0, 0))+
        scale_y_continuous(name="New cases", labels=comma)+
        facet_grid(countriesAndTerritories~.,scales="free") +
        theme_bw()+theme_classic()+theme(legend.position = "none")
    }
    
  }, height=function(){200*numCty()})
  
  output$PlotnewCasesperdayPP <- renderPlot({
    #Plot new Cases per day and 28-day moving average;
    
    dfCases <- dfCases[dfCases$countriesAndTerritories %in% clist() & 
                         dfCases$Cases>=1 &dfCases$ma28>0 & !is.na(dfCases$ma28),]
    
    
    #plot trend over input date range;
    dfCases <- dfCases %>% filter(input$date[2] >= as_date(dateRep), input$date[1] <= as_date(dateRep)) %>%
      mutate(newCasesPP = newCases/popData2019*100000,
             ma28PP = ma28/popData2019*100000,
             ma7PP = ma7/popData2019*100000)
    
    if (dateperiod()>42){
      ggplot(data=dfCases,
             aes(x=dateRep, y=newCasesPP)) +
        geom_bar(stat="identity", fill="sienna3") +
        geom_line(aes(x=dateRep, y=ma28PP), lwd=1, colour = "dark blue", alpha=0.6)+
        geom_line(aes(x=dateRep, y=ma7PP), lwd=1, colour = "dark red", alpha=0.6)+
        scale_x_date(name="Date",labels = date_format("%d %b %Y"),expand = c(0, 0))+
        scale_y_continuous(name="New cases per 100,000 ppl", labels=number_format(accuracy = 0.01))+
        facet_grid(countriesAndTerritories~.,scales="free") +
        theme_bw()+theme_classic()+theme(legend.position = "none")
    } else {
      ggplot(data=dfCases,
             aes(x=dateRep, y=newCasesPP)) +
        geom_bar(stat="identity", fill="sienna3") +
        geom_text(aes(label=ifelse(newCases==0,"",formatC(newCasesPP, format="f",digits=2))),
                  vjust=-0.25,size=3, colour="sienna3")+
        geom_line(aes(x=dateRep, y=ma28PP), lwd=1, colour = "dark blue", alpha=0.6)+
        geom_text(aes(x=dateRep, y=ma28PP, label=formatC(ma28PP, format="f",digits=2)),
                  vjust=-0.25,size=3, colour="dark blue")+
        geom_line(aes(x=dateRep, y=ma7PP), lwd=1, colour = "dark red", alpha=0.6)+
        geom_text(aes(x=dateRep, y=ma7PP, label=formatC(ma7PP, format="f",digits=2)),
                  vjust=-0.25,size=3, colour="dark red")+
        scale_x_date(name="Date",labels = date_format("%d %b %Y"),expand = c(0, 0))+
        scale_y_continuous(name="New cases per 100,000 ppl", labels=number_format(accuracy = 0.01))+
        facet_grid(countriesAndTerritories~.,scales="free") +
        theme_bw()+theme_classic()+theme(legend.position = "none")
    }
    
  }, height=function(){200*numCty()})  

  output$PlotnewDeathsperday <- renderPlot({
    #Plot new Cases per day and 28-day moving average;
    
    dfDeaths <- dfDeaths[dfDeaths$countriesAndTerritories %in% clist() & 
                         dfDeaths$Deaths>=1 & dfDeaths$deaths_ma28>0 & !is.na(dfDeaths$deaths_ma28),]
    #plot trend over input date range;
    dfDeaths <- dfDeaths %>% filter(input$date[2] >= as_date(dateRep), input$date[1] <= as_date(dateRep))
    
    
    if (dateperiod()>42){
      ggplot(data=dfDeaths,
             aes(x=dateRep, y=newDeaths)) +
        geom_bar(stat="identity", fill="sienna3") +
        geom_line(aes(x=dateRep, y=deaths_ma28), lwd=1, colour = "dark blue", alpha=0.6)+
        geom_line(aes(x=dateRep, y=deaths_ma7), lwd=1, colour = "dark red", alpha=0.6)+
        scale_x_date(name="Date",labels = date_format("%d %b %Y"),expand = c(0, 0))+
        scale_y_continuous(name="New deaths", labels=comma)+
        facet_grid(countriesAndTerritories~.,scales="free") +
        theme_bw()+theme_classic()+theme(legend.position = "none")
    } else {
      ggplot(data=dfDeaths,
             aes(x=dateRep, y=newDeaths)) +
        geom_bar(stat="identity", fill="sienna3") +
        geom_text(aes(label=ifelse(newDeaths==0,"",formatC(newDeaths, big.mark=",", format="d"))),
                  vjust=-0.25,size=3, colour="sienna4")+
        geom_line(aes(x=dateRep, y=deaths_ma28), lwd=1, colour = "dark blue", alpha=0.6)+
        geom_text(aes(x=dateRep, y=deaths_ma28, label=formatC(round(deaths_ma28), big.mark=",", format="d")),
                  vjust=-0.25,size=3, colour="dark blue")+
        geom_line(aes(x=dateRep, y=deaths_ma7), lwd=1, colour = "dark red", alpha=0.6)+
        geom_text(aes(x=dateRep, y=deaths_ma7, label=formatC(round(deaths_ma7), big.mark=",", format="d")),
                  vjust=-0.25,size=3, colour="dark red")+
        scale_x_date(name="Date",labels = date_format("%d %b %Y"),expand = c(0, 0))+
        scale_y_continuous(name="New deaths", labels=comma)+
        facet_grid(countriesAndTerritories~.,scales="free") +
        theme_bw()+theme_classic()+theme(legend.position = "none")
    }
    
  }, height=function(){200*numCty()})
  
  output$PlotnewDeathsperdayPP <- renderPlot({
    #Plot new Cases per day and 28-day moving average;
    
    dfDeaths <- dfDeaths[dfDeaths$countriesAndTerritories %in% clist() & 
                         dfDeaths$Deaths>=1 &dfDeaths$deaths_ma28>0 & !is.na(dfDeaths$deaths_ma28),]
    
    #plot trend over input date range;
    dfDeaths <- dfDeaths %>% filter(input$date[2] >= as_date(dateRep), input$date[1] <= as_date(dateRep)) %>%
      mutate(newDeathsPP = newDeaths/popData2019*100000,
             deaths_ma28PP = deaths_ma28/popData2019*100000,
             deaths_ma7PP = deaths_ma7/popData2019*100000)
    
    if (dateperiod()>42){
      ggplot(data=dfDeaths,
             aes(x=dateRep, y=newDeathsPP)) +
        geom_bar(stat="identity", fill="sienna3") +
        geom_line(aes(x=dateRep, y=deaths_ma28PP), lwd=1, colour = "dark blue", alpha=0.6)+
        geom_line(aes(x=dateRep, y=deaths_ma7PP), lwd=1, colour = "dark red", alpha=0.6)+
        scale_x_date(name="Date",labels = date_format("%d %b %Y"),expand = c(0, 0))+
        scale_y_continuous(name="New deaths per 100,000 ppl", labels=number_format(accuracy = 0.01))+
        facet_grid(countriesAndTerritories~.,scales="free") +
        theme_bw()+theme_classic()+theme(legend.position = "none")
    } else {
      ggplot(data=dfDeaths,
             aes(x=dateRep, y=newDeathsPP)) +
        geom_bar(stat="identity", fill="sienna3") +
        geom_text(aes(label=ifelse(newDeaths==0,"",formatC(newDeathsPP, format="f",digits=2))),
                  vjust=-0.25,size=3, colour="sienna3")+
        geom_line(aes(x=dateRep, y=deaths_ma28PP), lwd=1, colour = "dark blue", alpha=0.6)+
        geom_text(aes(x=dateRep, y=deaths_ma28PP, label=formatC(deaths_ma28PP, format="f",digits=2)),
                  vjust=-0.25,size=3, colour="dark blue")+
        geom_line(aes(x=dateRep, y=deaths_ma7PP), lwd=1, colour = "dark red", alpha=0.6)+
        geom_text(aes(x=dateRep, y=deaths_ma7PP, label=formatC(deaths_ma7PP, format="f",digits=2)),
                  vjust=-0.25,size=3, colour="dark red")+
        scale_x_date(name="Date",labels = date_format("%d %b %Y"),expand = c(0, 0))+
        scale_y_continuous(name="New deaths per 100,000 ppl", labels=number_format(accuracy = 0.01))+
        facet_grid(countriesAndTerritories~.,scales="free") +
        theme_bw()+theme_classic()+theme(legend.position = "none")
    }
    
    
  }, height=function(){200*numCty()})  
  
    
  output$PlotnewCasesLog <- renderPlot({
    #Plot new Cases against cumulative cases;
    
    dfCases <- dfCases[dfCases$countriesAndTerritories %in% clist() & dfCases$Cases>=1 &dfCases$ma7>=1,]
    dfCases <- dfCases %>% mutate(label = if_else(dateRep == max(dateRep),
                                                  as.character(countriesAndTerritories), ""))
    dfCases[,c("dateRep")] <- NULL
    dfCases <- dfCases[!duplicated(dfCases),]
    ggplot(data=dfCases,
           aes(x=Cases, y=ma7, group=countriesAndTerritories, colour=countriesAndTerritories)) +
      scale_x_continuous(name="Cumulative number of cases", labels=comma,trans='log10',
                   limits = c(1,max(dfCases$Cases))) +
      scale_y_continuous(name="New cases", labels=comma,trans='log10',
                         limits = c(1,max(dfCases$ma7)))+
      annotation_logticks(sides = "lb")+
      geom_line(lwd=1, alpha=0.6)+
      geom_point(alpha=0.6)+
      geom_text_repel(aes(label = label),
                      alpha = 1,
                      nudge_x = 1)+
      theme_bw()+theme_classic()+theme(legend.position = "none")
  }) 
  
  output$PlotCasePPLog <- renderPlot({
    #Plot cumulative per million curves;
    
    dfCases$CasesPP <- dfCases$Cases/dfCases$popData2019*100000;
    dfCases <- dfCases[dfCases$countriesAndTerritories %in% clist() & dfCases$CasesPP>0.000001,]
    ggplot(data=dfCases,
           aes(x=dateRep, y=CasesPP, group=countriesAndTerritories, colour=countriesAndTerritories)) +
      scale_x_date(name="Date",labels = date_format("%d %b %Y"),expand = c(0, 0))+
      scale_y_continuous(name="Cumulative number of cases per 100,000 ppl", labels=comma,trans='log10',
                         limits = c(min(dfCases$CasesPP),max(dfCases$CasesPP)))+
      annotation_logticks(sides = "l")+
      geom_line(lwd=1, alpha=0.6)+
      geom_point(alpha=0.6)+
      geom_text_repel(aes(label = label),
                      alpha = 1,
                      nudge_x = 1)+
      theme_bw()+theme_classic()+theme(legend.position = "none")
  })
  
  output$PlotDeathPPLog <- renderPlot({
    #Plot cumulative per million curves;
    
    dfDeaths$DeathsPP <- dfDeaths$Deaths/dfDeaths$popData2019*100000;
    dfDeaths <- dfDeaths[dfDeaths$countriesAndTerritories %in% clist() & dfDeaths$Deaths>0.000001,]
    ggplot(data=dfDeaths,
           aes(x=dateRep, y=DeathsPP, group=countriesAndTerritories, colour=countriesAndTerritories)) +
      scale_x_date(name="Date",labels = date_format("%d %b %Y"),expand = c(0, 0))+
      scale_y_continuous(name="Cumulative number of deaths per 100,000 ppl", labels=comma,trans='log10',
                         limits = c(min(dfDeaths$DeathsPP),max(dfDeaths$DeathsPP)))+
      annotation_logticks(sides = "l")+
      geom_line(lwd=1, alpha=0.6)+
      geom_point(alpha=0.6)+
      geom_text_repel(aes(label = label),
                      alpha = 1,
                      nudge_x = 1)+
      theme_bw()+theme_classic()+theme(legend.position = "none")
  })
  
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
  
  #### Trajectory ####
  output$PlotCaseTraj <- renderPlot({
    
    dfcases0 <- dfcases[,c((dim(dfcases)[2]-dateperiodsinceStart()):dim(dfcases)[2])] 
    dfcases0 <- dfcases0 - dfcases0[,rep(1,dateperiodsinceStart()+1)]
    dfcases0 <- dfcases0 %>% replace_with_na_all(condition = ~.x < 10)
    dfcases0 <- apply(dfcases0, 1, sort); #extract only non-NA values;
    if(class(dfcases0)!="matrix"){
      dfcases0 <- t(sapply(dfcases0, '[', seq(max(sapply(dfcases0, length))))); #put values in matrix;
    } else {dfcases0 <- t(dfcases0)}
    #adjust starting number downward to 100 if exceed 100;
    for (i in 1:dim(dfcases0)[1]){
      if(all(is.na(dfcases0[i,]))){next}
      dfcases0[i,] <- dfcases0[i,]-min(dfcases0[i,],na.rm=TRUE)+casesthresh1;
    }
    #make dataframe join with countries label;
    dfcases0 <- data.frame(dfcases$countriesAndTerritories,dfcases0)
    names(dfcases0) <- c("CountriesAndTerritories",paste("Day",1:(dim(dfcases0)[2]-1)))
    dfcases0 <- dfcases0[!is.na(dfcases0$`Day 1`),] 
    
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
    df_temp <- subset(df_temp,CountriesAndTerritories %in% clist()) #c("Australia","Brazil"))#
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
  
  output$PlotDeathTraj <- renderPlot({
    
    dfdeaths0 <- dfdeaths[,c((dim(dfdeaths)[2]-dateperiodsinceStart()):dim(dfdeaths)[2])] 
    dfdeaths0 <- dfdeaths0 - dfdeaths0[,rep(1,dateperiodsinceStart()+1)]
    dfdeaths0 <- dfdeaths0 %>% replace_with_na_all(condition = ~.x < 5)
    dfdeaths0 <- apply(dfdeaths0, 1, sort); #extract only non-NA values;
    # for (i in 1:length(dfdeaths0)){ #add deathsthreshold if first number is larger;
    #   if(length(dfdeaths0[[i]])!=0 & dfdeaths0[[i]][1]>deathsthresh1){
    #     dfdeaths0[[i]] <- c(deathsthresh1,dfdeaths0[[i]])
    #   } else {next}
    # }
    if(class(dfdeaths0)!="matrix"){
      dfdeaths0 <- t(sapply(dfdeaths0, '[', seq(max(sapply(dfdeaths0, length))))); #put values in matrix;
    } else {dfdeaths0 <- t(dfdeaths0)}
    
    #adjust starting number downward to threshold if exceed threshold;
    for (i in 1:dim(dfdeaths0)[1]){
      if(all(is.na(dfdeaths0[i,]))){next}
      dfdeaths0[i,] <- dfdeaths0[i,]-min(dfdeaths0[i,],na.rm=TRUE)+deathsthresh1;
    }
    
    #make dataframe join with countries label;
    dfdeaths0 <- data.frame(dfdeaths$countriesAndTerritories,dfdeaths0)
    names(dfdeaths0) <- c("CountriesAndTerritories",paste("Day",1:(dim(dfdeaths0)[2]-1)))
    dfdeaths0 <- dfdeaths0[!is.na(dfdeaths0$`Day 1`),]
    
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
