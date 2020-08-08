#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(utils)
library(httr)
#library(xlsx)
library(readxl)
library(tidyr)
library(lubridate)

#setwd("C:/Users/Olivia Oh/Documents/Farid/COVID-19CumulativeCurves/Covid-19_Log_Cumulative_Curves_app/")
#DATADIR <- "data/";
DATAFILE <- "COVID-19-geographic-disbtribution-worldwide.xlsx";

#download the dataset from the ECDC website to a local temporary file
# GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))
#read the Dataset sheet into "R". The dataset will be called "data".
# df <- read.csv(tf, stringsAsFactors=FALSE)

# URL <- paste0("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx");#"https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",Sys.Date()-1,".xlsx")
# download.file(URL, destfile=DATAFILE, mode="wb")
# df <- read_excel(DATAFILE)
df <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM", stringsAsFactors=FALSE)
df$dateRep <- as.Date(df$dateRep,"%d/%m/%Y")

timestamp1 <- format(max(df$dateRep),"%d %B %Y")

# # read in HK data from file separately
df_HK <- read.csv("HK.csv", sep=",",header=TRUE,comment.char="", stringsAsFactors=FALSE)
df_HK$dateRep <- as.Date(df_HK$dateRep,"%d/%m/%Y")
timestamp2 <- format(max(df_HK$dateRep),"%d %B %Y")

df <- rbind(df[,c("dateRep","cases","deaths","countriesAndTerritories","popData2019")],df_HK[,c("dateRep","cases","deaths","countriesAndTerritories","popData2019")])

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

clist1<- sort(unique(dfCases$countriesAndTerritories[dfCases$Cases>1]))

#Get top 2 most affected countries;
temp <- dfCases[as.Date(dfCases$dateRep)==as.Date(max(df$dateRep)),]
temp <- temp[order(-temp$Cases),]
topcountries<- as.character(temp$countriesAndTerritories[1:2])


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("COVID-19 figures"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       # numericInput("casesthresh",
       #              "Minimum Number of Cases:",100,0,10000,10),
      dateRangeInput("date", label = "Date Range:", 
                     start = as_date(max(df$dateRep))-28, end = as_date(max(df$dateRep))),
      checkboxGroupInput("clist","Select Countries And Territories",
                          clist1,topcountries)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
      tabPanel("New Cases",
               h2("New cases over the selected date range"),
               h3("Number of new cases (orange bars), 28-day moving average (blue line) and 7-day moving average (red line)."),
               plotOutput("PlotnewCasesperday", height="auto"),
               h3("Number of new cases per 100,000 population (orange bars), 28-day moving average per 100,000 population (blue line) and 7-day moving average per 100,000 population (red line)."),
               plotOutput("PlotnewCasesperdayPP", height="auto"),
               p(paste0("Data source: ECDC  (as of ",timestamp1,
                        ") and HK DOH (as of ",timestamp2,
                        "). Get data at https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide and 
                        https://www.chp.gov.hk/files/pdf/local_situation_covid19_en.pdf"),
                 style = "font-size:10px")
               ),
      tabPanel("New Deaths",
               h2("New deaths over the selected date range"),
               h3("Number of new deaths (orange bars), 28-day moving average (blue line) and 7-day moving average (red line)."),
               plotOutput("PlotnewDeathsperday", height="auto"),
               h3("Number of new deaths per 100,000 population (orange bars), 28-day moving average per 100,000 population (blue line) and 7-day moving average per 100,000 population (red line)."),
               plotOutput("PlotnewDeathsperdayPP", height="auto"),
               p(paste0("Data source: ECDC  (as of ",timestamp1,
                        ") and HK DOH (as of ",timestamp2,
                        "). Get data at https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide and 
                        https://www.chp.gov.hk/files/pdf/local_situation_covid19_en.pdf"),
                 style = "font-size:10px")
      ),
      # tabPanel("Mobility",
      #          h2("New cases and mobility over the selected date range"),
      #          h2("Country-specific trend of number of new cases 5-day moving average (blue line) and percent change residential from baseline (red line)."),
      #          plotOutput("PlotnewCasesMobperday", height="auto"),
      #          p(paste0("Data source: ECDC  (as of ",timestamp1,
      #                   "), HK DOH (as of ",timestamp2,
      #                   ") and Google Mobility data. Get data at https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide and
      #                   https://www.chp.gov.hk/files/pdf/local_situation_covid19_en.pdf and 
      #                   https://google.com/covid19/mobility"),
      #            style = "font-size:10px")
      # ),
      tabPanel("Over time",
               h2("The following figures show the growth of cumulative number of cases and deaths over time."),
               h2("Cumulative number per 100,000 population (log scale)"),
               p("Countries with greater population size, have greater number susceptible to infection during an outbreak."),
               p("Plotting the cumulative number per 100,000 population scales outbreak to the country's population size."),
               h3("Cases"),
               plotOutput("PlotCasePPLog"),
               h3("Deaths"),
               plotOutput("PlotDeathPPLog"),
               h2("Annex: Other figures"),
               p("The following graphs show the trend over time."),
               h2("Cumulative number (log scale)"),
               h3("Cases"),
               plotOutput("PlotCaseLog"),
               h3("Deaths"),
               plotOutput("PlotDeathLog"),
               h2("Cumulative number (linear scale)"),
               h3("Cases"),
               plotOutput("PlotCaseLinear"),
               h3("Deaths"),
               plotOutput("PlotDeathLinear"),
               p(paste0("Data source: ECDC  (as of ",timestamp1,
                        ") and HK DOH (as of ",timestamp2,
                        "). Get data at https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide and 
                        https://www.chp.gov.hk/files/pdf/local_situation_covid19_en.pdf"),
                 style = "font-size:10px")
      ),
      tabPanel("Over total",
               h3("Case trajectories (new vs cumulative)"),
               p("During the exponential phase of the outbreak, 
                 the number of new cases is proportional to the existing number of cases."),
               p("For each country, the slope is comparable during the exponential phase of the outbreak."),
               p("Countries where the exponential spread of the disease have stopped are observed to drop from this slope."),
               plotOutput("PlotnewCasesLog"),
               p(paste0("Data source: ECDC  (as of ",timestamp1,
                        ") and HK DOH (as of ",timestamp2,
                        "). Get data at https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide and 
                        https://www.chp.gov.hk/files/pdf/local_situation_covid19_en.pdf"),
                 style = "font-size:10px")
      ),
      tabPanel("Trajectories",
               #textOutput("textStartDate"),
               h2(textOutput("textStartDate")),
               p("The steeper the gradient of the line the shorter the doubling time."),
               h3("Case trajectories (log scale)"),
               plotOutput("PlotCaseTraj"),
               h3("Death trajectories (log scale)"),
               plotOutput("PlotDeathTraj"),
               p(paste0("Data source: ECDC  (as of ",timestamp1,
                        ") and HK DOH (as of ",timestamp2,
                        "). Get data at https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide and 
                        https://www.chp.gov.hk/files/pdf/local_situation_covid19_en.pdf"),
                 style = "font-size:10px"),
               h3("Country Information"),
               tableOutput("Countrytable"),
               p("CasesStart: Cases until Start Date"),
               p("DeathsStart: Deaths until Start Date"),
               p("CFR: Case Fatality Rate"),
               p("CasesPP: Cases per 100,0000 population"),
               p("DeathsPP: Deaths per 100,0000 population"),
               #p("AveNewCases7: 7-day average new cases"),
               p("AveNewCases: 28-day average new cases"),
               p("AveNewCasesPP: 28-day average new cases per 100,0000 population"),
               #p("AveNewDeaths7: 7-day average new deaths"),
               p("AveNewCases7PP: 7-day average new cases per 100,0000 population"),
               p("AveNewDeaths: 28-day average new deaths"),
               p("AveNewDeathsPP: 28-day average new deaths per 100,0000 population")
               
               ),
      tabPanel("Watchlist",
               h2("Selected countries information"),
               tableOutput("CountrytableSelect"),
               p("AveNewCases: 28-day average new cases"),
               p("AveNewCasesPP: 28-day average new cases per 100,0000 population"),
               p("Trend: UP when 28-day average new cases increased since 7 days ago, DOWN when cases decreased"),
               p(paste0("Data source: ECDC  (as of ",timestamp1,
                        ") and HK DOH (as of ",timestamp2,
                        "). Get data at https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide and 
                        https://www.chp.gov.hk/files/pdf/local_situation_covid19_en.pdf"),
                 style = "font-size:10px"),
               h3("Countries information"),
               tableOutput("CountrytableWatch"),
               p("AveNewCases: 28-day average new cases"),
               p("AveNewCasesPP: 28-day average new cases per 100,0000 population"),
               p("AveNewCases7PP: 7-day average new cases per 100,0000 population"),
               p("Trend: UP when 28-day average new cases increased since 7 days ago, DOWN when cases decreased"),
               p(paste0("Data source: ECDC  (as of ",timestamp1,
                        ") and HK DOH (as of ",timestamp2,
                        "). Get data at https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide and 
                        https://www.chp.gov.hk/files/pdf/local_situation_covid19_en.pdf"),
                 style = "font-size:10px")
      )
    ) 
    )
  )
))
