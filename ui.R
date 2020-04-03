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
library(tidyr)

#setwd("C:/Users/Olivia Oh/Documents/Farid/COVID-19CumulativeCurves/Covid-19_Log_Cumulative_Curves_app/")
#DATADIR <- "data/";
#DATAFILE <- "COVID-19-geographic-disbtribution-worldwide.xlsx";

#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))

#read the Dataset sheet into "R". The dataset will be called "data".
df <- read.csv(tf, stringsAsFactors=FALSE)
# read in HK data from file separately
df_HK <- read.csv("HK.csv", sep=",",header=TRUE,comment.char="", stringsAsFactors=FALSE)
df <- rbind(df,df_HK)
df$dateRep <- as.Date(df[,1],"%d/%m/%Y")

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

clist1<- sort(unique(dfCases$countriesAndTerritories[dfCases$Cases>100]))

#Get top 5 most affected countries;
temp <- dfCases[dfCases$dateRep==max(df$dateRep),]
temp <- temp[order(-temp$Cases),]
topcountries<- as.character(temp$countriesAndTerritories[1:5])


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Countries' cumulative number of cases"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       # numericInput("casesthresh",
       #              "Minimum Number of Cases:",100,0,10000,10),
       checkboxGroupInput("clist","Select Countries And Territories (Countries with fewer than 100 cases excluded)",
                          clist1,topcountries)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h2("Trajectories"),
      h3("Case trajectories (log scale)"),
      plotOutput("PlotCaseTraj"),
      h3("Death trajectories (log scale)"),
      plotOutput("PlotDeathTraj"),
      h2("Trends over time"),
      h3("Cumulative number of cases (log scale)"),
      plotOutput("PlotCaseLog"),
      h3("Cumulative number of cases (linear scale)"),
      plotOutput("PlotCaseLinear"),
      h3("Cumulative number of deaths (log scale)"),
      plotOutput("PlotDeathLog"),
      h3("Cumulative number of deaths (linear scale)"),
      plotOutput("PlotDeathLinear"),
      p(paste0("Data source: ECDC and HK DOH (as of ",format(max(df$dateRep),"%d %B %Y"),
               "). Get data at https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide and 
               https://www.chp.gov.hk/en/features/102465.html"),
        style = "font-size:10px")
    )
  )
))
