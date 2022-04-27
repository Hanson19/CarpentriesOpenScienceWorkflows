#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Dygraphs Demo
# Coder: Nate Jones (cnjones7@ua.edu)
# Date: 4/27/2022
# Purpose: Demo interactive dygraphs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Setup environment -------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
remove(list=ls())

#install requisite libraries
#install.packages(c('dplyr', 'lubridate', 'xts', 'dygraphs', 'dataRetrieval', 'htmlwidgets'))

#load packages
library(dplyr)
library(lubridate)
library(dygraphs)
library(xts)
library(dataRetrieval)
library(htmlwidgets)

#Download and organize flow data
df<-readNWISdv(siteNumbers = '02446500', 
               parameterCd = '00060', 
               startDate = '2014-10-01', 
               endDate = '2019-09-30')

#Tidy data  a it
df<-df %>% 
  select(date = Date, 
         flow = X_00060_00003) %>% 
  mutate(date = ymd(date))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create dygraphs function ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dygraph_ts_fun<-function(df){
  
  #format data
  df_xts<-df #%>% na.omit() 
  df_xts<-xts(df_xts, order.by=df_xts$date)
  df_xts<-df_xts[,-1]
  
  #Plot
  dygraph(df_xts) %>%
    dyRangeSelector() %>%
    dyLegend() %>%
    dyOptions(strokeWidth = 1.5) %>%
    dyOptions(labelsUTC = TRUE) %>%
    dyHighlight(highlightCircleSize = 5,
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = FALSE) %>%
    dyAxis("y", label = "Variable")
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Plot --------------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Plot in dygraphs
dygraph_ts_fun(df)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Host on git ----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: Create git repo

#Step 2: Within git repo, create docs folder

#Step 3: Go to repo settings>Pages and make the docs folder the source

#Step 4: Save the dygraphs html to the docs folder

#Save graph
m<-dygraph_ts_fun(df)
htmlwidgets::saveWidget(m, 'docs//plot.html')

#Step 5: Push to github

#step 6: Go to github page!  [yourusername].github.io/CarpentriesOpenScienceWorkflows/plot.html
#https://floodhydrology.github.io/CarpentriesOpenScienceWorkflows/plot.html