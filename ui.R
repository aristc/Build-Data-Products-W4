library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)

delays<-read.csv("./data/sncf-retards-idf.csv",header=TRUE,stringsAsFactors =TRUE,fileEncoding="latin1")
names(delays)[4:7]<-c("Line","Name","Punctuality","Ontime")
avgDelay<-delays %>% group_by(Line,Name) %>% summarize(LineAvg=100-mean(Punctuality,na.rm=TRUE))

header <- dashboardHeader(title = "Train delays in Paris Region Over the Last 3 Years", titleWidth = 600)

body <-  dashboardBody(
                fluidRow(
                    valueBoxOutput("nlinesBox"),
                    valueBoxOutput("progressBox"),
                    valueBoxOutput("approvalBox")
                        ),

                fluidRow(
                    box(
                     title = "Average Delays For Every Line",
                     h5("Color Code:"),
                     h5("- Red if Above Average"),
                     h5("- Green if Below Average"),
                     width = 6, height="auto",solidHeader = TRUE, status = "primary",
                     plotlyOutput("plot1")
                     ),
                    box(
                     title = "See Trend Over Time for Selected Line", width = 6, height="auto",solidHeader = TRUE, status = "primary",
                     selectInput("select", label = h5("Choose Line"),
                                  choices = avgDelay$Line),
                     verbatimTextOutput("value"),
                     plotlyOutput("plot2")
                     ),
                    fluidRow(
                        box(h5("Source: SNCF Open Data Accessed on 2016-12-04"))
                    )
))

dashboardPage(header, dashboardSidebar(disable=TRUE), body)
