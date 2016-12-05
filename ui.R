library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)

delays<-read.csv("sncf-retards-idf.csv")
names(delays)[4]<-c("Line")
names(delays)[6]<- c("Punctuality")
names(delays)[7]<-c("Ontime")
avgDelay<-delays %>% group_by(Line) %>% summarize(LineAvg=100-mean(Punctuality,na.rm=TRUE))
avgDelay<-avgDelay %>% mutate(vsTotalAvg=LineAvg-mean(LineAvg))

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
                     plotlyOutput("plot2")
                     ),
                    fluidRow(
                        box(h5("Source: SNCF Open Data"))
                    )
))

dashboardPage(header, dashboardSidebar(disable=TRUE), body)
