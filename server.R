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

##Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.

server <- function(input, output) {
    
    output$nlinesBox <- renderValueBox({
        valueBox(
            nrow(avgDelay), "Train Lines in Paris Region", icon = icon("train"),
            color = "orange"
        )
    })
    
    output$progressBox <- renderValueBox({
        valueBox(
            paste0(round(mean(avgDelay$LineAvg),0),'%'), "Trains Delayed on Average", icon = icon("clock-o"),
            color = "red"
        )
    })
    
    output$approvalBox <- renderValueBox({
        valueBox(
            round(mean(delays$Ontime),1), "Passengers On Time for 1 Late", icon = icon("smile-o"),
            color = "olive"
        )
    })
    
    output$plot1 <- renderPlotly( {
        avgDelay %>% plot_ly(x=~Line,y=~LineAvg, color = ~LineAvg>mean(LineAvg),colors =c('#009933','#cc0000'),showlegend = FALSE) %>% layout(
            xaxis = list(         
                title = "Train Lines",      
                showgrid = F),     
            yaxis = list(           
                title = "% of Trains Delayed")    
        )
    })
    
    output$plot2 <- renderPlotly( {
        delays %>% filter(Line==input$select) %>% plot_ly(x= ~Date,mode = "markers") %>% layout(
            xaxis = list(         
                showgrid = F),     
            yaxis = list(           
                title = "% of Trains Delayed")    
        ) %>%add_markers(y = ~100-Punctuality,showlegend = FALSE) %>%
            add_lines(y = ~fitted(loess(100-Punctuality~as.numeric(Date), na.action = na.exclude)),
                      line = list(color = '#07A4B5'),
                      name = "Loess Smoother", showlegend = FALSE)
    })

}
