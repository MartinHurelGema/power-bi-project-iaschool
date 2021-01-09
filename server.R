#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("ggplot2")
source('dataManipulation.R')
source('global.r')
library(dplyr)
library(readr)
library(odbc)
library(RMySQL)
library(RODBC)
library(tidyverse)
library(scales)

# Define server logic required to draw a histogram


shinyServer(function(input, output) {
    start_prog()
    
    
    output$LinePlot1 <- renderPlot({
        ggplot(data = populationMondiale(), aes(x = Year, y = s))+
            geom_line()+
            labs(title = "évolution de la poppulation mondiale de 2014 a 2017", # plot title
                 x = "dates", # x axis label
                 y = "poppulation (billion) " # y axis label
            )
    })
    output$PieChart <- renderPlot({
        ggplot(data = alimentation_proportion ,aes(x = '',y = n,fill = Element)) + 
            geom_bar(width = 1, stat = "identity", color = "white") +
            coord_polar("y", start = 0)+
            geom_text(aes(y=lab.ypos,label = percent(percent,scale = 1)), color = "white")+
            theme_void()+
            ggtitle('Proportion des céréales pour l’alimentation animale')
    })
    onSessionEnded(function(){
        stop_connection()    
    })
    
    # output$distPlot <- renderPlot({
    #     
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # 
    # })
    # output$b <- renderText({input$a})
})