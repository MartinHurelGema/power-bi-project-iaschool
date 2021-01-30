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
library(DT)
library(gridExtra)
library(grid)
# Define server logic required to draw a histogram


shinyServer(function(input, output) {
    start_prog()
    
    
    output$LinePlot1 <- renderPlot({
        ggplot(data = populationMondiale2(), aes(x = Year, y = n))+
            geom_line()+
            labs(title = "évolution de la poppulation mondiale de 2014 a 2017", # plot title
                 x = "dates", # x axis label
                 y = "poppulation (billion) " # y axis label
            )
    })
    
    output$Table1 <- renderTable(
        #print(input$Area)
        #print(dispo_alimentaion(input$Area))
        #ggplot(data = dispo_alimentaion(input$Area))
        dispo_alimentaion(input$Area)
    )
    
    output$Table2 <- renderTable(
        Cereal_consumption %>% select("Area Code","Area","Item Code", "Item", "Year", "origin", "is_cereal") %>% head(20)
    )
    
    output$Table3 <- renderTable(
        pivot_final %>% 
            head(20)
    )
    
    
    output$Table4 <- renderTable(
           cal 
    )
    
    output$Table5 <- renderTable(
          prot
          
    )
    
    output$Table6 <- renderTable(
        cal %>% distinct(Item,.keep_all=TRUE) %>% arrange(desc(cal)) %>% head(5)
    )
    
    output$Table7 <- renderTable(
        
        prot %>% head(5)
    )
    
    output$Table8 <- renderTable(
        
        dispos_veggies
    )
    
    output$Table9 <- renderTable(
        
        only_veggies_ratio_final
    )
    
    output$Table10 <- renderTable(
        dispos_no_waste_final
    )
    
    output$Table11 <- renderTable( 
        total_ratio_undernutrition_final
    )
    
    output$Table12 <- renderTable(
        undernutrition_numbers_final #3.2
    )
    
    output$Table13 <- renderTable(
        export_products_final   #3.3
    )
    
    output$Table14 <- renderTable(
        importations_final
    )
    
    output$Table15 <- renderTable(
        importation_ratio_1_final #3.5.1
    )
    
    output$Table16 <- renderTable(
        importation_ratio_2_final #3.5.2
    )
    
    output$Table17 <- renderTable(
        importation_ratio_ouses_disp_3_final#3.6.1
    )
    
    output$Table18 <- renderTable(
        importation_ratio_food_feed_3_final #3.6.2
    )
    
    output$Table19 <- renderTable(
        pivot_table #3.7
    )
    
    # dataset =  prot
    
    output$Table20 <- renderPlot({
        ggplot(pivot_final,aes(origin,calories_kg,fill=origin)) + geom_violin()
    })
                                   
    output$Table21 <- renderPlot({
        
        #ggplot(only_veggies_ratio_final, aes(x=Year, y=pourcentage_de_gens_kcal*population/100)) + geom_bar(stat="identity") 
        
        
        # ggplot(only_veggies_ratio_final, aes(x = population,"", fill=factor())) + 
        #     geom_bar(width = 1) +
        #     theme(axis.line = element_blank(), 
        #           plot.title = element_text(hjust=0.5)) + 
        #     labs(fill="class", 
        #          x=NULL, 
        #          y=NULL, 
        #          title="Pie Chart of class", 
        #          caption="Source: mpg")+ coord_polar(theta = "y", start=0)
        #     
        
        # ggplot(data = only_veggies_ratio_final , aes(x ="" ,y=population, fill = population )) + 
        #             geom_bar(width = 1, stat = "identity", color = "white") +
        #              coord_polar("y", start = 0)+
        #              theme_void()+
        #             ggtitle('Proportion des céréales pour l’alimentation animale')
    })
    
    output$PieChart <- renderPlot({
        alim_proportion
        ggplot(data = alimentation_proportion ,aes(x = '',y = n,fill = Element)) +
            geom_bar(width = 1, stat = "identity", color = "white") +
            coord_polar("y", start = 0)+
            geom_text(aes(y=lab.ypos,label = percent(percent,scale = 1)), color = "white")+
            theme_void()+
            ggtitle('Proportion des céréales pour l’alimentation animale')
    })
    
    
    
    
    
    # output$PieChart <- renderPlot({
    #     ggplot(data = alimentation_proportion ,aes(x = '',y = n,fill = Element)) +
    #         geom_bar(width = 1, stat = "identity", color = "white") +
    #         coord_polar("y", start = 0)+
    #         geom_text(aes(y=lab.ypos,label = percent(percent,scale = 1)), color = "white")+
    #         theme_void()+
    #         ggtitle('Proportion des céréales pour l’alimentation animale')
    # })
    # output$tbl = renderTable({data <- disp_alim_per_item %>% select(`Fat supply quantity (g/capita/day)`,
    #                                                                 `Food supply (kcal/capita/day)`,
    #                                                                 `Food supply quantity (kg/capita/yr)`,
    #                                                                 `Protein supply quantity (g/capita/day)`) %>% 
    #     summary() %>% as.data.frame.matrix()} )
    # if("Area" %in% output$Area){
    #     output$tbl = renderTable({data <- disp_alim_per_item %>% select(`Fat supply quantity (g/capita/day)`,
    #                                                                     `Food supply (kcal/capita/day)`,
    #                                                                     `Food supply quantity (kg/capita/yr)`,
    #                                                                     `Protein supply quantity (g/capita/day)`) %>% 
    #         summary() %>% as.data.frame.matrix()} )
    #     
    # }
    # else{
    #     output$tbl = renderTable({data <- disp_alim_per_item %>% filter(Area %in% output$Area) %>% select(`Fat supply quantity (g/capita/day)`,
    #                                                                     `Food supply (kcal/capita/day)`,
    #                                                                     `Food supply quantity (kg/capita/yr)`,
    #                                                                     `Protein supply quantity (g/capita/day)`) %>% 
    #         summary() %>% as.data.frame.matrix()} )
    #     
    # }
    # 
    
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
