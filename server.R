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
library(leaflet)
library(plotly)
library(Amelia)
library(geojsonio)
library(rgdal)
library(binr)
library(ggthemes)
library(scales)
library(ggfortify)
library(rjson)
# Define server logic required to draw a histogram


shinyServer(function(input, output) {
    start_prog()
    
    
    output$LinePlot1 <- renderPlot({
        ggplot(data = populationMondiale2(), aes(x = Year, y = n))+
            geom_line()+
            labs(title = "Évolution de la population mondiale de 2014 à 2017", # plot title
                 x = "Dates", # x axis label
                 y = "Population (milliard) " # y axis label
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
        alim_proportion()
        ggplot(data = alimentation_proportion ,aes(x = '',y = n,fill = Element)) +
            geom_bar(width = 1, stat = "identity", color = "white") +
            coord_polar("y", start = 0)+
            geom_text(aes(y=lab.ypos,label = percent(percent,scale = 1)), color = "white")+
            theme_void()+
            ggtitle('Proportion des céréales pour l’alimentation animale')
    })
    
    output$barChartCal <- renderPlot({
        newcal <- cal %>% distinct(Item,.keep_all=TRUE) %>% arrange(desc(cal)) %>% head(5)
        
        ggplot(newcal, aes(x = Item, y=cal-8500, fill= Item)) + # set up the plot
            geom_bar(stat="identity") # add the barpot
    })
    
    output$barChartProt <- renderPlot({
        newprot <- prot  %>% head(5)
        
        ggplot(newprot, aes(x = Item, y=protein_ratio, fill= Item)) + # set up the plot
            geom_bar(stat="identity") # add the barpot
    })
    
    output$smooth1 <- renderPlot({
        new <- dispos_veggies
        
        ggplot(new,aes(Year,dispo_cal))+geom_point()+geom_smooth()
    })
    
    output$smooth2 <- renderPlot({
        new <- dispos_veggies
        
        ggplot(new,aes(Year,dispo_protein))+geom_point()+geom_smooth()
    })
    
    output$barplotkcal <- renderPlot({
        data <- only_veggies_ratio_final
        
        condition <- c(data$pourcentage_de_gens_kcal,data$pourcentage_de_gens_protein)
        
        
        
        
        # ggplot(data, aes(x=Year, y=pourcentage_de_gens_kcal, fill=pourcentage_de_gens_protein)) +
        #     geom_bar(stat="identity", color="black", position=position_dodge())+
        #     theme_minimal()
        # 
        
        ggplot(data, aes(x = Year,y= pourcentage_de_gens_kcal - 120)) + geom_bar(stat="identity") # add the barpot
        
    })
    
    output$barplot2 <- renderPlot({
        data <- only_veggies_ratio_final
        
        condition <- c(data$pourcentage_de_gens_kcal,data$pourcentage_de_gens_protein)
        
    
        # ggplot(data, aes(x=Year, y=pourcentage_de_gens_kcal, fill=pourcentage_de_gens_protein)) +
        #     geom_bar(stat="identity", color="black", position=position_dodge())+
        #     theme_minimal()
        # 
        
        ggplot(data, aes(x = Year,y= pourcentage_de_gens_kcal - 120)) + geom_bar(stat="identity") # add the barpot
        
    })
  
    output$gdp <- renderPlot({
        # world <- geojsonio::geojson_read("json/world.geojson", what="sp")
        # temp <-  names(undernutrition_numbers_final)[names(undernutrition_numbers_final) == "Area"] <- "NAME"
        # 
        # newWorld <- left_join(x=world, y=undernutrition_numbers_final,by = "name")
        #     
        # leaflet(world)%>%
        #         addProviderTiles("MapBox", options = providerTileOptions(
        #             name = "mapbox.light",
        #             accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% addPolygons(~pal(undernutrition_numbers_final$people))
        # 
        # 
        #     names(world)
        #     head(world,1)
        
       
        # 
        # 
        # 
        # m <- leaflet(world) %>% addTiles()
        # b <- bins(undernutrition_numbers_final$people,target.bins = dim(undernutrition_numbers_final)[1],max.breaks=dim(undernutrition_numbers_final)[1])
        # labels <- sprintf(
        #     "<strong>%s</strong><br/>peoplee : millions",
        #     undernutrition_numbers_final$Area, undernutrition_numbers_final$people
        # ) %>% lapply(htmltools::HTML)
        # 
        # 
        # 
        # 
        # bins <- as.list(b$xval)
        # pal <- colorBin("YlOrRd", domain = undernutrition_numbers_final$people, bins = bins)
        # m %>% addPolygons(
        #     fillColor = ~pal(undernutrition_numbers_final$people),
        #     weight = 2,
        #     opacity = 1,
        #     color = "white",
        #     dashArray = "3",
        #     fillOpacity = 0.7,highlight = highlightOptions(
        #         weight = 5,
        #         color = "#666",
        #         dashArray = "",
        #         fillOpacity = 0.7,
        #         bringToFront = TRUE),
        #     label = labels,
        #     labelOptions = labelOptions(
        #         style = list("font-weight" = "normal", padding = "3px 8px"),
        #         textsize = "15px",
        #         direction = "auto")
        # )
        
        # world <- geojsonio::geojson_read("json/world.geojson", what="sp")
        # 
        # fig <- plot_ly() 
        # fig <- fig %>% add_trace(
        #     type="choroplethmapbox",
        #     geojson=world,
        #     #locations=locs,
        #     #z=color_by,
        #     #colorscale=fill,
        #     reversescale=TRUE,
        #     featureidkey="properties.NAME",
        #     zmin=25,
        #     zmax=125,
        #     marker=list(line=list(
        #         width=0),
        #         opacity=0.5
        #     )
        # )
        # 
        # fig <- fig %>% colorbar(title=color_bar)
        # fig <- fig %>% layout(
        #     title=title,
        #     mapbox=list(
        #         style="carto-positron",
        #         zoom =10.5,
        #         center=list(lon= 2.35, lat=48.86))
        #     
        # )
        # fig
        
        gdp <- read.csv("GDP.csv") %>% filter(Year == 2014)
        names(gdp)[names(gdp) == "Entity"] <- "Area"
        names(gdp)[names(gdp) ==  "Real.GDP.per.capita.in.2011US...2011.benchmark..Maddison.Project.Database..2018.."] <- "gdp"
        undernutrition_numbers_final$Year <- "2014"
        gdp$Year <- as.character(gdp$Year)
        gdp_undernutrition <- left_join(x=gdp,y=undernutrition_numbers_final[!is.na(undernutrition_numbers_final$people),],by=c("Year","Area")) %>%  filter(people < 30)
        pop <- population_table[population_table$Year == 2014,]
        pop$Year <- as.character(pop$Year)
        gdp_undernutrition <- left_join(x=gdp_undernutrition,y=pop,by=c("Area","Year"))
        ggplot(gdp_undernutrition,aes(x = gdp,y=people,size=Value)) +geom_point(alpha=0.3) + scale_size(range = c(.1, 24), name="Population (M)") +theme_light()
        
        })
    output$PieChart2 <- renderPlot({
        
        ggplot(data = total_ratio_undernutrition_final ,aes(x = '',y = n,fill = Element)) +
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
    
    output$map <- renderPlotly({
        world <- rjson::fromJSON(file="json/world.geojson")
        under_temp  <- read.csv("data_fao/undernourishment.csv")
        under_temp$ratios <- under_temp$Suite.of.Food.Security.Indicators...Prevalence.of.undernourishment..percent...3.year.average....210041...Value...6121....
        under_temp <- under_temp[!is.na(under_temp$ratios),] %>% group_by(Entity) %>% summarise(population=mean(ratios))
        
        under_temp$NAME <- under_temp$Entity
       
        
        fig <- plot_ly() 
        fig <- fig %>% add_trace(
            type="choroplethmapbox",
            geojson=world,
            locations=under_temp$NAME,
            z=under_temp$population,
            colorscale="YlOrRd",
            reversescale=TRUE,
            featureidkey="properties.NAME"
        )
        fig <- fig %>% colorbar(title = "population ratio")
        fig <- fig %>% layout(
            mapbox=list(
                style="carto-positron",
                zoom =1,
                height = "100%",
                width = "100%",
                center=list(lat=30.3753, lon=69.3451))
        )
        
        fig
      
        
    })
    
    
    output$map2 <- renderPlotly({
        world <- rjson::fromJSON(file="json/world.geojson")
        productions <- pivot_final[!is.na(pivot_final$Production),] %>% group_by(Area) %>% summarise(production=mean(Production))
        names(productions)[names(productions) == "Area"] <- "NAME"
        productions$NAME[productions$NAME == "United States of America"] <- "United States"
        
        fig <- plot_ly() 
        fig <- fig %>% add_trace(
            type="choroplethmapbox",
            geojson=world,
            locations=productions$NAME,
            z=productions$production,
            colorscale="Greens",
            reversescale=TRUE,
            featureidkey="properties.NAME"
        )
        fig <- fig %>% colorbar(title = "Production")
        fig <- fig %>% layout(
            mapbox=list(
                style="carto-positron",
                zoom =1,
                center=list(lat=30.3753, lon=69.3451))
        )
        fig
        
        
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
