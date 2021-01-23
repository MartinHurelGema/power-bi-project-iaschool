library(dplyr)
library(readr)
library(odbc)
library(RMySQL)
library(RODBC)
library(tidyr)

importData <<- function(){
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    # importing dataFrames
    if(!exists('vegetal')) vegetal <<- read_csv('data_fao/vegetal_product.csv')
    if(!exists('animal')) animal <<- read_csv('data_fao/animal_product.csv')
    if(!exists('undernourished')) undernourished <<- read_csv('data_fao/undernourished.csv')
    if(!exists('cereals')) cereals <<- read_csv('data_fao/cereals_product.csv')
    if(!exists('population')) population <<- read_csv('data_fao/population.csv')
    }

clean_data <- function(){
  # création de la table pivot
  vegetal$origin="vegetal"
  animal$origin="animal"
  All_products <<- rbind(vegetal,animal)
  
  # population mondiale
  population_table_tmp <- population[!population$`Area Code` %in% c(41,96,214,128),] 
  population_table <<- population_table_tmp %>% select(Area,`Area Code`,Value,Year)
  population_table$Value <- population_table$Value*1000
  
  products <- All_products[,!names(All_products) %in% c("Domain Code","Domain")]
  pivoted_table <- tidyr::pivot_wider(products,id_cols=c("Area Code","Area","Year","Item Code","Item","origin"),names_from= "Element",values_from = Value)
  
  cereals <- cereals[,!names(cereals) %in% c("Domain Code","Domain")]
  codes <- cereals %>% distinct(`Item Code`)
  pivoted_table <- pivoted_table %>% mutate(is_cereal = `Item Code` %in% pull(codes))
  
  
  Cereal_consumption <- pivoted_table %>% filter(is_cereal == TRUE)
  feed_sum <- sum(Cereal_consumption$Feed,na.rm = TRUE)
  #feed_sum/(sum(Cereal_consumption$Food,na.rm = TRUE)+feed_sum)
  
  
  
  
  population <<- population %>% mutate(population_value = Value*strtoi(gsub("[^0-9.]","", Unit))/1000000000)
  
  dispo_alim <<- rbind(animal %>% mutate(origin = 'animal'),vegetal %>% mutate(origin = 'vegetal')) %>% 
                
                select(`Item Code`,Item,`Area Code`,Area,Value,Year,Element,origin) %>% 
                  
                filter(Element %in% c(
                  'Food supply quantity (kg/capita/yr)',
                  'Food supply (kcal/capita/day)',
                  'Protein supply quantity (g/capita/day)',
                  'Fat supply quantity (g/capita/day)')) %>% 
                
                spread(key = Element,value = Value)
              
                
  cereals_list <<- rbind(animal %>% mutate(origin = 'animal'),vegetal %>% mutate(origin = 'vegetal')) %>% 
        select(`Item Code`,Item,`Area Code`,Area,Year,origin) %>%  
        mutate(is_cereal = ifelse(test = `Item Code` %in% c(2511,2805,2513,2514,2517,2520,2515,2516,2518),
                              yes = TRUE,
                              no = FALSE) )
  
  
  #Proportion des céréales pour l’alimentation animale
  alimentation_proportion  <<- rbind(animal %>% mutate(origin = 'animal'),vegetal %>% mutate(origin = 'vegetal')) %>%
            filter (Element %in% c('Feed','Food')) %>% group_by(Element) %>% summarise(n = sum(Value)) %>% 
            arrange(desc(n)) %>%
            mutate(lab.ypos = cumsum(n) - 0.5*n)
            
  alimentation_proportion <<- alimentation_proportion %>% mutate(percent = round((n * 100 )/sum(alimentation_proportion$n),digits=0) )
    
  # Calculer (pour chaque pays et chaque produit) la disponibilité alimentaire en kcal puis en kg de protéines 
  disp_alim_per_item <<- dispo_alim %>% inner_join(population %>% select(c("Year","Area Code","Value")) ,by=c("Year","Area Code"))
  
  # afficher le  10 des pays qui ont la meuilleur disponibilité alimentaire
  
  
  # afficher les 10 pays qui ont la pire disponibilitée alimentaire 
  
  }
dataBaseConnect <<- function(driver,server,database,usr,pwd,port){
  
  con_tmp <- dbConnect(MySQL(),
                   Driver = driver,# MySQL
                   host = server, #localhost
                   dbname = 'mysql',
                   user=usr, #root
                   password=pwd, 
                   Port = port) #3306
  
  dbGetQuery(con_tmp, 'CREATE DATABASE IF NOT EXISTS fao')
  
  con <- dbConnect(MySQL(),
                   Driver = driver,# MySQL
                   host = server, #localhost
                   dbname = database, # fao
                   user=usr, #root
                   password=pwd,
                   Port = port) #3306

  
  return(con)
  }

dataFrameToTable <- function(con,TableName,data,replace=FALSE){
  if (!dbExistsTable(con, TableName) || (replace==TRUE) ){
    dbWriteTable(con, name = TableName, value = data, row.names = FALSE) 
  }
}

insertDatabase <<- function(){
  
  con <<- dataBaseConnect("MySQL","localhost","fao","root","",3306)
  dataFrameToTable(con,'animal',animal)
  dataFrameToTable(con,'cereals',cereals)
  dataFrameToTable(con,'population',population)
  dataFrameToTable(con,'undernourished',undernourished)
  dataFrameToTable(con,'vegetal',vegetal)
  dataFrameToTable(con,'dispo_alim',dispo_alim)
  }

start_prog <<- function(){
    importData()
    clean_data()
    insertDatabase()
} 

stop_connection <<- function(){
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) 
    dbDisconnect(con)
}

