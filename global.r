library(dplyr)
library(readr)
library(odbc)
library(RMySQL)
library(RODBC)


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
            filter (Element %in% c('Feed','Food')) %>% group_by(Element) %>% summarise(n = n()) %>% 
            arrange(desc(n)) %>%
            mutate(lab.ypos = cumsum(n) - 0.5*n)
            
  alimentation_proportion <<- alimentation_proportion %>% mutate(percent = round((n * 100 )/sum(alimentation_proportion$n),digits=0) )
    
  # Calculer (pour chaque pays et chaque produit) la disponibilité alimentaire en kcal puis en kg de protéines 
  disp_alim_per_item <- dispo_alim %>% inner_join(population %>% select(c("Year","Area Code","Value")) ,by=c("Year","Area Code"))
  
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

