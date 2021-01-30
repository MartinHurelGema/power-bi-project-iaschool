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
  population_table_tmp <- population[!(population$`Area Code` %in% c(41,96,214,128)),] 
  population_table <- population_table_tmp %>% select(Area,`Area Code`,Value,Year)
  population_table$Value <- population_table$Value*1000
  
  
  #Country Initialization
  countryInit <<- population_table %>% select(Area)
  
  
  products <<- All_products[,!names(All_products) %in% c("Domain Code","Domain")]
  
  #pivoted = partout !!!! 
  pivoted_table <<- tidyr::pivot_wider(products,id_cols=c("Area Code","Area","Year","Item Code","Item","origin"),names_from= "Element",values_from = Value)
  
  cereals <<- cereals[,!names(cereals) %in% c("Domain Code","Domain")]
  codes <<- cereals %>% distinct(`Item Code`) 
  
  #2.1.1 & 2.2.1
  pivoted_table <<- pivoted_table %>% mutate(is_cereal = `Item Code` %in% pull(codes))
  
  
  #2.1.1
  Cereal_consumption <<- pivoted_table %>% filter(is_cereal == TRUE)
  feed_sum <- sum(Cereal_consumption$Feed,na.rm = TRUE)
  #2.1.2
  Cereal_percent <- feed_sum/(sum(Cereal_consumption$Food,na.rm = TRUE)+feed_sum)
  
  
   
  
  populations <- population_table %>% select(`Area Code`,Year,Value)

  #merge population with main table
  pivot_new <- pivoted_table[!(pivoted_table$`Area Code` %in% c(41,96,214,128)),]
  pivot_new <- left_join(x=pivot_new,y=populations,by=c("Area Code","Year"))
 
  
  pivot <- pivot_new[( (pivot_new$Food > 0)  & (pivot_new$`Food supply quantity (kg/capita/yr)` != 0)  & (pivot_new$`Food supply (kcal/capita/day)` != 0) & !is.na(pivot_new$`Food supply (kcal/capita/day)`) & (!is.na(pivot_new$`Food supply quantity (kg/capita/yr)`)) ),]
  
  pivot <- pivot %>% mutate(kg_capita_day = pivot$`Food supply quantity (kg/capita/yr)`/365 )
  pivot <- pivot %>% mutate(kcal_for_kg= pivot$`Food supply (kcal/capita/day)`/pivot$kg_capita_day)
  
  
  # les plus caloriques
  #2.3.1
  calories <- pivot %>% group_by(Item,`Item Code`,Area)  %>% summarise(cal = mean(kcal_for_kg))
  #2.2.2 ratio??? à calculer???? faite votre travail !!!
  
  cal <<- calories %>% filter(cal < 10000) %>% group_by(Item) %>% summarise(cal = mean(cal))
  pivot <-left_join(x=pivot,y=cal,by=c("Item"))
  pivot$kcal_for_kg <- NULL
  
  #rename columns 
  names(pivot)[names(pivot) == "cal"] <- "calories_kg"
  names(pivot)[names(pivot) == "Value"] <- "population_size"
  
  
  
  
  # les plus protéinés
  #2.2.3 manque ratio par pays
  prot <- pivot %>% group_by(Item,Area) %>% summarise(protein_ratio = `Protein supply quantity (g/capita/day)`/(kg_capita_day*1000) )
  prot <- prot %>% filter(protein_ratio < .42)
  prot <- prot %>% group_by(Item) %>% summarise(protein_ratio = mean(protein_ratio)) %>% arrange(desc(protein_ratio))
  
  pivot <- left_join(x=pivot,y=prot,by="Item")
  
  
  # disponibilité mondiale en végéteaux
  #2.4.1
  dispos_veggies <<- pivot %>% filter(origin == "vegetal") %>% group_by(Year)  %>% summarise(dispo_protein = sum(Food*1000000*protein_ratio) ,dispo_cal=sum(Food*1000000*calories_kg))
  
  ppl_year <- population_table %>% group_by(Year) %>% summarise(avg = sum(Value))
  
  only_veggies_ratio <- left_join(x=dispos_veggies,y=ppl_year, by="Year" )
  names(only_veggies_ratio)[names(only_veggies_ratio) == "avg"] <- "population"
  
  #2.4.2 + calcul à faire
  only_veggies_ratio <- only_veggies_ratio %>% mutate(kcal_on_veggies_capita = only_veggies_ratio$dispo_cal/only_veggies_ratio$population)
  only_veggies_ratio <<- only_veggies_ratio %>% mutate(protein_from_veggies_capita=only_veggies_ratio$dispo_protein/only_veggies_ratio$population)
  
  
  only_veggies_ratio <- only_veggies_ratio %>% mutate(pourcentage_de_gens_kcal = kcal_on_veggies_capita/365/2000*100)
  only_veggies_ratio <- only_veggies_ratio %>% mutate(pourcentage_de_gens_protein = protein_from_veggies_capita*1000/365/40*100)
  
  #no waste
  #2.4.2
  pivot <<- pivot %>% mutate(all_to_food=Food+Feed+Losses)
  
  
  #2.4.3
  dispos_no_waste <-  pivot[!is.na(pivot$Food) & !is.na(pivot$Feed) & !is.na(pivot$Losses),] %>% group_by(Year)  %>% summarise(dispo_protein = sum(all_to_food*1000000*protein_ratio) ,dispo_cal=sum(all_to_food*1000000*calories_kg))
  dispos_no_waste <- left_join(x=dispos_no_waste,y=ppl_year, by="Year" )
  names(dispos_no_waste)[names(dispos_no_waste) == "avg"] <- "population_size"
  dispos_no_waste <- dispos_no_waste %>% mutate(population_nourrie_kcal_p=dispo_cal/population_size/365/2000*100)
  dispos_no_waste <- dispos_no_waste %>% mutate(population_nourrie_kcal_prct=dispo_protein/population_size*1000/365/45*100)
  
  # 2.2.4
  # alim_for_all <- pivot[!is.na(pivot$Losses),]
  # alim_for_all <- alim_for_all[!is.na(alim_for_all$calories_kg),]
  # alim_for_all <- alim_for_all[!is.na(alim_for_all$Food),]  %>% group_by(Year.x) %>% summarise(disp_food = sum((Food*1000000-Losses*1000000)*calories_kg))
  # alim_for_all <- alim_for_all %>% rename(Year=Year.x)
  # evo_poppulation <- population_table %>% group_by(Year) %>% summarise(capital = sum(Value))
  # 
  # alim_for_all_by_capital <-  merge(alim_for_all,evo_poppulation,by.x='Year')
  # alim_for_all_by_capital <-  alim_for_all_by_capital %>% mutate(disp_food_for_human = capital*2100*365)
  # alim_for_all_by_capital <-  alim_for_all_by_capital %>% mutate(percent = disp_food/disp_food_for_human)
  
  
  ####3
  sous_nutrition <- pivot[!is.na(pivot$Losses),] %>% group_by(Year,Area,population_size) %>% summarise(cal = sum(calories_kg*1000000*Food) - sum(calories_kg*Losses*1000000) )
  sous_nutrition <- sous_nutrition %>% mutate(nb_personnes = population_size -  cal/365/2200) %>% filter(nb_personnes>0)
  
  # 3.3
  
  
  export_products <- pivot[!is.na(pivot$`Export Quantity`),] %>%select(Item,`Export Quantity`)%>%  
          group_by(Item) %>% summarise(quantity = sum(`Export Quantity`)) %>% arrange(desc(quantity)) %>% head(15)
  
  
  # 3.4
  importations <- pivot[!is.na(pivot$`Import Quantity`),] %>% select(Year,Area,Item,origin,`Import Quantity`) %>%  arrange(desc(`Import Quantity`)) %>% head(200)
  
  #3.5
  
  
  importation_ratio_1 <- pivot[ !is.na(pivot$`Other uses (non-food)`) & !is.na(pivot$`Domestic supply quantity`),] %>% 
    filter(`Domestic supply quantity` != 0 & `Other uses (non-food)` !=0) %>% group_by(Item) %>% 
    summarise(ouses_disp = mean( `Other uses (non-food)`/(`Other uses (non-food)`+`Domestic supply quantity`)),) %>% 
    filter(ouses_disp < 1) %>% 
    arrange(desc(ouses_disp)) %>% head(15)
  
  
  importation_ratio_2 <- pivot[ !is.na(pivot$Food) & !is.na(pivot$Feed),] %>% 
    filter(Food != 0 & Feed !=0) %>% group_by(Item) %>% 
    summarise(feed_food = mean( Feed/(Feed+Food)),) %>% 
    filter(feed_food < 1) %>% 
    arrange(desc(feed_food)) %>% head(15)
  
  
  
  #3.6
  importation_ratio_ouses_disp_3 <- importation_ratio_1 %>% head(3) 
  
  importation_ratio_food_feed_3 <- importation_ratio_2 %>% head(3)
  
  # 3.7
  
  
  
  
  
  
  
  # population <<- population %>% mutate(population_value = Value*strtoi(gsub("[^0-9.]","", Unit))/1000000000)
  # 
  # dispo_alim <<- rbind(animal %>% mutate(origin = 'animal'),vegetal %>% mutate(origin = 'vegetal')) %>% 
  #               
  #               select(`Item Code`,Item,`Area Code`,Area,Value,Year,Element,origin) %>% 
  #                 
  #               filter(Element %in% c(
  #                 'Food supply quantity (kg/capita/yr)',
  #                 'Food supply (kcal/capita/day)',
  #                 'Protein supply quantity (g/capita/day)',
  #                 'Fat supply quantity (g/capita/day)')) %>% 
  #               
  #               spread(key = Element,value = Value)
  #             
  #               
  # cereals_list <<- rbind(animal %>% mutate(origin = 'animal'),vegetal %>% mutate(origin = 'vegetal')) %>% 
  #       select(`Item Code`,Item,`Area Code`,Area,Year,origin) %>%  
  #       mutate(is_cereal = ifelse(test = `Item Code` %in% c(2511,2805,2513,2514,2517,2520,2515,2516,2518),
  #                             yes = TRUE,
  #                             no = FALSE) )
  # 
  # 
  # #Proportion des céréales pour l’alimentation animale
  # alimentation_proportion  <<- rbind(animal %>% mutate(origin = 'animal'),vegetal %>% mutate(origin = 'vegetal')) %>%
  #           filter (Element %in% c('Feed','Food')) %>% group_by(Element) %>% summarise(n = sum(Value)) %>% 
  #           arrange(desc(n)) %>%
  #           mutate(lab.ypos = cumsum(n) - 0.5*n)
  #           
  # alimentation_proportion <<- alimentation_proportion %>% mutate(percent = round((n * 100 )/sum(alimentation_proportion$n),digits=0) )
  #   
  # # Calculer (pour chaque pays et chaque produit) la disponibilité alimentaire en kcal puis en kg de protéines 
  # disp_alim_per_item <<- dispo_alim %>% inner_join(population %>% select(c("Year","Area Code","Value")) ,by=c("Year","Area Code"))
  # 
  # # afficher le  10 des pays qui ont la meuilleur disponibilité alimentaire
  # 
  # 
  # # afficher les 10 pays qui ont la pire disponibilitée alimentaire 
  
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
  dataFrameToTable(con,'pivot',pivot)
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

