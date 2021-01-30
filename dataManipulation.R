

populationMondiale2 <<- function(){
  population_table_tmp <- population[!population$`Area Code` %in% c(41,96,214,128),] 
  population_table <- population_table_tmp %>% select(Area,`Area Code`,Value,Year) 
  population_table$Value <- population_table$Value*1000
  evo_poppulation <- population_table %>% group_by(Year) %>% summarise(n = sum(Value))
  return(evo_poppulation)
}

alimentation_proportion <<- function(...){
  prop  <- rbind(animal %>% mutate(origin = 'animal'),vegetal %>% mutate(origin = 'vegetal')) %>%
    filter (Element %in% list(...)) %>% group_by(Element) %>% summarise(n = n()) %>% 
    arrange(desc(n)) %>%
    mutate(lab.ypos = cumsum(n) - 0.5*n)
  
  prop <- prop %>% mutate(percent = round((n * sum(prop$n) )/1000,digits=0) )
  return(prop)
}

dispo_alimentaion <<- function(country){
  
  # dispo_alim <<- rbind(animal %>% mutate(origin = 'animal'),vegetal %>% mutate(origin = 'vegetal')) %>% 
  # select(`Item Code`,Item,`Area Code`,Area,Value,Year,Element,origin) %>% 
  # filter(Element %in% c(
  #                 'Food supply quantity (kg/capita/yr)',
  #                 'Food supply (kcal/capita/day)',
  #                 'Protein supply quantity (g/capita/day)',
  #                 'Fat supply quantity (g/capita/day)')) %>% 
  # spread(key = Element,value = Value) %>% 
  # filter(Area == country, origin == "vegetal") %>%
  # mutate(is_cereal = "True")
  #Country initialization
  
  
  
  vegetal$origin="vegetal"
  animal$origin="animal"
  All_products <<- rbind(vegetal,animal)
  
  products <<- All_products[,!names(All_products) %in% c("Domain Code","Domain")]
  
   
  #pivoted_table <<- tidyr::pivot_wider(products,id_cols=c("Area Code","Area","Year","Item Code","Item","origin"),names_from= "Element",values_from = Value)
  
  cereals <<- cereals[,!names(cereals) %in% c("Domain Code","Domain")]
  codes <<- cereals %>% distinct(`Item Code`) 
  
  
  # print(country)
  # if(country == "All"){ 
  #   print("Ici ALL")
  #   
  #   pivoted_table <<- pivoted_table %>% mutate(is_cereal = `Item Code` %in% pull(codes))
  #   
  #   print(pivoted_table)
  #   
  #   pivoted_table <<- pivoted_table %>% mutate(is_cereal = `Item Code` %in% pull(codes)) %>% filter(Area == country)
  #   return(pivoted_table)
  # }else{
  #   return(pivoted_table %>% head(10))
  # 
  # }else if(country != "Veuillez selectionner un pays." && country != "All"){
  #   print("Ici Select")
  #   
  #   print("Ici Rien")
  # }
  
  
    pivoted_table <<- pivot_final %>% filter(Area == country)
    return(pivoted_table)
  
 
}
#dispo_alimentaion("France")

#2.1.1
cereals_list_verify <<- function(country){
  cereals_list <<- rbind(animal %>% mutate(origin = 'animal'),vegetal %>% mutate(origin = 'vegetal')) %>% 
        select(`Item Code`,Item,`Area Code`,Area,Year,origin) %>%  
         mutate(is_cereal = ifelse(test = `Item Code` %in% c(2511,2805,2513,2514,2517,2520,2515,2516,2518),
                                yes = TRUE,
                                 no = FALSE)) %>%  
    filter(Area == country) %>%
    distinct(`Item Code`, .keep_all = TRUE)
   
  
  return(cereals_list )
}
#cereals_list_verify("France")

#2.1.2
alim_proportion <<- function(){
alimentation_proportion  <<- rbind(animal %>% mutate(origin = 'animal'),vegetal %>% mutate(origin = 'vegetal')) %>%
             filter (Element %in% c('Feed','Food')) %>% group_by(Element) %>% summarise(n = sum(Value)) %>% 
             arrange(desc(n)) %>%
             mutate(lab.ypos = cumsum(n) - 0.5*n)
             
   alimentation_proportion <<- alimentation_proportion %>% mutate(percent = round((n * 100 )/sum(alimentation_proportion$n),digits=0))
   return(alimentation_proportion)
}
#alim_proportion()

#2.2.1
disp_alim_per_item_fct <<- function(){
disp_alim_per_item <<- dispo_alim %>% inner_join(population %>% select(c("Year","Area Code","Value")) ,by=c("Year","Area Code"))
return(disp_alim_per_item)
}
disp_alim_per_item_fct()

test <<- function(){
  return("Effectivement")
}




