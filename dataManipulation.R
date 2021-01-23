# Taille de la population mondiale
populationMondiale <<- function(){
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