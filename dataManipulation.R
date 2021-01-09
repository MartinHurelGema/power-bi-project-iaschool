# Taille de la population mondiale
populationMondiale <<- function(){
  pop <- population %>% subset(`Area Code` != 351 ) %>%  group_by(Year)%>%summarise(s = sum(population_value))
  return(pop)
}

alimentation_proportion <<- function(...){
  prop  <- rbind(animal %>% mutate(origin = 'animal'),vegetal %>% mutate(origin = 'vegetal')) %>%
    filter (Element %in% list(...)) %>% group_by(Element) %>% summarise(n = n()) %>% 
    arrange(desc(n)) %>%
    mutate(lab.ypos = cumsum(n) - 0.5*n)
  
  prop <- prop %>% mutate(percent = round((n * 100 )/sum(prop$n),digits=0) )
  return(prop)
}

