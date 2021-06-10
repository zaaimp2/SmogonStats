library(tidyverse)

plotUsageOverTime = function(data, pokemonToAnalyze){
  data %>% 
    filter(Pokemon %in% pokemonToAnalyze) %>% 
    ggplot(aes(date, UsagePercentage, color = Pokemon)) +
    geom_line() +
    geom_smooth()
}

plotTopNPokemon = function(data, topN = 10, dateAnalyzed){
  if(length(dateAnalyzed) >= 2){
    return("Error: Select only one date")
  }
  
  data %>% 
    filter(date == datesAnalyzed) %>% 
    filter(Rank <= topN) %>% 
    ggplot(aes(x = Pokemon, y = UsagePercentage)) +
    geom_col() +
    coord_flip()
}