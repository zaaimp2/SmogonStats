library(tidyverse)

plotUsageOverTime = function(data, pokemonToAnalyze){
  data %>% 
    filter(Pokemon %in% pokemonToAnalyze) %>% 
    ggplot(aes(date, UsagePercentage, color = Pokemon)) +
    geom_line() +
    geom_smooth()
}
