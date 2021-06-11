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
    filter(date == dateAnalyzed) %>% 
    filter(Rank <= topN) %>% 
    ggplot(aes(x = reorder(Pokemon, UsagePercentage), y = UsagePercentage, fill = Pokemon)) +
    geom_col() +
    coord_flip() +
    xlab("Pokemon") +
    ylab("Usage Percentage")
}

plotTopNIndividualStat = function(cleanedJsonOutput, topN = 10, desiredStatistic){
  if(desiredStatistic == "Checks and Countesrs"){
    outputVariable = "forceOutRate"
  } else {
    outputVariable = "Usage"
  }
  cleanedJsonOutput %>% 
    arrange(desc(get(outputVariable))) %>% 
    filter(row_number() <= topN) %>% 
    ggplot(aes(x = reorder(get(desiredStatistic), get(outputVariable)), y = get(outputVariable), fill = get(desiredStatistic))) +
    geom_col() +
    coord_flip() +
    xlab(desiredStatistic) +
    ylab(outputVariable) +
    labs(fill = desiredStatistic)
}

