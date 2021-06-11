library(tidyverse)
library(lubridate)

#Reads in smogon data from the webpage, binds everything to a single data table
readSmogonData = function(format = ""){
  smogonURL = "https://www.smogon.com/stats"
  initialDate = ym("2014-11") #First month of smogon data
  allMonths = getDates()
  urlsList = str_c(smogonURL, allMonths, format, sep ="/")
  urlsList = str_c(urlsList, ".txt")
  urlsList = urlsList[-length(urlsList)] #Hack because data for this month is not yet out
  allMonths = allMonths[-length(allMonths)]
  output = list()
  output = map2(urlsList, allMonths, processOneFile)
  output = bind_rows(output)
}

#Helper functions to get dates
getDates = function(initialDate = ym("2014-11")){ 
  numberOfMonths = ((initialDate %--% today()) %/% months(1)) + 1
  output = rep(ymd("1970-01-01"), numberOfMonths) #Initialize blank vector
  for (i in seq_along(output)){
    output[i] = initialDate + months(i-1)
  }
  output
  output = format(output, "%Y-%m")
  output = as.character(output)
}

#helper function to read and clean each file
processOneFile = function(urls, date){
  newColumnNames = c("Rank", "Pokemon", "UsagePercentage", "Raw", "RawPercentage", "Real", "RealPercentage")
  rawData = read_delim(urls, delim = "|", comment = "+", skip = 2, trim_ws = TRUE, col_types = cols())
  rawData = rawData %>% 
    select(-c(X1,X9)) %>% 
    filter(!row_number() == 1)
  names(rawData) = newColumnNames
  rawData = rawData %>% 
    mutate(across(where(~any(str_detect(.,"%"))), parse_number)) %>% 
    mutate(date = ym(date)) %>% 
    mutate(Raw = as.numeric(Raw), Real= as.numeric(Real)) %>% 
    mutate(Rank = parse_number(Rank))
}

createPokemonSpecificData = function(jsonFile, pokemon, desiredStatistic){ #requires the data side
  pokemonData = jsonFile[[pokemon]]
  pokemonData = pokemonData[[desiredStatistic]]
  pokemonTibble = as_tibble(pokemonData, .name_repair = "universal")
  pokemonTibble = pokemonTibble %>% 
    pivot_longer(everything(), names_to = desiredStatistic, values_to = "Usage")
}

createPokemonCounterData = function(jsonFile, pokemon){
  pokemonData = jsonFile[[pokemon]]
  pokemonData = pokemonData[["Checks and Counters"]]
  pokemonTibble = as_tibble(pokemonData, .name_repair = "universal")
  pokemonTibble = pokemonTibble %>% 
    filter(row_number() == 2)
  pokemonTibble = pokemonTibble %>% 
    pivot_longer(everything(), names_to = "Pokemon", values_to = "forceOutRate") %>% 
    mutate(forceOutRate = unlist(forceOutRate))
}
