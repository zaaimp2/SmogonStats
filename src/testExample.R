source("importAndClean.R")
source("plotData.R")

smogonData = readSmogonData("gen5ou-1500")
samplePokemon = c("Tyranitar, Heatran, Terrakion")
plotData(smogonData, samplePokemon)
