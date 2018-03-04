### HI-SAFE Design Optimization - ANALYSIS
### Programmer: Kevin Wolz
### Originally Created: 20 May 2016
### Last Updated: 20 May 2016

simu.data <- read.csv(paste0(outputDataPath, "/", PROJECT.NAME, "_Compiled_Simulation_Data.csv"), header = TRUE)
AF.data <- subset(simu.data, !(SimulationName %in% c("forestry_control", "crop_control")))
F.data <- subset(simu.data, SimulationName %in% c("forestry_control"))
C.data <- subset(simu.data, SimulationName %in% c("crop_control"))

N.SIMUS <- nrow(AF.data)/nrow(F.data)

##### LER CALCULATIONS #####
## PAR
AF.data$par.LER.tree <- AF.data$annualParInterceptedByTrees / rep(F.data$annualParInterceptedByTrees, N.SIMUS)
AF.data$par.LER.crop <- AF.data$annualParInterceptedByCrops / rep(C.data$annualParInterceptedByCrops, N.SIMUS)
AF.data$par.LER <- AF.data$par.LER.tree + AF.data$par.LER.crop

AF.data$par.LER.cum.tree <- ddply(AF.data, .(SimulationName), summarize, cumsum(annualParInterceptedByTrees))[,2] / rep(cumsum(F.data$annualParInterceptedByTrees), N.SIMUS)
AF.data$par.LER.cum.crop <- ddply(AF.data, .(SimulationName), summarize, cumsum(annualParInterceptedByCrops))[,2] / rep(cumsum(C.data$annualParInterceptedByCrops), N.SIMUS)
AF.data$par.LER.cum <- AF.data$par.LER.cum.tree + AF.data$par.LER.cum.crop

## WATER
AF.data$annualWaterUsedByCrop <- AF.data$annualWaterExtractedByCrop# + AF.data$annualWaterEvaporatedInCrop
C.data$annualWaterUsedByCrop <- C.data$annualWaterExtractedByCrop# + C.data$annualWaterEvaporatedInCrop

AF.data$annualWaterUsedByTrees <- AF.data$annualWaterExtractedByTrees# + AF.data$annualWaterEvaporatedInInterCrop
F.data$annualWaterUsedByTrees <- F.data$annualWaterExtractedByTrees# + F.data$annualWaterEvaporatedInInterCrop

AF.data$water.LER.tree <- AF.data$annualWaterUsedByTrees / rep(F.data$annualWaterUsedByTrees, N.SIMUS)
AF.data$water.LER.crop <- AF.data$annualWaterUsedByCrop / rep(C.data$annualWaterUsedByCrop, N.SIMUS)
AF.data$water.LER <- AF.data$water.LER.tree + AF.data$water.LER.crop

AF.data$water.LER.cum.tree <- ddply(AF.data, .(SimulationName), summarize, cumsum(annualWaterExtractedByTrees))[,2] / rep(cumsum(F.data$annualWaterExtractedByTrees), N.SIMUS)
AF.data$water.LER.cum.crop <- ddply(AF.data, .(SimulationName), summarize, cumsum(annualWaterExtractedByCrop))[,2] / rep(cumsum(C.data$annualWaterExtractedByCrop), N.SIMUS)
AF.data$water.LER.cum <- AF.data$water.LER.cum.tree + AF.data$water.LER.cum.crop

## YIELD
AF.data$yield.LER.tree <- AF.data$treeYield / rep(F.data$treeYield, N.SIMUS)
AF.data$yield.LER.crop <- AF.data$cropYield / rep(C.data$cropYield, N.SIMUS)
AF.data$yield.LER <- AF.data$yield.LER.tree + AF.data$yield.LER.crop

AF.data$yield.LER.cum.tree <- ddply(AF.data, .(SimulationName), summarize, cumsum(treeYield))[,2] / rep(cumsum(F.data$treeYield), N.SIMUS)
AF.data$yield.LER.cum.crop <- ddply(AF.data, .(SimulationName), summarize, cumsum(cropYield))[,2] / rep(cumsum(C.data$cropYield), N.SIMUS)
AF.data$yield.LER.cum <- AF.data$yield.LER.cum.tree + AF.data$yield.LER.cum.crop

## NITROGEN
#AF.data$N.LER.tree <- AF.data$nitrogenExtractedByTrees / rep(F.data$nitrogenExtractedByTrees, N.SIMUS)
#AF.data$N.LER.crop <- AF.data$nitrogenExtractedByCrops / rep(C.data$nitrogenExtractedByCrops, N.SIMUS)
#AF.data$N.LER <- AF.data$N.LER.tree + AF.data$N.LER.crop

#AF.data$N.LER.cum.tree <- ddply(AF.data, .(nitrogenExtractedByTrees), summarize, cumsum(treeYield))[,2] / rep(cumsum(F.data$nitrogenExtractedByTrees), N.SIMUS)
#AF.data$N.LER.cum.crop <- ddply(AF.data, .(SimulationName), summarize, cumsum(nitrogenExtractedByCrops))[,2] / rep(cumsum(C.data$nitrogenExtractedByCrops), N.SIMUS)
#AF.data$N.LER.cum <- AF.data$N.LER.cum.tree + AF.data$N.LER.cum.crop

## SOIL CARBON STORAGE
AF.data$soilC.REL <- AF.data$totalCarbonHumusStock / ((F.data$totalCarbonHumusStock + C.data$totalCarbonHumusStock)/2)

## TOTAL C STORAGE (SOIL + WOODY)
AF.data$C.storage <- AF.data$totalCarbonHumusStock + AF.data$annualTreesCarbonBranches + AF.data$annualTreesCarbonCoarseRoots + AF.data$annualTreesCarbonStem
F.data$C.storage <- F.data$totalCarbonHumusStock + F.data$annualTreesCarbonBranches + F.data$annualTreesCarbonCoarseRoots + F.data$annualTreesCarbonStem
C.data$C.storage <- C.data$totalCarbonHumusStock
AF.data$C.storage.REL <- AF.data$C.storage / ((F.data$C.storage + C.data$C.storage)/2)

write.table(AF.data, paste0(outputDataPath, "/", PROJECT.NAME, "_Processed_Data.csv"), sep=",", row.names = FALSE)