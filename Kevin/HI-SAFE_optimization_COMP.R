### HI-SAFE Design Optimization - COMPILATION
### Programmer: Kevin Wolz
### Originally Created: 19 May 2016
### Last Updated: 20 May 2016

setwd(WD)

## read in the design parameters combination data 
var.combos <- read.csv(paste(projectPath, "/", PROJECT.NAME, "_Combo_Library.csv", sep=""), header = TRUE, stringsAsFactors = FALSE)  
var.combos[(nrow(var.combos)+1):(nrow(var.combos)+2),] <- NA
var.combos$SimulationName[nrow(var.combos)-1] <- "forestry_control"
var.combos$SimulationName[nrow(var.combos)] <- "crop_control"

#RUNS.TO.READ <- var.combos$SimulationName %in% list.files(simulationDirPath, pattern = "annualplot", recursive = TRUE)

#var.combos <- subset(var.combos, SimulationName!="Run_15")
#var.combos <- var.combos[c(1:26,46,47),]

profiles <- c("annualplot", "annualtree")

## read the definitions of variables only from the first file (should be the same for all outputs for the whole experiment)
first_simu <- var.combos$SimulationName[1]
variables <- lapply(profiles, function(x) lectureHisAFe(paste0(simulationDirPath,"/", first_simu, "/output-", first_simu, ".sim/", first_simu, "_", x, ".txt" ), readata=FALSE)$variables)

print(paste("Reading simulation:", first_simu))
toto <- lapply(profiles, function(x) lectureHisAFe(paste0(simulationDirPath,"/", first_simu, "/output-", first_simu, ".sim/", first_simu, "_", x, ".txt" ))$data)
if(first_simu=="crop_control") {toto.df <- toto[[1]]} else{toto.df <- do.call("merge", toto)}
output.data <- toto.df

## read the data for all simulations of the experiment
for(simu in var.combos$SimulationName[2:nrow(var.combos)]){
  print(paste("Reading simulation:", simu))
  toto <- lapply(profiles, function(x) lectureHisAFe(paste0(simulationDirPath,"/", simu, "/output-", simu, ".sim/", simu, "_", x, ".txt" ))$data)
 if(simu=="crop_control") {toto.df <- toto[[1]]
 	} else{toto.df <- do.call("merge", toto)}  
  output.data <- rbind.fill(output.data, toto.df)
}
class(output.data) <- c("hop", class(output.data))
attr(output.data, "definitions") <- variables
attr(output.data, "experimentalplan") <- var.combos

## merge the data with the experimental design parameters & write to file
simu.data <- merge(output.data, var.combos)
simu.data$tree <- TREE
simu.data$crop <- CROP
write.table(simu.data, paste0(outputDataPath, "/", PROJECT.NAME, "_Compiled_Simulation_Data.csv"), sep=",", row.names = FALSE)