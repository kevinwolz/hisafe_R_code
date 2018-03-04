### HI-SAFE Design Optimization - GENERATOR
### Programmer: Kevin Wolz
### Originally Created: 19 May 2016
### Last Updated: 20 May 2016

##### RAW VALUES TO GRID EACH VARIABLE #####
ID.raw <- NA
within.raw <- if(WITHIN==1) seq(WITHIN.RANGE[1], WITHIN.RANGE[2], WITHIN.INT) else if(WITHIN==2) WITHIN.RANGE else WITHIN.FIX
between.raw <- if(BETWEEN==1) seq(BETWEEN.RANGE[1], BETWEEN.RANGE[2], BETWEEN.INT) else if(BETWEEN==2) BETWEEN.RANGE else BETWEEN.FIX
orient.raw <- if(ORIENT==1) seq(ORIENT.RANGE[1], ORIENT.RANGE[2], ORIENT.INT) else if(ORIENT==2) ORIENT.RANGE else ORIENT.FIX
stagger.raw <- if(STAGGER==1) seq(STAGGER.RANGE[1], STAGGER.RANGE[2], STAGGER.INT) else if(STAGGER==2) STAGGER.RANGE else STAGGER.FIX
crop.dist.raw <- if(CROP.DIST==1) seq(CROP.DIST.RANGE[1], CROP.DIST.RANGE[2], CROP.DIST.INT) else if(CROP.DIST==2) CROP.DIST.RANGE else CROP.DIST.FIX
lat.raw <- if(LAT==1) seq(LAT.RANGE[1], LAT.RANGE[2], LAT.INT) else if(LAT==2) LAT.RANGE else LAT.FIX
rp.depth.raw <-  if(RP.DEPTH==1) seq(RP.DEPTH.RANGE[1], RP.DEPTH.RANGE[2], RP.DEPTH.INT) else if(LAT==2) RP.DEPTH.RANGE else RP.DEPTH.FIX

##### WRITE OUT GRIDING SCENARIO #####
PARAMS <- c("within", "between", "orient", "stagger", "crop.dist", "lat", "rp.depth")
GRIDED <- c(WITHIN, BETWEEN, ORIENT, STAGGER, CROP.DIST, LAT, RP.DEPTH)

VALUES <- c(paste(round(within.raw, 2), collapse="; "), paste(round(between.raw, 2), collapse="; "), paste(round(orient.raw, 2), collapse="; "), paste(round(stagger.raw, 2), collapse="; "), paste(round(crop.dist.raw, 2), collapse="; "), paste(round(lat.raw, 2), collapse="; "), paste(round(rp.depth.raw, 2), collapse="; ")) 

N.LEVELS <- c(length(within.raw), length(between.raw), length(orient.raw), length(stagger.raw), length(crop.dist.raw), length(lat.raw), length(rp.depth.raw))

griding <- data.frame(param = PARAMS, grid.type = GRIDED, values = VALUES, n.levels = N.LEVELS)
griding$grid.type <- factor(griding$grid.type, labels = c("SEQ", "EXPLICIT", "FIXED")[unique(griding$grid.type)])

write.table(griding, paste(projectPath, "/", PROJECT.NAME, "_Griding_Scenario.csv", sep=""), row.names = FALSE, sep=",", quote = FALSE)

##### GENERATE ALL POSSIBLE COMBINATIONS OF VARIABLES #####
var.combos.raw <- expand.grid(ID.raw, within.raw, between.raw, orient.raw, stagger.raw, crop.dist.raw, lat.raw, rp.depth.raw)
names(var.combos.raw) <- c("SimulationName", "within", "between", "orient", "stagger", "crop.dist", "lat", "rp.depth")

##### CREATE FINAL COMBO LIBRARY DATA FRAME #####
var.combos <- var.combos.raw
var.combos$SimulationName <- paste0("Run_", formatC(1:nrow(var.combos), width = ceiling(log10(nrow(var.combos))), format = "d", flag = "0"))

N.combos <- nrow(var.combos)
print(paste("Number of Simulations =", N.combos))
print(paste("Number of Controls =", if(CONTROLS){2}else{0}))

write.table(var.combos, paste(projectPath, "/", PROJECT.NAME, "_Combo_Library.csv", sep=""), row.names = FALSE, sep=",", quote = FALSE)  

##### CREATE SIMULATION DIRECTORIES AND FILES #####
for(i in 1:nrow(var.combos)){
  runDir <- paste0(simulationDirPath, "/", var.combos$SimulationName[i])
  dir.create(runDir)
  dum <- file.copy(list.files(templatePath, full.names = TRUE), runDir, overwrite = TRUE)

  ## EDIT PLD FILE
  pld.file <- paste0(runDir, "/Plot_Description.pld")
  pld <- readLines(pld.file)
  pld[6] <- paste0("latitude = ", var.combos$lat[i])
  pld[13] <- paste0("treeLineOrientation = ", var.combos$orient[i])
  pld[14] <- paste0("spacingBetweenRows = ", var.combos$between[i])
  pld[15] <- paste0("spacingWithinRows = ", var.combos$within[i])
  writeLines(pld, pld.file)
  
  ## EDIT SIM FILE
  sim.file <- paste0(runDir, "/Simulation.sim")
  sim <- readLines(sim.file)
  sim[7] <- paste0("nbSimulations = ", N.YEARS)
  sim[8] <- paste0("simulationYearStart = ", 1996)
  sim[9] <- paste0("simulationDayStart = ", 1)
  sim[13] <- paste0("mainCropSpecies = ", CROP, ".tec")
  sim[15] <- paste0("treeCropDistance = ", var.combos$crop.dist[i])
  if(RP.DEPTH<3){
    sim[53] <- paste0("treeRootPruningYears = ", paste0(1:N.YEARS, collapse=","))
    sim[54] <- paste0("treeRootPruningDays = ", paste0(rep(365, N.YEARS), collapse=","))
    sim[55] <- paste0("treeRootPruningDistance = ", paste0(rep(var.combos$crop.dist[i], N.YEARS), collapse=","))
    sim[56] <- paste0("treeRootPruningDepth = ", paste0(rep(var.combos$rp.depth[i], N.YEARS), collapse=","))
  }
  writeLines(sim, sim.file)
  dum <- file.rename(paste0(runDir, "/Simulation.sim"), paste0(runDir, "/", var.combos$SimulationName[i], ".sim"))
}

##### CREATE SIMULATION CONTROL RUN DIRECTORIES & FILES #####
if(CONTROLS){
  forestryControlFrom <- paste0(WD, "/control_template_files/", TREE, "_forestry_control")
  forestryControlTo <- paste0(simulationDirPath, "/forestry_control")
  dir.create(forestryControlTo)
  dum <- file.copy(list.files(forestryControlFrom, full.names = TRUE), forestryControlTo, overwrite = TRUE)
  sim.file <- paste0(forestryControlTo, "/", TREE, "_forestry_control.sim")
  sim <- readLines(sim.file)
  sim[7] <- paste0("nbSimulations = ", N.YEARS)
  sim[8] <- paste0("simulationYearStart = ", 1996)
  sim[9] <- paste0("simulationDayStart = ", 1)
  writeLines(sim, sim.file)
  dum <- file.rename(paste0(forestryControlTo, "/", TREE, "_forestry_control.sim"), paste0(forestryControlTo, "/forestry_control.sim"))
  
  cropControlFrom <- paste0(WD, "/control_template_files/", CROP, "_crop_control")
  cropControlTo <- paste0(simulationDirPath, "/crop_control")
  dir.create(cropControlTo)
  dum <- file.copy(list.files(cropControlFrom, full.names = TRUE), cropControlTo, overwrite = TRUE)
  sim.file <- paste0(cropControlTo, "/", CROP, "_crop_control.sim")
  sim <- readLines(sim.file)
  sim[7] <- paste0("nbSimulations = ", N.YEARS)
  sim[8] <- paste0("simulationYearStart = ", 1996)
  sim[9] <- paste0("simulationDayStart = ", 1)
  writeLines(sim, sim.file)
  dum <- file.rename(paste0(cropControlTo, "/", CROP, "_crop_control.sim"), paste0(cropControlTo, "/crop_control.sim"))
}