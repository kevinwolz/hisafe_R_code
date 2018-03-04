### HI-SAFE Design Optimization - MAIN
### Programmer: Kevin Wolz
### Originally Created: 19 May 2016
### Last Updated: 21 May 2016

rm(list=ls())               # clear environment
cat("\014")                 # clear console
startTime <- proc.time()[3] # Start timer

##### USER PARAMETERS #####
WD <- "~/Desktop/RESEARCH/MWP/HI-SAFE/optimization"
setwd(WD)                             # Set working directory
TREE <- "poplar"                      # "poplar"
CROP <- "maize"                       #
PROJECT.NAME <- 'Poplar-Maize_Basic_Grid_20-WT' # Give each run a unique project name. Change this each time this script is run to prevent overwriting old data.
N.YEARS <- 20
COMPUTING <- "SERIAL" # PARALLEL does not actaully work any faster than in SERIAL
CONTROLS <- TRUE      # Run Forestry & Crop Control runs as well?

## REQUIRED PACKAGES
library(beepr)
library(mail)
if(COMPUTING=="PARALLEL"){
  library(foreach)
  library(snow)
  library(doSNOW)}
library(doBy)
library(lattice)
library(latticeExtra)
library(plyr)

##### FILE PARAMETERS #####
projectPath <- paste0(WD, '/optimization_runs/', PROJECT.NAME)
simulationDirPath <- paste0(projectPath, '/simulations')
templatePath <- paste0(WD, '/', TREE, '_template_files')
plotPath <- paste0(projectPath, "/plots")
outputDataPath <- paste0(projectPath, "/output_data")

##### CREATE PATHS #####
dir.create(projectPath,showWarnings=FALSE,recursive=TRUE)
dir.create(simulationDirPath,showWarnings=FALSE,recursive=TRUE)
dir.create(plotPath,showWarnings=FALSE,recursive=TRUE)
dir.create(outputDataPath,showWarnings=FALSE,recursive=TRUE)

##### SET RANGE & INTERVAL TO GRID VARIABLES #####

## TOGGLES FOR USING RANGE VS FIXED VALUES FOR GRIDING (1=seq across RANGE by INT, 2=use all values in RANGE, 3=FIXED)
WITHIN <- 1
BETWEEN <- 1
STAGGER <- 3
CROP.DIST <- 3
ORIENT <- 2
LAT <- 3
RP.DEPTH <- 3

## RANGES TO GRID EACH VARIABLE
WITHIN.RANGE <- c(3,9)         # tree spacing within row (m)
BETWEEN.RANGE <- c(5,25)       # spacing between rows (m)
STAGGER.RANGE <- c(0,0.75)      # alternate row staggering (multiple of within row spacing)
CROP.DIST.RANGE <- c(0.5,2.5)   # distance from the tree row that the main crops starts (m)
ORIENT.RANGE <- c(0,90)        # row orientation (degrees E of N, i.e. 0 to 360)
LAT.RANGE <- c(0,60)            # plot latitude (degrees N)
RP.DEPTH.RANGE <- c(0,1.0)

## INTERVAL TO GRID EACH VARIABLE
WITHIN.INT <- 3
BETWEEN.INT <- 10
STAGGER.INT <- 0.25
CROP.DIST.INT <- 1.0
ORIENT.INT <- 45
LAT.INT <- 30
RP.DEPTH.INT <- 0.25

## FIXED VALUES FOR EACH VARIABLE
WITHIN.FIX <- 5       # ~6 at the castle
BETWEEN.FIX <- 15     # ~13 at the castle
STAGGER.FIX <- 0      # no sure at the castle, but this is not currently modifyable in Hi-sAFe
CROP.DIST.FIX <- 0.5  # ~0.5-1 at the castle
ORIENT.FIX <- 0       # various at the castle
LAT.FIX <- 45         # 43 deg N at the castle
RP.DEPTH.FIX <- 0

##### RUN FILE PREPARATION #####
source("HI-SAFE_optimization_FUNCTIONS.R")
source("HI-SAFE_optimization_GEN.R")
source("HI-SAFE_optimization_RUN.R")
source("HI-SAFE_optimization_COMP.R")
source("HI-SAFE_optimization_ANALYSIS.R")
source("HI-SAFE_optimization_PLOTS.R")
source("HI-SAFE_optimization_SYNTH_PLOTS.R")

save.image(paste0(outputPath, "FULL_PREP_WORKSPACE_", PROJECT.NAME, ".RData"))

elapsedTime <- signif((proc.time()[3] - startTime)/60,3)
print(paste0("Done with all simulations: ", PROJECT.NAME, ". This took ", elapsedTime, " minutes."))
beep(2)
sendmail("7084769929@txt.att.net", message=paste0("HI-SAFE ", PROJECT.NAME, " Simulations Finished!"))