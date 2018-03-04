### HI-SAFE - MAIN
### Programmer: Kevin Wolz
### Originally Created: 19 May 2016
### Last Updated: 13 Oct 2016

rm(list=ls())               # clear environment
cat("\014")                 # clear console
startTime <- proc.time()[3] # Start timer

##### USER PARAMETERS #####
WD <- "~/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/HI-SAFE_RUNS"
setwd(WD)                             # Set working directory
TREE <- "walnut"                      # "poplar" "walnut"
CROP <- "alfalfa"                       # "durum-wheat" "maize
PROJECT.NAME <- 'Walnut-Alfalfa_Basic' # Give each run a unique project name. Change this each time this script is run to prevent overwriting old data.
N.YEARS <- 30
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
projectPath <- paste0(WD, '/output/', PROJECT.NAME)
simulationDirPath <- paste0(projectPath, '/simulations')
templateDir <- '/run_dir_template'
templatePath <- paste0(WD, templateDir)
plotPath <- paste0(projectPath, "/plots")
outputDataPath <- paste0(projectPath, "/output_data")

##### CREATE PATHS #####
dir.create(projectPath,showWarnings=FALSE,recursive=TRUE)
dir.create(simulationDirPath,showWarnings=FALSE,recursive=TRUE)
dir.create(plotPath,showWarnings=FALSE,recursive=TRUE)
dir.create(outputDataPath,showWarnings=FALSE,recursive=TRUE)

##### SET RANGE & INTERVAL TO GRID VARIABLES #####

## TOGGLES FOR USING RANGE VS FIXED VALUES FOR GRIDING (1=seq across RANGE by INT, 2=use all values in RANGE, 3=FIXED)
WITHIN <- 3
BETWEEN <- 2
STAGGER <- 3
CROP.DIST <- 3
ORIENT <- 3
LAT <- 3
RP.DEPTH <- 2

## RANGES TO GRID EACH VARIABLE
WITHIN.RANGE <- c(3,9)         # tree spacing within row (m)
BETWEEN.RANGE <- c(5,15)       # spacing between rows (m)
STAGGER.RANGE <- c(0,0.75)      # alternate row staggering (multiple of within row spacing)
CROP.DIST.RANGE <- c(0.5,2.5)   # distance from the tree row that the main crops starts (m)
ORIENT.RANGE <- c(0,90)        # row orientation (degrees E of N, i.e. 0 to 360)
LAT.RANGE <- c(0,60)            # plot latitude (degrees N)
RP.DEPTH.RANGE <- c(0,1)

## INTERVAL TO GRID EACH VARIABLE
WITHIN.INT <- 3
BETWEEN.INT <- 5
STAGGER.INT <- 0.25
CROP.DIST.INT <- 1.0
ORIENT.INT <- 45
LAT.INT <- 30
RP.DEPTH.INT <- 0.25

## FIXED VALUES FOR EACH VARIABLE
WITHIN.FIX <- 3       # ~6 at the castle
BETWEEN.FIX <- 15     # ~13 at the castle
STAGGER.FIX <- 0      # no sure at the castle, but this is not currently modifyable in Hi-sAFe
CROP.DIST.FIX <- 0.5  # ~0.5-1 at the castle
ORIENT.FIX <- 0       # various at the castle
LAT.FIX <- 45         # 43 deg N at the castle
RP.DEPTH.FIX <- 0     # not sure what they do at the castle

##### RUN FILE PREPARATION #####
source("HI-SAFE_FUNCTIONS.R")
source("HI-SAFE_GEN.R")
source("HI-SAFE_RUN.R")
source("HI-SAFE_COMP.R")
source("HI-SAFE_ANALYSIS.R")
source("HI-SAFE_PLOTS.R")
#source("HI-SAFE_SYNTH_PLOTS.R")

save.image(paste0(outputDataPath, "FULL_WORKSPACE_", PROJECT.NAME, ".RData"))

elapsedTime <- signif((proc.time()[3] - startTime)/60,3)
print(paste0("Done with all simulations: ", PROJECT.NAME, ". This took ", elapsedTime, " minutes."))
beep(2)
sendmail("7084769929@txt.att.net", message=paste0("HI-SAFE ", PROJECT.NAME, " Simulations Finished!"))
