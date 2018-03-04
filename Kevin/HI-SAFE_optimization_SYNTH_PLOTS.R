### HI-SAFE Design Optimization - SYNTHESIS PLOTS
### Programmer: Kevin Wolz
### Originally Created: 20 May 2016
### Last Updated: 20 May 2016

##### WALNUT/POPLAR-WHEAT + 0/90 ROW ORIENTATION #####
SYNTH.NAME <- "WP-WM_Basic_Grid_20"
PROJS <- c("Poplar-Wheat_Basic_Grid_20", "Walnut-Wheat_Basic_Grid_20", "Poplar-Maize_Basic_Grid_20", "Walnut-Maize_Basic_Grid_20")

WD <- "~/Desktop/RESEARCH/MWP/HI-SAFE/optimization"
setwd(WD)
source("HI-SAFE_optimization_FUNCTIONS.R")
plotPath <- paste0(WD, "/optimization_runs/synthesis_plots/", SYNTH.NAME, "/")
dir.create(plotPath, recursive = TRUE, showWarnings = FALSE)

## Create data specific for plots
plot.data <- read.csv(paste0(WD, "/optimization_runs/", PROJS[1], "/output_data/", PROJS[1], "_Processed_Data.csv"), header = TRUE)    
for(proj in PROJS[2:length(PROJS)]){
  proj.data <- read.csv(paste0(WD, "/optimization_runs/", proj, "/output_data/", proj, "_Processed_Data.csv"), header = TRUE)    
  plot.data <- rbind.fill(plot.data, proj.data)
}

plot.data$within <- factor(plot.data$within)
plot.data$between <- factor(plot.data$between)
plot.data$orient <- factor(plot.data$orient)
plot.data$lat <- factor(plot.data$lat)
plot.data$crop.dist <- factor(plot.data$crop.dist)
plot.data$rp.depth <- factor(plot.data$rp.depth)

proj.1.plot.data <- subset(plot.data, tree=="poplar" & orient == 0)
proj.2.plot.data <- subset(plot.data, tree=="walnut" & orient == 0)

PLOT.COLS <- c(8:15, 17:35, 37:44, 53:ncol(plot.data))

## Loop Through & Plot all Vars by within*between*orient
for(var in PLOT.COLS){
  proj.1.plot.data$plot.col <- proj.1.plot.data[,var]
  proj.2.plot.data$plot.col <- proj.2.plot.data[,var]
  var.name <- names(proj.1.plot.data)[var]
  YLIM <- range(c(range(proj.1.plot.data$plot.col),range(proj.2.plot.data$plot.col)))
  
  proj1.plot <- xyplot(plot.col ~ Year | between + within,
                       data = proj.1.plot.data,
                       groups = crop,
                       type = "b",
                       xlab = "Simulation Year",
                       ylab = var.name,
                       ylim = YLIM,
                       main =  var.name,
                       par.settings = StdLatticeTheme,
                       grid = TRUE,
                       as.table = TRUE,
                       scales = list(x=list(tck=-1, alternating=FALSE), y=list(rot=0, alternating=FALSE)),
                       strip = function(..., style) strip.default(..., style = 1, strip.names = c(TRUE, FALSE)),
                       key = list(text=list(c("Wheat", "Maize", "Poplar", "Walnut")),
                                  points=list(pch=c(16,16,NA,NA), col=c(cbPalette[1:2],"#000000","#000000")),
                                  lines = list(lty=c(1,1,1,2), lwd = c(2,2,2,2), col=c(cbPalette[1:2],"#000000","#000000")),
                                  columns=4)
  )
  
  
  proj2.plot <- xyplot(plot.col ~ Year | between + within,
                       data = proj.2.plot.data,
                       groups = crop,
                       lty = 2,
                       type = "b",
                       par.settings = StdLatticeTheme)
  
  pdf(paste0(plotPath, "/", SYNTH.NAME, "_", var.name, ".pdf", sep=""), width=11, height=8.5)
  print(proj1.plot + as.layer(proj2.plot))
  dev.off()
}

##### WALNUT/POPLAR-WHEAT + 0/90 ROW ORIENTATION #####
# SYNTH.NAME <- "Basic_Grid_20"
# WD <- "~/Desktop/RESEARCH/MWP/HI-SAFE/optimization"
# setwd(WD)
# source("HI-SAFE_optimization_FUNCTIONS.R")
# plotPath <- paste0(WD, "/optimization_runs/synthesis_plots/", SYNTH.NAME, "/")
# dir.create(plotPath, recursive = TRUE, showWarnings = FALSE)
# 
# ## Create data specific for plots
# proj1 <- "Poplar-Wheat_Basic_Grid_20"
# proj2 <- "Walnut-Wheat_Basic_Grid_20"
# proj.1.data <- read.csv(paste0(WD, "/optimization_runs/", proj1, "/output_data/", proj1, "_Processed_Data.csv"), header = TRUE)
# proj.2.data <- read.csv(paste0(WD, "/optimization_runs/", proj2, "/output_data/", proj2, "_Processed_Data.csv"), header = TRUE)
# #plot.data <- proj.1.data
# plot.data <- rbind(proj.1.data, proj.2.data)
# plot.data$within <- factor(plot.data$within)
# plot.data$between <- factor(plot.data$between)
# plot.data$orient <- factor(plot.data$orient)
# plot.data$lat <- factor(plot.data$lat)
# plot.data$crop.dist <- factor(plot.data$crop.dist)
# 
# plot.data.0 <- subset(plot.data, orient==0)
# plot.data.90 <- subset(plot.data, orient==90)
# proj.1.plot.data <- subset(plot.data, tree=="poplar")
# proj.2.plot.data <- subset(plot.data, tree=="walnut")
# 
# PLOT.COLS <- c(8:44, 53:ncol(plot.data))
# 
# ## Loop Through & Plot all Vars by within*between*orient
# for(var in PLOT.COLS){
#   proj.1.plot.data$plot.col <- proj.1.plot.data[,var]
#   proj.2.plot.data$plot.col <- proj.2.plot.data[,var]
#   var.name <- names(proj.1.plot.data)[var]
#   YLIM <- range(c(range(proj.1.plot.data$plot.col),range(proj.2.plot.data$plot.col)))
# 
#   proj1.plot <- xyplot(plot.col ~ Year | between + within,
#                      data = proj.1.plot.data,
#                      #outer = TRUE,
#                      groups = orient,
#                      type = "b",
#                      xlab = "Simulation Year",
#                      ylab = var.name,
#                      ylim = YLIM,
#                      main =  var.name,
#                      par.settings = StdLatticeTheme,
#                      grid = TRUE,
#                      #layout = c(3,3),
#                      as.table = TRUE,
#                      scales = list(x=list(tck=-1, alternating=FALSE), y=list(rot=0, alternating=FALSE)),
#                      #auto.key = list(space = "right",columns = 2, title = "Row Orient"),
#                      strip = function(..., style) strip.default(..., style = 1, strip.names = c(TRUE, FALSE)),
#                      key = list(text=list(c("Orient 0", "Orient 90", "Poplar", "Walnut")),
#                                  points=list(pch=c(16,16,NA,NA), col=c(cbPalette[1:2],"#000000","#000000")),
#                                  lines = list(lty=c(1,1,1,2), lwd = c(2,2,2,2), col=c(cbPalette[1:2],"#000000","#000000")),
#                                  columns=4)
#   )
# 
#   
#   proj2.plot <- xyplot(plot.col ~ Year | between + within,
#                        data = proj.2.plot.data,
#                        groups = orient,
#                        lty = 2,
#                        type = "b",
#                        par.settings = StdLatticeTheme)
# 
#   pdf(paste0(plotPath, "/", SYNTH.NAME, "_", var.name, ".pdf", sep=""), width=11, height=8.5)
#   print(proj1.plot + as.layer(proj2.plot))
#   dev.off()
# }