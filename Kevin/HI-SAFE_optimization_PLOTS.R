### HI-SAFE Design Optimization - PLOTS
### Programmer: Kevin Wolz
### Originally Created: 20 May 2016
### Last Updated: 20 May 2016

WD <- "~/Desktop/RESEARCH/MWP/HI-SAFE/optimization"

plot.data <- AF.data
plot.data$within <- factor(plot.data$within)
plot.data$between <- factor(plot.data$between)
plot.data$orient <- factor(plot.data$orient)
plot.data$lat <- factor(plot.data$lat)
plot.data$crop.dist <- factor(plot.data$crop.dist)
plot.data$rp.depth <- factor(plot.data$rp.depth)

PLOT.COLS <- c(8:44, 53:ncol(plot.data))

## SINGLE SPECIES AT A TIME
for(var in PLOT.COLS){
  plot.data$plot.col <- plot.data[,var]
  var.name <- names(plot.data)[var]
  var.plot <- xyplot(plot.col ~ Year | between + within,
                     data = plot.data,
                     groups = orient,
                     type = "b",
                     xlab = "Simulation Year",
                     ylab = var.name,
                     main =  var.name,
                     par.settings = StdLatticeTheme,
                     grid = TRUE,
                     #layout = c(3,3),
                     as.table = TRUE,
                     scales = list(x=list(tck=-1, alternating=FALSE), y=list(rot=0, alternating=FALSE)),
                     auto.key = list(space = "right",columns = 2, title = "Rt Prn Dpth"),
                     strip = function(..., style) strip.default(..., style = 1, strip.names = c(TRUE, FALSE)),
                     # key = list(text=list(c("DayCent Output", "Field Measurements")),
                     #            points=list(pch=c(NA,16), col=cbPalette[1:2]),
                     #            lines = list(lty=c(1,0), lwd = c(2,0), col=cbPalette[1:2]),
                     #            columns=2)
  )

  pdf(paste0(plotPath, "/", PROJECT.NAME, "_", var.name, ".pdf", sep=""), width=11, height=8.5)
  print(var.plot)
  dev.off()
}