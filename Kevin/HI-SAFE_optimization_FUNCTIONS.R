### HI-SAFE Design Optimization - FUNCTIONS
### Programmer: Kevin Wolz
### Originally Created: 19 May 2016
### Last Updated: 20 May 2016

runSimulations <- function(n){
  sim.path <- paste0(WD, "/", PROJECT.NAME, "/simulations/", n, "/Simulation.sim")
  setwd("/Applications/Capsis/")
  call <- paste0("sh capsis.sh -p script safe.pgms.ScriptGen ", sim.path)
  log <- system(call, wait = TRUE, intern = TRUE)
}  

##### SET LATTICE PLOT THEME #####
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
StdLatticeTheme <- custom.theme(symbol = cbPalette,
                                fill = cbPalette,
                                region = brewer.pal(n = 11, name = "Spectral"),
                                reference = "#e8e8e8",
                                bg = "transparent", fg = "black")
StdLatticeTheme$plot.polygon$col <- c(cbPalette, "#FFFFFF", "#000000")
StdLatticeTheme$strip.shingle$col <- c(cbPalette, "#FFFFFF", "#000000")
StdLatticeTheme$superpose.symbol$fill <- c(cbPalette, "#FFFFFF", "#000000")
StdLatticeTheme$superpose.polygon$col <- c(cbPalette, "#FFFFFF", "#000000")
StdLatticeTheme$strip.background$col <- "lightgrey"
StdLatticeTheme$par.xlab.text$cex <- 1.5
StdLatticeTheme$par.ylab.text$cex <- 1.5
StdLatticeTheme$par.main.text$cex <- 2
StdLatticeTheme$superpose.symbol$cex <- 0.5 #rep(1,8*3)
StdLatticeTheme$superpose.symbol$pch <- c(rep(16,8), rep(17,8), rep(15,8))
trellis.par.set(StdLatticeTheme)
#show.settings()


##### POST-PROCESSING FUNCTIONS FROM MARIE GOSME #####
lectureHisAFe<-function(file, readata=TRUE){
  #reads one output file
  #returns a list with two elements: the information on the variables (top part of the file) and the data (empty data frame if readata=FALSE)
  toto <- readLines(file)
  debutdonnees <- which(toto[-1]=="")[1]
  variables <- read.table(file, skip=7, nrows=debutdonnees-8, header=TRUE, sep="\t", encoding="latin1", stringsAsFactors = FALSE)
  if (readata) {
    donnees <- read.table(file, skip=debutdonnees, header=TRUE, sep="\t", encoding="latin1", stringsAsFactors = FALSE)
    donnees$Date<-as.Date(gsub(pattern="a.", replacement="", x=donnees$Date, fixed=TRUE), format="%d/%m/%Y")
    donnees<-donnees[!is.na(donnees$Date),]
  } else donnees<-data.frame()
  return(list(data=donnees, variables=variables))
}




# empile<-function(...){
#   N<-unique(unlist(lapply(list(...),names)))
#   result<-NULL
#   for(DF in list(...)){
#     x<-as.data.frame(lapply(N,function(n)if(n %in% colnames(DF)) DF[,n] else NA))
#     names(x)<-N
#     result<-rbind(result,x)
#   }
#   result
# } 
# 
# infos<-function(df) {
#   if (! "data.frame" %in% class(df)) print(paste("attention, c est pas un data.frame mais un", paste(class(df), collapse=" ")))
#   df<-as.data.frame(df)
#   #affiche les numeros et types et nb NA de chaque variable de df
#   print(paste(dim(df)[1]," lignes"))
#   return(data.frame(nom=colnames(df), num=1:length(df),  class=sapply(df, function(x) class(x)[1]), nb_NA=sapply(df, function(x) sum(is.na(x))) ))
# }
# 



# readVirtualExperiment<-function(experimentalplan, folder, profiles=c("trees", "plot", "monthCells")) {
#   #reads all the simulations given by the rownames of experimentalplan, in folder folder.
#   #profiles: names of the export profiles to read
#   #returns an object of class "hop" (Hisafe OutPuts), with 3 elements (one for each output profile), and attributes "experimentalplan" (which was passed as argument) and "definitions" (definitions of the variables)"
# 
#   #read the definitions of variables only from the first file (should be the same for all outputs for the whole experiment)
#   simu_name<-rownames(experimentalplan)[1]
#   names(profiles)<-profiles
#   variables<-lapply(profiles, function(x) lectureHisAFe(paste0(folder,"/" ,simu_name, "/output-",simu_name,".sim", "/", simu_name,"_", x, ".txt" ), readata=FALSE)$variables)
#   #read the data for all simulations of the experiment
#   lectures<-lapply(profiles, function(x) return(data.frame()))
#   for (i in 1: nrow(experimentalplan)) {
#     simu_name<-rownames(experimentalplan)[i]
#     print(paste("reading simulation", simu_name))
#     toto<-lapply(profiles, function(x) lectureHisAFe(paste0(folder,"/" ,simu_name, "/output-",simu_name,".sim", "/", simu_name,"_", x, ".txt" ))$data)
#     lectures<-mapply(rbind, lectures, toto)
#   }
#   class(lectures)<-c("hop", class(lectures))
#   attr(lectures, "definitions") <- variables
#   attr(lectures, "experimentalplan") <- experimentalplan
#   return(lectures)
# }

# summary.hop<-function(hop) {
#   # print some information about the variables in each element of object of class "hop" (Hisafe OutPuts)
#   lapply(names(hop), function (nom) {
#     print(nom)
#     toto<-merge(infos(hop[[nom]]), attr(hop, "definitions", exact = TRUE)[[nom]][,c("NomVariable", "Unité", "Description")], all=TRUE, by.x="nom", by.y="NomVariable")
#     toto<-toto[order(toto$num),]
#     print(toto)
#     return(nom)
#   })
# }
# 
# createdfcoordtime<-function(hop, yaxis=c(dbh="trees"), simulationNames=NULL, DayofYear=NULL, selectid=NULL) {
#   #prepares the dataframe for plotting, when xaxis is time: collects the variables in the different elements of hop, and reshapes when there are several individuals to get several columns
#   #returns a data.frame with columns "SimulationName", "xcoord", and 1 or several columns (ycoord and/or ycoord.1, ycoord.2)
#   #hop: object of class "hop" (Hisafe OutPuts)
#   #yaxis= named vector, with the names equal to the variables you want to plot and the values equal to the export profile where these variables can be found
#   #DayofYear (optional): only if xaxis="Time": vector of Julian days, only the data for these days is plotted
#   #selectid (optional): only for trees or cells: numeric vector to select specific ids of trees or cells to plot (default: all individuals are plotted)
#   dfcoord<-data.frame()
#   for (i in 1:length(yaxis)) {
#     scale<-unname(yaxis[i])
#     variable<-names(yaxis)[i]
#     if (scale %in% c("trees", "monthCells")) { #create one column for each tree/cell
#       if (!is.null(selectid)) {
#         existepas<-setdiff(selectid, hop[[scale]]$id)
#         if (length(existepas)>0) print(paste("warning: the following ids do not exist ", paste(existepas, collapse=" ")))
#         selected<-hop[[scale]][hop[[scale]]$id %in% selectid, ]
#       } else {selected<-hop[[scale]]}
#       #selected<-selected[!is.na(selected$Date),]
#       if (!is.null(simulationNames)) selected<-selected[selected$SimulationName %in% simulationNames,]
#       titi<-data.frame(xcoord=selected$Date, ycoord=selected[[variable]], id=selected$id, SimulationName=selected$SimulationName)
#       toto<-reshape(titi, direction="wide", v.names="ycoord", timevar="id", idvar= c("xcoord", "SimulationName"))
#     } else {
#       toto<-data.frame(xcoord=hop[[scale]]$Date, ycoord=hop[[scale]][[variable]], SimulationName=hop[[scale]][["SimulationName"]])
#       if (!is.null(simulationNames)) toto<-toto[toto$SimulationName %in% simulationNames,]
#     }
#     if (!is.null(DayofYear)) {toto<-toto[as.POSIXlt(toto$xcoord)$yday %in% DayofYear,]}
#     dfcoord<-empile(dfcoord, toto)
#     dfcoord<-dfcoord[-1,]
#     if (class(dfcoord$xcoord)=="numeric") dfcoord$xcoord<-as.Date(dfcoord$xcoord, origin="1970-01-01")
#   }
#   return(dfcoord)	
# }
# 
# addsimu<-function(formating, dfcoord, col=NULL, lty=NULL, lwd=NULL, pch=NULL, bg=NULL, cex=NULL){
#   if (!is.null(col)) formating$col<-col
#   if (!is.null(lty)) formating$lty<-lty
#   if (!is.null(lwd)) formating$lwd<-lwd
#   if (!is.null(pch)) formating$pch<-pch
#   if (!is.null(bg)) formating$bg<-bg
#   if (!is.null(cex)) formating$cex<-cex
#   for (i in 1:nrow(formating)) {
#     format<-formating[i,]
#     simu_name<-rownames(format)
#     print(simu_name)
#     xsubset<-dfcoord$xcoord[dfcoord$SimulationName==simu_name]
#     for (v in setdiff(names(dfcoord), c("xcoord", "SimulationName"))) {
#       ysubset<-dfcoord[dfcoord$SimulationName==simu_name, v]
#       if (!is.null(format$lty)) lines(xsubset, ysubset, col=format$col,lty=format$lty,lwd=format$lwd)
#       if (!is.null(format$pch)) points(xsubset, ysubset, col=format$col,pch=format$pch,bg=format$bg, cex=format$cex)
#     }
#   }
# }
# addid<-function(dfcoord, freq=0.1) {
#   # adds the ids on some randomly chosen points 
#   for (v in setdiff(names(dfcoord), c("SimulationName", "xcoord"))) {
#     nom<-gsub(pattern="ycoord.", replacement="", x=v, fixed=TRUE)
#     keep<-which(as.logical(rbinom(nrow(dfcoord), 1, freq)))
#     text(dfcoord$xcoord[keep], dfcoord[keep, v], nom)
#   }
# }
# addlegende<-function(formating, addlegend=FALSE) {
#   #if argument addlegend is boolean, add legend only if it is true (and allways lines on topleft and points on topright) ; 
#   # else addlegend should be a length 2 character vector giving the position of lines and then points 
#   if (class(addlegend)=="logical") {
#     if (addlegend) {
#       if (!is.null(formating$lty)) legend("topleft", legend=rownames(formating), col=formating$col,lty=formating$lty,lwd=formating$lwd, inset=0.01)
#       if (!is.null(formating$pch)) legend("topright", legend=rownames(formating), col=formating$col, pch=formating$pch, pt.bg=formating$bg, pt.cex=formating$cex, inset=0.01)
#     }
#   } else {
#     if (!is.null(formating$lty)) legend(addlegend[1], legend=rownames(formating), col=formating$col,lty=formating$lty,lwd=formating$lwd, inset=0.01)
#     if (!is.null(formating$pch)) legend(addlegend[2], legend=rownames(formating), col=formating$col,pch=formating$pch,pt.bg=formating$bg, pt.cex=formating$cex, inset=0.01)
#   }
# }
# plot.hop<-function(x, formating=NULL, xaxis="Time", yaxis=c(dbh="trees"), addlegend=FALSE, DayofYear=NULL, rangedate=NULL, selectid=NULL, addid=NULL, ...) {
#   #x= a hop object as returned from function readVirtualExperiment
#   #formating= a data.frame with the rownames equal to the simulationNames of the simulations you want to plot, and as columns, the formating (pch, col, bg, cex, lty, lwd) of points or lines
#   #xaxis= either "Time" or a named character, with the name equal to the variable you want to use as xaxis and the value equal to the export profile where this variable can be found
#   #yaxis= named vector, with the names equal to the variables you want to plot and the values equal to the export profile where these variables can be found
#   #addlegend (optional): either a logical (should the legend be plotted?) or a length 2 character vector taken from the possible locations of legend (“bottomright”, “bottom”, “bottomleft”, “left”, “topleft”, “top”, “topright”, “right”, “center”)
#   ##first one = legend of the lines, second= legend of the points 
#   #DayofYear (optional): only if xaxis="Time": vector of Julian days, only the data for these days is plotted
#   #rangedate (optional): only if xaxis="Time": range of dates you want to plot (important if you want to overlay other simulations with a different range of data)
#   #selectid (optional): only for trees or cells: numeric vector to select specific ids of trees or cells to plot
#   #addid (optional, use only if selectid is provided) : frequency of added id labels (]0,1])
#   if(is.null(formating)| nrow(formating)==0 | ncol(formating)==0) {print("formating must be provided, contain at least one row and have at least column pch or lty"); return()}
#   if (xaxis=="Time") {
#     dfcoord<-createdfcoordtime(x, yaxis, simulationNames=rownames(formating), DayofYear, selectid)
#     if (is.null(rangedate)) rangedate<-range(dfcoord$xcoord, na.rm=TRUE)
#     plot(rangedate, range(dfcoord[,setdiff(names(dfcoord), c("xcoord", "SimulationName"))], na.rm=TRUE), type="n", xlab="Time", ylab=paste(unique(names(yaxis)), collapse=" or "), ...)
#     addsimu(formating, dfcoord, ...)
#     if (!is.null(addid)) addid(dfcoord, addid)
#   } else {
#     
#     
#     
#     print("not done yet")
#   }
#   addlegende(formating, addlegend)
# }
# 
# 
# points.hop<-function(x, formating=NULL, xaxis="Time", yaxis=c(dbh="trees"), addlegend=FALSE, DayofYear=NULL, rangedate=NULL, selectid=NULL, addid=NULL, ...) {
#   if (xaxis=="Time") {
#     dfcoord<-createdfcoordtime(x, yaxis, simulationNames=rownames(formating), DayofYear, selectid)
#     addsimu(formating, dfcoord, ...)
#     if (!is.null(addid)) addid(dfcoord)
#   } else {
#     print("not done yet")
#   }
#   addlegende(formating, addlegend)
# }
# lines.hop<-points.hop
# 
# ################## map of one variable in cells
# map<-function(x, zaxis=c(monthCells=LAI), selectsimulations=NULL, selectdates=NULL, rows=NULL) {
#   toto<-x[[names(zaxis)]]
#   toto<-toto[as.character(toto$SimulationName) %in% selectsimulations & as.character(toto$Date) %in% selectdates,]
#   nbgraphes<-length(selectsimulations)*length(selectdates)
#   if (nbgraphes>1) {if (is.null(rows)) {
#     racine<-ceiling(sqrt(nbgraphes)) ; layout(matrix(1:racine^2, ncol=racine, byrow=TRUE))
#   } else {
#     if (rows=="date") {
#       layout(matrix(1:nbgraphes, nrow=length(selectdates), byrow=FALSE))
#     } else {
#       layout(matrix(1:nbgraphes, nrow=length(selectsimulations), byrow=TRUE))
#     }
#   }}
#   toto$zaxisaplotter<-eval(parse(text=zaxis), toto)
#   zlim<-range(toto$zaxisaplotter)
#   for (simu in selectsimulations) for (date in selectdates) {
#     dat<-toto[toto$SimulationName==simu & toto$Date==date, ]
#     if (nrow(toto)==0) {
#       print(paste("no data at date", date, "in simulation", simu))
#     } else {
#       matrice<-matrix(dat[order(dat$y, dat$x),"zaxisaplotter"], ncol=length(unique(dat$y)))
#       #filled.contour(z=matrice, main= paste(zaxis, simu, date, sep="\n"))
#       image(matrice, main=paste(zaxis, simu, date, sep="\n"), asp = ncol(matrice)/nrow(matrice), useRaster=TRUE, ylim=c(0,1), zlim=zlim, xaxt="n", yaxt="n")
#       abline(v=0.5, lty=2)
#     }
#   } 
# }
# 
# 
# ################# map of nitrogen fluxes
# 
# NitrogenMap<-function(df,stocksScale=1, fluxesScale=1, titre="Nitrogen Stocks and fluxes") {
#   #if (nrow(df)>1) layout(matrix(1:nrow(df), byrow=TRUE))
#   for (i in 1:nrow(df)){
#     x<-df[i,]
#     if (length(titre)==1) titre<-rep(titre, nrow(df))
#     if (nrow(df)>1) dev.new()
#     dev.new()
#     plot(c(0,100), c(0,100), type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n", main=titre[i])
#     rect(xleft=30-(x$residues*stocksScale/10), ybottom=50, xright=30, ytop=60, col = "lightgreen", border = "lightgreen")
#     text(30-(x$residues*stocksScale/10)/2, 55, "Residuals")
#     rect(xleft=30-sqrt(x$microorg*stocksScale), ybottom=40-sqrt(x$microorg*stocksScale), xright=30, ytop=40, col = "orange", border = "orange")
#     text(30-sqrt(x$microorg*stocksScale)/2, 40-sqrt(x$microorg*stocksScale)/2, "Microorg")
#     rect(xleft=55, ybottom=40, xright=55+sqrt(x$humus*stocksScale), ytop=40+sqrt(x$humus*stocksScale), col = "brown", border = "brown")
#     text(55+sqrt(x$humus*stocksScale)/2, 40+sqrt(x$humus*stocksScale)/2, "Humus")
#     rect(xleft=70, ybottom=30-sqrt(x$microorg*stocksScale)-sqrt(x$NO3*stocksScale), xright=70+sqrt(x$NO3*stocksScale), ytop=30-sqrt(x$microorg*stocksScale), col = "red", border = "red")
#     text(70+sqrt(x$NO3*stocksScale)/2, 30-sqrt(x$microorg*stocksScale)-sqrt(x$NO3*stocksScale)/2, "NO3")
#     arrows(x0=70, y0=30-sqrt(x$microorg*stocksScale)-sqrt(x$NO3*stocksScale), x1 = 0, y1 = 30-sqrt(x$microorg*stocksScale)-sqrt(x$NO3*stocksScale), length = 0, lwd=fluxesScale*x$NO3toPlant) #trait horizontal
#     arrows(x0=0, y0=30-sqrt(x$microorg*stocksScale)-sqrt(x$NO3*stocksScale), x1 = 0, y1 = 60, length = 0.25, lwd=fluxesScale*x$NO3toPlant)
#     arrows(x0=0, y0=80, x1 = 30, y1 = 80, length = 0, lwd=fluxesScale*x$PlanttoResidues) #trait horizontal
#     arrows(x0=30, y0=80, x1 = 30, y1 = 60, length = 0.25, lwd=fluxesScale*x$PlanttoResidues)
#     arrows(x0=30, y0=50, x1 = 30, y1 = 40, length = 0.25, lwd=fluxesScale*x$ResiduestoMicroorg)
#     arrows(x0=30, y0=40, x1 = 55, y1 = 40, length = 0.25, lwd=fluxesScale*x$MicroorgtoHumus)
#     arrows(x0=30, y0=40-sqrt(x$microorg*stocksScale), x1 = 70, y1 = 30-sqrt(x$microorg*stocksScale), length = 0.25, lwd=fluxesScale*x$MicroorgtoNO3)
#     arrows(x0=55+sqrt(x$humus*stocksScale), y0=40, x1 = 70, y1 = 30-sqrt(x$microorg*stocksScale), length = 0.25, lwd=fluxesScale*x$HumustoNO3)
#     arrows(x0=70+sqrt(x$NO3*stocksScale), y0=30-sqrt(x$microorg*stocksScale), x1 = 70+sqrt(x$NO3*stocksScale), y1 = 90, length = 0.25, lwd=fluxesScale*x$NO3toAtm)
#     arrows(x0=68+sqrt(x$NO3*stocksScale), y1=30-sqrt(x$microorg*stocksScale), x1 = 68+sqrt(x$NO3*stocksScale), y0 = 90, length = 0.25, lwd=fluxesScale*x$NO3toAtm)
#     arrows(x0=70+sqrt(x$NO3*stocksScale), y0=30-sqrt(x$microorg*stocksScale)-sqrt(x$NO3*stocksScale), x1 = 100, y1 = 30-sqrt(x$microorg*stocksScale)-sqrt(x$NO3*stocksScale), length = 0, lwd=fluxesScale*x$PlanttoResiduals) #trait horizontal
#     arrows(x0=100, y0=30-sqrt(x$microorg*stocksScale)-sqrt(x$NO3*stocksScale), x1 = 100, y1 = 60, length = 0.25, lwd=fluxesScale*x$NO3toTree)
#   }
# }
# 
# # x1<-data.frame(residues=10, microorg=50, humus=20, NO3=10,
# # 	PlanttoResiduals=4, ResiduestoMicroorg=0.5, 
# # 	MicroorgtoHumus=6, MicroorgtoNO3=10, HumustoNO3=3,
# # 	NO3toAtm=0.1, AtmtoNO3=5, NO3toPlant=1, NO3toTree=3)
# # NitrogenMap(x1, stocksScale=10, titre="jour 1")
# 
# # simuChristian<-lectureHisAFe("/Users/user/Documents/a_System/modelisation/Hi-SAFE/simulationsChristian/CD21_AF_A3_13x8_2_arbres_plot.txt")
# # reformate<-simuChristian$data[, c("Date", "nitrogenResidus", "microorgBiomass", "activeNitrogenHumusStock", "mineralNitrogenStock",
# # "nLeafLitter", "nitrogenImmobilisation",
# # "nitrogenHumification", "nitrogenRestitution", "nitrogenHumusMineralisation",
# # "nitrogenVolatilisation", "nitrogenFixation", "nitrogenExtractedByCrops", "nitrogenExtractedByTrees"
# # )]
# # names(reformate)<-c("date", "residues","microorg","humus","NO3","PlanttoResiduals","ResiduestoMicroorg", "MicroorgtoHumus",   
# # 	"MicroorgtoNO3","HumustoNO3","NO3toAtm","AtmtoNO3","NO3toPlant","NO3toTree"  )
# 
# # NitrogenMap(reformate[13000,], stocksScale=0.01, fluxesScale=0.1, titre="jour 13000")
# # jours<-c(10500, 11500, 12500, 13500)
# # NitrogenMap(reformate[jours,], stocksScale=0.1, fluxesScale=0.1, titre=paste("jour",  jours))
# 
# 
# 
# 
# 
# 
# 
