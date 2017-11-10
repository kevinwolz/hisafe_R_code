options(stringsAsFactors = FALSE) 
.libPaths("/Users/user/Documents/b_packageR")
rm(list=ls())
#source("/Users/user/Documents/a_System/modelisation/Hi-SAFE/progR_autourHisAFe/progR_readHOP.R")
source("/Users/user/Documents/b_maison/formationHisAFe/progR_readHOP.R")
# virtual experimental plan

	explan<-expand.grid(orientation=c(90,180), distance=c(17,35), stringsAsFactors = FALSE)
	rownames(explan)<-paste("Sim_Lat_L45_D", explan$distance,"_O",explan$orientation,sep="")
	#row names MUST be the names of the files, which MUST be the names of the simulations (as written in the output files)
######## here, run the simulations



######### read in the simulation outputs and save them as Rdata
	resultsim<-readVirtualExperiment(experimentalplan= explan, folder= "/Users/user/Documents/a_System/autreschercheurs/CelineBlitz/Latitude6")
	#as it is very long to read all the data, save the results as Rdata to be able to reopen them quickly
	#save(resultsim, file="/Users/user/Documents/a_System/autreschercheurs/CelineBlitz/Latitude4/Hoplu.Rdata")
	save(resultsim, file="/Users/user/Documents/b_maison/formationHisAFe/Hoplu.Rdata")
	summary(resultsim)

########### examples of functions to plot the data:
	options(stringsAsFactors = FALSE) 
	.libPaths("/Users/user/Documents/b_packageR")
	rm(list=ls())
	#load(file="/Users/user/Documents/a_System/autreschercheurs/CelineBlitz/Latitude4/Hoplu.Rdata")
	load(file="/Users/user/Documents/b_maison/formationHisAFe/Hoplu.Rdata")
	
	#source("/Users/user/Documents/a_System/modelisation/Hi-SAFE/progR_autourHisAFe/progR_readHOP.R")
	source("/Users/user/Documents/b_maison/formationHisAFe/progR_readHOP.R")

	
 	#prepare the formating of the points/lines for each simulation
	formats<-attr(resultsim, "experimentalplan")
	formats[,1:ncol(formats)]<-lapply(formats[,1:ncol(formats)], as.character)
	formats$pch=c(1,1,2,2) #distance
	formats$col=c(2,3,2,3) #orientation
	formats$bg=c(2,3,2,3) #orientation
	formats$lty=c(1,2,1,2) #orientation
	formats$lwd=c(3,3,1,1) #orientation
	formats$cex=c(1.1,1.1,0.8,0.8) #distance

	# plot the dbh as a function of time
	plot(resultsim, formating= formats[,c("col", "bg", "lty", "lwd", "cex")], xaxis="Time", yaxis=c(dbh="trees"), addlegend=TRUE)
	

	#dataframe "formatting" allows choosing which simulations are plotted and if points or lines are used
	##which simulations: those that are present in one line of it
	##if pch is given in formating, points are drawn
	##if lty is given, lines are drawn
	plot(resultsim, formating= formats[formats$distance==17 & formats$orientation==180,c("col", "lty", "lwd")], xaxis="Time", yaxis=c(dbh="trees"), addlegend=TRUE)
	#possible to add points or lines (actually, wether it is point or line is given by the formating rather than the name of the function)
	#if addlegend is not logical, it should be a character vector with 2 elements taken from the possible locations of legend (“bottomright”, “bottom”, “bottomleft”, “left”, “topleft”, “top”, “topright”, “right”, “center”)
	##first one = legend of the lines, second= legend of the points
	lines(resultsim, formating= formats[formats$distance==35 & formats$orientation==180,c("col", "lty", "lwd")], xaxis="Time", yaxis=c(dbh="trees"), addlegend=c("bottomright", NA))
	#if DayofYear is given (vector of Julian days), only the data for these days is plotted
	points(resultsim, formating= formats[formats$distance==35 & formats$orientation==90,c("col", "pch", "bg", "cex")], xaxis="Time", yaxis=c(dbh="trees"), addlegend=c(NA, "bottomleft"), DayofYear=1)
	
	#selectid allows representing only some selected individuals among cells (or trees), addid is the frequency of points labelled with the id
	#rangedate allows zooming only on a portion of the time
	#source("/Users/user/Documents/a_System/modelisation/Hi-SAFE/progR_autourHisAFe/progR_readHOP.R")
	plot(resultsim, formating= formats[formats$distance==35 & formats$orientation==90,c("col", "lty", "lwd")], 
		selectid=c(1, 100, 200), addid=0.1,
		rangedate=as.Date(c("2000-01-01", "2005-12-31")),
		xaxis="Time", yaxis=c(monthLai="monthCells"), addlegend=FALSE
	)
	
	#map a variable
	dev.new()
	map(resultsim, zaxis=c(monthCells="monthRelativeDiffuseParIncident"), 
		selectsimulations="Sim_Lat_L45_D35_O90", 
		selectdates="2000-05-31")
	)
	#if more than one date and/or simulation is chosen, it divides the window into
	# if "rows" is provided (either date or simulation), it uses the number of dates (respectively simulations) for number of rows
	map(resultsim, zaxis=c(monthCells="monthRelativeDiffuseParIncident"), 
		selectsimulations=c("Sim_Lat_L45_D35_O90", "Sim_Lat_L45_D35_O180"), 
		selectdates=c("2000-05-31","2015-05-31","2030-05-31"), rows="date"
	)
	dev.new(); map(resultsim, zaxis=c(monthCells="monthRelativeDiffuseParIncident"), 
		selectsimulations=c("Sim_Lat_L45_D35_O90", "Sim_Lat_L45_D17_O90"), 
		selectdates=c("2000-05-31","2015-05-31","2030-05-31"), rows="simulation"
	)
	# if rows is not provided, it uses the best n*n layout to show all graphs in a square layout
	dev.new(); map(resultsim, zaxis=c(monthCells="monthRelativeDiffuseParIncident"), 
		selectsimulations=c("Sim_Lat_L45_D35_O90"), 
		selectdates=c("1996-05-31", "2000-05-31","2005-05-31", "2010-05-31","2015-05-31", "2020-05-31", "2025-05-31", "2030-05-31", "2034-05-31")
	)
	
	#instead of variable, you can provide a formula (all data has to come from the same profile)
	dev.new(); map(resultsim, zaxis=c(monthCells="monthDiffuseParIntercepted + monthDirectParIntercepted"), 
		selectsimulations=c("Sim_Lat_L45_D35_O90", "Sim_Lat_L45_D35_O180"), 
		selectdates=c("2000-04-30","2030-04-30"), rows="date"
	)

	
	#plot something else as a function of dbh

	#draw map of the scene (or a "slice" of voxels)




