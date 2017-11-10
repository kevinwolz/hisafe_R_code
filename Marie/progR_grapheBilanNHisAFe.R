
NitrogenMap<-function(df,stocksScale=1, fluxesScale=1, titre="Stocks et flux d'azote") {
	#if (nrow(df)>1) layout(matrix(1:nrow(df), byrow=TRUE))
	for (i in 1:nrow(df)){
		x<-df[i,]
		if (length(titre)==1) titre<-rep(titre, nrow(df))
		if (nrow(df)>1) dev.new()
		plot(c(0,100), c(0,100), type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n", main=titre[i])
		rect(xleft=30-(x$residues*stocksScale/10), ybottom=50, xright=30, ytop=60, col = "lightgreen", border = "lightgreen")
		text(30-(x$residues*stocksScale/10)/2, 55, "Residuals")
		rect(xleft=30-sqrt(x$microorg*stocksScale), ybottom=40-sqrt(x$microorg*stocksScale), xright=30, ytop=40, col = "orange", border = "orange")
		text(30-sqrt(x$microorg*stocksScale)/2, 40-sqrt(x$microorg*stocksScale)/2, "Microorg")
		rect(xleft=55, ybottom=40, xright=55+sqrt(x$humus*stocksScale), ytop=40+sqrt(x$humus*stocksScale), col = "brown", border = "brown")
		text(55+sqrt(x$humus*stocksScale)/2, 40+sqrt(x$humus*stocksScale)/2, "Humus")
		rect(xleft=70, ybottom=30-sqrt(x$microorg*stocksScale)-sqrt(x$NO3*stocksScale), xright=70+sqrt(x$NO3*stocksScale), ytop=30-sqrt(x$microorg*stocksScale), col = "red", border = "red")
		text(70+sqrt(x$NO3*stocksScale)/2, 30-sqrt(x$microorg*stocksScale)-sqrt(x$NO3*stocksScale)/2, "NO3")
		arrows(x0=70, y0=30-sqrt(x$microorg*stocksScale)-sqrt(x$NO3*stocksScale), x1 = 0, y1 = 30-sqrt(x$microorg*stocksScale)-sqrt(x$NO3*stocksScale), length = 0, lwd=fluxesScale*x$NO3toPlant) #trait horizontal
		arrows(x0=0, y0=30-sqrt(x$microorg*stocksScale)-sqrt(x$NO3*stocksScale), x1 = 0, y1 = 60, length = 0.25, lwd=fluxesScale*x$NO3toPlant)
		arrows(x0=0, y0=80, x1 = 30, y1 = 80, length = 0, lwd=fluxesScale*x$PlanttoResidues) #trait horizontal
		arrows(x0=30, y0=80, x1 = 30, y1 = 60, length = 0.25, lwd=fluxesScale*x$PlanttoResidues)
		arrows(x0=30, y0=50, x1 = 30, y1 = 40, length = 0.25, lwd=fluxesScale*x$ResiduestoMicroorg)
		arrows(x0=30, y0=40, x1 = 55, y1 = 40, length = 0.25, lwd=fluxesScale*x$MicroorgtoHumus)
		arrows(x0=30, y0=40-sqrt(x$microorg*stocksScale), x1 = 70, y1 = 30-sqrt(x$microorg*stocksScale), length = 0.25, lwd=fluxesScale*x$MicroorgtoNO3)
		arrows(x0=55+sqrt(x$humus*stocksScale), y0=40, x1 = 70, y1 = 30-sqrt(x$microorg*stocksScale), length = 0.25, lwd=fluxesScale*x$HumustoNO3)
		arrows(x0=70+sqrt(x$NO3*stocksScale), y0=30-sqrt(x$microorg*stocksScale), x1 = 70+sqrt(x$NO3*stocksScale), y1 = 90, length = 0.25, lwd=fluxesScale*x$NO3toAtm)
		arrows(x0=68+sqrt(x$NO3*stocksScale), y1=30-sqrt(x$microorg*stocksScale), x1 = 68+sqrt(x$NO3*stocksScale), y0 = 90, length = 0.25, lwd=fluxesScale*x$NO3toAtm)
		arrows(x0=70+sqrt(x$NO3*stocksScale), y0=30-sqrt(x$microorg*stocksScale)-sqrt(x$NO3*stocksScale), x1 = 100, y1 = 30-sqrt(x$microorg*stocksScale)-sqrt(x$NO3*stocksScale), length = 0, lwd=fluxesScale*x$PlanttoResiduals) #trait horizontal
		arrows(x0=100, y0=30-sqrt(x$microorg*stocksScale)-sqrt(x$NO3*stocksScale), x1 = 100, y1 = 60, length = 0.25, lwd=fluxesScale*x$NO3toTree)
	}
	
}

x1<-data.frame(residues=10, microorg=50, humus=20, NO3=10,
	PlanttoResiduals=4, ResiduestoMicroorg=0.5, 
	MicroorgtoHumus=6, MicroorgtoNO3=10, HumustoNO3=3,
	NO3toAtm=0.1, AtmtoNO3=5, NO3toPlant=1, NO3toTree=3)
NitrogenMap(x1, stocksScale=10, titre="jour 1")

simuChristian<-lectureHisAFe("/Users/user/Documents/a_System/modelisation/Hi-SAFE/simulationsChristian/CD21_AF_A3_13x8_2_arbres_plot.txt")
reformate<-simuChristian$data[, c("Date", "nitrogenResidus", "microorgBiomass", "activeNitrogenHumusStock", "mineralNitrogenStock",
"nLeafLitter", "nitrogenImmobilisation",
"nitrogenHumification", "nitrogenRestitution", "nitrogenHumusMineralisation",
"nitrogenVolatilisation", "nitrogenFixation", "nitrogenExtractedByCrops", "nitrogenExtractedByTrees"
)]
names(reformate)<-c("date", "residues","microorg","humus","NO3","PlanttoResiduals","ResiduestoMicroorg", "MicroorgtoHumus",   
	"MicroorgtoNO3","HumustoNO3","NO3toAtm","AtmtoNO3","NO3toPlant","NO3toTree"  )

NitrogenMap(reformate[13000,], stocksScale=0.01, fluxesScale=0.1, titre="jour 13000")
jours<-c(10500, 11500, 12500, 13500)
NitrogenMap(reformate[jours,], stocksScale=0.1, fluxesScale=0.1, titre=paste("jour",  jours))




