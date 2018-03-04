# methodes pour la lecture et le traitement des donnees concernant les racines





# lecture de fichier et enregistrement en .Rdata
rootDataConverter=function(inFileName,outFileName){
	for(i in 1:length(inFileName)){
		temp = read.table(paste("data/",inFileName[i],sep=""),header=FALSE,dec=".",sep="\t")
		names(temp)=c("year","month","x","y","z","frdens")
		save(temp,file=paste("Rdata/",outFileName[i],sep=""))
		remove(temp)
	}
}

# import des donnees relatives aux racines
importRoots=function(nom,param=NULL,syst=NULL,variables=NULL){
	chemin="Rdata/"
	
	load(paste(chemin,"info.Rdata",sep=""))
	if(is.null(param)){
		param=info[[which(names(info)==nom)]]$param
	}
	if(is.null(syst)){
		syst=info[[which(names(info)==nom)]]$syst
	}
	
	output=list()
	simulation=listeFileNames(param,syst)
	simulationNames=simulation$names

	for(i in 1:length(simulationNames)){
		output[[i]]=importRdataFile(paste(chemin,nom,"_",simulationNames[i],".Rdata",sep=""))
		if(!is.null(variables)){
			output[[i]]=output[[i]][,sapply(names(output[[i]]), "%in%", variables)]
		}
	}
	names(output)=simulationNames
	output$info=list()
	output$info$simulations=simulation
	output$info$param=param
	output$info$syst=syst
	output$info$nb=length(simulationNames)
	output$info$variables=names(output[[1]])
	
	return(output)

}

# extraction des donnees correspondant au systeme racinaire d'un jour donne
getRootSyst=function(dat,id,year,month,day=1,fonction=NULL,...){
	output=dat[[id]][(dat[[id]]$year==year)&(dat[[id]]$month==month)&(dat[[id]]$day==day),]
	if(!is.null(fonction)){
		output=fonction(output,...)
	}
	return(output)
}

#getRootProfile=function(syst){
#	pos=as.factor(paste("x",syst$x,"y",syst$y))
#	lz=0.2
#	z=seq(from=0.1, to=max(syst$z), by=lz)
#	output=matrix(nrow=length(z)

# renvoie
getRootProfile=function(syst,y=NULL,x=NULL,nx=NULL,ny=NULL,freq=FALSE,lz=0.2){
	if(is.null(y)){
		y=as.numeric(levels(as.factor(syst$y)))
	}
	if(is.null(x)){
		x=as.numeric(levels(as.factor(syst$x)))
	}
	idx=rep(FALSE,length(syst$x))
	idy=rep(FALSE,length(syst$x))
	for(i in 1:length(x)){
		idx=idx|(syst$x==x[i])
	}
	for(i in 1:length(y)){
		idy=idy|(syst$y==y[i])
	}
	if(!is.null(nx)){
		for(i in 1:length(nx)){
			idx=idx & (syst$x != nx[i])
		}
	}
	if(!is.null(ny)){
		for(i in 1:length(ny)){
			idy=idy & (syst$y != ny[i])
		}
	}
	
	
	id=(idx & idy)
	z=(1:floor(4/lz))*lz-lz/2
	rootProfile=matrix(ncol=2,nrow=length(z))
	rootProfile[,1]=z
	for(i in 1:length(z)){
		rootProfile[i,2]=sum(syst$frdens[id & (syst$z==as.factor(z[i]))])
	}
	if(freq){
		rootProfile[,2]=rootProfile[,2]/sum(rootProfile[,2])
	}
	return(rootProfile)
}



getRootProp=function(syst,x=NULL,y=NULL,nx=NULL,ny=NULL,z=NULL, nz=NULL){
	if(is.null(x)){
		x=as.numeric(levels(as.factor(syst$x)))
	}
	if(is.null(y)){
		y=as.numeric(levels(as.factor(syst$y)))
	}
	if(is.null(z)){
		z=as.numeric(levels(as.factor(syst$z)))
	}
	
	if(!is.null(nx)){
		for(i in 1:length(nx)){
			x=x[x!=nx[i]]
		}
	}
	if(!is.null(ny)){
		for(i in 1:length(ny)){
			y=y[y!=ny[i]]
		}
	}
	if(!is.null(nz)){
		for(i in 1:length(nz)){
			z=z[z!=nz[i]]
		}
	}
	idx=rep(FALSE,length(syst$x))
	idy=rep(FALSE,length(syst$x))
	idz=rep(FALSE,length(syst$x))
	for(i in 1:length(x)){
		idx=idx|(syst$x==x[i])
	}
	for(i in 1:length(y)){
		idy=idy|(syst$y==y[i])
	}
	for(i in 1:length(z)){
		idz=idz|(syst$z==z[i])
	}
	id=(idx & idy & idz)
	return(sum(syst$frdens[id])/sum(syst$frdens))
}
	
	

getCenter=function(syst){
	output=list()
	output$x0=syst$x[syst$pfrdens==-1]
	output$y0=syst$y[syst$pfrdens==-1]
	return(output)
}

getZ50=function(x){return(x$z[order(x$z)][min(which(cumsum(x$frdens[order(x$z)])/sum(x$frdens)>=0.5))])}

getRootExtension=function(syst){
	output=list()
	output$z=max(syst$z)+0.1
	x0=syst$x[syst$pfrdens==-1]
	y0=syst$y[syst$pfrdens==-1]
	output$x=c(abs(min(syst$x)-x0)+0.5,max(syst$x)-x0+0.5)
	output$y=c(abs(min(syst$y)-y0)+0.5,max(syst$y)-y0+0.5)
	return(output)
}

getRootedVolume=function(syst,voxelVolume=0.2){
	return(length(syst$x)*voxelVolume)
}

getMeanDistance=function(syst,center){
	som=0
	for(i in 1:length(syst$x)){
		d=sqrt((syst$x[i]-center$x)^2+(syst$y[i]-center$y)^2+(syst$z[i]-center$z)^2)
		som=som+d*syst$frdens[i]
	}
	return(som/sum(syst$frdens))
}


plotRoot=function(rsyst,grid=FALSE,box=TRUE,...){
	require(rgl)
	rgl.open()
	par3d(...)
	YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
	co = colorRampPalette(YlOrBr, space = "Lab")
	cols=co(1500)         

	for(i in 1:length(rsyst$x)){
		
		mod=sqrt((rsyst$x[i]-rsyst$px[i])^2+(rsyst$y[i]-rsyst$py[i])^2+(rsyst$z[i]-rsyst$pz[i])^2)
		if(mod<2){
			compte=0
			r=rsyst$ray[i]
			if(rsyst$x[i]==rsyst$px[i]){
				x=rsyst$x[i]+r*sin(1:2000*pi/10)
				compte=compte+1
			} else {
				x=seq(from=rsyst$x[i],to=rsyst$px[i],length=2000)
			}
			if(rsyst$y[i]==rsyst$py[i]){
				y=rsyst$y[i]+r*sin(1:2000*pi/10)*(compte==0)+r*cos(1:2000*pi/10)*(compte>=1)
				compte=compte+1
			} else {
				y=seq(from=rsyst$y[i],to=rsyst$py[i],length=2000)
			}
			if(rsyst$z[i]==rsyst$pz[i]){
				z=-(rsyst$z[i]+r*cos(1:2000*pi/10))
			} else {
				z=-(seq(from=rsyst$z[i],to=rsyst$pz[i],length=2000))
			}
			if(rsyst$pfrdens[i]==-1){
				rsyst$pfrdens[i]=rsyst$frdens[i]
			}
			idCol=max(1,min(floor(rsyst$frdens[i]),1500))
			rgl.linestrips(x,z,y,col=cols[idCol],lwd=2)
		}
	}
	if(box)	rgl.bbox()
	if(grid){
		centerpos=which(rsyst$pz==0)
		center=c(rsyst$x[centerpos],rsyst$y[centerpos],-rsyst$z[centerpos])
		addGrid(center,xlim=c(min(rsyst$x),max(rsyst$x)),ylim=c(min(rsyst$y),max(rsyst$y)),zlim=c(min(rsyst$z),max(rsyst$z)),xstep=1,ystep=1,zstep=0.2)
	}
}


addGrid=function(center,xlim,ylim,zlim,xstep,ystep,zstep){
	
	zlim=-zlim[order(-zlim)]
	# quels x
	x=c(center[1]-xstep/2,center[1]+xstep/2)
	while((x[1])>=xlim[1]){
		x=c(x[1]-xstep,x)
	}
	while((x[length(x)])<=xlim[2]){
		x=c(x,x[length(x)]+xstep)
	}
	
	# quels y
	y=c(center[2]-ystep/2,center[2]+ystep/2)
	while((y[1])>=ylim[1]){
		y=c(y[1]-ystep,y)
	}
	while((y[length(y)])<=ylim[2]){
		y=c(y,y[length(y)]+ystep)
	}

	#quels z
	z=c(center[3]-zstep/2,center[3]+zstep/2)
	while((z[1])>=zlim[1]){
		z=c(z[1]-zstep,z)
	}
	while((z[length(z)])<=zlim[2]){
		z=c(z,z[length(z)]+zstep)
	}
	
	# tracer
	for(i in x){
		for(j in y){
			lines3d(c(i,i),c(min(z),max(z)),c(j,j),alpha=1,col=1)
		}
		for(k in z){
			lines3d(c(i,i),c(k,k),c(min(y),max(y)),alpha=1,col=1)
		}
	}
	for(j in y){
		for(k in z){
			lines3d(c(min(x),max(x)),c(k,k),c(j,j),alpha=1,col=1)
		}
	}
}

ApplyToRoots=function(systs,fonction,...){
	output=list()
	datefac=as.factor(paste("year",systs$year,"month",systs$month,"day",systs$day,sep="_"))
	for(i in 1:nlevels(datefac)){
		output[[i]]=fonction(systs[datefac==levels(datefac)[i],],...)
	}
	names(output)=as.character(levels(datefac))
}


getProf=function(syst){
	return(max(syst$z))
}


ApplyToSysts=function(root,fonction,...){
	temp=sapply(root[1:(length(root)-1)],function(x,tim){tim=as.factor(paste(x$year,x$month));return(unsplit(sapply(split(x,tim),fonction,...),unique(tim)))})
	yearMonth=data.frame(t(sapply(unique(paste(root[[1]]$year,root[[1]]$month)),function(x){return(strsplit(x," ")[[1]])})))
	names(yearMonth)=c("year","month")
	attr(temp,"fac")=yearMonth
	return(temp)
}
	
	
