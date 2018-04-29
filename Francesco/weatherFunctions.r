
weatherGeneration = function(realWeather,firstYear,lastYear,option="na.sample"){
	years=firstYear:lastYear
	bis=apply(matrix(years),1,isBissextile)
	realYears=as.numeric(levels(as.factor(realWeather$year)))
	realBis=apply(matrix(realYears),1,isBissextile)

	nLines = 365*sum(bis==FALSE)+366*sum(bis==TRUE)

	# creation de la matrice de sortie
	output=matrix(ncol=11,nrow=nLines)

	p=0
	for(i in years){

		#remplissage des colonnes year, month, day
		nd=nday(i)
		output[(p+1):(p+nd),1]=rep(i,nd)	#year
		output[(p+1):(p+nd),2]=month(i)		#month
		output[(p+1):(p+nd),3]=day(i)		#day

		#remplissage des autres colonnes

		if(option=="na.sample"){
			# les annees manquantes sont tirees avec remise dans les annees existantes

			if(sum(levels(as.factor(realWeather$year))==i)==1){
				y=i
			} else {
				y=sample(realYears[isBissextile(i)==realBis],1)
        print (paste("sampled"),i) # inserted by Reyes
			}

		}

		if(option=="sample"){
			y=sample(realYears[isBissextile(i)==realBis],1)
		}

		output[(p+1):(p+nd),4]	= realWeather$tmax[realWeather$year==y]
		output[(p+1):(p+nd),5]	= realWeather$tmin[realWeather$year==y]
		output[(p+1):(p+nd),6]	= realWeather$hrmax[realWeather$year==y]
		output[(p+1):(p+nd),7]	= realWeather$hrmin[realWeather$year==y]
		output[(p+1):(p+nd),8]	= realWeather$rg[realWeather$year==y]
		output[(p+1):(p+nd),9]	= realWeather$rain[realWeather$year==y]
		output[(p+1):(p+nd),10]	= realWeather$wind[realWeather$year==y]
		output[(p+1):(p+nd),11]	= realWeather$waterTableDepth[realWeather$year==y]


		p=p+nd
	}

	return(output)
}

month=function(year){
	l=c(31,28,31,30,31,30,31,31,30,31,30,31)
	if(isBissextile(year)){
		l[2]=29
	}
	out=vector()
	for(i in 1:12){
		out=c(out,rep(i,l[i]))
	}
	return(out)
}

day=function(year){
	l=c(31,28,31,30,31,30,31,31,30,31,30,31)
	if(isBissextile(year)){
		l[2]=29
	}
	out=vector()
	for(i in 1:12){
		out=c(out,1:l[i])
	}
	return(out)
}


nday=function(year){
	bis=isBissextile(year)
	if(bis){
		return(366)
	} else {
		return(365)
	}
}

isBissextile=function(year){
	out=FALSE

	if(year%%4==0){
		out=TRUE
		if(year%%100==0){
			out=FALSE
			if(year%%400==0){
				out=TRUE
			}
		}
	}
	return(out)
}
