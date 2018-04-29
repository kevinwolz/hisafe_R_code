getDayLength=function(julianDay,latitude=43.6){
	om = 0.017202 *(julianDay-3.244)
	teta = om + 0.03344 * sin(om) * (1 + 0.021 * cos(om)) - 1.3526
	sidec = 0.3978*sin(teta)
	sunDeclination = asin(sidec)
	codec = cos(sunDeclination)
	silat =sin(latitude/180*pi)
	colat =cos(latitude/180*pi)
	sinR = 0.01064
	AA = (-sinR - sidec * silat) / (codec * colat);
	dayLength =24/pi* acos(AA)
	return(dayLength)
}

getETP=function(tmoy,rg,vent,julianDay,hrmoy,latitude=43.6) {
	gama = 0.65
	latitude = 43.6

	delta = getDelta(tmoy)
	airVapourP=getVpSat(tmoy)*hrmoy/100
	dsat = getVpSat(tmoy) - airVapourP
	L = (2500840-2358.6*tmoy)/1000000


	fracinsol =((rg/getExtraRad(julianDay,latitude)) - 0.18) / 0.62
	fracinsol = sapply(fracinsol,max,0)
	var1 =(tmoy + 273.16)^4/1000000000
	var2 =(0.1 + 0.9*fracinsol)
	var3 = 0.56 - 0.08*sqrt(airVapourP)
	rglo = 4.9 * var1 * var2 * var3
	rnetp =(1-0.2)*rg-rglo
	etp = (rnetp/L*delta/(delta+gama)+(gama/(delta+gama))*(0.26*(1+0.54*vent))*dsat)
	etp = sapply(etp,max,0)
	return(etp)
}

getVpSat=function(temp){
	return (6.107 *	(1 +sqrt(2)*sin(pi*temp/3/180))^8.827)
}

getDelta=function(temp){
	return (getVpSat(temp+0.5) - getVpSat(temp-0.5))
}

getExtraRad=function(julianDay,latitude=43.6){
	om = 0.017202 *(julianDay-3.244)
	teta = om + 0.03344 * sin(om) * (1 + 0.021 * cos(om)) - 1.3526
	sidec = 0.3978*sin(teta)
	sunDeclination = asin(sidec)
	codec = cos(sunDeclination)
	silat =sin(latitude/180*pi)
	colat =cos(latitude/180*pi)
	sinR = 0.01064
	AA = (-sinR - sidec * silat) / (codec * colat);
	dayLength =24/pi* acos(AA)
	CC = 1370 * 3600* 1.e-6
	CC = CC * (1 + 0.033 * cos(2 * pi * (julianDay-4)/366))
	G0 = silat * sidec * dayLength
	G0 = G0 + colat * codec * (24/pi)*sin((pi/12)*(dayLength/2));
	return(G0 * CC)
}

getDoy=function(year,month,day){
	isBi=(((year%%4)==0)&(!((year%%100)==0)))|((year%%400)==0)
	nday=c(31,28,31,30,31,30,31,31,30,31,30,31)
	doy=day
	for(i in 1:11){
		doy=doy+nday[i]*(month>i)
	}

	doy=doy+isBi*(month>2)
	return(doy)
}

getDay=function(doy,year=1){
	isBi=(((year%%4)==0)&(!((year%%100)==0)))|((year%%400)==0)
	nday=c(31,28+isBi,31,30,31,30,31,31,30,31,30,31)
	day=doy
	sto=rep(F,length(day))
	i=1
	while((sum(sto)<length(sto))&(i<=11)){
		nouv=(day<=nday[i])
		day=day-nday[i]*((!nouv)&(!sto))
		sto=(sto|nouv)
		i=i+1
	}
	if(sum(day>31)>0){
		day[day>31]=getDay(day[day>31]-31,year+1)
	}
	return(day)
}

getMonth=function(doy,year=1){
	if(length(year)!=length(doy)){
		year=rep(year,length(doy))
	}
	isBi=(((year%%4)==0)&(!((year%%100)==0)))|((year%%400)==0)
	changeYear=(doy>(365+isBi))
	while(sum(changeYear)>0){
		doy[changeYear]=doy[changeYear]-365-isBi[changeYear]
		year[changeYear]=year[changeYear]+1
		isBi=(((year%%4)==0)&(!((year%%100)==0)))|((year%%400)==0)
		changeYear=(doy>(365+isBi))
	}
	month=rep(0,length(doy))
	nday=c(31,28,31,30,31,30,31,31,30,31,30,31)
	j=1
	while((sum(month==0)>0)&(j<=12)){
		month[doy<=(nday[j]+(j==2)*isBi)]=j
		doy[doy<=(nday[j]+(j==2)*isBi)]=500
		doy=doy-nday[j]-(j==2)*isBi
		j=j+1
	}
	return(month)
}

getYear=function(doy,year=1){
	if(length(year)!=length(doy)){
		year=rep(year,length(doy))
	}
	isBi=(((year%%4)==0)&(!((year%%100)==0)))|((year%%400)==0)
	changeYear=(doy>(365+isBi))
	while(sum(changeYear)>0){
		doy[changeYear]=doy[changeYear]-365-isBi[changeYear]
		year[changeYear]=year[changeYear]+1
		isBi=(((year%%4)==0)&(!((year%%100)==0)))|((year%%400)==0)
		changeYear=(doy>(365+isBi))
	}
	return(year)
}

isBis=function(year){
	return((((year%%4)==0)&(!((year%%100)==0)))|((year%%400)==0))
}

getEf=function(obs,pred){
	return(1-sum((obs-pred)^2,na.rm=T)/sum((obs-mean(obs,na.rm=T))^2,na.rm=T))
}

getRbiais=function(obs,pred){
	return(mean(obs-pred,na.rm=T)/mean(obs,na.rm=T))
}

saturation=function(nappe,profondeur){
	sat=vector()
	sat[1]=1*(nappe[1]>profondeur)
	for(i in 2:length(nappe)){
		sat[i]=(sat[i-1]+1)*(nappe[i]>profondeur)
	}
	return(sat)
}

predNappe=function(init,rain,etp, a=1.4e-05, b=2.25, l=120, m=6, n=0.6, ru=300, prof_nappe_max=540){
  # prof_nappe_max = 540 in AF et prof_nappe_max=  400 in TF

	sol=vector()
	sol[1]=100
	for(i in 2:length(rain)){
		sol[i]=sol[i-1]-etp[i]+min(500,rain[i])
		sol[i]=max(min(sol[i],ru),0)
	}

	glo=vector()
	glo[1]=init
	glo[2]=init

	for(i in 3:length(rain)){
		glo[i]=glo[i-1]-a*(prof_nappe_max+glo[i-1])^b
		pluie=rain[i-1]
		so=sol[i-2]
		glo[i]=glo[i]+((so+pluie)>l)*m*pluie^n
	}

	return(glo)
}
