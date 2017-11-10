options(stringsAsFactors = FALSE) 

empile<-function(...){
  N<-unique(unlist(lapply(list(...),names)))
  result<-NULL
  for(DF in list(...)){
    x<-as.data.frame(lapply(N,function(n)if(n %in% colnames(DF)) DF[,n]
else NA))
    names(x)<-N
    result<-rbind(result,x)
  }
  result
} 

##### read in format V5.1 (always keeps all the parameters, even if they are for formalisms that are not used)
lectureplt<-function(fichier){
	toto<-readLines(fichier, encoding="latin1")
	toto<-strsplit(toto, split=":", fixed=TRUE)
	toto<-rapply(toto, function(x) gsub("^\\s+|\\s+$", "", x), classes="character", how="replace") #on enleve les tabs, leading et trailing blanks
	resultat<-list()
	debutsforma<-which(unlist(lapply(toto, "[[",1))=="F")
	debutsforma<-c(debutsforma, length(toto)+1)
	for (i in 1:(length(debutsforma)-1)){ #pour chaque formalisme
		formalisme<-toto[[debutsforma[i]]][2]
		parametresforma<-numeric()
		for (j in (debutsforma[i]+1):(debutsforma[i+1]-1)) { #pour chaque ligne a l interieur du formalisme
				split<-strsplit(toto[[j]][length(toto[[j]])], split="  ", fixed=TRUE)[[1]] #on prend ce qui est apres le : (s il y e na un) et on split par double espace (on prend [[1]] pour que soit pas liste)
				if (length(split)==1) split<-strsplit(split,split=" ", fixed=TRUE)[[1]] #si pas de double espace, on split par espace
				param<-gsub("^\\s+|\\s+$", "",split[[length(split)]]) #on enleve les tabs, leading et trailing blanks
				names(param)<-split[[1]]
				parametresforma<-c(parametresforma, param)
		}
		nouveauformalisme<-list(parametresforma)
		names(nouveauformalisme)<-formalisme
		resultat<-c(resultat,nouveauformalisme)
	}
	return(resultat)
}

##### read in format V8 (if keepall is TRUE, keeps all parameters, if keepall is FALSE)
lectureparam<-function(pliste, prefix="") {x<-pliste$text; names(x)<-ifelse(prefix=="", pliste$".attrs"["nom"], paste(prefix, pliste$".attrs"["nom"], sep=".")); return(x)}
lectureoption<-function(pliste) {
	nomoption<-unname(pliste$".attrs"["nomParam"])
	valeurchoix<-unname(pliste$".attrs"["choix"])
	names(valeurchoix)<-nomoption
	pliste<-pliste[1:(length(pliste)-1)]
	bonchoix<- unname(unlist(lapply(pliste, function(x) {if (class(x) == "list") return(x$'.attrs'["code"]== valeurchoix) else return(x["code"]==valeurchoix)})))
	resultat<-c(valeurchoix, do.call(lectureliste, args=list(pliste=pliste[bonchoix][[1]], nomparent=nomoption)))
	return(resultat)
}
lecturetv<-function(pliste) {
	nbvar<-length(pliste[names(pliste)=="variete"])
	if (nbvar>1) print(paste("attention! lectureXML ne lit que la premiere variété (il y en avait ", nbvar))
	pliste<-pliste[[1]] #we read only the first cultivar
	nomparent<-unname(pliste$".attrs"["nom"])
	resultat<-lectureliste(pliste, nomparent=nomparent)
	return(resultat)
}
lectureoptionv<-function(pliste){
	resultat<-lectureliste(pliste)
	return(resultat)
}
lectureliste<-function(pliste, nomparent="", prefix="") {
	resultat<-character(0)
	for (i in 1:length(pliste)) {
		if (class(pliste[i])!="list") {} else {
			if (names(pliste)[i]=="param") resultat<-c(resultat, lectureparam(pliste[[i]])) else {
				if (names(pliste)[i]=="option") resultat<-c(resultat, lectureoption(pliste[[i]])) else {
					if (names(pliste)[i]==".attrs") {} else {
						if (names(pliste)[i]=="tv") {resultat<-c(resultat, lecturetv(pliste[[i]]))} else {
							if (names(pliste)[i]=="optionv") {resultat<-c(resultat, lectureoptionv(pliste[[i]]))} else {
								print(paste("lecture de", names(pliste)[i], "pas encore codée (dans", nomparent,")"))
							}
						}
					}
				}
			}
		}
	}
	return(resultat)
}
lecturepltxml<-function(fichier, keepall=FALSE){ #keepall= garde toutes les valeurs, memes celles de formalismes pas utilises
	toto<-gsub("^\\s+|\\s+$", "", readLines(fichier, encoding="latin1")) #enlever les tabs 
	toto<-toto[-c(1,2)] #enlever les 2 premieres lignes
	debutsforma<-which(substr(toto,1, 11)=="<formalisme")
	finsforma<-which(substr(toto,1, 12)=="</formalisme")
	resultat<-list()
	for (i in 1:length(debutsforma)){ #pour chaque formalisme
		formalisme<-gsub(pattern='"', replacement="", gsub(pattern=">", replacement="", strsplit(toto[debutsforma[i]],split="nom=", fixed=TRUE)[[1]][[2]]))
		if (keepall) {
			parametresforma<-numeric()
			for (j in (debutsforma[i]+1):(finsforma[i]-1)) { #pour chaque ligne a l interieur du formalisme
					quoi<-strsplit(toto[j], split=" ", fixed=TRUE)[[1]][1]
					if (quoi=="<param") {
						param<-strsplit(gsub(pattern=">", replacement="<", toto[j]), split="<", fixed=TRUE)[[1]][3]
						names(param)<-strsplit(strsplit(toto[j], split="nom=", fixed=TRUE)[[1]][2], split='"', fixed=TRUE)[[1]][2]
					} else if (quoi=="<option") {
						param<-strsplit(strsplit(toto[j], split="choix=", fixed=TRUE)[[1]][2], split='"', fixed=TRUE)[[1]][2]
						names(param)<-strsplit(strsplit(toto[j], split="nomParam=", fixed=TRUE)[[1]][2], split='"', fixed=TRUE)[[1]][2]
					}
					parametresforma<-c(parametresforma, param)
			}
			
		} else {
			library("XML") #easier
			node<-parseXMLAndAdd(txt=paste(toto[(debutsforma[i]+1):(finsforma[i]-1)], collapse="\n"))
			liste<-xmlToList(node)
			parametresforma<-lectureliste(liste)
		}
		nouveauformalisme<-list(parametresforma)
		names(nouveauformalisme)<-formalisme
		resultat<-c(resultat,nouveauformalisme)
	}
	return(resultat)
}


newpltfromxml<-function(pltv8, templatev5) {
	toto<-readLines(templatev5, encoding="latin1")
	paramsxml<-lecturepltxml(pltv8)
	renommages<-c("phyllotherme", "ratiodurvieI", "nbjgrain", "allocfrmax", "stdrpnou")
	names(renommages)<-c("plastochrone", "durvieI", "nbjgrains", "allocamx", "sdrpnou")
	#"codtefcroi" n existe jamais
	#codesymbiose n existe jamais
	#hunod
	result<-character(0)
	for (line in 1:length(toto)){
		split<-strsplit(toto[line],split="%", fixed=TRUE)[[1]]
		if (length(split)>1){#il y avait un parametre, indique par %
			#on cherche le parametre dans pltv8
			paramname<-split[2]
			paramval<-unlist(unname(paramsxml))[paramname]
			if (is.na(paramval)) {#donc pas de parametre
				if (paramname %in% names(renommages)) {
					ligne<-paste(split[1], unlist(unname(paramsxml))[renommages[paramname]][1], sep="")
					result<-c(result, ligne)
				} else { #param n est pas dans la version 8
					if (paramname=="codtefcroi") {
						ligne<-paste(split[1], 1, sep="")
						result<-c(result, ligne) 
						print(paste("attention: le parametre codtefcroi (ligne ", line, ") n'a pas été trouvé dans le fichier version8, il a ete fixe a 1 par defaut", sep=""))
					} else if (paramname=="codesymbiose") {
						ligne<-paste(split[1], 2, sep="")
						result<-c(result, ligne) 
						print(paste("attention: le parametre codesymbiose (ligne ", line, ") n'a pas été trouvé dans le fichier version8, il a ete fixe a 2 par defaut", sep=""))
					} else if (paramname=="hunod") {
						ligne<-paste(split[1], 1.5, sep="")
						result<-c(result, ligne) 
						print(paste("attention: le parametre hunod (ligne ", line, ") n'a pas été trouvé dans le fichier version8, il a ete fixe a 1.5 par defaut", sep=""))
					} else	{
						print(paste("attention: le parametre ", paramname, " (ligne ", line, ") n'a pas été trouvé dans le fichier version8", sep=""))
						result<-c(result, toto[line])
					}
				}
			} else {
				if (length(paramval)>1) print(paste("attention: il y a plusieurs valeurs possibles pour le parametre", paramname, "on a pris le premier mais il faut verifier"))
				ligne<-paste(split[1], paramval[1], sep="")
				result<-c(result, ligne)
			}
		} else { #pas de % donc pas de parametre
			result<-c(result, toto[line])
		}
	}
	print("ne pas oublier de changer le nom de la culture")
	print("ATENTION: dans le template, il n y avait qu une variete, donc le nouveau fichier ne contient que la premiere variete (ne pas oublier de changer son nom")
	return(result)
}

compareplt<-function(x,y,xanglais=TRUE, yanglais=TRUE, affiche=TRUE, nomx="x", nomy="y"){
	francais <- c("nom plante" ,"développement" ,"début de végétation", "feuillage", "interception du rayonnement", 
	"croissance en biomasse", "repartition entre organes", "croissance et rendement", "racines", "gel", "eau", "azote", "variétal")
	names(francais)<-c("plant name and group", "phasic development", "emergence and starting", "leaves", "radiation interception", 
	"shoot biomass growth", "partitioning of biomass in organs", "yield formation", "roots", "frost", "water", "nitrogen", "cultivar parameters")
	if (xanglais & ! yanglais) names(x)[names(x) %in% names(francais)]<-francais[names(x)[names(x) %in% names(francais)]]
	if (!xanglais & yanglais) names(y)[names(y) %in% names(francais)]<-francais[names(y)[names(y) %in% names(francais)]]
	if (affiche) for (forma in union(names(x), names(y))) {
		print(paste("###########################", forma, "###########################"))
		
		parametres<-intersect(names(x[[forma]]), names(y[[forma]]))
		df<-data.frame(parametre=parametres, x=x[[forma]][parametres], y=y[[forma]][parametres])
		differents<-df[ (df$x!=df$y) & (!is.na(as.numeric(df$x)) & !is.na(as.numeric(df$y)) & as.numeric(df$x)!=as.numeric(df$y)),]
		if(nrow(differents)>0) {print("differents:"); print(differents)}

		manquants<-setdiff(names(y[[forma]]), names(x[[forma]]))
		if(length(manquants)>0) {print("manquants dans x"); print(y[[forma]][manquants])}

		manquants<-setdiff(names(x[[forma]]), names(y[[forma]]))
		if(length(manquants)>0) {print("manquants dans y"); print(x[[forma]][manquants])}
	}
	dfx<-data.frame()
	for(forma in names(x)) for (paramname in names(x[[forma]])) dfx<-rbind(dfx, data.frame(formalisme=forma, parametre=paramname, valeur=x[[forma]][paramname]))
	dfx[!is.na(as.numeric(dfx[,3])),3]<-as.numeric(dfx[!is.na(as.numeric(dfx[,3])),3])
	names(dfx)[3]<-nomx
	dfy<-data.frame()
	for(forma in names(y)) for (paramname in names(y[[forma]])) dfy<-rbind(dfy, data.frame(formalisme=forma, parametre=paramname, valeur=y[[forma]][paramname]))
	dfy[!is.na(as.numeric(dfy[,3])),3]<-as.numeric(dfy[!is.na(as.numeric(dfy[,3])),3])
	names(dfy)[3]<-nomy
	comparaison<-merge(dfx, dfy, all=TRUE)
	comparaison<-comparaison[
		(is.na(comparaison[,3]) & !is.na(comparaison[,4]))
		| (!is.na(comparaison[,3]) & is.na(comparaison[,4]))
		| (!is.na(comparaison[,3]) & !is.na(comparaison[,4]) & comparaison[,3]!=comparaison[,4])
	,]
	return(comparaison)
}

########################exemples d utilisation

	orge<-newpltfromxml(pltv8="/Users/user/Documents/a_System/modelisation/STICS/JavaSTICS-v131-stics-v841/plant/barley_plt.xml",
		templatev5="/Users/user/Mes documents/Copie_speciesLibraries/template_mainCrop.plt")
	connection<-file("/Users/user/Mes documents/Copie_speciesLibraries/nouvellesplantes/barley.plt", encoding="latin1")#oblige car sinon les caracteres speciaux passent pas sous mac
	write(orge,file=connection)
	close(connection)

	ble1<-lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/wheat.plt")
	ble2<-lecturepltxml("/Users/user/Documents/a_System/modelisation/STICS/JavaSTICS-v131-stics-v841/plant/wheat_plt.xml")
	ble3<-lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/parametresenvoyesparMarieLaunay/Ble.plt")

	compareplt(ble1, ble2, xanglais=FALSE)
	compareplt(ble1, ble3, xanglais=FALSE, yanglais=FALSE)


	compareplt(lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/durum-wheat.plt"), 
		lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/parametresenvoyesparMarieLaunay/Bledur.plt"),
		 xanglais=FALSE, yanglais=FALSE)
	compareplt(lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/rape.plt"), 
		lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/parametresenvoyesparMarieLaunay/Colza.plt"),
		 xanglais=FALSE, yanglais=FALSE)
	compareplt(lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/alfalfa.plt"), 
		lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/parametresenvoyesparMarieLaunay/luzerne.plt"),
		 xanglais=FALSE, yanglais=FALSE)
	compareplt(lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/maize.plt"), 
		lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/parametresenvoyesparMarieLaunay/Mais.plt"),
		 xanglais=FALSE, yanglais=FALSE)
	compareplt(lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/grass.plt"), 
		lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/parametresenvoyesparMarieLaunay/Prairie.plt"),
		 xanglais=FALSE, yanglais=FALSE)
	compareplt(lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/soybean.plt"), 
		lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/parametresenvoyesparMarieLaunay/Soja.plt"),
		 xanglais=FALSE, yanglais=FALSE)
	compareplt(lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/sunflower.plt"), 
		lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/parametresenvoyesparMarieLaunay/Tournsol.plt"),
		 xanglais=FALSE, yanglais=FALSE)

	"
	ça m'a permis de vérifier quels paramètres avaient été modifiés dans Hi-sAFe par rapport à leur paramétrage standard. C'est principalement des différences sur le blé dur car il a sans doute été adapté en fonction des données qu'on avait à Montpellier, mais il y a quelques différences qui m'ont posé question (surtout la question 4):
	1) dans le paramétrage du Colza que tu m'as envoyé, il est indiqué que le colza est une monocotylédone ; je suppose que c'est une erreur qui a été corrigée dans les versions suivantes, mais on ne sait jamais...
	Oui.
	2) dans le paramétrage du Maïs que tu m'as envoyé, le paramètre qui s'appelle allocamx dans tous les autres fichiers s'appelle allocfrmax, est-ce parce qu'il y a un formalisme spécial pour le Ma¨s et je dois le corriger dans mon fichier plante, ou est-ce une erreur?
	Non, c'est encore une erreur.
	3) dans le paramétrage de la luzerne que tu as envoyé, le paramètre codebeso vaut 2 (modèle résistif) alors que j'avais 1 (coefficient cultural dans mon fichier), que conseilles-tu de prendre?
	Je garderais codebeso = 1.
	4) plus important: dans tous les fichiers que tu as envoyés, le paramètre longsperac (longueur spécifique des racines) est très petit (11e-3 à 0.0055), mais dans notre paramétrage du blé dur, il est très grand (18182).
	Oui car il y a eu une modif du formalisme entre deux versions. Il faudrait que je vois dans la version que tu utilises (ou veux utiliser) qu'elle est l'unité de longspêrac pour te donner les bonnes valeurs.
	4bis) J'ai été intriguée par cette différence, et j'ai fait des simulations avec les deux valeurs, et elles donnent rigoureusement la même chose. Ça me parait étrange, car une telle différence de longueur spécifique devrait conduire à de grosses différences en termes de densité racinaire. As-tu une idée de la cause de cette absence de réponse?
	Oui, c'est parce que longsperac est utilisé a posteriori pour traduire de la longueur racinaire en biomasse racinaire (fournie en sortie). Mais la densité racinaire est donnée en longueur de racines par volume de sol, et calculée en amont.
	"

	######pour verification: on reprend une culture qu on a deja
	ble<-newpltfromxml(pltv8="/Users/user/Documents/a_System/modelisation/STICS/JavaSTICS-v131-stics-v841/plant/wheat_plt.xml",
		templatev5="/Users/user/Mes documents/Copie_speciesLibraries/template_mainCrop.plt")
	connection<-file("/Users/user/Mes documents/Copie_speciesLibraries/nouvellesplantes/ble.plt", encoding="latin1")#oblige car sinon les caracteres speciaux passent pas sous mac
	write(ble,file=connection)
	close(connection)

	compareplt(x=lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/wheat.plt"), 
		y=lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/nouvellesplantes/ble.plt"),
		 xanglais=FALSE, yanglais=FALSE)

	colza<-newpltfromxml(pltv8="/Users/user/Documents/a_System/modelisation/STICS/JavaSTICS-v131-stics-v841/plant/rapeseed_plt.xml",
		templatev5="/Users/user/Mes documents/Copie_speciesLibraries/template_mainCrop.plt")
	connection<-file("/Users/user/Mes documents/Copie_speciesLibraries/nouvellesplantes/rapeseed.plt", encoding="latin1")#oblige car sinon les caracteres speciaux passent pas sous mac
	write(colza,file=connection)
	close(connection)

	compareplt(x=lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/rape.plt"), 
		y=lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/nouvellesplantes/rapeseed.plt"),
		 xanglais=FALSE, yanglais=FALSE)



	compareplt(lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/rape.plt"), 
		lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/parametresenvoyesparMarieLaunay/Tournsol.plt"),
		 xanglais=FALSE, yanglais=FALSE)

	ble1<-lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/wheat.plt")
	ble2<-lecturepltxml("/Users/user/Documents/a_System/modelisation/STICS/JavaSTICS-v131-stics-v841/plant/wheat_plt.xml")
	ble3<-lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/parametresenvoyesparMarieLaunay/Ble.plt")

	compareplt(x=lecturepltxml("/Users/user/Documents/a_System/modelisation/STICS/JavaSTICS-v131-stics-v841/plant/barley_plt.xml")
		, y=lecturepltxml("/Users/user/Documents/a_System/modelisation/STICS/JavaSTICS-v131-stics-v841/plant/barleyint_plt.xml")
		, xanglais=TRUE, yanglais=TRUE)
	compareplt(x=lecturepltxml("/Users/user/Documents/a_System/modelisation/STICS/JavaSTICS-v131-stics-v841/plant/barley_plt.xml")
		, y=lecturepltxml("/Users/user/Documents/a_System/modelisation/STICS/JavaSTICS-v131-stics-v841/plant/winterbarley_plt.xml")
		, xanglais=TRUE, yanglais=TRUE)

	compareplt(x=lecturepltxml("/Users/user/Documents/a_System/modelisation/STICS/JavaSTICS-v131-stics-v841/plant/pea_plt.xml")
		, y=lecturepltxml("/Users/user/Documents/a_System/modelisation/STICS/JavaSTICS-v131-stics-v841/plant/peaint_plt.xml")
		, xanglais=TRUE, yanglais=TRUE)

	########################### tputes les cultures nouvelles
	nouvelle<-newpltfromxml(pltv8="/Users/user/Documents/a_System/modelisation/STICS/JavaSTICS-v131-stics-v841/plant/pea_plt.xml",
		templatev5="/Users/user/Mes documents/Copie_speciesLibraries/template_mainCrop.plt")
	connection<-file("/Users/user/Mes documents/Copie_speciesLibraries/nouvellesplantes/pea.plt", encoding="latin1")#oblige car sinon les caracteres speciaux passent pas sous mac
	write(nouvelle,file=connection)
	close(connection)

	nouvelle<-newpltfromxml(pltv8="/Users/user/Documents/a_System/modelisation/STICS/JavaSTICS-v131-stics-v841/plant/barley_plt.xml",
		templatev5="/Users/user/Mes documents/Copie_speciesLibraries/template_mainCrop.plt")
	connection<-file("/Users/user/Mes documents/Copie_speciesLibraries/nouvellesplantes/barley.plt", encoding="latin1")#oblige car sinon les caracteres speciaux passent pas sous mac
	write(nouvelle,file=connection)
	close(connection)

	nouvelle<-newpltfromxml(pltv8="/Users/user/Documents/a_System/modelisation/STICS/JavaSTICS-v131-stics-v841/plant/wheat_plt.xml",
		templatev5="/Users/user/Mes documents/Copie_speciesLibraries/template_mainCrop.plt")
	connection<-file("/Users/user/Mes documents/Copie_speciesLibraries/nouvellesplantes/wheat.plt", encoding="latin1")#oblige car sinon les caracteres speciaux passent pas sous mac
	write(nouvelle,file=connection)
	close(connection)

	nouvelle<-newpltfromxml(pltv8="/Users/user/Documents/a_System/modelisation/STICS/JavaSTICS-v131-stics-v841/plant/rapeseed_plt.xml",
		templatev5="/Users/user/Mes documents/Copie_speciesLibraries/template_mainCrop.plt")
	connection<-file("/Users/user/Mes documents/Copie_speciesLibraries/nouvellesplantes/rape.plt", encoding="latin1")#oblige car sinon les caracteres speciaux passent pas sous mac
	write(nouvelle,file=connection)
	close(connection)


	compareplt(lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/nouvellesplantes/rape.plt"), 
		lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/rape.plt"),
		 xanglais=FALSE, yanglais=FALSE)

	##### verif pour Isabelle qui avait des parametres differents sur deux ordis
	compareplt(lectureplt("/Users/user/Documents/a_System/modelisation/Hi-SAFE/verification/param.safe.par"), 
		lectureplt("/Users/user/Documents/a_System/modelisation/Hi-SAFE/verification/param.stics.par"),
		 xanglais=FALSE, yanglais=FALSE)


##### traduction pour Daniel des durum wheat

fichiersdepart<-list.files(path = "/Users/user/Mes documents/Copie_speciesLibraries/parametresSTICS_v841/", pattern = "DurumWheat_", full.names = FALSE)
for (fich in fichiersdepart) {
	nouvelle<-newpltfromxml(pltv8=paste0("/Users/user/Mes documents/Copie_speciesLibraries/parametresSTICS_v841/", fich),
		templatev5="/Users/user/Mes documents/Copie_speciesLibraries/template_mainCrop.plt")
	fich<-gsub(pattern="_plt.xml", replacement=".plt", x=fich)
	connection<-file(paste0("/Users/user/Mes documents/Copie_speciesLibraries/varietesdurumwheat_traduitesfromparametresSTICS_V841/", fich), encoding="latin1")#oblige car sinon les caracteres speciaux passent pas sous mac
	write(nouvelle,file=connection)
	close(connection)
}
#compare parameter files to know which parameters are different:
# varietes<-gsub(pattern="DurumWheat_", replacement="", x=fichiersdepart)
# varietes<-gsub(pattern="_plt.xml", replacement="", x=varietes)
# comparaisons<- data.frame()
# for (varx in varietes) for (vary in varietes) if (varx!=vary) {
# 	comparaisons<-merge(comparaisons, 
# 		compareplt(lectureplt(paste0("/Users/user/Mes documents/Copie_speciesLibraries/varietesdurumwheat_traduitesfromparametresSTICS_V841/DurumWheat_", varx, ".plt")), 
# 			lectureplt(paste0("/Users/user/Mes documents/Copie_speciesLibraries/varietesdurumwheat_traduitesfromparametresSTICS_V841/DurumWheat_", vary, ".plt")),
# 			 xanglais=FALSE, yanglais=FALSE, affiche=FALSE, nomx=varx, nomy=vary
# 		)
# 	, all=TRUE)
# }
# comparaisons<-aggregate(comparaisons[,varietes], by= comparaisons[,1:2], function(x) unique(x[!is.na(x)]))
# write.table(comparaisons, "/Users/user/Mes documents/Copie_speciesLibraries/varietesdurumwheat_traduitesfromparametresSTICS_V841/comparaisons.txt", sep="\t", quote=FALSE, row.names=FALSE, dec=",")

varietes<-gsub(pattern="DurumWheat_", replacement="", x=fichiersdepart)
varietes<-gsub(pattern="_plt.xml", replacement="", x=varietes)
parametres<- data.frame()
datforma<-data.frame()
for (i in 1:length(varietes)) {
	varx<-varietes[i]
	pltx<-lecturepltxml(paste0("/Users/user/Mes documents/Copie_speciesLibraries/parametresSTICS_v841/DurumWheat_", varx, "_plt.xml"), keepall=FALSE)
	pltxaplat<-unlist(unname(pltx))
	#on memorise les formalismes d origine
	formalismes<-character() ; for (j in 1:length(pltx)) formalismes<-c(formalismes, rep(names(pltx[j]), length(pltx[[j]])))
	toto<-data.frame(formalisme=formalismes, parametre=names(pltxaplat))
	datforma<-unique(rbind(datforma, toto))
	pltxaplatecrase<-character()
	for (k in 1: length(pltxaplat)) pltxaplatecrase[names(pltxaplat)[k]]<-pltxaplat[k] #pour ecraser les parametres en double (cas ou parametre varietal remplace un parametre specifique)
	datx<-data.frame(param=names(pltxaplatecrase), valeur=pltxaplatecrase)
	names(datx)[2]<-varx
	if (i==1) parametres<-datx else parametres<-merge(parametres, datx, all=TRUE)
}
comparaisons<-as.data.frame(t(as.matrix(parametres[,varietes])))
colnames(comparaisons)<-parametres$param
comparaisons[,]<-lapply(comparaisons,function(x) {if (! any(is.na(as.numeric(x[!is.na(x)]))) ) x<-as.numeric(x) ; return(x) } )
differents<-comparaisons[, unlist(lapply(comparaisons,function(x) length(unique(x))!=1))]
fixes<-comparaisons[, unlist(lapply(comparaisons,function(x) length(unique(x))==1))]



write.table(differents, "/Users/user/Mes documents/Copie_speciesLibraries/varietesdurumwheat_traduitesfromparametresSTICS_V841/comparaisonsXMLnokeep.txt", sep="\t", quote=FALSE, row.names=TRUE, dec=",")
rownames(datforma)<-datforma$parametre
francais <- c("nom plante" ,"développement" ,"début de végétation", "feuillage", "interception du rayonnement", 
	"croissance en biomasse", "repartition entre organes", "croissance et rendement", "racines", "gel", "eau", "azote", "variétal")
names(francais)<-c("plant name and group", "phasic development", "emergence and starting", "leaves", "radiation interception", 
	"shoot biomass growth", "partitioning of biomass in organs", "yield formation", "roots", "frost", "water", "nitrogen", "cultivar parameters")
datforma$formalismefrancais<-francais[datforma$formalisme]
write.table(datforma[colnames(differents),], "/Users/user/Mes documents/Copie_speciesLibraries/varietesdurumwheat_traduitesfromparametresSTICS_V841/formalismesorigine.txt", sep="\t", quote=FALSE, row.names=FALSE, dec=",")


######### comparaison avec bledur.plt (fichier de Marie launay), durum-wheat (code de Hi-sAFe) et durum-wheat_these (code Hi-sAFe)

toto<-lectureplt("/Users/user/Mes documents/Copie_speciesLibraries/durum-wheat_these.plt")

