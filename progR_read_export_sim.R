

readsim<-function(filepath) {
	lignes<-scan(file=filepath, what="character", encoding="latin1", sep="\n")
	lignes<-lignes[! lignes %in% c("#", "# ")]
	withdata<-grepl(pattern=" = ", x=lignes)
	comment<-substr(lignes, start=1, stop=1)=="#"
	titles<- !withdata & comment
	resultat<-list()
	for (i in 1:length(lignes)) {
		if (titles[i]){
			nomliste<-gsub(pattern=" ", replacement="_", gsub("^\\s+|\\s+$", "", gsub(pattern="#", replacement="", lignes[i]))) #on enleve les tabs, leading et trailing blanks
			toto<-list(c())
			names(toto)<-nomliste
			resultat<-c(resultat,toto)
		} else {
			notcomented<-lignes[i]
			if (comment[i]) notcomented<-substr(notcomented, start=2, stop=10000) #remove first comment sign only (in case there is a definition after another comment)
			nomelement<-unlist(lapply(strsplit(notcomented, split="=", fixed=TRUE), "[[", 1))
			nomelement<-gsub("^\\s+|\\s+$", "",nomelement) #on enleve les tabs, leading et trailing blanks
			valeurelement<-unlist(lapply(strsplit(notcomented, split="=", fixed=TRUE), "[[", 2))
			valeurelement<-gsub("^\\s+|\\s+$", "",valeurelement) #on enleve les tabs, leading et trailing blanks
			vraievaleur<-unlist(lapply(strsplit(valeurelement, split="#", fixed=TRUE), "[[", 1))
			#for now, we don t read what is commented, but could be range or allowed values
			toto<-list(list(value=vraievaleur, wascommented=comment[i], range=NA, accepted=NA))
			names(toto)<-nomelement
			resultat[[nomliste]]<-c(resultat[[nomliste]], toto)
		}
	}
	return(resultat)
}
toto<-readsim("/Users/user/Documents/b_sortiesHisAFe/CelineBlitz_Latitude6/Sim_Lat_L25_D17_O90/Sim_Lat_L25_D17_O90.sim") 
str(toto)
exportsim<-function(simlist, filepath) {
	lignes<-character(0)
	for (i in 1:length(simlist)){ #chapters
		lignes<-paste(lignes, paste("\n#", names(simlist)[i]), sep="\n")
		if (length(simlist[[i]])>0) {
			for (j in 1:length(simlist[[i]])) { #elements
				commentsign<-ifelse(simlist[[i]][[j]]$wascommented, "#", "")
				additionnalcomments<-paste(ifelse(is.na(simlist[[i]][[j]]$range), "", simlist[[i]][[j]]$range), ifelse(is.na(simlist[[i]][[j]]$accepted), "", simlist[[i]][[j]]$accepted))
				if (additionnalcomments!=" ") additionnalcomments<-paste("#", additionnalcomments)
				lignes<-paste(lignes, paste(commentsign, names(simlist[[i]])[j], "=", simlist[[i]][[j]]$value, additionnalcomments), sep="\n")
			}
		}
	}
	write(lignes, file=filepath)
}
exportsim(toto, "/Users/user/Documents/b_sortiesHisAFe/CelineBlitz_Latitude6/Sim_Lat_L25_D17_O90/Sim_Lat_L25_D17_O90_reexporte.sim") 

