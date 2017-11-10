########################################################################
#  SCRIPT de GENERATION de répertoire de simulation                    #
#  On part d'un répertoire de réference qui contient tous les fichiers #
#  On copie dans autant de répertoire que de simulation générée        #
#  On modifie les paramètres de l'arbre dans chaque répertoire         #
#                                                                      #
#  Script écrit par Isabelle LECOMTE le 15/10/2015                     #
#   and adapted by Francesco Reyes during 2017                         #
########################################################################

# ORDER of SIMULATIONS
# past: AF,F,A; present: AF,F,A; future: AF,F,A
#simulationYearStart = 1995
simulationYearStart = c(1951,1991,2031)
mainCropSpecies = c("durum-wheat-allur.plt", "weed.plt","durum-wheat-allur.plt")
mainCropItk = c("durum-wheat-restinclieres.tec","weed-restinclieres.tec","durum-wheat-restinclieres.tec")

Treatment <- c("AF", "F", "A")
sysAgr <- data.frame( nbTrees = c(1,1,0),
                      spacingBetweenRows = c(13,7,1),
                      spacingWithinRows = c(9,7,1),
                      geometryOption = c(1,1,3)
                      )

# Selected Voxels for which tooutput have to be saved
SafeVoxel <- c("false\t{1;3;7;11;13;17;23;28;31;33;35;38;45;46;47;53;55;57;58;59;60;61;63;65}",
               "false\t{1;4;7;9;13;17;18;19;22;24;25;26;28}",
               "true\t{}"
               )


baseWD <- "D:/Francesco/Simulation"   #getwd()

repertoire=  "A2_nappe_V8_3mdepth" #"A2_nappe_V8" #"A2_nappe" #"A2_nappe_1995_2013" #"A2_nappe"   #"AFA2bis" #"A2" #_plus5_1962"

# Time reference to add to the name of the new folder containing the batch of results
temps_sim <- format(Sys.time(), format='%Y-%m-%d_%H-%M')
lot <- paste(repertoire, temps_sim, sep="_")

#---Positionnement dans repertoire de base de tout les simulations
setwd(baseWD)

dir.create(lot)
#lotWD <- paste(baseWD,lot,sep="/")

fichier=paste(repertoire,".sim",sep="")


for (l in 1:length(simulationYearStart) )
{
  for (s in 1:dim(sysAgr)[1] )
  {
        #------------ Creation nouveau répertoire ----------------#
#        nouveau_repertoire=paste(repertoire,'_YR',simulationYearStart[l],"_T",sysAgr$nbTrees[s],"_BR",sysAgr$spacingBetweenRows[s],"_WR",sysAgr$spacingWithinRows[s],sep="")            
        nouveau_repertoire=paste(repertoire,"_",Treatment[s],'_YR',simulationYearStart[l],"_T",sysAgr$nbTrees[s],"_BR",sysAgr$spacingBetweenRows[s],"_WR",sysAgr$spacingWithinRows[s],sep="")                    
        path_nouveau_repertoire=paste(lot,nouveau_repertoire,sep="/")
        dir.create(path_nouveau_repertoire)
                
        #------------ Recopie contenu du répertoire de réference dans le nouveau  -------#
        system(
          paste( "robocopy", repertoire, path_nouveau_repertoire, "/e", sep=" ")
        )
                
        #------------ Positionnement dans repertoire du fichier .sim et Renommage fichier .sim ----------------#
        nouveau_sim=paste(nouveau_repertoire,".sim",sep="")
        setwd(path_nouveau_repertoire)
        file.rename(fichier, nouveau_sim)


        #------------ Modification paramètres dans le fichier ".sim" ----------------#
        param <- file(nouveau_sim, open = "r")
        sortie <- file("myFile.txt", open = "w")
        
        while (length(line <- readLines(param, n = 1))) {
          res <- unlist(strsplit(line, '='))

          if (!is.na(match("simulationYearStart ", res)))
          {
            nouvelle_ligne=paste("simulationYearStart = ",simulationYearStart[l],sep="")
            writeLines(nouvelle_ligne, con = sortie)
          }
          else if (!is.na(match("mainCropSpecies ", res)))
          {
            nouvelle_ligne=paste("mainCropSpecies = ",mainCropSpecies[s],sep="")
            writeLines(nouvelle_ligne, con = sortie)
          }
          else if (!is.na(match("mainCropItk ", res)))
          {
            nouvelle_ligne=paste("mainCropItk = ",mainCropItk[s],sep="")
            writeLines(nouvelle_ligne, con = sortie)
          }          
          else {
            writeLines(line, con = sortie)
          }
        }
        
        close(sortie)
        close(param)
        
        #------------ on remplace par le nouveau fichier ------#
        file.rename(nouveau_sim, paste( substr(fichier,1,nchar(fichier)-4), ".old", sep="") )
        file.rename("myFile.txt", nouveau_sim)

        
        
        #------------ Positionnement dans le su-repertoire du fichier plot et Modification de ses paramètres  ----------------#
        setwd("plotDescription")
        param <- file("Restinclieres_AF_A2.pld", open = "r")
        sortie <- file("myFile.txt", open = "w")
        
        while (length(line <- readLines(param, n = 1))) {
          res <- unlist(strsplit(line, '='))
          
          if (!is.na(match("nbTrees ", res)))
          {
            nouvelle_ligne=paste("nbTrees = ",sysAgr$nbTrees[s],sep="")
            writeLines(nouvelle_ligne, con = sortie)
          }   
          else if (!is.na(match("spacingBetweenRows ", res)))
          {
            nouvelle_ligne=paste("spacingBetweenRows = ",sysAgr$spacingBetweenRows[s],sep="")
            writeLines(nouvelle_ligne, con = sortie)
          }           
          else if (!is.na(match("spacingWithinRows ", res)))
          {
            nouvelle_ligne=paste("spacingWithinRows = ",sysAgr$spacingWithinRows[s],sep="")
            writeLines(nouvelle_ligne, con = sortie)
          }
          else if (!is.na(match("geometryOption ", res)))
          {
            nouvelle_ligne=paste("geometryOption = ",sysAgr$geometryOption[s],sep="")
            writeLines(nouvelle_ligne, con = sortie)
          }           
          else {
            writeLines(line, con = sortie)
          }
        }
              
        close(sortie)
        close(param)
        
        #------------ on remplace par le nouveau fichier ------#
        file.rename("Restinclieres_AF_A2.pld", "Restinclieres_AF_A2.old")
        file.rename("myFile.txt", "Restinclieres_AF_A2.pld")
        

        
        #------------ Positionnement dans le repertoire "/exportParameters" et Modification des paramètres du fichier root ----------------#
        setwd(paste(baseWD,"/",path_nouveau_repertoire, "/exportParameters", sep=""))
        param <- file("roots.pro", open = "r")
        sortie <- file("myFile.txt", open = "w")
        
        #sx_ligne <- "SafeVoxel\t{SafeVoxel-treeCoarseRootBiomass;SafeVoxel-treeRootDensity;SafeVoxel-volume}\t"
        #dx_ligne <- "\t{}"
        
        while (length(line <- readLines(param, n = 1))) {
          res <- unlist(strsplit(line, '\t'))
          print (res)
          
          if (!is.na(match("SafeVoxel", res)))
          {
            # nouvelle_ligne=paste(sx_ligne,SafeVoxel[s],dx_ligne,sep="")
            nouvelle_ligne=paste(res[1],res[2],SafeVoxel[s],res[5],sep="\t")
            writeLines(nouvelle_ligne, con = sortie)
            print (paste("ligne changé avec",nouvelle_ligne))
          }   
          else {
            writeLines(line, con = sortie)
          }
        }
        
        close(sortie)
        close(param)
        
        #------------ on remplace par le nouveau fichier ------#
        file.rename("roots.pro", "roots.old")
        file.rename("myFile.txt", "roots.pro")        
        
        #------------ Positionnement dans le repertoire "/exportParameters" et Modification des paramètres du fichier voxels ----------------#
        setwd(paste(baseWD,"/",path_nouveau_repertoire, "/exportParameters", sep=""))
        param <- file("voxels.pro", open = "r")
        sortie <- file("myFile.txt", open = "w")

        while (length(line <- readLines(param, n = 1))) {
          res <- unlist(strsplit(line, '\t'))
          print (res)
          
          if (!is.na(match("SafeVoxel", res)))
          {
            nouvelle_ligne=paste(res[1],res[2],SafeVoxel[s],res[5],sep="\t")
            writeLines(nouvelle_ligne, con = sortie)
            print (paste("ligne changé avec",nouvelle_ligne))
          }   
          else {
            writeLines(line, con = sortie)
          }
        }
        
        close(sortie)
        close(param)
        
        #------------ on remplace par le nouveau fichier ------#
        file.rename("voxels.pro", "voxels.old")
        file.rename("myFile.txt", "voxels.pro")                
        

        
        #------------ on remonte au répertoire de base ------#
#         setwd(baseWD)
        
        #-------------  LANCEMENT DE Hi-Safe en BATCH
        
        nom_sim= paste(baseWD,"/",path_nouveau_repertoire,"/",nouveau_sim,sep="")
        chaine= paste("capsis -p script safe.pgms.ScriptGen ",nom_sim,sep="")
        print (chaine) 
        
        setwd("C:/projets/capsis4")
        
        system(chaine, wait = TRUE,show.output.on.console = TRUE)

        setwd(baseWD)

  }
}


