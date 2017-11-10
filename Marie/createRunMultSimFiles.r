########################################################################
#  HISAFE SCRIPT multiple execution                                    #
#  Latitude variation  execution                                      #
#                                                                      #
#  Isabelle LECOMTE 09/12/2015                                           #
########################################################################



latitude=c(25,35,45,55,65)

distance=c(17,35) 
 
orientation=c(90,180)



baseWD <- getwd()




for (l in 1:length(latitude) )
{
  
  repertoire=paste("Sim_Lat_L",latitude[l],sep="")
  fichier=paste(repertoire,".sim",sep="")
  
  
  for (d in 1:length(distance) )
  {  
    for (o in 1:length(orientation) )
    {

        
        #------------ new simulation folder creation ----------------#
        nouveau_repertoire=paste(repertoire,"_D",distance[d],"_O",orientation[o],sep="")    
        dir.create(nouveau_repertoire) 
       
        #------------ reference folder copy  -------#
        flist <- list.files(repertoire,  full.names = TRUE)
        file.copy(flist, nouveau_repertoire)
        
        #------------ Rename sim  file ----------------#
        nouveau_sim=paste(nouveau_repertoire,".sim",sep="")
        setwd(nouveau_repertoire)
        file.rename(fichier, nouveau_sim)
        
        #------------ modify plot file  ----------------#
        param <- file("Restinclieres_AF_A2.pld", open = "r")
        sortie <- file("myFile.txt", open = "w")
        
        while (length(line <- readLines(param, n = 1))) {
           res <- unlist(strsplit(line, '='))

          if (!is.na(match("latitude ", res)))
          {
            nouvelle_ligne=paste("latitude = ",latitude[l],sep="")
            writeLines(nouvelle_ligne, con = sortie)
          }
          else if (!is.na(match("treeLineOrientation ", res)))
          {
            nouvelle_ligne=paste("treeLineOrientation = ",orientation[o],sep="")
            writeLines(nouvelle_ligne, con = sortie)
          }   
          else if (!is.na(match("spacingBetweenRows ", res)))
          {
            nouvelle_ligne=paste("spacingBetweenRows = ",distance[d],sep="")
            writeLines(nouvelle_ligne, con = sortie)
          }           
          else {
            writeLines(line, con = sortie)
          }
        }
               
        close(sortie)
        close(param)
        
        #------------ rename old plot file  ------#
        file.rename("Restinclieres_AF_A2.pld", "Restinclieres_AF_A2.old")
        file.rename("myFile.txt", "Restinclieres_AF_A2.pld")
        
        #------------ going back to execution folder ------#
        setwd(baseWD)
        
        #-------------  running hisafe
        
        nom_sim= paste(baseWD,"/",nouveau_repertoire,"/",nouveau_sim,sep="")
        chaine= paste("capsis -p script safe.pgms.ScriptGen ",nom_sim,sep="")
        print (chaine) 
        
        setwd("d:/projets/capsis4")
        
        system(chaine, wait = TRUE,show.output.on.console = TRUE)
        
        setwd(baseWD)
        
 
          
    }# for orientation
  }# for distance
} #for latitude



