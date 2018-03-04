### HI-SAFE Design Optimization - RUN
### Programmer: Kevin Wolz
### Originally Created: 19 May 2016
### Last Updated: 21 May 2016

##### RUN SIMULATIONS IN SERIAL #####
if(COMPUTING == "SERIAL"){
  out <- file(paste0(projectPath, "/", PROJECT.NAME, "_simulation_log.txt"), open="w")
  cat("Beginning simulation: ", PROJECT.NAME, "\n\n", sep="", file=out)
  setwd("/Applications/Capsis/")
  
  for(simulation in list.files(simulationDirPath)){
    #[29:length(list.files(simulationDirPath))]){
    #simulation <- list.files(simulationDirPath)[1]
    simulationStartTime <- proc.time()[3]
    sim.path <- list.files(paste0(simulationDirPath, "/", simulation), pattern=".sim", full.names = TRUE)
    call <- paste0("sh capsis.sh -p script safe.pgms.ScriptGen ", sim.path)
    log <- system(call, wait = TRUE, intern = TRUE)
    simulationElapsedTime <- signif((proc.time()[3] - simulationStartTime)/60,3)
    #print(paste0("Done with simulation: ", PROJECT.NAME, " - ", simulation, ". This took ", simulationElapsedTime, " minutes."))
    #print(paste0("The efficincy of this run was: ", signif(simulationElapsedTime/N.YEARS,3), " minutes/year."))
    cat("Done with simulation: ", PROJECT.NAME, " - ", simulation, ". This took ", simulationElapsedTime, " minutes.\n", sep="", file=out)
    cat("The efficincy of this run was: ", signif(simulationElapsedTime/N.YEARS,3), " minutes/year.\n\n", sep="", file=out)
    beep(1)
  }
}
close(out)

##### RUN SIMULATIONS IN PARALLEL #####
if(COMPUTING == "PARALLEL"){
  RUNS <- list.files(simulationDirPath)
  
  myCluster <- makeCluster(spec=4, type="SOCK")
  clusterExport(myCluster, c("WD", "PROJECT.NAME"))
  registerDoSNOW(myCluster)
  
  log <- foreach(n = RUNS, .inorder = FALSE) %dopar% { runSimulations(n) }
  
  stopCluster(myCluster)
  registerDoSEQ()
}

##### CHANGE WD BACK TO CORRECT FOLDER #####
setwd(WD)