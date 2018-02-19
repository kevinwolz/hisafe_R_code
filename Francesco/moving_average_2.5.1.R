
moving_average <- function(Dati, var, window_size, Treat="Treat", Year="Year", Stops="Stops"){
  #########################################################################################################
  # Given a dataframe,
  # it returns:
  # the standard deviation, mean and maximum
  # of the variable (var) 
  # considered over the data having "Year" variable within the moving window of size = window_size
  #   made by Francesco Reyes, 2017                                                                       #
  #########################################################################################################
  
  if (!(window_size %% 2)){
    stop("'window_size' must be an odd number")
  }
  
  radius <- window_size %/% 2
  Dati$mean <- NA
  Dati$sd <- NA
  
  for (s in unique(Dati[,Stops])){
    for (i in unique(Dati[,Treat][Dati[,Stops]==s])){
      for (j in unique(Dati[,Year][Dati[,Treat]==i & Dati[,Stops]==s])){
        
        Dati$sd[ Dati[,Treat]==i & Dati[,Year]==j & Dati[,Stops]==s ] <-
          sd( Dati[,var][ Dati[,Year] >= ( j - radius ) &
                            Dati[,Year] <= ( j + radius ) &
                            Dati[,Treat] == i &
                            Dati[,Stops] == s] )
        
        Dati$mean[ Dati[,Treat]==i & Dati[,Year]==j & Dati[,Stops]==s] <-
          mean( Dati[,var][ Dati[,Year] >= ( j - radius ) &
                              Dati[,Year] <= ( j + radius ) &
                              Dati[,Treat] == i &
                              Dati[,Stops] == s] )
        
        Dati$max[ Dati[,Treat]==i & Dati[,Year]==j & Dati[,Stops]==s] <-
          max( Dati[,var][ Dati[,Year] >= ( j - radius ) &
                             Dati[,Year] <= ( j + radius ) &
                             Dati[,Treat] == i &
                             Dati[,Stops] == s] )
        #print(Dati[,Year][ Dati[,Year] >= ( j - radius ) &
        #                    Dati[,Year] <= ( j + radius ) &
        #                    Dati[,Treat] == i]  )
      }    
    }
  }
  
  return(Dati)
}
