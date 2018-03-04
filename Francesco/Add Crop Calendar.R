#########################################################
#        Functions to add Crop calendar
# by Francesco Reyes
# surely the same things in other languages 
# you can find all around online
#########################################################


# add DOY_crop
{
  seeding_day <- 300
  Dati$JJ_crop <- NA
  for (s in unique(Dati$SimulationName)){
    for (y in c( min(unique(Dati$Year[Dati$SimulationName==s]))-1, unique(Dati$Year[Dati$SimulationName==s]) )){
      print (y)
      if (y %% 400 == 0 | (y %% 4 == 0 & y %% 100 != 0)){
        BIS <- TRUE
      } else {
        BIS <- FALSE }
      
      if (BIS){ 
        nb_days_year <- 366
      } else { 
        nb_days_year <- 365 }
      
      # Fill calendar crop:
      # from date of seeding to end of solar year
      Dati$JJ_crop[Dati$JulianDay >= seeding_day & Dati$JulianDay <= nb_days_year & Dati$Year == y & Dati$SimulationName == s] <-
        c(1:(nb_days_year-seeding_day+1))
      
      # from beginning of solar year to next seeding date excluded
      nb_cells_to_fill <- length(Dati$JJ_crop[Dati$JulianDay >= 1 & Dati$JulianDay < seeding_day & Dati$Year == y+1 & Dati$SimulationName == s])
      nb_filling_cells <- length(c(1:(seeding_day-1))+(nb_days_year-seeding_day+1))
      
      Dati$JJ_crop[Dati$JulianDay >= 1 & Dati$JulianDay < seeding_day & Dati$Year == y+1 & Dati$SimulationName == s] <-
        Dati$JulianDay[Dati$JulianDay >= 1 & Dati$JulianDay < seeding_day & Dati$Year == y+1 & Dati$SimulationName == s] +
        (nb_days_year-seeding_day+1)
      #( c(1:(seeding_day-1))+ (nb_days_year-seeding_day+1) )[ (nb_filling_cells-nb_cells_to_fill+1) : nb_filling_cells ]
    }
  }
}

# add Year_crop
{
  seeding_day <- 300
  Dati$Year_crop <- NA
  
  for (s in unique(Dati$SimulationName)){
    for (y in min(unique(Dati$Year[Dati$SimulationName==s])):(max(unique(Dati$Year[Dati$SimulationName==s]))-0) ){
      print (y)
      
      if (y %% 400 == 0 | (y %% 4 == 0 & y %% 100 != 0)){
        nb_days_year <- 366
      } else {
        nb_days_year <- 365 }
      
      # Fill calendar crop:
      # from date of seeding to end of solar year
      Dati$Year_crop[Dati$Year == y & Dati$SimulationName == s & 
                       Dati$JulianDay >= 1 & Dati$JulianDay < seeding_day] <- y-1
      
      Dati$Year_crop[Dati$Year == y & Dati$SimulationName == s &
                       Dati$JulianDay >= seeding_day & Dati$JulianDay <= nb_days_year] <- y
    }
  }
}
