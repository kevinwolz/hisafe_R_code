
multi_plots <- function(Dati, X, Ys, axis_vars, cum_vars="", Treat="Treat", funct = "mean", stops = c(1950,1990,2030), leg_option = TRUE, Plot= TRUE, invert_graphs=FALSE){
  #########################################################################################################
  # multi_plots:                                                                                          #
  # Aggregates and plot Ys variables according to the X and Treat variables.                              #
  # Especially useful to aggregate and plot multiple dataseries across treatments and time periods.       #
  # It is an extended, generalized version of "nine_plots".                                               #
  #                                                                                                       #
  # - "Dati", data frame                                                                                  #
  # - "X" variable                                                                                        #
  # - "Ys", vector of Y variables                                                                         #
  # - "axis_vars", Y axis where to plot different variables                                               #
  # - "cum_vars", variables for which computing cumulative within the X limits defined by stops;          #
  # - "funct", function used to aggregate data (see aggregate(R base package))                            #
  # - "stops", X limits used to split the dataframe. It associates 'Past','Present','Future' categories   #
  #            to parts of data delimited by "stops" values.                                              #
  #   made by Francesco Reyes, 2017                                                                       #
  #########################################################################################################
  
  library(plotrix)
  Dati$Treat <- as.factor(Dati[,Treat])
  
  # Create df of aggregated variables
  {
    list_df <- list()
    for (i in Ys){
      bool_1 <- TRUE
      for (sim in unique(Dati$SimulationName)){
        
        print(paste(sim,i))
        # define index to retrieve the function used to aggregate the current Ys variable
        n_funct <- which(Ys==i)
        # print (paste("function index is ",funct[n_funct]))
        functio <- getFunction(funct[n_funct])
        #print (functio)
        
        ACY <- aggregate(Dati[,i][Dati$SimulationName==sim], 
                         list(Treat = Dati$Treat[Dati$SimulationName==sim], 
                              varX = Dati[,X][Dati$SimulationName==sim]), 
                         FUN=functio)
        names(ACY)[2] <- X
        names(ACY)[3] <- i
        
        # assign NAs to variables that have only zero values during the simulated period (because their species are actually not there)
        if ( length (unique(ACY[,3]))==1 & unique(ACY[,3])==0 ){
          ACY[,3]<-NA
        }
        # # altenratively: fill with NAs the variables treeYield variables for _A_ simulations, and cropYield variables for _F_ simulations
        # if ( ACY$Treat == "_A_" & i == "treeYield"){
        #   ACY$treeYield <- NA
        # }else if(ACY$Treat == "_F_" & i == "cropYield"){
        #   ACY$cropYield <- NA
        # }
        
        # only for the first variable of Ys create 'ALL'(with name for its second variable=X)
        if (bool_1){
          ALL_1 <- ACY
          print ("first simulation and variable")
          bool_1 <- FALSE
          
          
          # then bind the each other aggregated part of the dataset to ALL
        } else {
          # print (paste("dimensions of ALL", dim(ALL)))
          # print (ACY)
          ALL_1 <- rbind (ALL_1, ACY)
          # print (paste("new dimensions of ALL", dim(ALL)))
          
        }
        # and give name to the newly aggregated variable
        # names(ALL)[ dim(ALL)[2] ] <- i
        
      }
      # create df (only after first variable have been fully aggregated)
      if (i==Ys[1]){
        ALL <- ALL_1
        print("ALL created")
      }else{
        ALL <- cbind (ALL, ALL_1[3])
      }
      
    } 
  }
  
  
  # Split df according to "stops" values of X
  {
    label_stops <- c("Past", "Present", "Future")
    ALL$Stops[ ALL[,X] <= stops[2]] <- 
      label_stops[1]
    
    ALL$Stops[ ALL[,X] > stops[2] &  ALL[,X]<= stops[3] ] <- 
      label_stops[2]
    
    ALL$Stops[ ALL[,X] > stops[3]] <- 
      label_stops[3]
    
    # stops <- c(stops, max(Dati[,X]) )
    # label_stops <- c("Past", "Present", "Future")
    # ALL$Stops <- NA
    # j <- 0
    # for ( i in 1:( length(stops)-1 ) ){
    #   j <- j + 1
    #   ALL$Stops[ ALL[,X]> stops[i] &  ALL[,X]<= stops[i+1] ] <- label_stops[j]
    # }
    
  }
  
  # Compute cumulative of selected variables
  if ( cum_vars != "" ){
    
    for (i in cum_vars){
      print (paste("cumulate ",i,sep=""))
      
      cum_var <- paste("cum_",i, sep="")
      ALL[,cum_var] <- NA
      
      for ( s in unique(ALL$Stops) ){
        
        for ( t in unique(ALL$Treat[ ALL$Stops==s ]) ){
          
          for (ics in unique(ALL[,X][ ALL$Stops==s & ALL$Treat==t ])){
            
            ALL[ ,cum_var ] [ ALL$Stops==s & ALL$Treat==t & ALL[,X]==ics] <-
              sum(ALL[,i][ ALL$Treat==t & ALL[,X]<=ics & ALL$Stops==s ])
            
          }
          
        }
      }
    }
    
  }
  
  # PLOT
  if (Plot){
    # Define vector of variables to plot
    vars_plot <- Ys
    for ( i in vars_plot){
      if (i %in% cum_vars){
        vars_plot[which(vars_plot == i)] <- paste("cum_",i, sep="")
      }
    }
    print (cat("Plot ",vars_plot, fill=TRUE))
    
    if (invert_graphs){
      var_x <- "Stops"
      var_y <- "Treat"
    }else{
      var_y <- "Stops"
      var_x <- "Treat"    
    }
    
    # Set number of plots for vertical and horizontal axes
    N_Treat <- length(unique(ALL[,var_x]))       # number of plots in horizontal direction
    N_period <- length(unique(ALL[,var_y]))          # number of plots in vertical direction
    par( mfrow = c( N_period,  N_Treat) )
    
    Leg <- FALSE                                    # boolean: until FALSE triggers legend and becomes TRUE
    print (paste("Leg is",Leg))
    
    plus_low_limit <- 0
    nb_years <- plus_low_limit + 40
    
    par(mar=c(2.1,4.1,4.1,2.1))
    
    # Y1 and Y2 limits, common to all plots
    
    # maxs maximum values of ...
    maxs <- rep( NA,length(vars_plot) )
    # pchs: list of pch values to be used for each variable in the plots
    pchs <- c(1,16,7,8,12,13,4)
    
    for ( i in vars_plot ){
      maxs[ which(vars_plot %in% i) ] <- max( ALL[,i], na.rm=TRUE )       
    }
    print (maxs)
    # extract the max-value of the variables to be plotted on each axis(axis = 2 is left, = 4 is right)
    if ( any(axis_vars %in% 4 ) ){
      
      maxs[ which( axis_vars %in% 4 )  ] <- 
        max( maxs[ which( axis_vars %in% 4 )  ] )
      
    }
    
    if ( any(axis_vars %in% 2 ) ){
      
      maxs[ which( axis_vars %in% 2 )  ] <- 
        max( maxs[ which( axis_vars %in% 2 )  ] )
      
    }
    
    for (s in unique(ALL[,var_y])){
      
      for (t in unique(ALL[,var_x])){
        
        par(new=FALSE) # not to super-pose different Treatments or Stops
        
        for (v in vars_plot) {
          print (paste ("variable", v, t, s))
          
          # define x limits of single graph
          print(ALL[,X][ALL[,var_x]==t & ALL[,var_y]==s], na.rm=TRUE)
          min_xlim <- min(ALL[,X][ALL[,var_x]==t & ALL[,var_y]==s], na.rm=TRUE)
          max_xlim <- max(ALL[,X][ALL[,var_x]==t & ALL[,var_y]==s], na.rm=TRUE)
          
          
          # define sequence of years ending in 0, later used to make vertical grid
          bool_years <- seq(min_xlim,max_xlim)/10==trunc(seq(min_xlim,max_xlim)/10)
          vertical_grid <- seq(min_xlim,max_xlim)[bool_years]
          
          max_Y <- maxs[ which( vars_plot %in% v ) ]
          pch_Y <- pchs[ which( vars_plot %in% v ) ]
          axis_var <- axis_vars[ which( vars_plot %in% v ) ]
          
          plot( ALL[ ,X ] [ ALL[,var_x]==t & ALL[,var_y]==s ], 
                ALL[ ,v ] [ ALL[,var_x]==t & ALL[,var_y]==s ],
                xlim = c( min_xlim, max_xlim),
                axes = F,
                ylim = c( 0, max_Y ),
                main = t,
                ylab = "", xlab = "", pch = pch_Y)
          axis(
            axis_var, 
            ylim = c( 0, max_Y ),lwd=1)
          
          axis(1, xlim = c( min_xlim, max_xlim), lwd=1)
          
          abline(h=(seq(0,max_Y,max_Y/5)), col="black", lty="dotted")
          
          ## Create Automatic Vertical Grid in positions so to divide space in 5 sectors
          #abline(v=(seq(min_xlim,max_xlim,(max_xlim-min_xlim)/5)), col="black", lty="dotted")
          
          ## Create Automatic Vertical Grid in positions each "10" years
          abline(v=vertical_grid, col="black", lty="dotted")
          
          ## Add legend on first plot
          #         while (Leg == FALSE){
          #           # find empty spot for the legend
          #           rectangle <- maxEmptyRect(c(min_xlim, max_xlim), 
          #                                     c(0, max_Y),
          #                                     ALL[ ,X ] [ ALL$Treat==t & ALL$Stops==s ], 
          #                                     ALL[ ,v ] [ ALL$Treat==t & ALL$Stops==s ]
          #           )
          #                                 
          #         
          #           legend(x=c(rectangle$rect[1]+0.1*(rectangle$rect[3]-rectangle$rect[1])), #x="topleft",
          #                  y=c(rectangle$rect[4]-0.1*(rectangle$rect[4]-rectangle$rect[2])),
          #                  vars_plot,
          #                  pchs[1:length(vars_plot)],
          #                  cex=1.2)
          #           Leg <- TRUE
          #         }
          
          # super-pose plots with same Treat and Stops
          par(new=TRUE)
          
        }
      }
      
    }
    # Produce a legend, the position changes with "inset"
    if (leg_option == TRUE){
      legend(x="topleft", #inset=c(-0,-3*max_Y), #3.7),
             vars_plot,
             pch=pchs[1:length(vars_plot)],
             cex=1.2)
    }
    par(new=FALSE)    
  }
  
  return(ALL)
}
