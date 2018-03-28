## FUNCTIONS

 # Functions for scaling/unscaling a scalar or vector to/from the interval [0,1]
   sc   <- function( x  , xmin, xmax ) {(x-xmin)/(xmax-xmin)}
   unsc <- function( scx, xmin, xmax ) {xmin+scx*(xmax-xmin)}

## SITE CONDITIONS
   list_params        <- sitelist ; list_matrix_weather <- sitelist
   list_calendar_fert <- sitelist ; list_calendar_Ndep  <- sitelist
   list_days_harvest  <- sitelist ; list_NDAYS          <- sitelist
   for (s in 1:nSites) {
     source( sitesettings_filenames[s] )
     list_params       [[s]] <- params        ; list_matrix_weather[[s]] <- matrix_weather
     list_calendar_fert[[s]] <- calendar_fert ; list_calendar_Ndep [[s]] <- calendar_Ndep
     list_days_harvest [[s]] <- days_harvest  ; list_NDAYS         [[s]] <- NDAYS   
   } 
   
## MEASUREMENTS ##
   database <- sitelist      ; database_mm <- sitelist
   ndata    <- rep(0,nSites) ; ndata_mm    <- rep(0,nSites)
   for (s in 1:nSites) {
     dataset_all <- read.table( sitedata_filenames[s],
                                header=F, sep="", stringsAsFactors=FALSE )
       varNames  <- dataset_all[,1]
       varNames[varNames=="ASH"    ] <- "F_ASH"
       varNames[varNames=="CP"     ] <- "F_PROTEIN"
       varNames[varNames=="CPLEAF" ] <- "F_PROTEIN"
       varNames[varNames=="CPSTEM" ] <- "F_PROTEIN"
       varNames[varNames=="DIG"    ] <- "F_DIGEST_DM"
       varNames[varNames=="DIGLEAF"] <- "F_DIGEST_LV"
       varNames[varNames=="DIGSTEM"] <- "F_DIGEST_ST"
       varNames[varNames=="DMLEAF" ] <- "DMLV"
       varNames[varNames=="DMSTEM" ] <- "DMST"
       varNames[varNames=="dNDF"   ] <- "F_DIGEST_WALL"
       varNames[varNames=="IVTD"   ] <- "F_DIGEST_DM"
       varNames[varNames=="NDF"    ] <- "F_WALL_DM"
       varNames[varNames=="NDFLEAF"] <- "F_WALL_LV"
       varNames[varNames=="NDFSTEM"] <- "F_WALL_ST"
       varNames[varNames=="NC"     ] <- "NSH_DMSH"
     dataset_all[,1]  <- varNames
     database   [[s]] <- dataset_all[which(dataset_all$V1!='FRTILG'),]
     database_mm[[s]] <- dataset_all[which(dataset_all$V1=='FRTILG'),]
     ndata[s]         <- dim(database   [[s]])[[1]]
     ndata_mm[s]      <- dim(database_mm[[s]])[[1]]
   }

   data_name  <- sitelist ; data_mm_name  <- sitelist
   data_value <- sitelist ; data_mm_value <- sitelist
   data_sd    <- sitelist ; data_mm_min   <- sitelist ; data_mm_max <- sitelist ; data_mm_cv <- sitelist
   data_year  <- sitelist ; data_mm_year  <- sitelist 
   data_doy   <- sitelist ; data_mm_doy   <- sitelist
   for (s in 1:nSites) {
     data_name [[s]] <- database[[s]][,1]
     data_year [[s]] <- database[[s]][,2]
     data_doy  [[s]] <- database[[s]][,3]
     data_value[[s]] <- database[[s]][,4]
   }
   
   # Now find the maximum value for each variable across the whole database
   BCvarname <- NULL
   for (s in 1:nSites) BCvarname <- c( BCvarname, data_name[[s]] )
   BCvarname <- unique( BCvarname ) ; nBCvar <- length( BCvarname )
   BCvarmax  <- rep( 0, nBCvar )
   for (s in 1:nSites) {
     for (v in 1:nBCvar) {
	     iv <- which( data_name[[s]]==BCvarname[v] )
	     if( length(iv)>0 ) BCvarmax[v] <- max( BCvarmax[v], data_value[[s]][iv] )
  	 }
   }
   
   for (s in 1:nSites) {
     ndata_s       <- length(data_name[[s]])
	   data_sd [[s]] <- rep( NA, ndata_s )
     for (i in 1:ndata_s) {
	     iv              <- match( data_name[[s]][i], BCvarname )
	     data_max_s_i    <- BCvarmax[iv]
	     data_sdmod_s_i  <- sqrt( data_max_s_i * data_value[[s]][i] )
	     data_sd[[s]][i]                                            <- data_sdmod_s_i * cv_default[s]
	     if( data_name[[s]][i] == "DM"     )        data_sd[[s]][i] <- data_sdmod_s_i * cv_DM[s]
	     if( data_name[[s]][i] == "LAI"    )        data_sd[[s]][i] <- data_sdmod_s_i * cv_LAI[s]
	     if( data_name[[s]][i] == "TILTOT" )        data_sd[[s]][i] <- data_sdmod_s_i * cv_TILTOT[s]
	     if( data_name[[s]][i] == "YIELD"  )        data_sd[[s]][i] <- data_sdmod_s_i * cv_YIELD[s]
	     if( data_name[[s]][i] == "F_PROTEIN"     ) data_sd[[s]][i] <- data_sdmod_s_i * cv_F_PROTEIN[s]
	     if( data_name[[s]][i] == "F_DIGEST_DM"   ) data_sd[[s]][i] <- data_sdmod_s_i * cv_F_DIGEST[s]
	     if( data_name[[s]][i] == "F_DIGEST_LV"   ) data_sd[[s]][i] <- data_sdmod_s_i * cv_F_DIGEST[s]
	     if( data_name[[s]][i] == "F_DIGEST_ST"   ) data_sd[[s]][i] <- data_sdmod_s_i * cv_F_DIGEST[s]
	     if( data_name[[s]][i] == "F_DIGEST_WALL" ) data_sd[[s]][i] <- data_sdmod_s_i * cv_F_DIGEST[s]
	     if( data_name[[s]][i] == "F_WALL_DM" )     data_sd[[s]][i] <- data_sdmod_s_i * cv_F_WALL[s]
	     if( data_name[[s]][i] == "F_WALL_LV" )     data_sd[[s]][i] <- data_sdmod_s_i * cv_F_WALL[s]
	     if( data_name[[s]][i] == "F_WALL_ST" )     data_sd[[s]][i] <- data_sdmod_s_i * cv_F_WALL[s]
	     if( data_name[[s]][i] == "LT50" )          data_sd[[s]][i] <- sd_LT50[s]
	   }
   }
   
   for (s in 1:nSites) {
     data_mm_name [[s]] <- database_mm[[s]][,1]
     data_mm_year [[s]] <- database_mm[[s]][,2]
     data_mm_doy  [[s]] <- database_mm[[s]][,3]
     data_mm_value[[s]] <- database_mm[[s]][,4]
     data_mm_min  [[s]] <- rep( 0, ndata_mm[s] )
     data_mm_max  [[s]] <- rep( 1, ndata_mm[s] )
     data_mm_cv   [[s]] <- cv_mm_default[s]
  	 i_FRTILG           <- which(data_mm_name[[s]]=='FRTILG')
     data_mm_cv   [[s]][i_FRTILG] <- cv_mm_FRTILG[s]
   }

## LINKING DATA TO MODEL OUTPUTS
 # The list of outputNames, which is defined in the generic initialisation file
 # "initialise_BASGRA_general.R", lists the names of all model outputs.
 # Note that the names used in the files with calibration data must be the
 # same as the names in the list of outputNames.
 # The data_index gives the model output number for each data point:
   data_index <- sitelist ; data_mm_index <- sitelist
   for (s in 1:nSites) {
     nextdata_index     <- sapply( 1:ndata   [s], function(i)
                                   which(as.character(outputNames)==data_name[[s]][i]) )
     nextdata_mm_index  <- sapply( 1:ndata_mm[s], function(i)
                                   which(as.character(outputNames)==data_mm_name[[s]][i]) )
     data_index   [[s]] <- nextdata_index
     data_mm_index[[s]] <- nextdata_mm_index
   }

## PRIOR DISTRIBUTION FOR THE PARAMETERS ##
   df_params_BC <- read.table( file_prior, header=F, sep="" )
   parname_BC   <-               df_params_BC[,1]
   parmin_BC    <-               df_params_BC[,2]
   parmod_BC    <-               df_params_BC[,3]
   parmax_BC    <-               df_params_BC[,4]
   parsites_BC  <- as.character( df_params_BC[,5] )
   ip_BC        <- match( parname_BC, row.names(df_params) )
   np_BC        <- length(ip_BC)
   
   ip_BC_site   <- sitelist ; icol_pChain_site <- sitelist
   for (p in 1:np_BC) {
     for ( s in 1:nSites ) {
       if( s %in% eval( parse( text = parsites_BC[p] ) ) ) {
         ip_BC_site[[s]]       <- cbind( ip_BC_site[[s]]      , ip_BC[p] )
         icol_pChain_site[[s]] <- cbind( icol_pChain_site[[s]], p        )
       }
     }
   }
################################################################################
   
 # The MCMC shall walk through scaled parameter space
   scparmod_BC  <- sc( parmod_BC, xmin=parmin_BC, xmax=parmax_BC )
 # Setting up MCMC-chains for the LINE-parameters
   pChain       <- matrix( 0, nrow=nChain, ncol=np_BC )
 # We start the chain at the mode of the prior parameter distribution
   scpValues_BC <- scparmod_BC ; pChain[1,] <- scpValues_BC
   
 # CALCULATING THE VALUE OF THE PRIOR AT THE START OF THE CHAIN
 # Setting a and b of the beta-distribution conform {min,mode,max} with a+b=6 
   a_prior      <- 1. + 4 * (parmod_BC-parmin_BC) / (parmax_BC-parmin_BC)
   b_prior      <- 6. - a_prior
 # Calculate the np_BC beta-probabilities, log-transform and sum
   logPrior0    <- sum( dbeta(scpValues_BC, a_prior, b_prior, log=T) )
   
# DEFINING PROPOSAL DISTRIBUTIONS
   
# Library MASS has a routine for multivariate normal random number generation
   library(MASS)
   fPropGelman <- 2.38^2 / np_BC # Proposal scaling factor (Gelman et al. 1996)

## PROPOSAL DISTRIBUTION - parameters ##
   var_prior   <- a_prior*b_prior / ((1+a_prior+b_prior)*(a_prior+b_prior)^2)
   vcovProp    <- diag(var_prior) * fPropGelman * fPropTuning
   
################################################################################
      
## FIRST RUN OF THE MODEL FOR EACH SITE, WITH CALCULATION OF LIKELIHOOD ##
   list_output             <- sitelist
   list_output_calibr_rows <- sitelist ; list_output_mm_calibr_rows <- sitelist   
   pValues_BC              <- unsc( scpValues_BC, xmin=parmin_BC, xmax=parmax_BC )
   
   for (s in 1:nSites) {
   # Site-specific model initialisation
     params        <- list_params       [[s]] ; matrix_weather <- list_matrix_weather[[s]]
     calendar_fert <- list_calendar_fert[[s]] ; calendar_Ndep  <- list_calendar_Ndep [[s]] 
     days_harvest  <- list_days_harvest [[s]] ; NDAYS          <- list_NDAYS         [[s]]
   # Values of calibration parameters at the start of the chain
     params[ ip_BC_site[[s]] ]    <- pValues_BC[ icol_pChain_site[[s]] ]
     output                       <- run_model( p    = params        ,
                                                w    = matrix_weather,
                                                calf = calendar_fert ,
                                                calN = calendar_Ndep ,
                                                h    = days_harvest  ,
                                                n    = NDAYS          )
	 list_output[[s]]             <- output
     list_output_calibr_rows[[s]] <- sapply (1:ndata[s], function(i) 
         which( output[,2]==data_year[[s]][i] & output[,3]==data_doy[[s]][i] )
       )
     if(dim(database_mm[[s]])[1]>0) {
       list_output_mm_calibr_rows[[s]] <- sapply (1:ndata_mm[s], function(i) 
         which( output[,2]==data_mm_year[[s]][i] & output[,3]==data_mm_doy[[s]][i] ) )
     }
   }
   
   calc_logL_s <- function( s=1, output=output ) {
     output_calibr <- if(ndata[s]==1) {
                      output[list_output_calibr_rows[[s]], data_index[[s]]]
       } else { diag( output[list_output_calibr_rows[[s]], data_index[[s]]] ) }
     if(dim(database_mm[[s]])[1]>0) {
         output_mm_calibr <- if(ndata_mm[s]==1) {
                          output[list_output_mm_calibr_rows[[s]], data_mm_index[[s]]]
           } else { diag( output[list_output_mm_calibr_rows[[s]], data_mm_index[[s]]] ) }
     }
     logLs      <- flogL(output_calibr,data_value[[s]],data_sd[[s]])
     logLs_mm   <- 0
     if(dim(database_mm[[s]])[1]>0) {
       logLs_mm <- flogL_mm( output_mm_calibr, data_mm_value[[s]],
                             data_mm_min[[s]], data_mm_max[[s]], data_mm_cv[[s]] )
     }
     if( siteBC[s] ) logL_s <- logLs + logLs_mm else logL_s <- 0
	 return( logL_s )
   }

   calc_sum_logL <- function( list_output = list_output ) {
     sum_logL <- 0 ; for (s in 1:nSites) {
       sum_logL <- sum_logL + calc_logL_s( s, list_output[[s]] )
     }
   return( sum_logL )
   }
   
   logL0 <- calc_sum_logL( list_output )

## The first values for MaxL and MAP parameter vectors
   scparMaxL_BC <- scpValues_BC ; logMaxL <-             logL0
   scparMAP_BC  <- scpValues_BC ; logMAP  <- logPrior0 + logL0
   
################################################################################
## FUNCTION FOR LIKELIHOOD CALCULATION for each variable separately
   
  calc_statsi_s <- function( s=1, output=output ) {
     output_calibr <- if(ndata[s]==1) {
                      output[list_output_calibr_rows[[s]], data_index[[s]]]
       } else { diag( output[list_output_calibr_rows[[s]], data_index[[s]]] ) }
     if(dim(database_mm[[s]])[1]>0) {
         output_mm_calibr <- if(ndata_mm[s]==1) {
                          output[list_output_mm_calibr_rows[[s]], data_mm_index[[s]]]
           } else { diag( output[list_output_mm_calibr_rows[[s]], data_mm_index[[s]]] ) }
     }
     datais  <- cbind(data_index[[s]],data_year[[s]],data_doy[[s]],data_value[[s]])
	 outis   <- output_calibr
	 SSEis   <- sapply( 1:ndata[s], function(i) { (output_calibr[i]-data_value[[s]][i])^2 } )
     logLis  <- sapply( 1:ndata[s], function(i) {
       flogL(output_calibr[i],data_value[[s]][i],data_sd[[s]][i]) } )
	 datai_s <- datais
     outi_s  <- matrix( c(data_index[[s]],outis ), ncol=2 )
     SSEi_s  <- matrix( c(data_index[[s]],SSEis ), ncol=2 )
     logLi_s <- matrix( c(data_index[[s]],logLis), ncol=2 )
     if(dim(database_mm[[s]])[1]>0) {
       datais_mm <- cbind(data_mm_index[[s]],data_mm_year[[s]],data_mm_doy[[s]],data_mm_value[[s]])
	   outis_mm  <- output_mm_calibr
       SSEis_mm  <- sapply( 1:ndata_mm[s], function(i) { (output_mm_calibr[i]-data_mm_value[[s]][i])^2 } )
       logLis_mm <- sapply( 1:ndata_mm[s], function(i) {
         flogL_mm(output_mm_calibr[i],data_mm_value[[s]][i],
                  data_mm_min[[s]][i],data_mm_max[[s]][i],data_mm_cv[[s]][i]) } )
       datai_s   <- rbind( datais ,  datais_mm )
       outi_s    <- rbind( outi_s ,
                           matrix( c(data_mm_index[[s]],outis_mm ), ncol=2 ) )
       SSEi_s    <- rbind( SSEi_s ,
                           matrix( c(data_mm_index[[s]],SSEis_mm ), ncol=2 ) )
       logLi_s   <- rbind( logLi_s,
                           matrix( c(data_mm_index[[s]],logLis_mm), ncol=2 ) )
     }
     list_statsi_s <- list( datai_s=datai_s, outi_s=outi_s, SSEi_s=SSEi_s, logLi_s=logLi_s )
     return( list_statsi_s )
   }

################################################################################
## FUNCTIONS FOR PLOTTING
   
  plot_outputs_data_s <- function(
    isite        = 1,
    list_runs    = list_runs,
    nruns        = length(list_runs),
  	leg_title    = "LEGEND",
	  leg          = as.character(1:nruns),
    cols         = 1:nruns,
    lwds	       = rep(3,nruns),
	  ltys         = rep(1,nruns) ) {
   
  s <- isite  
    
  outputsMeasured      <- unique(data_index[[s]])   
  if(dim(database_mm[[s]])[1]>0) {
    outputsMeasured_mm <- unique(data_mm_index[[s]])
    noutputsMeasured   <- length(outputsMeasured) + length(outputsMeasured_mm)
  } else {
    noutputsMeasured   <- length(outputsMeasured)
  }
  nrowsPlots           <- ceiling(sqrt(noutputsMeasured+1))
  ncolsPlots           <- ceiling((noutputsMeasured+1)/nrowsPlots)
  par(mfrow=c(nrowsPlots,ncolsPlots),omi=c(0,0,0.5,0), mar=c(2, 2, 2, 1) )

  for (p in outputsMeasured) {
    datap     <- which( data_name[[s]] == as.character(outputNames[p]) )
    lcl       <- data_value[[s]][datap] - data_sd[[s]][datap]
    ucl       <- data_value[[s]][datap] + data_sd[[s]][datap]	
  	g_range_p <- range( sapply( 1:nruns, function(i){range(list_runs[[i]][,p])} ) )
    g_range   <- range( g_range_p, lcl, ucl )	
    plot( list_runs[[1]][,1], list_runs[[1]][,p], type='l',
          xlab="", ylab="", ylim=g_range, cex.main=1,
		      main=paste(outputNames[p]," ",outputUnits[p],sep=""),
          col=cols[1], lwd=lwds[1], lty=ltys[1] )
    if (nruns>=2) {
	  for (i in 2:nruns) {
	    points( list_runs[[i]][,1], list_runs[[i]][,p], type='l',
	            col=cols[i], lwd=lwds[i], lty=ltys[i] )
	  }
	}
    points( data_year[[s]][datap]+(data_doy[[s]][datap]-0.5)/366, data_value[[s]][datap],
            col='blue', lwd=1, cex=1 )
    arrows( data_year[[s]][datap]+(data_doy[[s]][datap]-0.5)/366, ucl,
            data_year[[s]][datap]+(data_doy[[s]][datap]-0.5)/366, lcl,
            col='blue', lwd=1, angle=90, code=3, length=0.05 )
  }
   
  if(dim(database_mm[[s]])[1]>0) {
    for (p in outputsMeasured_mm) {
      datap     <- which( data_mm_name[[s]] == as.character(outputNames[p]) )
      ucl       <- data_mm_max[[s]][datap]
      lcl       <- data_mm_min[[s]][datap]
      g_range_p <- range( sapply( 1:nruns, function(i){range(list_runs[[i]][,p])} ) )
      g_range   <- range( g_range_p, lcl, ucl )	
      plot( list_runs[[1]][,1], list_runs[[1]][,p], type='l',
            xlab="", ylab="", ylim=g_range, cex.main=1,
		        main=paste(outputNames[p]," ",outputUnits[p],sep=""),
            col=cols[1], lwd=lwds[1], lty=ltys[1] )
    if (nruns>=2) {
	  for (i in 2:nruns) {
	    points( list_runs[[i]][,1], list_runs[[i]][,p], type='l',
	            col=cols[i], lwd=lwds[i], lty=ltys[i] )
	  }
	}

      points( data_mm_year[[s]][datap]+(data_mm_doy[[s]][datap]-0.5)/366, data_mm_value[[s]][datap],
              col='blue', lwd=1, cex=1 )
      arrows( data_mm_year[[s]][datap]+(data_mm_doy[[s]][datap]-0.5)/366, ucl,
              data_mm_year[[s]][datap]+(data_mm_doy[[s]][datap]-0.5)/366, lcl,
              col='blue', lwd=1, angle=90, code=3, length=0.05 )
    }
  }
  plot(1,type='n', axes=FALSE, xlab="", ylab="")
  legend( "bottomright", title = leg_title, legend=leg,
          col=cols, lty=ltys, lwd=lwds )
  sitenames <- gsub( ".R", "", sub(".*BASGRA_","",sitesettings_filenames) )
  if( siteBC[s] ) siteType <- "CALIBRATION site" else siteType <- "TEST site"
  mtext( paste("SITE ",s," (",sitenames[s],": ",siteType,")",sep=""),
         side=3, line=1, outer=TRUE, cex=1, font=2)   
  }

##
  
BC_plot_outputs <- function( varNames=c("CLV","CST") ) {

  ios = sapply(1:length(varNames),function(i){ which(outputNames==varNames[i]) } )

  params_BC_ModePrior <- parmod_BC
  params_BC_MAP       <- unsc( scparMAP_BC , xmin=parmin_BC, xmax=parmax_BC )
  params_BC_MaxL      <- unsc( scparMaxL_BC, xmin=parmin_BC, xmax=parmax_BC )

  list_outputPriorMode <- vector( "list", nSites )
  list_outputMAP       <- vector( "list", nSites )
  list_outputMaxL      <- vector( "list", nSites )

  for (s in 1:nSites) {
    params          <- list_params       [[s]] ; matrix_weather <- list_matrix_weather[[s]]
    calendar_fert   <- list_calendar_fert[[s]] ; calendar_Ndep  <- list_calendar_Ndep [[s]] 
    days_harvest    <- list_days_harvest [[s]] ; NDAYS          <- list_NDAYS         [[s]]
    ip_BC_s         <- ip_BC_site        [[s]]
    icol_pChain_s   <- icol_pChain_site  [[s]]
    # Calculate model output for the prior mode
    params[ip_BC_s] <- params_BC_ModePrior[icol_pChain_s]
    outputPriorMode <- run_model(params,matrix_weather,
                                 calendar_fert,calendar_Ndep,
                                 days_harvest,NDAYS)
    # Calculate model output for the MAP parameter vector
    params[ip_BC_s] <- params_BC_MAP      [icol_pChain_s]
    outputMAP       <- run_model(params,matrix_weather,
                                 calendar_fert,calendar_Ndep,
                                 days_harvest,NDAYS)
    # Calculate model output for the MaxL parameter vector
    params[ip_BC_s] <- params_BC_MaxL     [icol_pChain_s]
    outputMaxL      <- run_model(params,matrix_weather,
                                 calendar_fert,calendar_Ndep,
                                 days_harvest,NDAYS)
    list_outputPriorMode[[s]] <- outputPriorMode
    list_outputMAP      [[s]] <- outputMAP
    list_outputMaxL     [[s]] <- outputMaxL
  }

  dev.set()
  pdf( paste('BC_outputs',format(Sys.time(),"_%H_%M.pdf"),sep=""),
      paper="a4r", width=11, height=8 )
  nrowsPlots <- ceiling( sqrt( nSites+1 ) )
  ncolsPlots <- ceiling( (nSites+1) / nrowsPlots )

  for( io in ios ) {
    par(mfrow=c(nrowsPlots,ncolsPlots),omi=c(0,0,0.5,0), mar=c(2, 2, 2, 1) )
    for (s in 1:nSites) {
      if( s==1 ) {
        g_range <- range( list_outputPriorMode[[s]][,io],
                          list_outputMAP      [[s]][,io],
                          list_outputMaxL     [[s]][,io] )
      } else {
        g_range <- range( g_range,
                          list_outputPriorMode[[s]][,io],
                          list_outputMAP      [[s]][,io],
                          list_outputMaxL     [[s]][,io] )	
      }
    }
    for (s in 1:nSites) {
      plot(   list_outputPriorMode[[s]][,1], list_outputPriorMode[[s]][,io], type='l',
              main=paste("Site",s), 
              xlab="", ylab="", ylim=g_range, cex.main=1,
              col=1, lwd=3, lty=1 )
      points( list_outputMAP      [[s]][,1], list_outputMAP      [[s]][,io], type='l',
	            col=2, lwd=3, lty=1 )
      points( list_outputMaxL     [[s]][,1], list_outputMaxL     [[s]][,io], type='l',
	            col=3, lwd=3, lty=1 )
    }
    plot(1,type='n', axes=FALSE, xlab="", ylab="")
    legend( "bottomright", title="LEGEND", legend=c("Prior","MAP","MaxL"),
            col=1:3, lty=rep(1,3), lwd=rep(3,3) )
    mtext( paste( outputNames[io], outputUnits[io] ),
           side=3, line=1, outer=TRUE, cex=1, font=2)   
  }
  
  dev.off()

}
