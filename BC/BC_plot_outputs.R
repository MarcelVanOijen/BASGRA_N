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
