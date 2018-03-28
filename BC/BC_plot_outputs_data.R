params_BC_ModePrior <-   parmod_BC
params_BC_MAP       <- unsc( scparMAP_BC , xmin=parmin_BC, xmax=parmax_BC )
params_BC_MaxL      <- unsc( scparMaxL_BC, xmin=parmin_BC, xmax=parmax_BC )

dev.set()
pdf( paste('BC_outputs_data',format(Sys.time(),"_%H_%M.pdf"),sep=""),
        paper="a4r", width=11, height=8 )

for (s in 1:nSites) {
  params          <- list_params       [[s]] ; matrix_weather <- list_matrix_weather[[s]]
  calendar_fert   <- list_calendar_fert[[s]] ; calendar_Ndep  <- list_calendar_Ndep [[s]] 
  days_harvest    <- list_days_harvest [[s]] ; NDAYS          <- list_NDAYS         [[s]]
  ip_BC_s         <- ip_BC_site        [[s]]
  icol_pChain_s   <- icol_pChain_site  [[s]]
# Calculate model output for the prior mode
  params[ip_BC_s] <- params_BC_ModePrior[icol_pChain_s]
  outputPriorMode <- run_model(params,matrix_weather,calendar_fert,calendar_Ndep,days_harvest,NDAYS)
# Calculate model output for the MAP parameter vector
  params[ip_BC_s] <- params_BC_MAP      [icol_pChain_s]
  outputMAP       <- run_model(params,matrix_weather,calendar_fert,calendar_Ndep,days_harvest,NDAYS)
# Calculate model output for the MaxL parameter vector
  params[ip_BC_s] <- params_BC_MaxL     [icol_pChain_s]
  outputMaxL      <- run_model(params,matrix_weather,calendar_fert,calendar_Ndep,days_harvest,NDAYS)
# Calculate model output for a sample from the posterior
# Take a sample (of size nSample) from the chain generated using MCMC
  nSample         <- 100
  nStep           <- (nChain-nBurnin) / nSample
  outputSample    <- array( 0, c(nSample,NDAYS,NOUT) )
  ii              <- 0   
  for (j in seq(nBurnin+nStep, nChain, nStep)) {
    ii <- ii+1
    params_j           <- unsc( pChain[j,], xmin=parmin_BC, xmax=parmax_BC )
    params[ip_BC_s]    <- params_j[icol_pChain_s]
    outputSample[ii,,] <- run_model(params,matrix_weather,calendar_fert,calendar_Ndep,days_harvest,NDAYS)
  } # end of sample loop
# Analyse the posterior output sample: calculate quantiles 5% and 95%
#   q5  <- sapply( 1:NOUT, function(i) sapply(1:NDAYS,function(j)quantile(outputSample[,j,i],0.05)) )
#   q95 <- sapply( 1:NOUT, function(i) sapply(1:NDAYS,function(j)quantile(outputSample[,j,i],0.95)) )
  q5  <- sapply( 1:NOUT, function(i) sapply(1:NDAYS,function(j)quantile(outputSample[,j,i],0.05,na.rm=T)) )
  q95 <- sapply( 1:NOUT, function(i) sapply(1:NDAYS,function(j)quantile(outputSample[,j,i],0.95,na.rm=T)) )

# Plot
  list_runs <- list( outputPriorMode, outputMAP, outputMaxL, q5, q95 )
  plot_outputs_data_s( isite       = s,
                       list_runs   = list_runs,
                       leg_title   = "BC",
                       leg         = c("Prior","MAP","MaxL"),
                       cols        = c( "red", "black", "green", "black", "black" ),
                       lwds        = c( 3, 3, 2, 2, 2 ),
                       ltys        = c( 1, 1, 1, 3, 3 ) )   
}
dev.off()   
