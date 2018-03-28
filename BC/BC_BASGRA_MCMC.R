## Running the MCMC ##
   nAccepted <- 0
   cat("Iteration","|","% Accepted","|","logPrior0","|","logL0","\n")
   for(j in 2:nChain)
     {
     # Give intermediate output to the screen for tracking the progress of the MCMC
       jint <- min(1000,round(nChain/20))
       if(j%%jint == 0) cat( "Iteration", j, "|", round(100*nAccepted/j),
                             "%", "|", logPrior0, "|", logL0, ", ", date(), "\n" )

     # After burn-in, check if we want to adjust the proposal distribution
       if(j==nBurnin) {
         if(adaptproposal_direction) {
           cor_pChain <- cor( unique( pChain[round(nBurnin/2):(nBurnin-1),] ) )
           for(r in 1:np_BC) {
             for(c in 1:np_BC) { vcovProp[r,c] <-
               cor_pChain[r,c] * sqrt(vcovProp[r,r]*vcovProp[c,c]) }
           }
         }
         if(adaptproposal_stepsize) {
           fAcc     <- nAccepted / nBurnin
           vcovProp <- vcovProp * min(1,fAcc/0.25)
         }
       }
     # Select the candidate parameter vector and calculate its prior probability
       sccandidatepValues_BC <- mvrnorm( n=1, scpValues_BC, vcovProp,
                                         tol=1e-6, empirical=FALSE)
       reflectionFromMin     <- pmin( 0, sccandidatepValues_BC   )
       reflectionFromMax     <- pmax( 0, sccandidatepValues_BC-1 )
       sccandidatepValues_BC <- sccandidatepValues_BC - 2*reflectionFromMin -
                                                        2*reflectionFromMax
       logPrior1             <- sum( dbeta(sccandidatepValues_BC,
                                     a_prior, b_prior, log=T) )
     # Unscale parameters, run the model for each site and calculate likelihood
       candidatepValues_BC <- unsc( sccandidatepValues_BC, xmin=parmin_BC, xmax=parmax_BC )
       for (s in 1:nSites) {
         params        <- list_params       [[s]] ; matrix_weather <- list_matrix_weather[[s]]
         calendar_fert <- list_calendar_fert[[s]] ; calendar_Ndep  <- list_calendar_Ndep [[s]] 
         days_harvest  <- list_days_harvest [[s]] ; NDAYS          <- list_NDAYS         [[s]]
         params[ ip_BC_site[[s]] ] <- candidatepValues_BC[ icol_pChain_site[[s]] ]
         output                    <- run_model( p    = params        ,
                                                 w    = matrix_weather,
                                                 calf = calendar_fert ,
                                                 calN = calendar_Ndep ,
                                                 h    = days_harvest  ,
                                                 n    = NDAYS          )
	     list_output[[s]]          <- output
       }
       logL1 <- calc_sum_logL( list_output )
   
     # Check whether the candidate parameter vector is accepted, and extend the parameter chain.
       logalpha         <- logPrior1 + logL1 - (logPrior0 + logL0)
       if (log(runif(1,0,1)) < logalpha) {
         scpValues_BC   <- sccandidatepValues_BC
         logPrior0      <- logPrior1
         logL0          <- logL1
         if (logL0 > logMaxL) {
           logMaxL      <- logL0
           scparMaxL_BC <- scpValues_BC }
         if ((logPrior0 + logL0) > logMAP) { 
           logMAP       <- logPrior0 + logL0
           scparMAP_BC  <- scpValues_BC }
         nAccepted <- nAccepted + 1
       }
       pChain[j,] <- scpValues_BC
     }