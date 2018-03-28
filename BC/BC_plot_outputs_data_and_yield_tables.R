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

# Creation of outputMAP for all sites and years

if(s>1) outputMAP_added<-outputMAP_allyears_allsites else outputMAP_added<-0
outputMAP_allyears_allsites<-(rbind(outputMAP_added,outputMAP))


}

dev.off()   

# Export of outputMAP for all sites and years





outputMAP_TIMCOM<-as.data.frame(cbind(outputMAP_allyears_allsites[,2],outputMAP_allyears_allsites[,3],outputMAP_allyears_allsites[,28],
                                      outputMAP_allyears_allsites[,5]/0.45,outputMAP_allyears_allsites[,10]/0.45, outputMAP_allyears_allsites[,14],
                                      outputMAP_allyears_allsites[,33],outputMAP_allyears_allsites[,34]))



rownames(outputMAP_TIMCOM)<-NULL

names(outputMAP_TIMCOM)[1] <- "Year"
names(outputMAP_TIMCOM)[2] <- "DOY"
names(outputMAP_TIMCOM)[3] <- "total above-ground biomass, dry matter"
names(outputMAP_TIMCOM)[4] <- "total leaf biomass, dry matter"
names(outputMAP_TIMCOM)[5] <- "total stem biomass, dry matter"
names(outputMAP_TIMCOM)[6] <- "LAI"     
names(outputMAP_TIMCOM)[7] <- "SLA"  
names(outputMAP_TIMCOM)[8] <- "Tiller density"

outputMAP_TIMCOM_clean<-outputMAP_TIMCOM[-1,]
rownames(outputMAP_TIMCOM_clean)<-NULL

harv_yield_control<-cbind(0,outputMAP_TIMCOM[,3]-88.5)
outputMAP_harvestable_yield<-outputMAP_TIMCOM[,3]-88.5
harv_yield_max<-max(harv_yield_control)

harv_yield_control2<-apply(harv_yield_control, 1, max)

outputMAP_TIMCOM_2<-as.data.frame(cbind(outputMAP_allyears_allsites[,2],outputMAP_allyears_allsites[,3],harv_yield_control2,
                                      outputMAP_allyears_allsites[,5]/0.45,outputMAP_allyears_allsites[,10]/0.45, outputMAP_allyears_allsites[,14],
                                      outputMAP_allyears_allsites[,33],outputMAP_allyears_allsites[,34]))

names(outputMAP_TIMCOM_2)[1] <- "Year"
names(outputMAP_TIMCOM_2)[2] <- "DOY"
names(outputMAP_TIMCOM_2)[3] <- "Harvestable biomass, dry matter"
names(outputMAP_TIMCOM_2)[4] <- "total leaf biomass, dry matter"
names(outputMAP_TIMCOM_2)[5] <- "total stem biomass, dry matter"
names(outputMAP_TIMCOM_2)[6] <- "LAI"     
names(outputMAP_TIMCOM_2)[7] <- "SLA"  
names(outputMAP_TIMCOM_2)[8] <- "Tiller density"

outputMAP_TIMCOM_2_clean<-outputMAP_TIMCOM_2[-1,]
rownames(outputMAP_TIMCOM_2_clean)<-NULL


write.table(outputMAP_TIMCOM_clean,
            file=paste("BASGRA","_",site_cultivar_name,"_","local",".csv",sep=""),
            row.names=F,,
            col.names=T,
            sep=","
)

write.table(outputMAP_TIMCOM_2_clean,
            file=paste("BASGRA","_",site_cultivar_name,"_","2","_","local",".csv",sep=""),
            row.names=F,,
            col.names=T,
            sep=","
)
