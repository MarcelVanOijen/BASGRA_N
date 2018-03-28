 # We will write the plots to a pdf file:
   pagew  <- 11 ; pageh <- 8
   pdf( paste("BC_parameters_boxplots",format(Sys.time(),"_%H_%M.pdf"),sep=""),
        paper="A4r", width=pagew, height=pageh)

   parname_BC_unique <- unique(parname_BC)
   np_BC_unique      <- length(parname_BC_unique)

## Boxplots ##
   nrowsPlots <- min(  8, ceiling( sqrt(np_BC_unique*pageh/pagew) ) )
   ncolsPlots <- min( 10, ceiling( np_BC_unique/nrowsPlots ) )
   par( mfrow = c(nrowsPlots,ncolsPlots) )
   par( mar   =c(2, 2, 2, 1))
   nbreaks <- 20
   for(h in 1:np_BC_unique) {
     cols <- which( parname_BC == parname_BC_unique[h] )
     i_thin <- round( seq( nBurnin, nChain, length.out=min(nChain,1000) ) )
     pars <- unsc( pChain[i_thin,cols], xmin=parmin_BC[cols], xmax=parmax_BC[cols] )
     boxplot( pars, main=parname_BC_unique[h],
              ylim=c(min(parmin_BC[cols]),max(parmax_BC[cols])) )
     abline( h=mean(parmod_BC[cols]), col="red" )
   }

## Closing ##
   dev.off()
