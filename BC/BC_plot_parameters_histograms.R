## Preparing for plotting ##
 # Read the parameter names
   titles <- parname_BC

 # We will write the plots to a pdf file:
   pagew  <- 11 ; pageh <- 8
   pdf( paste("BC_parameters_histograms",format(Sys.time(),"_%H_%M.pdf"),sep=""),
        paper="A4r", width=pagew, height=pageh)
   
## Histogram plots ##
   nrowsPlots <- min(  8, ceiling( sqrt((np_BC+1)*pageh/pagew) ) )
   ncolsPlots <- min( 10, ceiling( (np_BC+1)/nrowsPlots ) )
   par( mfrow = c(nrowsPlots,ncolsPlots) )
   par( mar   =c(2, 2, 2, 1))
   nbreaks <- 20
   for (i in seq(1,np_BC)){
        hist( unsc( pChain[nBurnin:nChain,i], xmin=parmin_BC[i], xmax=parmax_BC[i] ),
              xlab="", ylab="", main=paste(titles[i],parsites_BC[i]), cex.main=1,
              breaks=nbreaks, freq=FALSE, xlim=c(parmin_BC[i],parmax_BC[i]) )
        parseq_BC <- seq(parmin_BC[i],parmax_BC[i],(parmax_BC[i]-parmin_BC[i])/100)
        points( parseq_BC, dbeta( (parseq_BC-parmin_BC[i])/(parmax_BC[i]-parmin_BC[i]),
                                  a_prior[i], b_prior[i] ) /
                           (parmax_BC[i]-parmin_BC[i]),
                type='l',col='red')
   }
   plot(1,type='n', axes=FALSE, xlab="", ylab="")
   plot_colors <- c("red","black")
   legend("bottomright", c("Prior","Post."),
          bty="n", lty=1, lwd=3, col=plot_colors, title = "LEGEND:")

## Closing ##
   dev.off()
