## BC_BASGRA_MCMC_init_Saerheim_nutritive.R ##

## MCMC chain length
   nChain                  <- as.integer(1000) ; nBurnin <- as.integer(nChain/5)
   adaptproposal_direction <- FALSE
   adaptproposal_stepsize  <- TRUE

## FILE FOR PRIOR PARAMETER DISTRIBUTION
   file_prior    <- "parameters/parameters_BC_Saerheim_nutritive.txt"

## LIKELIHOOD FUNCTION ##
   source('BC/fLogL_Sivia.R')
   source('BC/fLogL_mm_Beta.R')
   
## SETTINGS FOR THE DIFFERENT CALIBRATION SITES (at least one site)
   sitesettings_filenames <- c("initialisation/initialise_BASGRA_Saerheim_2000_09_Grindstad.R",
                               "initialisation/initialise_BASGRA_Saerheim_2000_10_Grindstad.R",
                               "initialisation/initialise_BASGRA_Saerheim_2001_11_Grindstad.R",
                               "initialisation/initialise_BASGRA_Saerheim_2001_12_Grindstad.R",
                               "initialisation/initialise_BASGRA_Saerheim_2002_13_Grindstad.R",
                               "initialisation/initialise_BASGRA_Saerheim_2002_14_Grindstad.R")                                
   sitedata_filenames     <- c("data/data_calibration_Saerheim_2000_09_Grindstad_nutritive.txt",
                               "data/data_calibration_Saerheim_2000_10_Grindstad_nutritive.txt",
                               "data/data_calibration_Saerheim_2001_11_Grindstad_nutritive.txt",
                               "data/data_calibration_Saerheim_2001_12_Grindstad_nutritive.txt",
                               "data/data_calibration_Saerheim_2002_13_Grindstad.txt",
                               "data/data_calibration_Saerheim_2002_14_Grindstad_nutritive.txt")
   nSites     <- length(sitedata_filenames)
   sitelist   <- vector( "list", nSites )
   siteBC     <- rep( TRUE, nSites )
   siteBC[c(3,4)] <- FALSE

   cv_default    <- rep( 0.5 , nSites )
   cv_DM         <- rep( 0.05, nSites )
   cv_LAI        <- rep( 0.1 , nSites )
   cv_TILTOT     <- rep( 0.2 , nSites )
   cv_YIELD      <- rep( 0.05, nSites )
   cv_F_PROTEIN  <- rep( 0.3 , nSites )
   cv_F_DIGEST   <- rep( 0.15, nSites )
   cv_F_WALL     <- rep( 0.15, nSites )
   cv_YIELD      <- rep( 0.05, nSites )
   sd_LT50       <- rep( 5   , nSites )
   cv_mm_default <- rep( 0.2 , nSites )
#    cv_mm_FRTILG  <- rep( 0.2 , nSites )
   cv_mm_FRTILG  <- rep( 0.3 , nSites )
   
## PROPOSAL TUNING FACTOR  
   fPropTuning   <- 0.05 # This factor is used to modify Gelman's suggested average step length
                         # (2.38^2 / np_BC) which seems too big

## GENERAL INITIALISATION FOR MCMC
   source('BC/BC_BASGRA_MCMC_init_general.R')
