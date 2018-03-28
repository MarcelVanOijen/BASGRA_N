## 1. INITIALISE MCMC ##
   source('BC/BC_BASGRA_MCMC_init_Saerheim_nutritive.R')

## 2. RUNNING THE MCMC ##
   source('BC/BC_BASGRA_MCMC.R')

## 3. PRINTING AND PLOTTING ##
   source('BC/BC_export_parModes.R')
   source('BC/BC_plot_parameters_traceplots.R')
   source('BC/BC_plot_parameters_histograms.R')
   source('BC/BC_plot_parameters_boxplots.R')
   source('BC/BC_plot_outputs_data.R')
   BC_plot_outputs( c("F_PROTEIN","F_DIGEST_DM","F_DIGEST_DMSH",
                      "F_WALL_LV","F_WALL_ST") )

## 4. SAVING WORKSPACE
   # imagefilename <- paste( "BC_Saerheim_nutritive",
   #                         format(Sys.time(),"%H_%M.Rdata"), sep="" )
   # save.image(file=imagefilename)
   