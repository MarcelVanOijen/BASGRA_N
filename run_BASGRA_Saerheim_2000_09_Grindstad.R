source("initialisation/initialise_BASGRA_Saerheim_2000_09_Grindstad.R")

# output <- run_model( p=params, w=matrix_weather, calf = calendar_fert,
#   				           calN = calendar_Ndep, h=days_harvest, n=NDAYS )
# output <- run_model( params, matrix_weather, calendar_fert,
#                      calendar_Ndep, days_harvest, NDAYS )
output <- run_model()

plot_output( vars=c("DAVTMP","LAI","NSH","LT50")   )
plot_output( vars=outputNames[c(5:11,14)         ] )
plot_output( vars=outputNames[40:52              ] )
plot_output( vars=outputNames[53:61              ] )
plot_output( vars=outputNames[66:68              ] )
plot_output( vars=outputNames[69:72              ] )
plot_output( vars=outputNames[73:85              ] )
plot_output( vars=outputNames[c(9,96:97,57,61,66)] )

# SA("NFERTMULT",
#   file_init="initialisation/initialise_BASGRA_Saerheim_2000_09_Grindstad.R")
