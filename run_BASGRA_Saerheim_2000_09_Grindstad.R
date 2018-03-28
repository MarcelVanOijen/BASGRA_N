source("initialisation/initialise_BASGRA_Saerheim_2000_09_Grindstad.R")
# output <- run_model( p=params, w=matrix_weather, calf = calendar_fert,
#   				           calN = calendar_Ndep, h=days_harvest, n=NDAYS )
# output <- run_model( params, matrix_weather, calendar_fert,
#                      calendar_Ndep, days_harvest, NDAYS )
output <- run_model()
plot_output( vars=outputNames[c(5:11,14)] )
plot_output( vars=outputNames[40:52     ] )
plot_output( vars=outputNames[53:61     ] )
plot_output( vars=outputNames[66:68     ] )
plot_output( vars=outputNames[69:72     ] )
plot_output( vars=outputNames[73:85     ] )

plot_output( vars=outputNames[c(9,96:97,57,61,66)] )

# SA("NFERTMULT",
#    file_init="initialisation/initialise_BASGRA_Saerheim_2000_09_Grindstad.R")

# cbind(outputNames[73:85],output[ 1,73:85])
# cbind(outputNames[73:85],output[85,73:85])
# cbind(outputNames[73:85],output[86,73:85])

# output_results <- as.data.frame( cbind( output[,c(2:3,28)],
#                                         output[,c(5,10)]/0.45,
#                                         output[,c(14,33:34)] ) )
# rownames(output_results) <- NULL
# names( output_results ) <- c( "Year", "DOY",
#                               "total above-ground biomass, dry matter",
#                               "total leaf biomass, dry matter",
#                               "total stem biomass, dry matter",
#                               "LAI", "SLA", "Tiller density" )
# write.table( output_results,
#              file="BASGRA_Saerheim_2000_09_Grindstad.csv",
#              row.names=F, col.names=T, sep="," )
