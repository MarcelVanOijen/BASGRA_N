## initialise_BASGRA_Saerheim_2001_12_Grindstad.R ##

## 1. GENERAL INITIALISATION ##
   dyn.load("BASGRA.DLL")
   source('initialisation/initialise_BASGRA_general.R')

## 2. SITE CONDITIONS ##
   year_start     <- as.integer(2001)
   doy_start      <- as.integer(115)
   NDAYS          <- as.integer(250)
   file_weather   <- 'weather/weather_01_Saerheim_format_bioforsk.txt'
   file_params    <- 'parameters/parameters.txt'
     parcol       <- 13  
   
## 3. CREATE WEATHER INPUT AND CALENDARS ##
   matrix_weather    <- read_weather_Bioforsk(year_start,doy_start,NDAYS,file_weather)
    calendar_fert[1,] <- c( 2001, 115, 140*1000/ 10000      ) # 140 kg N ha-1 applied on day 115
    calendar_fert[2,] <- c( 2001, 186,  80*1000/ 10000      ) #  80 kg N ha-1 applied on day 186
#    calendar_fert[3,] <- c( 2001, 123, 0*1000/ 10000      ) # 0 kg N ha-1 applied on day 123
   calendar_Ndep[1,] <- c( 1900,   1,  2*1000/(10000*365) ) #  2 kg N ha-1 y-1 N-deposition in 1900
   calendar_Ndep[2,] <- c( 1980, 366, 20*1000/(10000*365) ) # 20 kg N ha-1 y-1 N-deposition in 1980
   calendar_Ndep[3,] <- c( 2100, 366, 20*1000/(10000*365) ) # 20 kg N ha-1 y-1 N-deposition in 2100
   days_harvest [1,] <- c( 2001, 186 )
#    days_harvest [2,] <- c( 2000, 238 )
#    days_harvest [3,] <- c( 2000, 172 )
#    days_harvest [4,] <- c( 2000, 214)
#    days_harvest [5,] <- c( 2001, 171 )
#    days_harvest [6,] <- c( 2001, 219 )
   
   days_harvest      <- as.integer(days_harvest)
   
## 4. CREATE VECTOR "PARAMS" ##
   df_params      <- read.table(file_params,header=T,sep="\t",row.names=1)
   params         <- df_params[,parcol]
   
## 5. CREATE EMPTY MATRIX y ##
   y              <- matrix(0,NDAYS,NOUT)
   