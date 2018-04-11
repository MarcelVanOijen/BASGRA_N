flogL_mm <- function(sims,data,data_min=0,data_max=1,cv=NULL) {
  scsims      <- (sims-data_min) / (data_max-data_min)
  scmode      <- (data-data_min) / (data_max-data_min)
  aplusb      <- 3
  a           <- scmode * (aplusb-2) + 1
  b           <- aplusb - a
  sum( dbeta(scsims,a,b,log=T) )
}
