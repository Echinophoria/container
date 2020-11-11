conv_stO2 <- function(DO, Temperature, Pressure, Salinity, RH){
  sal_factor <- exp(-Salinity * (0.017674-10.754/(Temperature+273.15)^2))
  DO_st_press <- exp(-139.34411+(1.575701*10^5)/(273.15+Temperature)-(6.642308*10^7)/(273.15+Temperature)^2+(1.2438*10^10)/(273.15+Temperature)^3-(8.621949*10^11)/(273.15+Temperature)^4)
  VP_RH <- (10^(8.107131-1730.63/(235+Temperature)))*0.13332239 * RH / 100
  Theta <- 0.000975-(1.426*10^-5)*Temperature+(6.436*10^-8)*Temperature^2
  Press_factor <- ((Pressure/101.325)-(VP_RH/101.325))*(1-Theta*(Pressure/101.325))/(1-(VP_RH/101.325))*(1-Theta)
  DO_mg_l <- Press_factor*DO_st_press*sal_factor*DO/100
  return(DO_mg_l)
}
