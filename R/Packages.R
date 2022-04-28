#' Esta funcao carrega os pacotes necessarios

#' @export

Packages = function(){

  if(!require(pacman)) install.packages("pacman")
    pacman::p_load(lubridate,optimx,hydroGOF,fitdistrplus,
                   e1071,evd,GEVcdn,PearsonDS,tidyverse,
                   devtools)
    
  if (!"smwrBase" %in% installed.packages()[,"Package"]){
    install.packages("smwrBase")
    devtools::install_github("USGS-R/smwrData")
    devtools::install_github("USGS-R/smwrBase")
    devtools::install_github("USGS-R/smwrGraphs")
  }

  cat("\014")

}
