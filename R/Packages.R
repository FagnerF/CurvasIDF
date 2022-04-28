#' Esta funcao carrega os pacotes necessarios

#' @export

Packages = function(){

  {# install packages
    if (!"lubridate" %in% installed.packages()[,"Package"]){
      install.packages("lubridate")
    }
    if (!"optimx" %in% installed.packages()[,"Package"]){
      install.packages("optimx")
    }
    if (!"hydroGOF" %in% installed.packages()[,"Package"]){
      install.packages("hydroGOF")
    }
    if (!"fitdistrplus" %in% installed.packages()[,"Package"]){
      install.packages("fitdistrplus")
    }
    if (!"e1071" %in% installed.packages()[,"Package"]){
      install.packages("e1071")
    }
    if (!"evd" %in% installed.packages()[,"Package"]){
      install.packages("evd")
    }
    if (!"GEVcdn" %in% installed.packages()[,"Package"]){
      install.packages("GEVcdn")
    }
    if (!"PearsonDS" %in% installed.packages()[,"Package"]){
      install.packages("PearsonDS")
    }
    if (!"tidyverse" %in% installed.packages()[,"Package"]){
      install.packages("tidyverse")
    }    
    if (!"smwrBase" %in% installed.packages()[,"Package"]){
      install.packages("smwrBase")
      devtools::install_github("USGS-R/smwrData")
      devtools::install_github("USGS-R/smwrBase")
      devtools::install_github("USGS-R/smwrGraphs")
    }
    {# Load packages
      library(lubridate)
      library(optimx)
      library(hydroGOF)
      library(fitdistrplus)
      library(e1071)      
      library(evd)
      library(GEVcdn)
      library(PearsonDS)
      library(tidyverse)
      library(smwrBase)      
    }
  }

  cat("\014")

}
